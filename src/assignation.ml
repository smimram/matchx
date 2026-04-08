open Extlib
open Common

(** Favor stuents picking mandatory courses. *)
let favor_mandatory = true

(** Try to drop a course when two overlap. *)
let drop_overlaps = true

type slot = Slot.t
type course = Course.t

module List = struct
  include List

  (** Find the greatest element in a list which is above an element. *)
  let find_greatest_above lt x l =
    List.fold_left (fun cur y ->
        match cur with
        | None -> if lt x y then Some y else cur
        | Some x -> if lt x y then Some y else cur
      ) None l
end

(** Relations between slots and courses. *)
module Relation = struct
  module SlotMap = Map.Make(struct type t = slot let compare = Slot.compare end)
  module CourseMap = Map.Make(struct type t = course let compare = Course.compare end)

  type t =
    {
      mutable course : course SlotMap.t; (** course attributed to a slot *)
      mutable slots : slot list CourseMap.t; (** set of slots in a course *)
    }

  let empty () =
    {
      course = SlotMap.empty;
      slots = CourseMap.of_seq @@ List.to_seq @@ Course.map (fun c -> c, []);
    }

  (** Course assigned to a slot. *)
  let course rel s = SlotMap.find_opt s rel.course

  (** Slots assigned to a course. *)
  let slots rel c = CourseMap.find c rel.slots

  (** Slots assigned to a student. *)
  let slots_of_student rel st =
    SlotMap.bindings rel.course |> List.map fst |> List.filter (fun b -> Slot.student b = st)

  (** Add a relation between a slot and a course. *)
  let add rel s c =
    assert (not (SlotMap.mem s rel.course));
    rel.course <- SlotMap.add s c rel.course;
    let already = slots rel c in
    assert (not @@ List.mem s already);
    rel.slots <- CourseMap.add c (s::already) rel.slots

  (** Remove a relation between a slot and a course. *)
  let remove rel s c =
    assert (course rel s = Some c);
    assert (List.mem s @@ slots rel c);
    rel.course <- SlotMap.remove s rel.course;
    rel.slots <- CourseMap.add c (List.remove s @@ slots rel c) rel.slots

  let to_string rel =
    Student.map
      (fun s ->
         Printf.sprintf "- %s: %s\n" (Student.to_string s) (PA.to_string s.pa)
         ^
         let sl = slots_of_student rel s in
         List.map
           (fun p ->
              let sl =
                List.filter (fun sl -> Slot.period sl = p) sl
                |> List.map (fun sl -> sl, course rel sl)
                |> List.map (fun (sl, c) -> Printf.sprintf "%s (%s)" (Option.value ~default:"???" @@ Option.map Course.to_string c) (PA.Block.to_string sl.Slot.block))
                |> String.concat ", "
              in
              Printf.sprintf "  - %s: %s" (Period.to_string p) sl
           ) [1;2]
         |> String.concat "\n"
      )
    |> String.concat "\n"

  let to_string_by_course rel =
    Course.map
      (fun c ->
        let students = List.map Slot.student @@ slots rel c in
        (* Printf.sprintf "- %s (%d%s): %s" (Course.to_string c) (List.length students) (if Course.has_numerus_clausus c then " / " ^ string_of_int (Course.numerus_clausus c) else "") (String.concat ", " @@ List.map Student.to_string students) *)
        Printf.sprintf "- %s : %d%s" (Course.to_string c) (List.length students) (if Course.has_numerus_clausus c then " / " ^ string_of_int (Course.numerus_clausus c) else "")
      )
    |> String.concat "\n"

  let to_csv rel =
    Printf.sprintf "nom,prenom,id\n"
    ^
    String.concat "" @@ Student.map
      (fun s ->
         let courses =
           slots_of_student rel s
           |> List.map (course rel)
           |> List.filter_map Fun.id
           |> List.map Course.to_string
           |> String.concat ","
         in
         Printf.sprintf "%s,%s,%s,%s\n" s.Student.lastname s.Student.firstname s.Student.id courses
      )
end

(** This is the main function to compute assignations. *)
let compute () =
  print_endline "Computing assignations...";

  (* Relation between slots and students. *)
  let rel = Relation.empty () in
  (* Remaining preferences for each slot. This associates a queues of prefered courses (in order) for each slot. Two slots in the same block share the same queue (so that they will take different courses). *)
  let queues =
    Student.map
      (fun s ->
         let pa = s.pa in
         List.map
           (fun b ->
              let q = try List.assoc b s.choices with Not_found -> warning "%s: no choices in %s/%s" (Student.to_string s) (PA.to_string pa) (PA.Block.to_string b); [] in
              let q = Queue.of_list q in
              List.init (PA.Block.choose b) (fun i -> Slot.make ~student:s ~pa ~block:b ~number:i, q)
           ) (PA.blocks pa)
         |> List.flatten
      )
    |> List.flatten
    |> List.shuffle
  in
  (* Slots without an attributed course (yet). *)
  let single = Queue.of_list @@ List.map fst queues in
  (* Round count. *)
  let rounds = ref 0 in
  (* Drops count. *)
  let drops = ref 0 in

  while not (Queue.is_empty single) do
    (* Printf.printf "%d remaining assignations\r" (Queue.length single); *)
    incr rounds;

    (* Find a single slot. *)
    let slot = Queue.pop single in
    (* Queue associated to the slot. *)
    let queue = List.assoc slot queues in
    (* Student. *)
    let student = Slot.student slot in

    (* Drop an assignation of a slot to a course. *)
    let drop slot course explanation =
      explain "%s" explanation;
      incr drops;
      Relation.remove rel slot course;
      Queue.add slot single
    in

    (* Assign a slot to a course. *)
    let assign slot course =
      Relation.add rel slot course;

      if drop_overlaps then
        (* Make sure that we don't have a schedule conflict. *)
        let st = Slot.student slot in
        let sl = Relation.slots_of_student rel st |> List.map (fun sl -> sl, Relation.course rel sl) in
        let overlap c1 c2 =
          match c1, c2 with
          | Some c1, Some c2 -> Course.overlap c1 c2
          | _ -> false
        in
        List.iter_unordered_pairs
          (fun (sl1,c1) (sl2,c2) ->
             if overlap c1 c2 then
               let (sl, c), (_sl', c') = if sl2 < sl1 && not (Queue.is_empty (List.assoc sl2 queues)) then (sl1,c1),(sl2,c2) else (sl2,c2),(sl1,c1) in
               drop sl (Option.get c) (Printf.sprintf "%s: dropping %s because it overlaps with %s" (Student.to_string student) (Course.to_string @@ Option.get c) (Course.to_string @@ Option.get c'))
          ) sl
    in

    if Queue.is_empty queue then warning "%s: no more choices in %s" (Student.to_string student) (Slot.to_string slot) else
      (* Pick the prefered course for the slot. *)
      let course = Queue.pop queue in
      (* Slots already assigned to this course. *)
      let already = Relation.slots rel course in

      (* Are we below the numerus clausus? *)
      if List.length already < course.nc then assign slot course

      (* If the course does not rank the students then we keep things as is. *)
      else if course.rank = None then Queue.add slot single

      else
        (* Try to find someone to remove. *)
        let rank = Option.get course.rank in
        (* We favor students according to rankings. *)
        let lt slot1 slot2 =
          (Student.rank rank @@ Slot.student slot1) < (Student.rank rank @@ Slot.student slot2)
        in
        (* We favor mandatory slots. *)
        let lt slot1 slot2 =
          if not favor_mandatory then lt slot1 slot2 else
            match Slot.mandatory slot1, Slot.mandatory slot2 with
            | true, false -> true
            | false, true -> false
            | _ -> lt slot1 slot2
        in
        match List.find_greatest_above lt slot already with
        | Some kicked ->
          (* Printf.printf "kicking %s from %s\n" (Student.to_string @@ Slot.student kicked) (Course.to_string course); *)
          drop kicked course (Printf.sprintf "%s: kicked from %s by %s (%d vs %d in %s)" (Student.to_string @@ Slot.student kicked) (Course.to_string course) (Student.to_string student) (Student.rank rank @@ Slot.student kicked) (Student.rank rank student) rank);
          assign slot course;

        | None ->
          (* That did not work, try next choice. *)
          Queue.add slot single
  done;
  info "%d rounds (for %d slots)" !rounds (Student.count () * 8);
  info "%d drops" !drops;
  rel

(** Check that the assignation makes sense. *)
let check rel =
  Student.iter (fun st ->
      (* Slots for the student. *)
      let slots = Relation.slots_of_student rel st in
      (* Courses taken by the student. *)
      let courses = List.map (Relation.course rel) slots |> List.filter_map Fun.id in

      (* All slots are asssigned. *)
      List.iter (fun sl -> if Relation.course rel sl = None then warning "%s: unassigned slot %s" (Student.to_string st) (Slot.to_string sl)) slots;

      (* We have (at least) 4 courses per period *)
      let slots1 = List.filter (fun s -> Slot.period s = 1) slots in
      let slots2 = List.filter (fun s -> Slot.period s = 2) slots in
      if List.length slots1 < 4 then warning "%s: only %d courses in P1" (Student.to_string st) (List.length slots1);
      if List.length slots2 < 4 then warning "%s: only %d courses in P2" (Student.to_string st) (List.length slots2);

      (* We have distinct courses. *)
      List.iter_unordered_pairs (fun c1 c2 -> if c1 = c2 then warning "%s: assigned the course %s twice" (Student.to_string st) (Course.to_string c1)) courses;

      (* All courses are compatible. *)
      List.iter_unordered_pairs (fun c1 c2 -> if Course.overlap c1 c2 then warning "%s: the following courses overlap: %s and %s" (Student.to_string st) (Course.to_string c1) (Course.to_string c2)) courses;

      (* PA-specific validations. *)
      List.assoc_all st.pa !PA.validate |> List.iter (fun f -> f (Student.to_string st) courses)
    )
