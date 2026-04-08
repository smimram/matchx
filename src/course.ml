open Extlib
open Common

type t =
  {
    id : string;
    mutable nc : int; (* numerus clausus *)
    course_timeslot : Timeslot.t;
    mutable rank : string option; (* rank used to sort *)
  }

let to_string c = c.id

let has_numerus_clausus c = c.nc < max_int

let period c = Timeslot.period c.course_timeslot

let db = ref []

let of_string_opt s =
  let s = if String.count_char '_' s = 1 then s ^ "_EP" else s in
  List.find_opt (fun c -> c.id = s) !db

let of_string ?where ?(add=false) s =
  match of_string_opt s with
  | Some c -> c
  | None ->
    if not add || String.contains s ' ' then
      failwith ?where "%s: could not find course" s
    else
      (
        warning ?where "%s: could not find course, adding it" s;
        let c = { id = s; nc = max_int; course_timeslot = Timeslot.dummy; rank = None } in
        db := c :: !db;
        c
      )

let load dir =
  print_endline "Loading courses...";
  (* Load timeslots. *)
  let where = "timeslots.csv" in
  db :=
    CSV.of_file (Filename.concat dir where)
    |> List.map (fun row ->
        let id = row "Nouveau code de cours" in
        let course_timeslot =
          if row "Nouveaux créneaux" = "non programmé" then
            Timeslot.dummy
          else
            let period = row "Période" |> Period.of_string in
            let day = Day.of_string @@ row "Joursemaine" in
            let start = Time.of_string @@ row "Début" in
            let ending = Time.of_string @@ row "fin" in
            period, day, start, ending
        in
        { id; nc = max_int; course_timeslot; rank = None }
      );
  (* Checks *)
  List.iter_unordered_pairs (fun c1 c2 -> if c1.id = c2.id then warning ~where "%s: same course twice" c1.id) !db;
  (* Load numerus clausus. *)
  let where = "numerus_clausus.csv" in
  CSV.of_file (Filename.concat dir where)
  |> List.iter (fun row ->
      let id = row "id" in
      let c = of_string ~where ~add:true id in
      let nc = row "nc" in
      let nc = if List.mem nc [""; "Non applicable"; "Pas proposé"] then max_int else match int_of_string_opt nc with Some n -> n | None -> failwith "%s: invalid numerus clausus %s" id nc in
      let rank = row "rang" in
      let rank = if rank = "" then None else Option.some @@ row "rang" in
      c.nc <- nc;
      c.rank <- rank
    );
  (* Sort. *)
  db := List.sort compare !db;
  (* Print. *)
  List.iter (fun c ->
      print @@ Printf.sprintf "- %s: %s" c.id (if c.nc = max_int then "-" else string_of_int c.nc);
      (match c.rank with None -> () | Some r -> print_string @@ " / " ^ r);
      print_newline ()
    ) !db

let db () = !db

let compare = compare

let iter f = List.iter f @@ db ()

let map f = List.map f @@ db ()

let count () = List.length @@ db ()

let overlap c1 c2 = Timeslot.overlap c1.course_timeslot c2.course_timeslot
