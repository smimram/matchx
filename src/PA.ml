(** PA. *)

open Extlib
open Common

(** Blocks. *)
module Block = struct
  type t =
    {
      period : int;
      id : string;
      mandatory : bool; (** Whether the block is mantatory. *)
      choose : int; (** Number of courses to be chosen. *)
      courses : Course.t list; (** Courses available. *)
    }

  let to_string b = b.id

  (** Courses in the block. *)
  let courses b = b.courses

  (** Number of courses to be chosen in this block. *)
  let choose b = b.choose

  let period b = b.period

  let mandatory b = b.mandatory
end

type block = Block.t

type t =
  {
    id : string;
    mutable blocks : block list; (** Blocks. *)
    mutable dependencies : (Course.t * Course.t) list; (* a is a dependency for b *)
  }

let to_string pa = pa.id

let courses pa =
  List.map Block.courses pa.blocks |> List.flatten |> List.sort_uniq Course.compare

let db = ref []

(** Functions to validate the list of courses. *)
(* First argument is student name, for debugging. *)
(* This is not part of the PA in order to keep pa comparable (without having to compare functions). *)
let validate = ref ([] : (t * (string -> Course.t list -> unit)) list)

let of_string ?(add=false) s =
  match List.find_opt (fun pa -> pa.id = s) !db with
  | Some pa -> pa
  | None ->
    if add then
      (
        warning ~where:"pa.csv" "%s: could not find PA, adding it" s;
        let pa = { id = s; blocks = []; dependencies = [] } in
        db := pa :: !db;
        pa
      )
    else
      failwith "unknown pa: %s" s

let load dir =
  print_endline "Loading PA...";
  db :=
    CSV.of_file (Filename.concat dir "pa.csv")
    |> List.map (fun row ->
        let id = row "id" in
        let pa = { id; blocks = []; dependencies = [] } in
        pa
      );
  List.iter_unordered_pairs (fun pa1 pa2 -> if pa1.id = pa2.id then warning "%s: same PA twice" pa1.id) !db;
  print_endline "Loading rules...";
  let re_dep =
    let open Re in
    let word = rep @@ diff ascii space in
    compile @@ seq [bos; group word; str " prérequis pour "; group (seq [word; rep (seq [str " et "; word])]); eos]
  in
  let re_within =
    let open Re in
    compile @@ seq ([bos; group ~name:"obligatoire" @@ opt @@ str "obligatoire"; opt @@ str " / "; group ~name:"k" @@ rep digit ; str " parmi " ; group ~name:"n" @@ rep digit; eos])
  in
  let where = "rules.csv" in
  CSV.of_file (Filename.concat dir where)
  |> List.iter
    (fun row ->
       let pa = String.trim @@ row "PA" in
       let pa = of_string ~add:true pa in
       let period = row "Période" in
       let rule = row "règles" in
       let add_validation f = validate := (pa, f) :: !validate in
       (* Printf.printf "rule at %s: %s\n" period rule; *)
       match rule with
       | "prérequis" ->
         let dep = row "liste de cours" in
         let g = try Re.exec re_dep dep with Not_found -> failwith "invalid dependency : %s" dep in
         let a, b = Course.of_string ~where @@ Re.Group.get g 1, Re.Group.get g 2 in
         let b = Re.split (Re.compile (Re.str " et ")) b |> List.map (Course.of_string ~where) in
         let l = List.map (fun b -> a,b) b in
         pa.dependencies <- l @ pa.dependencies
       | "nombre de cours IME" ->
         assert (row "liste de cours" = "Au moins trois cours IME sur P1 & P2");
         let f student courses =
           let ime =
             courses
             |> List.map Course.to_string
             |> List.filter (String.starts_with ~prefix:"IME")
             |> List.length
           in
           if ime < 3 then warning "%s: only %d IME courses instead of 3" student ime
         in
         add_validation f
       | "projet P1 & P2" ->
         let re =
           let open Re in
           compile @@ seq [str "si "; group @@ rep @@ wordc; str " en P1 alors "; group @@ rep @@ wordc; str " en P2"]
         in
         let g = Re.exec re @@ row "liste de cours" in
         let p1 = Re.Group.get g 1 in
         let p2 = Re.Group.get g 2 in
         let f student courses =
           if List.mem (Course.of_string p1) courses && not (List.mem (Course.of_string p2) courses) then
             warning "%s: should take %s because %s was taken" student p2 p1
         in
         add_validation f
       | _ ->
         let period = Period.of_string period in
         let block = row "bloc" in
         let mandatory, choose, total =
           let gn = Re.group_names re_within in
           let g = Re.exec re_within rule in
           Re.Group.get g (List.assoc "obligatoire" gn) = "obligatoire",
           int_of_string @@ Re.Group.get g @@ List.assoc "k" gn,
           int_of_string @@ Re.Group.get g @@ List.assoc "n" gn
         in
         let courses = row "liste de cours" in
         let courses = String.split_on_char ';' courses |> List.map String.trim |> List.map (Course.of_string ~where ~add:true) in
         if List.length courses <> total then warning ~where "got %d courses instead of %d for %s" (List.length courses) total (row "PA");
         let block = { Block.id = block; period; mandatory; choose; courses} in
         pa.blocks <- block :: pa.blocks
    );
  (* Print the rules. *)
  List.iter (fun pa ->
      print @@ Printf.sprintf "- %s\n" (to_string pa);
      for p = 1 to 2 do
        print @@ Printf.sprintf "  - %s\n" (Period.to_string p);
        List.iter (fun b ->
            if Block.period b = p then
              print @@ Printf.sprintf "    - %s: %d in %s\n" (Block.to_string b) (Block.choose b) (String.concat "," @@ List.map Course.to_string b.Block.courses)
          ) @@ List.rev pa.blocks
      done
    ) !db;
  (* Check that rules are consistent. *)
  List.iter (fun pa ->
      for period = 1 to 2 do
        let p = List.filter (fun b -> b.Block.period = period) pa.blocks |> List.map Block.choose |> List.fold_left (+) 0 in
        if p <> 4 then warning "we have %d courses to choose in P%d in %s" p period (to_string pa)
      done
    ) !db;
  (* Check that every course belongs to a PA. *)
  Course.iter (fun c ->
      if not @@ List.exists (fun pa -> List.mem c @@ courses pa) !db then warning ~where "%s: does not belong to any PA" @@ Course.to_string c
    )
  (* List.iter (fun pa -> print_endline @@ to_string pa) !db; *)

let db () = !db

let iter f = List.iter f @@ db ()

let block_of_string pa period block =
  match List.find_opt (fun b -> b.Block.period = period && b.Block.id = block) pa.blocks with
  | Some b -> b
  | None -> failwith "could not find block %s in %s" block (to_string pa)

let blocks pa = pa.blocks

let map f = List.map f @@ db ()

let count () = List.length @@ db ()
