(** Students. *)

open Extlib
open Common

type t =
  {
    lastname : string;
    firstname : string;
    id : string; (** Unique identifier (matricule). *)
    mail : string;
    pa : PA.t; (** Chosen PA. *)
    mutable ranks : (string * int) list; (** Rank in various departments (this is a total order based on GPA). *)
    mutable motivations : string list; (** Freeform text explaining the choices performed (this is not used). *)
    mutable choices : (PA.block * Course.t list) list; (** Ordered list of choices in each block. *)
  }

let to_string s = s.firstname ^ " " ^ s.lastname

let rank r s = List.assoc r s.ranks

let db = ref []

let of_id id =
  try List.find (fun s -> s.id = id) !db
  with Not_found -> failwith "could not find student %s" id

let load dir =
  print_endline "Loading students...";
  let where = "choices.csv" in
  CSV.of_file (Filename.concat dir where)
  |> List.iter (fun row ->
      let id = row "Matricule" in
      let firstname = row "Prénom" in
      let lastname = row "Nom" in
      let mail = row "Email établissement" in
      let pa = PA.of_string @@ row "PA thématique" in
      let motivations = row "Motivations" in
      let period = Period.of_string @@ row "période" in
      let block = PA.block_of_string pa period @@ row "bloc" in
      let s =
        match List.find_opt (fun s -> s.id = id) !db with
        | Some s -> s
        | None ->
          let choices = [] in
          let s = { id; firstname; lastname; mail; pa; choices; ranks = []; motivations = [] } in
          db := s :: !db;
          s
      in
      s.motivations <- motivations :: s.motivations;
      let choices =
        List.init 25
          (fun i ->
             "choix " ^ string_of_int (i+1)
             |> row
             |> String.split_on_char ' '
             |> List.hd
          )
        |> List.filter_map (fun c ->
            if c = "" then None else
              match Course.of_string_opt c with
              | Some c -> Some c
              | None -> warning ~where "could not find course %s, dropping it" c; None
          )
      in
      s.choices <- (block,choices) :: s.choices
    );
  (* Ordre des motivations. *)
  List.iter (fun s -> s.motivations <- List.rev s.motivations) !db;
  (* Checks. *)
  List.iter (fun s -> if s.choices = [] then warning ~where "%s: no choices" s.id) !db;
  List.iter_unordered_pairs (fun s1 s2 -> if s1.id = s2.id then warning ~where "%s: same student twice" s1.id) !db;
  (* GPA. *)
  let where = "gpa.csv" in
  let ranks =
    CSV.headers_of_file (Filename.concat dir where)
    |> List.filter_map (String.residual_opt ~prefix:"rang ")
  in
  info "ranks: %s" (String.concat ", " ranks);
  CSV.of_file (Filename.concat dir where)
  |> List.iter (fun row ->
      try
        let id = row "id" in
        let s = of_id id in
        s.ranks <- List.map (fun r -> r, int_of_string @@ row ("rang " ^ r)) ranks;
      with e -> warning ~where "ignoring error: %s" @@ Printexc.to_string e
    );
  List.iter (fun s -> if s.ranks = [] then warning ~where "%s: no ranks" @@ to_string s) !db;
  (* Sort. *)
  db := List.sort compare !db;
  (* Print. *)
  print_string "\n# Students (with number of ranked courses)\n\n";
  List.iter (fun s ->
      let courses = s.choices |> List.map snd |> List.flatten |> List.length in
      print_string @@ Printf.sprintf "- %s: %d\n" (to_string s) courses;
      (* Printf.printf "  %s\n" @@ String.concat ", " @@ List.map (fun (r,v) -> Printf.sprintf "%s: %d" r v) s.ranks *)
    ) !db

let db () = !db

let compare = compare

let count () = List.length @@ db ()

let map f = List.map f @@ db ()

let iter f = List.iter f @@ db ()

let filter p = List.filter p @@ db ()

(** Students who might choose a given course. *)
let might_choose c =
  filter (fun s -> List.exists (fun (_,courses) -> List.mem c courses) s.choices) |> List.sort compare

(*
(** Generate the list of students to rank. *)
let to_rank () =
  print_endline "Generating lists of students to rank...";
  Course.iter (fun c ->
      if Course.has_numerus_clausus c then
        let oc = open_out @@ Printf.sprintf "../torank/%s.csv" @@ Course.to_string c in
        output_string oc "id,firstname,lastname,mail,pa,motivations\n";
        List.iter (fun s ->
            let motivations =
              s.motivations
              |> List.map (Re.replace ~all:true (Re.compile @@ Re.str "\"") ~f:(fun _ -> "\"\""))
              |> List.map (Printf.sprintf "\"%s\"")
              |> String.concat ","
            in
            output_string oc (
              Printf.sprintf "%s,%s,%s,%s,%s,%s\n"
                s.id
                s.firstname
                s.lastname
                s.mail
                (PA.to_string s.pa)
                motivations
            )
          ) (might_choose c);
        close_out oc
    )
*)
