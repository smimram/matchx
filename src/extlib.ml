let log = open_out "../output.log"

let print s =
  output_string log s;
  print_string s

let print_string = print

let print_newline () = print "\n"

let print_endline s = print s; print "\n"

(** Provide some explaination about the decisions. *)
let explain fmt =
  Printf.ksprintf (fun s -> print_endline ("XX: " ^ s)) fmt

let info ?where fmt =
  let where =
    match where with
    | Some w -> "in " ^ w ^ ": "
    | None -> ""
  in
  Printf.ksprintf (fun s -> print_endline ("II: " ^ where ^ s)) fmt

let warning ?where fmt =
  let where =
    match where with
    | Some w -> "in " ^ w ^ ": "
    | None -> ""
  in
  Printf.ksprintf (fun s -> print_endline ("WW: " ^ where ^ s)) fmt

let failwith ?where fmt =
  let where =
    match where with
    | Some w -> "in " ^ w ^ ": "
    | None -> ""
  in  
  Printf.ksprintf (fun s -> failwith (where ^ s)) fmt

module CSV = struct
  include Csv

  let of_file f =
    let f = open_in f in
    let c = of_channel ~has_header:true f in
    let c = Rows.input_all c in
    let find = Row.find in
    (* let find c k = match Row.find_opt c k with Some x -> x | None -> raise Not_found in *)
    Stdlib.close_in f;
    List.map find c

  let headers_of_file f =
    let f = open_in f in
    let c = of_channel ~has_header:true f in
    let h = Rows.header c in
    Stdlib.close_in f;
    h  
end

module List = struct
  include List

  let assoc_all x l =
    filter_map (fun (y,v) -> if x = y then Some v else None) l
  
  (* Backward compatibility *)
  let find_index p l =
    let rec aux n = function
      | x::l -> if p x then Some n else aux (n+1) l
      | [] -> None
    in
    aux 0 l

  let iter_unordered_pairs f l =
    let rec aux = function
      | x::l -> List.iter (f x) l; aux l
      | [] -> ()
    in
    aux l

  let shuffle l =
    List.map snd @@ List.sort Stdlib.compare @@ List.map (fun c -> Random.bits (), c) l

  let remove x l =
    List.filter (fun y -> x <> y) l
end

module String = struct
  include String

  let count_char c s =
    let ans = ref 0 in
    String.iter (fun c' -> if c' = c then incr ans) s;
    !ans

  (** Residual of a string after a prefix. *)
  let residual_opt ~prefix s =
    let n = String.length prefix in
    if starts_with ~prefix s then Some (String.sub s n (String.length s - n))
    else None
end

module Queue = struct
  include Queue

  let of_list l =
    Queue.of_seq @@ List.to_seq l
end

module File = struct
  let write fname s =
    let oc = open_out fname in
    output_string oc s;
    close_out oc
end
