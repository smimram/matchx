(** Slots. *)

(** A block specifies a number of slots, ie courses that have to be taken within a specified list. *)

open Common

module Block = PA.Block

type t =
  {
    student : Student.t;
    pa : PA.t;
    block : PA.block;
    number : int; (** number of the course in the block *)
  }

let make ~student ~pa ~block ~number = { student; pa; block; number; }

let compare = compare

let student s = s.student

let pa s = s.pa

let mandatory s = Block.mandatory s.block

let period s = Block.period s.block

let to_string s = Printf.sprintf "%s/%s/%s/%d" (PA.to_string @@ pa s) (Period.to_string @@ Block.period s.block) (Block.to_string s.block) s.number
