open Extlib

module Period = struct
  type t = int

  let to_string p = "P" ^ string_of_int p

  let of_string p =
    let p = match String.residual_opt ~prefix:"P" p with Some period -> period | None -> p in
    let p = match int_of_string_opt p with Some n -> n | None -> failwith "invalid period %s" p in
    p
end

module Day = struct
  type t = int

  let of_string d =
    match String.lowercase_ascii d with
    | "lundi" -> 0
    | "mardi" -> 1
    | "mercredi" -> 2
    | "jeudi" -> 3
    | "vendredi" -> 4
    | d -> failwith "unknown day %s" d
end

module Time = struct
  type t = float

  let of_string t =
    match String.split_on_char 'h' t with
    | [h;m] ->
      let h = float_of_string h in
      let m = float_of_string m /. 60. in
      h +. m
    | _ -> failwith "invalid time %s" t
end

module Timeslot = struct
  type t = Period.t * Day.t * Time.t * Time.t

  let dummy : t = -1, -1, 0., 0.

  let period (p,_,_,_) = p

  let overlap ts1 ts2 =
    let p1, d1, s1, t1 = ts1 in
    let p2, d2, s2, t2 = ts2 in
    ts1 <> dummy && ts2 <> dummy
    && p1 = p2 && d1 = d2
    && ((s1 <= s2 && s2 <= t1) || (s2 <= s1 && s1 <= t2))
end
