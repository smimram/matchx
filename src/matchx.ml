open Extlib

let () =
  Printexc.record_backtrace true;
  (* Random.self_init (); *)
  let dir = "../data" in
  Course.load dir;
  PA.load dir;
  Student.load dir;
  print_newline ();
  print_endline "Welcome to matchx!";
  print_newline ();
  print_endline "We have";
  print @@ Printf.sprintf "- %d courses\n%!" (Course.count ());
  print @@ Printf.sprintf "- %d students\n%!" (Student.count ());
  print @@ Printf.sprintf "- %d PA\n%!" (PA.count ());
  print_newline ();
  let a = Assignation.compute () in
  print_endline @@ Assignation.Relation.to_string a;
  print_endline @@ Assignation.Relation.to_string_by_course a;
  Assignation.check a;
  File.write "../output.csv" @@ Assignation.Relation.to_csv a
