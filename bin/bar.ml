(* let main () = *)
(*   let%lwt () = Hlwm.Bar.main () in *)
(*   Lwt.wait () |> fst *)

(* let () = Lwt_main.run (main ()) *)
let () = Hlwm.Bar.run ()
