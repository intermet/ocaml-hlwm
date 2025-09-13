(* let main () = *)
(*   let%lwt () = Hlwm.Bar.main () in *)
(*   Lwt.wait () |> fst *)

(* let () = Lwt_main.run (main ()) *)
(* let current_tag = Hlwm.Herbstclient.current_tag () *)
let current_tag = int_of_string (Array.get Sys.argv 1)
let () = Hlwm.Bar.run current_tag
