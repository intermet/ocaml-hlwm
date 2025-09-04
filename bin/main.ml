open! Hlwm.H

let buffer = Buffer.create 50
let () = eval buffer Nil cmd
let () = Stdio.print_endline (Buffer.contents buffer)
