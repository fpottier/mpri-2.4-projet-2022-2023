open Surface

(**This exception is raised by the two functions below. It carries an error
   message. *)
exception TestFailure of string

(**[test_forward_mode prog prog'] compares the Surface [prog] and the Surface
   program [prog'] that is obtained after performing just forward-mode AD. The
   behavior of the functions [f] and [df] is tested. *)
val test_forward_mode: prog -> prog -> unit

(**[test_reverse_mode prog prog'] compares the Surface [prog] and the Surface
   program [prog'] that is obtained after performing forward-mode AD,
   unzipping, and transposition. The behavior of the functions [f] and [tcdf]
   is tested. *)
val test_reverse_mode: prog -> prog -> unit
