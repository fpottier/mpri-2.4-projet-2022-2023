open Surface

(**This exception is raised by the two functions below. It carries an error
   message. *)
exception TestFailure of string

(**[test_forward_mode prog prog'] compares the Surface program [prog]
   and the Surface program [prog'] that is obtained after performing
   just forward-mode AD. The behavior of the functions [f] and [df] is
   tested. *)
val test_forward_mode: prog -> prog -> unit

(**[test_unzip names prog prog'] compares the Linear program [prog]
   and the Linear program [prog'] that is obtained after performing
   unzipping of all the functions in [names]. The linear outputs
   of the functions [f] and [cf] are tested. *)
val test_unzip: names -> Linear.prog -> Linear.prog -> unit

(**[test_reverse_mode prog prog'] compares the Surface program [prog]
   and the Surface program [prog'] that is obtained after performing
   forward-mode AD, unzipping, and transposition. The behavior of the
   functions [f] and [tcdf] is tested. *)
val test_reverse_mode: prog -> prog -> unit
