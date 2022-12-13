(* [fresh, clear = make()] constructs a new generator of fresh names.
   The generator is accessible via the functions [fresh] and [clear].
   [fresh()] returns a fresh integer, while [clear()] resets the
   generator to its initial state. *)
val make : unit -> (unit -> int) * (unit -> unit)
