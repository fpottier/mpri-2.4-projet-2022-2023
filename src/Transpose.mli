open Linear

(**[transform fs prog] produces a transformed program where every
   function [f] in the list [fs] has been transposed.

   Every function [f] in the list [fs] must have zero unrestricted
   outputs.

   The transposed function [tf] has the same unrestricted arguments and
   unrestricted results as the function [f]. (It has no unrestricted
   results.) Its linear arguments and linear results are those of [f],
   exchanged: linear arguments become linear results, and vice-versa.
   The order of arguments and results is preserved. *)
val transform: names -> prog -> prog
