open Linear

(**[transform prog] performs forward-mode AD on the program [prog].
   The result is a transformed program.

   Every function [f] in the program [prog] must have unrestricted
   arguments and unrestricted results only. (No linear arguments or
   linear results.)

   Every function or expression in the program [prog] must have
   exactly one unrestricted result. (No expressions with zero result
   or with more than one result.)

   The function [f] is still present in the transformed program. In
   addition, a new function [df] is created.

   For every argument [x] of [f], [df] takes an unrestricted argument
   [x] and a linear argument [dx]. Similarly, for every result [y] of
   [f], [df] returns an unrestricted result [y] and a linear result
   [dy].

   In the transformed program, a linear variable may be used an
   arbitrary number of times (that is, zero, one, or more times).
   This can be later fixed by using [DupDropInsertion]. *)
val transform: prog -> prog
