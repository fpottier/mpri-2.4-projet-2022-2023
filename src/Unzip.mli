open Linear

(**[transform fs prog] returns a transformed version of the program [prog]
   where every function [f] in the list [fs] has been transformed into
   three functions, named [uf], [lf] and [cf].

   In the program [prog], inside every function, two distinct local
   variables must never have the same name. This property can be
   established by using [Freshen.freshen].

   The function [uf] takes the unrestricted parameters of [f] and
   returns its unrestricted results plus a vector of unrestricted
   auxiliary results, whose number and type cannot be known in
   advance.

   The function [lf] takes the auxiliary results of [uf] plus the
   linear parameters of [f] and returns the linear results of [f].

   The function [cf] combines [uf] and [lf]. It takes the parameters
   of [f], both unrestricted and linear, calls [uf] and [lf] in
   succession, and returns the linear results of [f]. *)
val transform : names -> prog -> prog
