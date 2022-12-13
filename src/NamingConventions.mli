open Linear

(* This module defines our naming conventions regarding function names. *)

(* We assume that there are no collisions. If at some point, because of bad
   luck, two functions have the same name, then this will be detected by the
   Linear type-checker and the program will be rejected. Because this is only
   a prototype implementation, we consider this behavior acceptable. *)

(**[derivative] transforms a function name [f] into the name of the derivative
   function, [df]. This function is constructed by ForwardMode. *)
val derivative : name -> name

(**[unrestricted] transforms a function name [df] into the name of the
   unrestricted-fragment function, [udf]. This function is constructed by
   Unzip. *)
val unrestricted : name -> name

(**[linear] transforms a function name [df] into the name of the
   linear-fragment function, [ldf]. This function is constructed by Unzip. *)
val linear : name -> name

(**[combined] transforms a function name [df] into the name of the
   combined function [cdf], which calls [udf] and [ldf]. This function
   is constructed by Unzip. *)
val combined : name -> name

(**[transpose] transforms a function name [cdf] into the name of the
   transposed function, [tcdf]. This function is constructed by
   Transpose. *)
val transpose : name -> name
