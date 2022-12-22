(* -------------------------------------------------------------------------- *)

(* Operations on vectors of reals. *)

open Real

type vector =
  real array

let length : vector -> int =
  Array.length

let (@) : vector -> vector -> vector =
  Array.append

let project i (y : vector) : real =
  assert (let m = length y in 0 <= i && i < m);
  y.(i)

let update j (x : vector) (x'_j : real) : vector =
  assert (let n = length x in 0 <= j && j < n);
  let x' = Array.copy x in
  x'.(j) <- x'_j;
  x'

(* -------------------------------------------------------------------------- *)

(* Operations on matrices. *)

type matrix =
  real array array

let height : matrix -> int =
  Array.length

let width (matrix : matrix) : int =
  assert (height matrix > 0);
  let n = Array.length matrix.(0) in
  assert (Array.for_all (fun row -> Array.length row = n) matrix);
  n

let matrix_has_dimensions m n matrix =
  height matrix = m &&
  (m = 0 || width matrix = n)

let init_matrix m n f : matrix =
  let matrix = Array.make_matrix m n zero in
  assert (matrix_has_dimensions m n matrix);
  for i = 0 to m - 1 do
    for j = 0 to n - 1 do
      matrix.(i).(j) <- f i j
    done
  done;
  matrix

let sum n (f : int -> real) : real =
  let s = ref zero in
  for j = 0 to n - 1 do
    s := !s +. f j
  done;
  !s

let product m n (matrix : matrix) (x : vector) : vector =
  assert (matrix_has_dimensions m n matrix);
  assert (length x = n);
  Array.init m (fun i ->
    sum n (fun j ->
      matrix.(i).(j) *. x.(j)
    )
  )

let transpose m n (matrix : matrix) : matrix =
  assert (matrix_has_dimensions m n matrix);
  let result = init_matrix n m (fun i j ->
    matrix.(j).(i)
  ) in
  assert (matrix_has_dimensions n m result);
  result

(* -------------------------------------------------------------------------- *)

(* Numeric derivation. *)

(* Let [f] be a function of type [R → R]. Then, [derivative f x] is a numeric
   approximation of the derivative of [f] at [x]. *)

(* We do not use the division operator [/.] because we have set it up to fail
   if the divisor appears to be near zero. We use multiplication instead. *)

let dx =
  of_float 1e-10

let inv_two_dx =
  of_float 0.5e10

let derivative (f : real -> real) (x : real) : real =
  (f(x +. dx) -. f(x -. dx)) *. inv_two_dx

(* Let [f] be a function of type [R^n → R^m]. Then, [jacobian m n f x] is
   a numeric approximation of the Jacobian matrix of [f] at [x], which we
   write [Jf(x)].

   The Jacobian matrix is the matrix of the partial derivatives of [f] at [x].
   Its dimension is m x n; its entries are ∂f_i/∂x_j(x).

   https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant *)

let jacobian m n (f : vector -> vector) (x : vector) : matrix =
  init_matrix m n (fun i j ->
    (* Define f_i at x as a function of x_j. *)
    let f_i x_j = project i (f (update j x x_j)) in
    (* Compute an approximation of its derivative at x_j. *)
    derivative f_i x.(j)
  )

(* -------------------------------------------------------------------------- *)

(* Determining whether two linear values are close. *)

open SurfaceInterpreter

let rec close_values (observed : value) (expected : value) =
  match observed, expected with
 | VReal x1, VReal x2 ->
     close x1 x2
 | VTuple vs1, VTuple vs2 ->
     assert (List.compare_lengths vs1 vs2 = 0);
     List.for_all2 close_values vs1 vs2
 | (VReal _ | VTuple _), _ ->
     assert false

(* Determining whether two vectors of real numbers are close. *)

let close (observed : vector) (expected : vector) =
  assert (length observed = length expected);
  Array.for_all2 close observed expected

(* -------------------------------------------------------------------------- *)

(* Choices. *)

open Surface

(* These functions are used to pick vectors [x] and perturbations [dx]. *)

(* In [pick_real], we pick relatively few different values, namely 3,
   because the complexity of testing is exponential. In forward mode,
   for instance, the complexity of testing a function of [n] inputs is
   proportional to 3^{2n}. *)

let reals =
  List.map of_float [ -2.0; 0.0; 2.0 ]

let pick_real (k : real -> 'a) : 'a =
  List.iter k reals

let rec pick_list n (k : real list -> 'a) : 'a =
  if n = 0 then
    k []
  else
    pick_real @@ fun x ->
    pick_list (n - 1) @@ fun xs ->
    k (x :: xs)

let pick_vector n (k : vector -> 'a) : 'a =
  pick_list n (fun xs -> k (Array.of_list xs))

let rec pick_value ty (k : value -> 'a) : 'a =
  match ty with
  | TUnknown ->
      invalid_arg "pick_value: missing type information"
  | TReal ->
      pick_real (fun x -> k (VReal x))
  | TTuple tys ->
      pick_values tys (fun vs -> k (VTuple vs))

and pick_values tys k =
  match tys with
  | [] ->
      k []
  | ty :: tys ->
      pick_value ty @@ fun v ->
      pick_values tys @@ fun vs ->
      k (v :: vs)

(* -------------------------------------------------------------------------- *)

(* Printing. *)

open Printf
open SurfacePrinter

let show_real =
  Real.to_string

let show_wildcard _x =
  "_"

(* A comma-separated list. *)

let show_list show xs =
  String.concat ", " (List.map show xs)

let parens s =
  "(" ^ s ^ ")"

let show_raw_vector (x : vector) =
  show_list show_real (Array.to_list x)

let show_vector (x : vector) =
  parens (show_raw_vector x)

let show_raw_wildcards (x : vector) =
  show_list show_wildcard (Array.to_list x)

let show_vector_half1 (x : vector) =
  parens (
    if length x = 0 then "" else
      show_raw_vector x ^ ", " ^ show_raw_wildcards x
  )

let show_vector_half2 (x : vector) =
  parens (
    if length x = 0 then "" else
      show_raw_wildcards x ^ ", " ^ show_raw_vector x
  )

let show_tprogs (prog, _) (prog', _) =
  "Original program:\n\n" ^
  PPrintExtra.to_string (print_program prog) ^
  "Transformed program:\n\n" ^
  PPrintExtra.to_string (print_program prog')

let show_linprogs prog prog' =
  "Original program:\n\n" ^
  PPrintExtra.to_string (LinearPrinter.print_program prog) ^
  "Transformed program:\n\n" ^
  PPrintExtra.to_string (LinearPrinter.print_program prog')

let show_value =
  SurfaceInterpreter.print_value

let show_values vs =
  show_list show_value vs

(* -------------------------------------------------------------------------- *)

(* We test at the Surface level, using the Surface interpreter. *)

(* We might have chosen instead to test at the Linear level. That
   would not make much of a difference. We just need to be careful
   because the translation from Linear back to Surface introduces
   a tuple when a function has more than one result. *)

(* Our differentiation tests only check functions of type R^n → R^m. *)

open SurfaceTypeChecker

(* Let reals m stand for:
   - the type real, if m = 1;
   - the m-tuple type [real; ...; real], if m <> 1. *)

let reals m =
  if m = 1 then TReal else TTuple (List.init m (fun _ -> TReal))

(* Suppose the function f has type R^n → R^m.

   In Surface,
   f takes n arguments of type real
   and returns a result of type reals m.

   In Linear, after forward-mode AD,
   f takes n unrestricted arguments of type real
   and returns one unrestricted result of type reals m.

   In Linear,
   df takes n unrestricted arguments of type real
        and n linear arguments of type real
   and returns one unrestricted result of type reals m
           and one linear result of type reals m.

   If df is translated back to Surface
     (which is the case when testing forward-mode AD),
   then in Surface
   df takes 2n arguments of type real
   and returns a result of type [reals m; reals m].

   In Linear, after unzipping,
   cdf takes n unrestricted arguments of type real
         and n linear arguments of type real
         (just like df)
   and returns one linear result of type reals m.

   In Linear, after transposing,
   tcdf takes n unrestricted arguments of type real
          and one linear argument of type reals m
   and returns n linear arguments of type real.

   After tcdf is translated back to Surface
     (which is the case when testing reverse-mode AD),
   then in Surface
   tcdf takes n arguments of type real
          and one argument of type reals m
   and returns one result of type reals n. *)

(* -------------------------------------------------------------------------- *)

(* Recognition of certain types. *)

(* [is_real] tests if a type is [real]. *)

let is_real ty : bool =
  ty = TReal

(* [are_reals] tests if a list of types contains [real] types only. *)

let are_real tys : int option =
  if List.for_all is_real tys then Some (List.length tys) else None

(* [is_reals ty] tests if the type [ty] is of the form [reals m],
   and if so, returns [m]. *)

let is_reals ty : int option =
  match ty with
  | TUnknown ->
      assert false
  | TReal ->
      Some 1
  | TTuple [_ty] ->
      None
  | TTuple tys ->
      are_real tys

(* [is_reals_reals fty] determines whether the Surface function type [fty]
   corresponds to the type of a mathematical function of type R^n → R^m. *)

let is_reals_reals fty : (int * int) option =
  let domain, codomain = fty in
  match are_real domain, is_reals codomain with
  | Some n, Some m ->
      Some (n, m)
  | _, _ ->
      None

(* -------------------------------------------------------------------------- *)

(* Conversions. *)

(* [inject] turns a vector of reals into a list of Surface expressions. *)

let inject (x : vector) : expr list =
  lits (Array.to_list x)

(* [inject_reals m] turns a vector of reals into a Surface expression of type
   [reals m]. *)

let inject_reals m (x : vector) : expr =
  assert (length x = m);
  if m = 1 then
    lit x.(0)
  else
    TupleIntro (inject x)

(* [project_reals m] turns a Surface value of type [reals m] into a vector of
   reals. *)

let project_reals m (y : value) : vector =
  match y with
  | VTuple vs ->
      assert (m = List.length vs);
      assert (m <> 1);
      Array.of_list (List.map (as_real dummy) vs)
  | v ->
      assert (m = 1);
      Array.make 1 (as_real dummy v)

(* [unpair] turns a tuple of arity 2 into a pair of values. *)

let unpair (v : value) : value * value =
  match v with
  | VTuple [v1; v2] ->
      v1, v2
  | _ ->
      assert false

(* [well_typed_value ty v] determines whether the value [v]
   admits the type [ty]. *)

let rec well_typed_value ty v =
  match ty, v with
  | TUnknown, _ ->
      invalid_arg "is_well_typed"
  | TReal, VReal _ ->
      true
  | TTuple tys, VTuple vs ->
      well_typed_values tys vs
  | (TReal | TTuple _), (VReal _ | VTuple _) ->
      false

and well_typed_values tys vs =
  List.for_all2 well_typed_value tys vs

(* -------------------------------------------------------------------------- *)

(* Evaluating Surface expressions. *)

(* A typed program is a pair of a program and its function type environment. *)

type tprog =
  prog * fenv

(* Expressions are evaluated in an empty environment. *)

let empty =
  Env.empty

(* [interpret_f tprog m n f] interprets the function [f] as a mathematical
   function of a vector in R^n to a vector in R^m. *)

let interpret_f (tprog : tprog) m n (f : name) (x : vector) : vector =
  let prog, fenv = tprog in
  assert (length x = n);
  (* Construct a function call. *)
  let e = FunCall (f, inject x) in
  (* Check that it is well-typed. *)
  check_expr fenv e (reals m);
  (* Evaluate it. *)
  project_reals m (eval prog empty e)

(* [interpret_l tprog f] interprets the linear function [f] as
   a mathematical function from (unrestricted and linear) input values
   to output values. *)

include struct
  open Linear
  open LinearInterpreter

  let interpret_l prog (f : name) (uvs : values) (lvs : values) : LinearInterpreter.result =
    let ubs = List.mapi (fun i _ -> U (string_of_int i), ()) uvs in
    let lbs = List.mapi (fun i _ -> L (string_of_int i), ()) lvs in
    let env =
      bind_many (Env.empty, Env.empty) (ubs, lbs) (uvs, lvs) in
    (* Construct a function call. *)
    let e : expr = FunCall (f, List.map fst ubs, List.map fst lbs) in
    (* Evaluate it. *)
    eval prog env e
end
(* [interpret_df tprog m n df] interprets the function [df] as a mathematical
   function of a pair of vectors in R^n to a pair of vectors in R^m. *)

type vectors =
  vector * vector

let interpret_df (tprog : tprog) m n (df : name) (x, dx : vectors) : vectors =
  let prog, fenv = tprog in
  assert (length x = n);
  assert (length dx = n);
  (* Construct a function call. *)
  let e = FunCall (df, inject (x @ dx)) in
  (* Check that it is well-typed. *)
  check_expr fenv e (TTuple [reals m; reals m]);
  (* Evaluate it. *)
  let y, dy = unpair (eval prog empty e) in
  project_reals m y, project_reals m dy

(* [interpret_tcdf tprog m n tcdf] interprets the function [tcdf] as a
   mathematical function of a vector in R^n and a vector in R^m to
   a vector in R^n. *)

let interpret_tcdf tprog m n (tcdf : name) (x, dy : vectors) : vector =
  let prog, fenv = tprog in
  assert (length x = n);
  assert (length dy = m);
  (* Construct a function call. *)
  let (@) = List.append in
  let e = FunCall (tcdf, inject x @ [inject_reals m dy]) in
  (* Check that it is well-typed. *)
  check_expr fenv e (reals n);
  (* Evaluate it. *)
  project_reals n (eval prog empty e)

(* -------------------------------------------------------------------------- *)

(* Utilities. *)

let robustly f x =
  try f x with NearZero -> ()

exception TestFailure of string

let fail mode tprog tprog' format =
  ksprintf (fun msg ->
    let msg =
      sprintf "A problem was detected while testing %s-mode AD.\n\n" mode ^
      show_tprogs tprog tprog' ^
      msg
    in
    raise (TestFailure msg)
  ) format

let fail_lin test_descr prog prog' format =
  ksprintf (fun msg ->
    let msg =
      sprintf "A problem was detected while testing %s.\n\n" test_descr ^
      show_linprogs prog prog' ^
      msg
    in
    raise (TestFailure msg)
  ) format

(* -------------------------------------------------------------------------- *)

(* Testing forward-mode AD on one function [f]. *)

(* Suppose f is a function of type R^n → R^m, and suppose df is its
   forward-mode derivative, of type R^2n → R^2m.

   If x ∈ R^n is a point
   and dx ∈ R^n is a perturbation,
   we expect df(x, dx)
   to produce (y, dy)
   where y = f(x)
   and dy = Jf(x).dx
   where Jf(x) is the Jacobian matrix of f at x.

   The matrix Jf(x) is the matrix of the partial derivatives of f at x:
   its dimension is m * n;
   is entries are ∂f_i/∂x_j(x).

   The product Jf(x).dx is a matrix/vector product.

   https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant *)

(* We test the above property with several values of x and dx, so
   this test is quite costly, especially when n is large. *)

(* If, for some value of [x], we hit a possible division by zero, we skip this
   test. We do not attempt to determine whether the division by zero is real
   or is an artifact of limited precision. We do not attempt to determine
   whether the division by zero arises while evaluating [f], while evaluating
   the Jacobian of [f], or while evaluating [df]. Division is a pain. *)

let test_forward_mode tprog tprog' m n f df =
  let fail format = fail "forward" tprog tprog' format in
  on_failure (fail "%s") @@ fun () ->

  (* Pick a point x ∈ R^n. *)
  pick_vector n @@ robustly @@ fun x ->

  (* Evaluate y = f(x). *)
  let y = interpret_f tprog m n f x in

  (* The result of f(x) should be the same in both programs. *)
  let y' = interpret_f tprog' m n f x in
  if not (close y' y) then
    fail "The function %s is not preserved.\n\
          Observed behavior:\n  %s%s = %s\n\
          Expected behavior:\n  %s%s = %s\n"
      f
      f (show_vector x) (show_vector y')
      f (show_vector x) (show_vector y);

  (* Compute the Jacobian matrix Jf(x). *)
  let jfx : matrix = jacobian m n (interpret_f tprog m n f) x in

  (* Pick a perturbation dx ∈ R^n (which need not be small). *)
  pick_vector n @@ fun dx ->

  (* Evaluate (y', dy') = df(x, dx). *)
  let y', dy' = interpret_df tprog' m n df (x, dx) in

  (* Check y' = y. *)
  if not (close y' y) then
    fail "The function %s seems wrong (incorrect value component).\n\
          Observed behavior:\n  %s%s = %s\n\
          Expected behavior:\n  %s%s = %s\n"
      df
      df (show_vector (x @ dx)) (show_vector (y' @ dy'))
      df (show_vector (x @ dx)) (show_vector_half1 y);

  (* Check dy' = Jf(x) . dx. *)
  let dy = product m n jfx dx in
  if not (close dy' dy) then
    fail "The function %s seems wrong (incorrect derivative component).\n\
          Observed behavior:\n  %s%s = %s\n\
          Expected behavior:\n  %s%s = %s\n"
      df
      df (show_vector (x @ dx)) (show_vector (y' @ dy'))
      df (show_vector (x @ dx)) (show_vector_half2 dy)

(* -------------------------------------------------------------------------- *)

(* Testing the unzipping transform on one function [f]. *)

(* For each linear function f of type (tyus; tyls) → (tyu's; tyl's)
   unzipping produces a function
     cf : (tyus; tyl's) → (; tyl's)

   We check that this function gives the same results
   as the linear output of the original function f. *)

let test_unzip prog prog' f f_ty uf cf =
  let fail format = fail_lin "unzip" prog prog' format in
  on_failure (fail "%s") @@ fun () ->

  let ((utys, ltys), (out_utys, out_ltys)) = f_ty in

  (* Pick input values (uxs; lxs) *)
  pick_values utys @@ robustly @@ fun uxs ->
  pick_values ltys @@ robustly @@ fun lxs ->

  (* Evaluate (uys; lys) = f(uxs; lxs). *)
  let (uys, lys) = interpret_l prog f uxs lxs in
  if not (well_typed_values out_utys uys
          && well_typed_values out_ltys lys)
  then fail "test_unzip (%s): type error" f;

  (* Note: we could also call [uf] to get the unrestricted outputs of
     the unzipping of [f], and compare that to the [uys]. But [uf]
     does not only contain unrestricted outputs, it also has a trace
     of intermediate computations worth reusing.

     There is no easy way to separate the trace from the output
     values -- how to do this may depend on calling conventions chosen
     differently by each student. So we gave up on checking the
     unrestricted outputs for now.

     The easier approach to restore testing would be to extend the
     specification of Unzip to require producing a variant of [uf]
     that does not include the trace information. *)
  ignore uf;

  (* Evaluate lys' = cf(uxs; lxs) *)
  let (li, lys') = interpret_l prog' cf uxs lxs in
  if li <> [] then
    fail "The function %s should not return unrestricted outputs" cf;

  if not (well_typed_values out_ltys lys')
  then fail "test_unzip (%s): type error" f;

  (* (uys; lys) should be (uys'; lys') *)
  if not (List.for_all2 close_values lys lys')
  then
    fail "The function %s is not preserved by unzipping.\n\
          %s(%s; %s) = (%s; %s)\n\
          %s(%s; %s) = (; %s)\n"
      f
      f (show_values uxs) (show_values lxs) (show_values uys) (show_values lys)
      cf (show_values uxs) (show_values lxs) (show_values lys')

(* -------------------------------------------------------------------------- *)

(* Testing reverse-mode AD on one function [f]. *)

(* Suppose f is a function of type R^n → R^m, and suppose tcdf is its
   reverse-mode derivative, of type R^(n+m) → R^n.

   If x ∈ R^n
   and dy ∈ R^m
   we expect tcdf(x, dy)
   to produce dx ∈ R^n
   where dx^T = dy^T.Jf(x)
         -- where dx^T, a line vector, is the transpose of dx
   or equivalently,
   where dx = Jf(x)^T.dy

   As before,
   the matrix Jf(x) is the matrix of the partial derivatives of f at x:
   its dimension is m * n;
   is entries are ∂f_i/∂x_j(x).

   The product Jf(x)^T.dy is a matrix/vector product.

   https://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant *)

let test_reverse_mode tprog tprog' m n f tcdf =
  let fail format = fail "reverse" tprog tprog' format in
  on_failure (fail "%s") @@ fun () ->

  (* Pick x ∈ R^n. *)
  pick_vector n @@ robustly @@ fun x ->

  (* Compute the Jacobian matrix Jf(x). *)
  let jfx : matrix = jacobian m n (interpret_f tprog m n f) x in
  (* Transpose it. *)
  let jfxt : matrix = transpose m n jfx in

  (* Pick dy ∈ R^m. *)
  pick_vector m @@ fun dy ->

  (* Evaluate dx' = tcdf(x, dy). *)
  let dx' = interpret_tcdf tprog' m n tcdf (x, dy) in

  (* Check dx' = Jf(x)^T . dy. *)
  let dx = product n m jfxt dy in
  if not (close dx' dx) then
    fail "The function %s seems wrong.\n\
          Observed behavior:\n  %s%s = %s\n\
          Expected behavior:\n  %s%s = %s\n"
      tcdf
      tcdf (show_vector (x @ dy)) (show_vector dx')
      tcdf (show_vector (x @ dy)) (show_vector dx)

(* -------------------------------------------------------------------------- *)

(* Performing tests on a program [prog], translated to [prog']. *)

let verbose =
  false

let test_forward_mode prog prog' =
  (* Type-check the programs. *)
  let fenv = infer prog in
  let fenv' = infer prog' in
  (* For each function [f] in the source program, *)
  prog |> List.iter @@ function Decl (f, _, _, _) ->
  (* if [f] has type [R^n -> R^m], *)
  is_reals_reals (Env.lookup fenv f) |> Option.iter @@ fun (n, m) ->
  (* then test [df]. *)
  let df = NamingConventions.derivative f in
  if verbose then printf "Testing %s (forward) (m = %d, n = %d)...\n%!" f m n;
  test_forward_mode (prog, fenv) (prog', fenv') m n f df

let test_unzip names prog prog' =
  (* Type-check the programs. *)
  let prog = LinearTypeChecker.check `Lenient prog in
  let fenv = LinearTypeChecker.environment prog in
  let prog' = LinearTypeChecker.check `Lenient prog' in
  (* For each function [f] in the source program, *)
  prog
  |> List.iter @@ function Linear.Decl (_range, f, _ubs, _lbs, _def) ->
  if not (List.mem f names)
  then () else begin
    let uf = NamingConventions.unrestricted f in
    let cf = NamingConventions.combined f in
    let f_ty = Env.lookup fenv f in
    let only_reals =
      let only_reals tys = List.for_all ((=) TReal) tys in
      let ((a, b), (c, d)) = f_ty in
      List.for_all only_reals [a; b; c; d]
    in
    (* only test functions with type [R^n -> R^m].
       We don't need this assumption -- our tests work fine on richer types
       but filtering help keep the testing time manageable. *)
    if not only_reals then ()
    else begin
      if verbose then printf "Testing %s (unzip)...\n%!" f;
      test_unzip prog prog' f f_ty uf cf
    end
  end

let test_reverse_mode prog prog' =
  (* Type-check the programs. *)
  let fenv = infer prog in
  let fenv' = infer prog' in
  (* For each function [f] in the source program, *)
  prog |> List.iter @@ function Decl (f, _, _, _) ->
  (* if [f] has type [R^n -> R^m], *)
  is_reals_reals (Env.lookup fenv f) |> Option.iter @@ fun (n, m) ->
  (* then test [tcdf]. *)
  let tcdf = NamingConventions.(transpose (combined (derivative f))) in
  if verbose then printf "Testing %s (reverse) (m = %d, n = %d)...\n%!" f m n;
  test_reverse_mode (prog, fenv) (prog', fenv') m n f tcdf

(* -------------------------------------------------------------------------- *)
