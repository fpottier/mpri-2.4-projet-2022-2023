include Map.Make(String)

type 'a env =
  'a t

let lookup env x =
  find x env

let bind env x v =
  add x v env

let remove env x =
  remove x env

let bind_many env xs vs =
  List.fold_left2 bind env xs vs

exception Collision of string

let bind_new env x v =
  match lookup env x with
  | exception Not_found ->
      bind env x v
  | _ ->
      raise (Collision x)

let bind_many_new env xs vs =
  List.fold_left2 bind_new env xs vs

let check_distinct xs =
  ignore (bind_many_new empty xs xs)

let extract env =
  match choose_opt env with
  | None ->
      None
  | Some (x, v) ->
      let env = remove env x in
      Some (env, x, v)
