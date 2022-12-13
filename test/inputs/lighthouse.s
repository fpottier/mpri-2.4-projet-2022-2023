let tan (x : real) =
  sin(x) / cos(x)

let lighthouse (nu : real, gamma : real, omega : real, t : real) =
  (* Rename the inputs. *)
  let vm3 = nu in
  let vm2 = gamma in
  let vm1 = nu in
  let v0 = t in
  (* Compute. *)
  let v1 = vm1 * v0 in
  let v2 = tan(v1) in
  let v3 = vm2 - v2 in
  let v4 = vm3 * v2 in
  let v5 = v4 / v3 in
  let v6 = v5 in
  let v7 = v5 * vm2 in
  (* Produce our outputs in a tuple. *)
  [ v6, v7 ]
