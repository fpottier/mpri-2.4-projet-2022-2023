type key = string

(**An environment is an immutable map of identifiers (strings) to data. *)
type 'a env

(**[empty] is the empty environment. *)
val empty : 'a env

(**[lookup env x] returns the datum associated with [x] in the environment
   [env]. It raises [Not_found] if there is no such datum. *)
val lookup : 'a env -> key -> 'a

(**[bind env x v] is an environment that extends [env] with a mapping of
   [x] to [v]. This new mapping can hide an existing mapping. *)
val bind : 'a env -> key -> 'a -> 'a env

(**[remove env x] is an environment obtained from [env] by removing
   a mapping of [x] to some value, if there is one. *)
val remove : 'a env -> key -> 'a env

(**[bind_many env xs vs] is an environment that extends [env] with a mapping
   of every variable in the list [xs] to the corresponding datum in the list
   [vs]. [Invalid_argument _] is raised if the lists [xs] and [vs] do not
   have the same length. *)
val bind_many : 'a env -> key list -> 'a list -> 'a env

exception Collision of key

(**[bind_new env x v] is an environment that extends [env] with a mapping of
   [x] to [v]. This new mapping is not allowed to hide an existing mapping:
   if this occurs, then [Collision x] is raised. *)
val bind_new : 'a env -> key -> 'a -> 'a env

(**[bind_many_new env xs vs] is an environment that extends [env] with a
   mapping of every variable in the list [xs] to the corresponding datum in
   the list [vs]. [Invalid_argument _] is raised if the lists [xs] and [vs]
   do not have the same length. The new mappings are created one by one, and
   [Collision _] is raised if a new mapping hides an earlier mapping. *)
val bind_many_new : 'a env -> key list -> 'a list -> 'a env

(**[check_distinct xs] checks that the names in the list [xs] are pairwise
   distinct. If this is not the case, then [Collision _] is raised. *)
val check_distinct : key list -> unit

(**[is_empty env] determines whether [env] is empty. *)
val is_empty : 'a env -> bool

(**If [env] is empty, then [extract env] returns [None]. Otherwise, it
   returns [Some (env', x, v)] where [x : v] is an arbitrary entry in
   [env] and [env'] is [env] deprived of this entry. *)
val extract : 'a env -> ('a env * key * 'a) option

(**[bindings env] is a list of the key-value pairs in [env]. *)
val bindings : 'a env -> (key * 'a) list
