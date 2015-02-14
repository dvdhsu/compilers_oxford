(* lab2/dict.mli *)

type ident = string

type ptype = 
    Integer 
  | Boolean 
  | Array of int * ptype
  | Void

(* |def| -- definitions in environment *)
type def = 
  { d_tag: ident;               (* Name *)
    d_type: ptype;              (* Type *)
    d_lab: string }             (* Global label *)

type environment

(* |define| -- add a definition, raise Exit if already declared *)
val define : def -> environment -> environment

(* |lookup| -- search an environment or raise Not_found *)
val lookup : ident -> environment -> def

(* |init_env| -- initial empty environment *)
val init_env : environment

(* |type_size| -- returns the size of a specific type *)
val type_size : ptype -> int

(* |is_array| -- returns whether the type is an array *)
val is_array : ptype -> bool

(* |base_type| -- returns base type of the array *)
val base_type : ptype -> ptype
