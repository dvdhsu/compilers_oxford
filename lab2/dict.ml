(* lab2/dict.ml *)

(* Environments are implemented using a library module that 
   represents mappings by balanced binary trees. *)

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

module IdMap = Map.Make(struct type t = ident  let compare = compare end)

type environment = Env of def IdMap.t

let can f x = try f x; true with Not_found -> false

(* |define| -- add a definition *)
let define d (Env e) = 
  if can (IdMap.find d.d_tag) e then raise Exit;
  Env (IdMap.add d.d_tag d e)

(* |lookup| -- find definition of an identifier *)
let lookup x (Env e) = IdMap.find x e

(* |init_env| -- empty environment *)
let init_env = Env IdMap.empty

(* |type_size| -- returns the size of a specific type *)
let rec type_size =
  function
      Integer -> 4
    | Boolean -> 1
    | Array (size, t) -> (type_size t) * size
    | Void -> 0

(* |is_array| -- returns whether the type is an array *)
let is_array = 
  function
      Array (size, t) -> true
    | _ -> false

(* |base_type| -- returns base type of the array *)
let base_type = 
  function
      Array (_, t) -> t
    | _ -> raise Exit;
