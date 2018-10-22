(* * Types (hashconsed) *)
 
(* ** Imports *)
open Abbrevs
open Util

(* ** Identifiers *)

module Lenvar : (module type of Id) = Id

module Tysym : (module type of Id) = Id

module Groupvar : (module type of Id) = Id

module Permvar : (module type of Id) = Id


(* ** Types and type nodes *)

type ty = {
  ty_node : ty_node;
  ty_tag : int
}
and ty_node =
  | BS of Lenvar.id					
  | Bool
  | G of Groupvar.id
  | TySym of Tysym.id
  | Fq
  | Prod of ty list
  | Int
  | ArrFq of Lenvar.id
  | ArrG of Lenvar.id
  | ArrBSs of Lenvar.id					
(* ** Equality, hashing, and hash consing *)

let equal_ty : ty -> ty -> bool = (==)
let hash_ty t = t.ty_tag
let compare_ty t1 t2 = t1.ty_tag - t2.ty_tag

module Hsty = Hashcons.Make (struct
  type t = ty

  let equal t1 t2 =
    match t1.ty_node, t2.ty_node with
    | BS lv1, BS lv2               -> Lenvar.equal lv1 lv2               
    | Bool, Bool                     -> true
    | G gv1, G gv2                   -> Groupvar.equal gv1 gv2
    | TySym ts1, TySym ts2           -> Tysym.equal ts1 ts2
    | Fq, Fq                         -> true
    | Prod ts1, Prod ts2             -> list_eq_for_all2 equal_ty ts1 ts2
    | _                              -> false
    | ArrFq lv1, ArrFq lv2           -> Lenvar.equal lv1 lv2
    | ArrG lv1, ArrG lv2           -> Lenvar.equal lv1 lv2	      
    | ArrBSs lv1, ArrBSs lv2           -> Lenvar.equal lv1 lv2
  let hash t =
    match t.ty_node with
    | BS lv        -> hcomb 1 (Lenvar.hash lv)				
    | Bool          -> 2
    | G gv          -> hcomb 3 (Groupvar.hash gv)
    | TySym gv      -> hcomb 4 (Tysym.hash gv)
    | Fq            -> 5
    | Prod ts       -> hcomb_l hash_ty 6 ts
    | Int           -> 7
    | ArrFq lv5      -> hcomb 8 (Lenvar.hash lv5)
    | ArrG lv      -> hcomb 8 (Lenvar.hash lv)
    | ArrBSs lv      -> hcomb 8 (Lenvar.hash lv)                              

  let tag n t = { t with ty_tag = n }
end)

(** Create [Map], [Set], and [Hashtbl] modules for types. *)
module Ty = StructMake (struct
  type t = ty
  let tag = hash_ty
end)
module Mty = Ty.M
module Sty = Ty.S
module Hty = Ty.H

(* ** Constructor functions *)

let mk_ty n = Hsty.hashcons {
  ty_node = n;
  ty_tag  = (-1)
}

let mk_BS lv = mk_ty (BS lv)				
let mk_ArrFq lv5 = mk_ty (ArrFq lv5)			
let mk_ArrG lv = mk_ty (ArrG lv)				
let mk_ArrBSs lv = mk_ty (ArrBSs lv)			

let mk_G gv = mk_ty (G gv)

let mk_TySym ts = mk_ty (TySym ts)

let mk_Fq = mk_ty Fq

let mk_Bool = mk_ty Bool

let mk_Int = mk_ty Int

let mk_Prod tys =
  match tys with
  | [t] -> t
  | _   -> mk_ty (Prod tys)

(* ** Indicator and destructor functions *)

let is_G ty = match ty.ty_node with
  | G _ -> true
  | _   -> false

let is_Fq ty = match ty.ty_node with
  | Fq -> true
  | _  -> false

let is_Prod ty = match ty.ty_node with
  | Prod _ -> true
  | _      -> false

let destr_G_exn ty =
  match ty.ty_node with
  | G gv -> gv
  | _    -> raise Not_found

let destr_BS_exn ty =					
  match ty.ty_node with
  | BS lv -> lv				
  | _     -> raise Not_found

let destr_ArrFq_exn ty =			
  match ty.ty_node with
  | ArrFq lv5 -> lv5					
  | _     -> raise Not_found

let destr_ArrG_exn ty =	   			           
  match ty.ty_node with
  | ArrG lv -> lv					
  | _     -> raise Not_found

let destr_ArrBSs_exn ty =	   		
  match ty.ty_node with
  | ArrBSs lv -> lv					
  | _     -> raise Not_found

let destr_Prod_exn ty =
  match ty.ty_node with
  | Prod ts -> ts
  | _       -> raise Not_found

let destr_Prod ty =
  match ty.ty_node with
  | Prod ts -> Some ts
  | _       -> None

(* ** Pretty printing *)

let pp_group fmt gv =
  if Groupvar.name gv = ""
  then F.fprintf fmt "G"
  else F.fprintf fmt "G_%s" (Groupvar.name gv)

let rec pp_ty fmt ty =
  match ty.ty_node with
  | BS lv             -> F.fprintf fmt "BS_%s" (Lenvar.name lv)		(*Changes*)
  | ArrFq lv5         -> F.fprintf fmt "ArrFq_%s" (Lenvar.name lv5)
  | ArrG lv           -> F.fprintf fmt "ArrG_%s" (Lenvar.name lv)
  | ArrBSs lv         -> F.fprintf fmt "ArrBSs_%s" (Lenvar.name lv)
  | Bool              -> F.fprintf fmt "Bool"
  | Fq                -> F.fprintf fmt "Fq"
  | TySym ts          -> F.fprintf fmt "%s" (Tysym.name ts)
  | Prod ts           -> F.fprintf fmt "(%a)" (pp_list " * " pp_ty) ts
  | Int               -> F.fprintf fmt "Int"
  | G gv ->
    if Groupvar.name gv = ""
    then F.fprintf fmt "G"
    else F.fprintf fmt "G_%s" (Groupvar.name gv)
