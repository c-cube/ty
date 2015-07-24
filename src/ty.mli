
(** {1 Representation of OCaml types *)

(** Description of type ['a] *)
type 'a ty =
  | Int : int ty
  | Bool : bool ty
  | Sum : 's sum -> 's ty
  | Record : ('r, 'fields) record -> 'r ty
  | Tuple : ('t, 'a) tuple -> 't ty

and 'a hlist =
  | HNil : unit hlist
  | HCons : 'a * 'b hlist -> ('a * 'b) hlist

(** Description of list of types *)
and 'a ty_list =
  | TNil : unit ty_list
  | TCons : 'a ty * 'b ty_list -> ('a * 'b) ty_list

(** Sum type ['s] *)
and 's sum = {
  sum_name : string;
  sum_variants : 's variant_list;
}

(** List of variants for the sum type ['s] *)
and 's variant_list =
  | VNil : 's variant_list
  | VCons : string * ('s, 'a) variant * 's variant_list -> 's variant_list

(** A variant of the sum type ['s] *)
and ('s, 'a) variant = {
  variant_name : string;
  variant_args : 'a ty_list;
  variant_make : 'a hlist -> 's;
  variant_get : 's -> 'a hlist option;  (* projection *)
}

(** Description of record of type ['r] with fields ['fields] *)
and ('r, 'fields) record = {
  record_name : string;
  record_args : ('r, 'fields) field_list;
  record_make : 'fields hlist -> 'r;  (* build record *)
}

and (_, _) field_list =
  | RNil : ('r, unit) field_list
  | RCons : ('r, 'a) field * ('r, 'b) field_list -> ('r, 'a * 'b) field_list

and ('r, 'a) field = {
  field_name : string;
  field_ty : 'a ty;
  field_get : 'r -> 'a;
}

(** A tuple of type ['t], where ['a] is the nested version of ['t].
    For instance we would have
    [((int * bool * float), (int * (bool * (float * unit)))) tuple]
*)
and ('t, 'a) tuple = {
  tuple_args : 'a ty_list;
  tuple_get : 't -> 'a hlist;
  tuple_make : 'a hlist -> 't;
}

(** {2 Generic functions} *)

val print : 'a ty -> Format.formatter -> 'a -> unit
(** [print ty out x] prints [x] on [out] in a generic way, using
    [ty] to traverse [x] *)

