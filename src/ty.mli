
(** {1 Representation of OCaml types *)

(** Description of type ['a] *)
type ('a,'x) rty =
  | Rec : ('x,'x) rty
  | Unit : (unit,'x) rty
  | Int : (int,'x) rty
  | Bool : (bool,'x) rty
  | List : ('a,'x) rty -> ('a list,'x) rty
  | Sum : ('s, 'v, 'x) sum -> ('s,'x) rty
  | Record : ('r, 'fields, 'x) record -> ('r,'x) rty
  | Tuple : ('t, 'a, 'x) tuple -> ('t,'x) rty
  | Lazy : ('a,'x) rty -> ('a Lazy.t,'x) rty
  | Fun : ('a,'x) rty * ('b,'x) rty -> ('a -> 'b, 'x) rty

and 'a hlist =
  | HNil : unit hlist
  | HCons : 'a * 'b hlist -> ('a * 'b) hlist

(** Description of list of types *)
and ('a,'x) ty_list =
  | TNil : (unit,'x) ty_list
  | TCons : ('a,'x) rty * ('b,'x) ty_list -> ('a * 'b,'x) ty_list

(** Sum type ['s] *)
and ('s, 'v, 'x) sum = {
  sum_name : string;
  sum_variants : ('s, 'v, 'x) variant_list;
  sum_match : 'ret. ('s, 'v, 'x, 'ret) variant_match -> 's -> 'ret;
}

(** List of variants for the sum type ['s] *)
and ('s, 'v, 'x) variant_list =
  | VNil : ('s, unit, 'x) variant_list
  | VCons : ('s, 'a, 'x) variant * ('s, 'b, 'x) variant_list -> ('s, 'a * 'b, 'x) variant_list

(** Pattern matching encoding on sums *)
and ('s, 'v, 'x, 'ret) variant_match =
  | VM_nil : ('s, unit, 'x, 'ret) variant_match
  | VM_cons : ('a hlist -> 'ret) * ('s, 'b, 'x, 'ret) variant_match -> ('s, 'a * 'b, 'x, 'ret) variant_match

(** A variant of the sum type ['s] *)
and ('s, 'a, 'x) variant = {
  variant_name : string;
  variant_args : ('a,'x) ty_list;
  variant_make : 'a hlist -> 's;
}

(** Description of record of type ['r] with fields ['fields] *)
and ('r, 'fields, 'x) record = {
  record_name : string;
  record_args : ('r, 'fields, 'x) field_list;
  record_make : 'fields hlist -> 'r;  (* build record *)
}

and (_, _, _) field_list =
  | RNil : ('r, unit, 'x) field_list
  | RCons : ('r, 'a, 'x) field * ('r, 'b, 'x) field_list -> ('r, 'a * 'b, 'x) field_list

and ('r, 'a, 'x) field = {
  field_name : string;
  field_ty : ('a,'x) rty;
  field_get : 'r -> 'a;
  field_set : ('a -> 'r) option; (* None if field immutable *)
}

(** A tuple of type ['t], where ['a] is the nested version of ['t].
    For instance we would have
    [((int * bool * float), (int * (bool * (float * unit)))) tuple]
*)
and ('t, 'a, 'x) tuple = {
  tuple_args : ('a,'x) ty_list;
  tuple_get : 't -> 'a hlist;
  tuple_make : 'a hlist -> 't;
}

type 'a ty = ('a,'a) rty


(** {2 Helpers} *)

val mk_field : ?set:('a -> 'r) -> string -> ty:('a,'x) rty -> get:('r -> 'a) -> ('r, 'a, 'x) field
val mk_variant : string -> args:('a,'x) ty_list -> make:('a hlist -> 's) -> ('s, 'a, 'x) variant

val int : int ty
val bool : bool ty
val unit : unit ty
val option : ('a,'x) rty -> ('a option,'x) rty
val list : ('a,'x) rty -> ('a list,'x) rty
val lazy_ : ('a,'x) rty -> ('a Lazy.t,'x) rty
val ref : ('a,'x) rty -> ('a ref,'x) rty

val pair : ('a,'x) rty -> ('b,'x) rty -> ('a * 'b,'x) rty
val triple : ('a,'x) rty -> ('b,'x) rty -> ('c,'x) rty -> ('a * 'b * 'c,'x) rty

(** {2 Generic functions} *)

val print : 'a ty -> Format.formatter -> 'a -> unit
(** [print ty out x] prints [x] on [out] in a generic way, using
    [ty] to traverse [x] *)

