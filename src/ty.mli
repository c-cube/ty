
(** {1 Representation of OCaml types *)

type (_, _) maybe_eq =
  | Refl : ('a, 'a) maybe_eq
  | NotEq : ('a, 'b) maybe_eq

(** Description of type ['a] *)
type 'a ty

(** View of ['a ty] *)
type 'a view =
  | Rec : 'a ty lazy_t -> 'a view
  | Unit : unit view
  | Bool : bool view
  | Int : int view
  | Int32 : int32 view
  | Int64 : int64 view
  | Nativeint : nativeint view
  | Float : float view
  | Char : char view
  | Option : 'a ty -> 'a option view
  | String : string view
  | Bytes : bytes view
  | List : 'a ty -> 'a list view
  | Array : 'a ty -> 'a array view
  | Sum : ('s, 'v) sum -> 's view
  | Record : ('r, 'fields) record -> 'r view
  | Tuple : ('t, 'a) tuple -> 't view
  | Lazy : 'a ty -> 'a lazy_t view
  | Fun : 'a ty * 'b ty -> ('a -> 'b) view

and 'a hlist =
  | HNil : unit hlist
  | HCons : 'a * 'b hlist -> ('a * 'b) hlist

(** Description of list of types *)
and 'a ty_list =
  | TNil : unit ty_list
  | TCons : 'a ty * 'b ty_list -> ('a * 'b) ty_list

(** Sum type ['s] *)
and ('s, 'v) sum = {
  sum_name : string;
  sum_variants : ('s, 'v) variant_list;
  sum_match : 'ret. ('s, 'v, 'ret) variant_match -> 's -> 'ret;
}

(** List of variants for the sum type ['s] *)
and ('s, 'v) variant_list =
  | VNil : ('s, unit) variant_list
  | VCons : ('s, 'a) variant * ('s, 'b) variant_list -> ('s, 'a * 'b) variant_list

(** Pattern matching encoding on sums *)
and ('s, 'v, 'ret) variant_match =
  | VM_nil : ('s, unit, 'ret) variant_match
  | VM_cons : ('a hlist -> 'ret) * ('s, 'b, 'ret) variant_match -> ('s, 'a * 'b, 'ret) variant_match

(** A variant of the sum type ['s] *)
and ('s, 'a) variant = {
  variant_name : string;
  variant_args : 'a ty_list;
  variant_make : 'a hlist -> 's;
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
  field_set : ('a -> 'r) option; (* None if field immutable *)
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

(** {2 Basic Operations} *)

val view : 'a ty -> 'a view
(** Deconstruct the head of the ['a ty] *)

val equal : 'a ty -> 'b ty -> ('a, 'b) maybe_eq
(** Compare two dynamic types {b nominally} (i.e. even if ['a] and
    ['b] are structurally equal, ['a ty] and ['b ty] might not
    be equal) *)

(** {2 Helpers} *)

val mk_field : ?set:('a -> 'r) -> string -> ty:'a ty -> get:('r -> 'a) -> ('r, 'a) field
val mk_variant : string -> args:'a ty_list -> make:('a hlist -> 's) -> ('s, 'a) variant

val bool : bool ty
val int : int ty
val int32 : int32 ty
val int64 : int64 ty
val nativeint : nativeint ty
val float : float ty
val unit : unit ty
val char : char ty
val option : 'a ty -> 'a option ty
val string : string ty
val bytes : bytes ty
val list : 'a ty -> 'a list ty
val array : 'a ty -> 'a array ty
val lazy_ : 'a ty -> 'a Lazy.t ty
val ref : 'a ty -> 'a ref ty

val pair : 'a ty -> 'b ty -> ('a * 'b) ty
val triple : 'a ty -> 'b ty -> 'c ty -> ('a * 'b * 'c) ty

val mk_sum : ('s, _) sum -> 's ty
val mk_record : ('r, _) record -> 'r ty
val mk_tuple : ('t, _) tuple -> 't ty
val mk_rec : ('a ty -> 'a ty) -> 'a ty

(* helpers *)

val mk_sum_rec : ('s ty -> ('s, _) sum) -> 's ty
val mk_record_rec : ('r ty -> ('r, _) record) -> 'r ty

(** {2 Table with ['a ty] keys} *)

module type TABLE = sig
  type t
  type 'a value

  val create : ?size:int -> unit -> t

  val get : t -> 'a ty -> 'a value option

  val set : t -> 'a ty -> 'a value -> unit
end

module MkTable(X : sig type 'a t end) : TABLE with type 'a value = 'a X.t

(** {2 Generic functions} *)

val print : 'a ty -> Format.formatter -> 'a -> unit
(** [print ty out x] prints [x] on [out] in a generic way, using
    [ty] to traverse [x] *)

