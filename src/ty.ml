
(* Part of Ty, a simple reification of OCaml types. See the LICENSE file *)

(** {1 Representation of OCaml types *)

(* TODO: reflect ppx attributes (or manually set attributes) in types!
   required for customization. List of possible attributes:

    - ty.opaque         (* for printing *)
    - ty.bimap (f, f')  (* where f : 'a -> 'b, f' : 'b -> 'a *)
    - ty.optional       (* on option types *)
    *)

type (_, _) maybe_eq =
  | Refl : ('a, 'a) maybe_eq
  | NotEq : ('a, 'b) maybe_eq

module Id : sig
  type 'a t
  val fresh : unit -> 'a t
  val equal : 'a t -> 'b t -> ('a, 'b) maybe_eq
end = struct
  type 'a t = int
  let fresh =
    let r = ref 0 in
    fun () -> incr r; !r
  let equal : type a b. a t -> b t -> (a, b) maybe_eq =
    fun i j -> if i=j then (Obj.magic Refl : (a, b) maybe_eq) else NotEq
end

(** Description of type ['a] *)
type 'a ty = {
  id: 'a Id.t;
  view: 'a view;
}

and 'a view =
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

let view a = a.view

let equal a b = Id.equal a.id b.id

(** {2 Helpers} *)

let mk_field ?set name ~ty ~get = {
  field_name=name;
  field_ty=ty;
  field_get=get;
  field_set=set
}

let mk_variant name ~args ~make = {
  variant_name=name;
  variant_args=args;
  variant_make=make;
}

let make_ v = {id=Id.fresh(); view=v; }

let bool = make_ Bool
let int = make_ Int
let int32 = make_ Int32
let int64 = make_ Int64
let nativeint = make_ Nativeint
let float = make_ Float
let unit = make_ Unit
let char = make_ Char
let lazy_ x = make_ (Lazy x)

let string = make_ String
let bytes = make_ Bytes
let list x = make_ (List x)
let array x = make_ (Array x)

let mk_sum s = { id=Id.fresh(); view=Sum s }
let mk_record r = { id=Id.fresh(); view=Record r }
let mk_tuple t = { id=Id.fresh(); view=Tuple t }

let mk_rec d =
  (* tie the knot:
     - create lazy record
     - apply [d] to it, lazily
     - make a view out of that *)
  let rec ty = lazy { id=Id.fresh(); view=Rec (lazy (d (Lazy.force ty))); } in
  Lazy.force ty

let option x = make_ (Option x)

let pair a b = mk_tuple {
  tuple_args=TCons (a, TCons (b, TNil));
  tuple_get = (fun (x,y) -> HCons (x, HCons (y, HNil)));
  tuple_make = (fun (HCons (x, HCons (y, HNil))) -> x,y);
}

let triple a b c = mk_tuple {
  tuple_args=TCons (a, TCons (b, TCons (c, TNil)));
  tuple_get = (fun (x,y,z) -> HCons (x, HCons (y, HCons (z, HNil))));
  tuple_make = (fun (HCons (x, HCons (y, HCons (z, HNil)))) -> x,y,z);
}

let ref ty = mk_record {
  record_name = "ref";
  record_args = RCons (mk_field "contents" ~ty ~get:(!), RNil);
  record_make = (fun (HCons (x, HNil)) -> ref x);
}

(** {2 Table with ['a ty] keys} *)

module type TABLE = sig
  type t
  type 'a value

  val create : ?size:int -> unit -> t

  val get : t -> 'a ty -> 'a value option

  val set : t -> 'a ty -> 'a value -> unit
end

module MkTable(X : sig type 'a t end) = struct
  type 'a value = 'a X.t

  type wrap_ty = Wrap : 'a ty -> wrap_ty
  (* hide type param *)

  type wrap_key_val = WrapKV : 'a ty * 'a value -> wrap_key_val
  (* wrap key ty/value pair *)

  module H = Hashtbl.Make(struct
    type t = wrap_ty
    let hash = Hashtbl.hash
    let equal (Wrap a) (Wrap b) = match Id.equal a.id b.id with
      | Refl -> true
      | NotEq -> false
  end)

  type t = wrap_key_val H.t

  let create ?(size=16) () = H.create size

  let get : type a. t -> a ty -> a value option
  = fun t x ->
    try
      let (WrapKV (ty, v)) = H.find t (Wrap x) in
      match Id.equal x.id ty.id with
      | Refl -> Some v
      | NotEq -> assert false
    with Not_found -> None

  let set : type a. t -> a ty -> a value -> unit
  = fun t x v ->
    H.replace t (Wrap x) (WrapKV (x, v))
end

(** {2 Generic functions} *)

type fmt = Format.formatter

let pp_list ?(sep=", ") pp_item out l =
  let rec print l = match l with
    | x::((_::_) as l) ->
      pp_item out x;
      Format.fprintf out "%s@," sep;
      print l
    | x::[] -> pp_item out x
    | [] -> ()
  in
  print l

let rec print : type a. a ty -> fmt -> a -> unit
  = fun ty out x -> match view ty with
  | Rec (lazy dty) -> print dty out x
  | Unit -> Format.fprintf out "()"
  | Bool ->  Format.fprintf out "%B" x
  | Int -> Format.fprintf out "%d" x
  | Int32 -> Format.fprintf out "%li" x
  | Int64 -> Format.fprintf out "%Li" x
  | Nativeint -> Format.fprintf out "%ni" x
  | Float -> Format.fprintf out "%F" x
  | Char -> Format.fprintf out "%c" x
  | Option ty ->
      begin match x with
      | Some y -> Format.fprintf out "@[<hov>Some (@,%a)@]" (print ty) y
      | None -> Format.fprintf out "None"
      end
  | String -> Format.fprintf out "%S" x
  | Bytes -> Format.fprintf out "%S" x (* TODO: use Bytes? *)
  | List ty ->
      Format.fprintf out "@[<hov>[%a]@]" (pp_list ~sep:"; " (print ty)) x
  | Array ty ->
      Format.fprintf out "@[<hov>[|%a|]@]"
        (pp_list ~sep:"; " (print ty)) (Array.to_list x)
  | Lazy ty -> Format.fprintf out "lazy %a" (print ty) (Lazy.force x)
  | Fun (_, _) -> Format.fprintf out "<fun>"
  | Tuple tup ->
      Format.fprintf out "@[<hov>(%a)@]"
        (print_hlist ~sep:", " ~n:0 tup.tuple_args)
        (tup.tuple_get x)
  | Record r ->
      Format.fprintf out "@[<hov>{%a}@]"
        (print_fields ~n:0 r.record_args) x
  | Sum s ->
      let rec cases : type v. (a, v) variant_list -> (a, v, unit) variant_match
        = function
        | VNil -> VM_nil
        | VCons (v, tail) ->
            let pp_case args = match v.variant_args with
              | TNil -> Format.fprintf out "%s" v.variant_name
              | _ ->
                Format.fprintf out "@[<hov>%s@ (%a)@]" v.variant_name
                  (print_hlist ~sep:", " ~n:0 v.variant_args) args
            in
            VM_cons (pp_case, cases tail)
      in
      s.sum_match (cases s.sum_variants) x

and print_hlist : type l. sep:string -> n:int -> l ty_list -> fmt -> l hlist -> unit
  = fun ~sep ~n tyl out l ->
  match tyl, l with
  | TNil, HNil  -> ()
  | TCons (ty, tyl'), HCons (x, l') ->
      if n > 0 then Format.fprintf out "%s@," sep;
      print ty out x;
      print_hlist ~sep ~n:(n+1) tyl' out l'

and print_fields : type r f. n:int -> (r, f) field_list -> fmt -> r -> unit
  = fun ~n l out r -> match l with
  | RNil -> ()
  | RCons (field, l') ->
      if n > 0 then Format.fprintf out ";@ ";
      Format.fprintf out "@[<hv2>%s: %a@]" field.field_name
        (print field.field_ty) (field.field_get r);
      print_fields ~n:(n+1) l' out r


(* TODO: hashtable with [_ ty] as keys, for overloading generic functions
 * take inspiration from the way `Printexc` overloads exn printing *)
