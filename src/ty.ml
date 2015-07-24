
(* Part of Ty, a simple reification of OCaml types. See the LICENSE file *)

(** {1 Representation of OCaml types *)

(* TODO: reflect ppx attributes (or manually set attributes) in types!
   required for customization. List of possible attributes:

    - ty.opaque         (* for printing *)
    - ty.bimap (f, f')  (* where f : 'a -> 'b, f' : 'b -> 'a *)
    - ty.optional       (* on option types *)
    *)

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

let int = Int
let bool = Bool
let unit = Unit
let lazy_ x = Lazy x

let option x = Sum {
  sum_name="option";
  sum_variants= (
    let v_none = mk_variant "None" ~args:TNil ~make:(fun HNil -> None) in
    let v_some = mk_variant "Some" ~args:(TCons (x, TNil))
      ~make:(fun (HCons (x, HNil)) -> Some x)
    in
    VCons (v_none, VCons (v_some, VNil))
  );
  sum_match=fun (VM_cons (f_none, VM_cons (f_some, VM_nil))) v ->
    match v with
    | None -> f_none HNil
    | Some x -> f_some (HCons (x, HNil))
}

let list x = List x

let pair a b = Tuple {
  tuple_args=TCons (a, TCons (b, TNil));
  tuple_get = (fun (x,y) -> HCons (x, HCons (y, HNil)));
  tuple_make = (fun (HCons (x, HCons (y, HNil))) -> x,y);
}

let triple a b c = Tuple {
  tuple_args=TCons (a, TCons (b, TCons (c, TNil)));
  tuple_get = (fun (x,y,z) -> HCons (x, HCons (y, HCons (z, HNil))));
  tuple_make = (fun (HCons (x, HCons (y, HCons (z, HNil)))) -> x,y,z);
}

let ref ty = Record {
  record_name = "ref";
  record_args = RCons (mk_field "contents" ~ty ~get:(!), RNil);
  record_make = (fun (HCons (x, HNil)) -> ref x);
}

(* PRINT *)

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

let rec print : type a x. (a,x) rty -> fmt -> a -> unit
  = fun ty out x -> match ty with
  | Rec -> assert false (* TODO *)
  | Unit -> Format.fprintf out "()"
  | Int -> Format.fprintf out "%d" x
  | Bool ->  Format.fprintf out "%B" x
  | List ty -> Format.fprintf out "@[<hov>[%a]@]" (pp_list ~sep:"; " (print ty)) x
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
      let rec cases : type v. (a, v,_) variant_list -> (a, v, _, unit) variant_match
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

and print_hlist : type l x. sep:string -> n:int -> (l,x) ty_list -> fmt -> l hlist -> unit
  = fun ~sep ~n tyl out l ->
  match tyl, l with
  | TNil, HNil  -> ()
  | TCons (ty, tyl'), HCons (x, l') ->
      if n > 0 then Format.fprintf out "%s@," sep;
      print ty out x;
      print_hlist ~sep ~n:(n+1) tyl' out l'

and print_fields : type r f x. n:int -> (r, f,x) field_list -> fmt -> r -> unit
  = fun ~n l out r -> match l with
  | RNil -> ()
  | RCons (field, l') ->
      if n > 0 then Format.fprintf out ";@ ";
      Format.fprintf out "@[<hv2>%s: %a@]" field.field_name
        (print field.field_ty) (field.field_get r);
      print_fields ~n:(n+1) l' out r


(* TODO: hashtable with [_ ty] as keys, for overloading generic functions
 * take inspiration from the way `Printexc` overloads exn printing *)
