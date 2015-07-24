
(* Part of Ty, a simple reification of OCaml types. See the LICENSE file *)

(** {1 Representation of OCaml types *)

(* TODO: reflect ppx attributes (or manually set attributes) in types!
   required for customization. List of possible attributes:

    - ty.opaque         (* for printing *)
    - ty.bimap (f, f')  (* where f : 'a -> 'b, f' : 'b -> 'a *)
    - ty.optional       (* on option types *)
    *)

(** Description of type ['a] *)
type 'a ty =
  | Int : int ty
  | Bool : bool ty
  | Sum : ('s, 'v) sum -> 's ty
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

(* PRINT *)

type fmt = Format.formatter

let rec print : type a. a ty -> fmt -> a -> unit
  = fun ty out x -> match ty with
  | Int -> Format.fprintf out "%d" x
  | Bool ->  Format.fprintf out "%B" x
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
