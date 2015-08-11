
(* small test file *)

type color =
  | Red
  | Blue
  | Green

let ty_color = Ty.(
  let v_red = mk_variant "Red" ~args:TNil ~make:(fun HNil -> Red) in
  let v_blue = mk_variant "Blue" ~args:TNil ~make:(fun HNil -> Blue) in
  let v_green= mk_variant "Green" ~args:TNil ~make:(fun HNil -> Green) in
  mk_sum {
    sum_name="color";
    sum_variants=VCons (v_red, VCons (v_blue, VCons (v_green, VNil)));
    sum_match=fun
      (VM_cons (f_red, VM_cons (f_blue, VM_cons (f_green, VM_nil)))) v ->
        match v with
        | Red -> f_red HNil
        | Blue -> f_blue HNil
        | Green -> f_green HNil
  }
)

type point = {
  x: int;
  y: int;
  color: color;
}

let ty_point = Ty.(
  let f_x = mk_field "x" ~ty:int ~get:(fun r->r.x) in
  let f_y = mk_field "y" ~ty:int ~get:(fun r->r.y) in
  let f_c = mk_field "color" ~ty:ty_color ~get:(fun r->r.color) in
  mk_record {
    record_name = "point";
    record_args = RCons (f_x, RCons (f_y, RCons (f_c, RNil)));
    record_make = (fun (HCons (x, HCons (y, HCons (color, HNil)))) -> {x;y;color});
  }
)

type 'a myoption =
  | MySome of 'a
  | MyNone

let myoption x = Ty.(mk_sum {
  sum_name="myoption";
  sum_variants= (
    let v_none = mk_variant "MyNone" ~args:TNil ~make:(fun HNil -> MyNone) in
    let v_some = mk_variant "MySome" ~args:(TCons (x, TNil))
      ~make:(fun (HCons (x, HNil)) -> MySome x)
    in
    VCons (v_none, VCons (v_some, VNil))
  );
  sum_match=fun (VM_cons (f_none, VM_cons (f_some, VM_nil))) v ->
    match v with
    | MyNone -> f_none HNil
    | MySome x -> f_some (HCons (x, HNil))
})

type nat = O | S of nat

let ty_nat =
  Ty.mk_rec (fun self ->
    let open Ty in
    let v_o = mk_variant "O" ~args:TNil ~make:(fun HNil -> O) in
    let v_s = mk_variant "S" ~args:(TCons(self,TNil)) ~make:(fun (HCons(n,HNil)) -> S n) in
    mk_sum {
      sum_name="nat";
      sum_variants=VCons (v_o, VCons (v_s, VNil));
      sum_match=fun
        (VM_cons (f_o, VM_cons (f_s, VM_nil))) v ->
          match v with
          | O -> f_o HNil
          | S n -> f_s (HCons(n,HNil))
    }
  )

let rec mk_nat = function
  | 0 -> O
  | n -> S (mk_nat (n-1))

let () =
  List.iter
    (fun p -> Format.printf "point %a@." (Ty.print ty_point) p)
    [ {x=1; y=2; color=Red }
    ; {x=0; y=42; color=Blue }
    ];
  List.iter
    (fun p -> Format.printf "nat %a@." (Ty.print ty_nat) p)
    [ O
    ; S O
    ; S (S O)
    ; mk_nat 10
    ];
