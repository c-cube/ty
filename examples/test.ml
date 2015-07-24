
(* small test file *)

type color =
  | Red
  | Blue
  | Green

let ty_color = Ty.(
  let v_red = {variant_name="red"; variant_args=TNil; variant_make=fun HNil -> Red} in
  let v_blue = {variant_name="blue"; variant_args=TNil; variant_make=fun HNil -> Blue} in
  let v_green = {variant_name="green"; variant_args=TNil; variant_make=fun HNil -> Green} in
  Sum {
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
  let f_x = {field_name = "x"; field_ty = Int; field_get = fun r->r.x} in
  let f_y = {field_name = "y"; field_ty = Int; field_get = fun r->r.y} in
  let f_c = {field_name = "color"; field_ty = ty_color; field_get = fun r->r.color} in
  Record {
    record_name = "point";
    record_args = RCons (f_x, RCons (f_y, RCons (f_c, RNil)));
    record_make = (fun (HCons (x, HCons (y, HCons (color, HNil)))) -> {x;y;color});
  }
)

let () =
  List.iter
    (fun p -> Format.printf "point %a@." (Ty.print ty_point) p)
    [ {x=1; y=2; color=Red }
    ; {x=0; y=42; color=Blue }
    ]

