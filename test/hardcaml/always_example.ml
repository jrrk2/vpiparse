open Hardcaml

(*
let something =
  let open Signal in
  let a = input "a" 1 in
  let b = input "b" 1 in
  let c = Always.Variable.wire ~default:gnd in
  let d = Always.Variable.wire ~default:gnd in
  let e = Always.Variable.wire ~default:gnd in
  Always.(compile [
    (* Assignments. *)
    c <-- (a ^: b );

    (* [if_] statements. *)
    if_ (a ==: b) [
      d <-- vdd;
    ] [
      d <-- gnd;
    ];

    (* [when_] is like [if_], with an empty [else] *)
    when_ c.value [
      e <--. 1;
    ];
  ]);
;;
*)

let clock = Signal.input "clock" 1
let clear = Signal.input "clear" 1
let a = Signal.input "a" 8
let b = Signal.input "b" 8
let r_sync = Reg_spec.create ~clock ~clear ()

let always_circuit =
  let open Signal in
  (* [wire] and [register] variable declarations. *)
  let c_wire = Always.Variable.wire ~default:(Signal.zero 8) in
  let c_reg = Always.Variable.reg ~enable:Signal.vdd r_sync ~width:8 in
  (* The program block with a call to [compile] *)
  Always.(compile [
    if_ (a ==: b) [
      c_wire <-- (sll a 1);
      c_reg  <-- (sll a 1)
    ] [
      c_wire <-- (a +: b);
      c_reg  <-- (a +: b);
    ]
  ]);
  (* the [c_wire.value] are assigned appropriately by the Always
  compiler. *)
  Hardcaml.Circuit.create_exn ~name:"creat" (List.map (fun (s,v) -> output s v) ["c_wire", c_wire.value; "c_reg", c_reg.value])
;;

Hardcaml.Rtl.print Verilog always_circuit;;
