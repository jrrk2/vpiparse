open Base
open Hardcaml_of_verilog

let oth_verilog_design = ref None

let convert_verilog ?verbose ?passes verilog_file module_name =
  (* Create a [Verilog_design] which represents the files and modules in the design hierarchy *)
  print_endline "create";
  let verilog_design =
    Verilog_design.create
      ~top:(Verilog_design.Module.create ~module_name ~path:verilog_file ())
      ()
  in
  oth_verilog_design := Some verilog_design;
  print_endline "synth";
  (* Synthesize to a [Netlist] *)
  let%bind.Or_error netlist = Netlist.create ?verbose ?passes verilog_design in
  print_endline "create netlist";
  (* Convert to a Hardcaml [Circuit] *)
  let%bind.Or_error verilog_circuit =
    Verilog_circuit.create netlist ~top_name:(Verilog_design.top_name verilog_design)
  in
  print_endline "to hardcaml";
  let rslt = Verilog_circuit.to_hardcaml_circuit verilog_circuit in
  print_endline "result";
  rslt

let rslt = convert_verilog ?verbose:(Some true) "outputparser/examples/count0.v" "count";;

