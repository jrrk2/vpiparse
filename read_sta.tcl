  output_string fd ("read_lib liberty/NangateOpenCellLibrary_typical.lib\n");
  output_string fd ("read_verilog multiplier_test_map.v\n");
  output_string fd ("current_design multiplier_test\n");
  output_string fd ("link\n");
  output_string fd ("report_checks -unconstrained\n");
  output_string fd ("\n");
