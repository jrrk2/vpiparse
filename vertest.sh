rm -rf obj_dir && verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC $* && make ./Input_verilator && ./Input_verilator obj_dir/V* $*
