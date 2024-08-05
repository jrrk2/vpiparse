rm -rf obj_dir && verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC $* && ./Source_combined vertest.lua obj_dir/V* $*
