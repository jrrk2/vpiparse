rm -rf slpp_all uhdm.txt
surelog -parse -sverilog $*
uhdm-dump slpp_all/surelog.uhdm > uhdm.txt
rm -f ../../Input_top
make -C ../.. ./Input_top
../../Input_top

