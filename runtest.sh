surelog -parse -sverilog $*
uhdm-dump slpp_all/surelog.uhdm > uhdm.txt
rm -f Input_top
make ./Input_top
./Input_top

