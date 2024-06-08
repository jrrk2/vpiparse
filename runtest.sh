rm -f slpp_all/surelog.uhdm uhdm.txt
surelog -parse -sverilog -fileunit $*
uhdm-dump slpp_unit/surelog.uhdm > uhdm.txt
make ./Input
./Input uhdm.txt $*
