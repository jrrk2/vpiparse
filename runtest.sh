rm -f slpp_all/surelog.uhdm uhdm.txt
surelog -parse -sverilog $*
uhdm-dump slpp_all/surelog.uhdm > uhdm.txt
make ./Input
./Input uhdm.txt $*
