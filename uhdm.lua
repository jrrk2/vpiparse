uhdm="slpp_all/surelog.uhdm"
src=Sys.arg(2)
print(src)
execute("rm -f "..uhdm)
execute("surelog -parse -sverilog "..src)
print(uhdm)
pipe.uhdm("uhdm-dump "..uhdm,src)
print("completed")
