dofile("library.lua")

print("Hello".."UHDM/Verilator")
v = Sys.arg(2)
w = Sys.arg(3)
maplib="liberty/NangateOpenCellLibrary_typical"

goldver,goldmap=verilator_parse(maplib,w)

revver,revmap=uhdm_parse(maplib,v)

allsat(goldver,revver)

ilangver=rtlil_parse(v)
verver,vermap=verible_parse(maplib,v)
rtlilsat(ilangver,verver)

print(itms.itm())
