dofile("library.lua")

print("Hello".."UHDM/Verilator")
v = Sys.arg(2)
-- w = Sys.arg(3)
maplib="liberty/NangateOpenCellLibrary_typical"

--goldver,goldmap=verilator_parse(maplib,v)

--revver,revmap=uhdm_parse(maplib,v)

--allsat(goldver,revver)

verver,vermap=verible_parse(maplib,v)
rtlilsat(v,verver)

print(itms.itm())
