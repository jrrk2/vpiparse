
print(Sys.test("Hello","World"))

v = Sys.arg(2)
lib = "liberty/simcells"
ver = verible.tranlst(v)
print(ver)
gold = pipe.rtlil("yosys -q -q -p 'read_verilog -sv "..v.."; synth; write_ilang'")
print(gold)
lib = liberty.read(lib)
print(lib)
cnv = hardcaml.cnv(ver)
print(cnv)
rev=verible.cnvitm(lib,cnv)
goldsat=verible.satitm(gold)
revsat=verible.satitm(rev)
print(verible.cmpitm(goldsat,revsat))
print(itms.itm())
topmod=itms.nam(ver)
print(topmod)
itms.dump("_gold",gold)
itms.dump("_rev",rev)
print(external.eqv(topmod))
maplib="liberty/NangateOpenCellLibrary_typical"
libmap = liberty.read(maplib)
print(lib)
itmmap=verible.mapitm(libmap,cnv)
print(itmmap)
itms.dump("_map",itmmap)
print(external.sta(topmod.."_map.v",topmod,maplib))