print("Hello".."UHDM/Verilator")
v = Sys.arg(2)
uhdm="slpp_all/surelog.uhdm"
execute("rm -f "..uhdm)
execute("surelog -parse -sverilog "..v)
print(uhdm)
ver=pipe.uhdmtop("uhdm-dump "..uhdm,v)
print(ver)

lib = "liberty/simcells"
xml="obj_dir"
execute("rm -f "..xml.."/*")
execute("verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC "..v)
goldxml = verilator.tranxml(xml,v)
print("gold = "..goldxml)
lib = liberty.read(lib)
print(lib)
goldcnv = hardcaml.cnv(goldxml)
print(goldcnv)
gold=verible.cnvitm(lib,goldcnv)
cnv = hardcaml.cnv(ver)
print(cnv)
rev=verible.cnvitm(lib,cnv)
goldsat=verible.satitm(gold)
revsat=verible.satitm(rev)
print(verible.cmpitm(goldsat,revsat))
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
print(itms.itm())
