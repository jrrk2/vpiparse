
function map(lib,cnv)
  return verible.mapitm(lib,hardcaml.cnv(cnv))
end

function convert(cnv)
  return verible.cnvitm(cnv)
end

function uhdm_parse(v)
  uhdm="slpp_all/surelog.uhdm"
  execute("rm -f "..uhdm)
  execute("surelog -parse -sverilog "..v)
  print(uhdm)
  ver=pipe.uhdmtop("uhdm-dump "..uhdm)
  print(ver)
  return ver
end

function readlib()
  lib = "liberty/simcells"
  lib = liberty.read(lib)
  print(lib)
  return lib
end

function verilator_parse(lib,v)
  xml="obj_dir"
  execute("rm -f "..xml.."/*")
  execute("verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC "..v)
  goldxml = verilator.tranxml(xml)
  print("gold = "..goldxml)
  return goldxml
end

function eqv(topmod,gold,rev)
  itms.dump("_gold",gold)
  itms.dump("_rev",rev)
  print(external.eqv(topmod))
end

function z3sat(gold,rev)
  goldsat=verible.satitm(gold)
  revsat=verible.satitm(rev)
  print(z3.cmp(goldsat,revsat))
end

function minisat(gold,rev)
  goldsat=verible.satitm(gold)
  revsat=verible.satitm(rev)
  print(verible.cmpitm(goldsat,revsat))
end

function gatemap(cnv)
maplib="liberty/NangateOpenCellLibrary_typical"
libmap = liberty.read(maplib)
print(libmap)
itmmap=map(libmap,cnv)
print(itmmap)
itms.dump("_map",itmmap)
topmod=itms.nam(ver)
print(topmod)
print(external.sta(topmod.."_map.v",topmod,maplib))
end
