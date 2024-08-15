
function map(lib,cnv)
  return verible.mapitm(lib,hardcaml.cnv(cnv))
end

function gatemap(maplib,cnv)
  local libmap = liberty.read(maplib)
  print(libmap)
  local itmmap=map(libmap,cnv)
  print(itmmap)
  return itmmap
end

function convert(cnv)
  return verible.cnvitm(cnv)
end

function uhdm_parse(maplib,v)
  local uhdm="slpp_all/surelog.uhdm"
  execute("rm -f "..uhdm)
  execute("surelog -parse -sverilog "..v)
  print(uhdm)
  local ver=pipe.uhdmtop("uhdm-dump "..uhdm)
  print(ver)
  local revmap=gatemap(maplib,ver)
  sta(maplib,revmap)
  return ver, revmap
end

function readlib()
  local lib = liberty.read("liberty/simcells")
  print(lib)
  return lib
end

function verilator_parse(maplib,v)
  local xml="obj_dir"
  execute("rm -f "..xml.."/*")
  execute("verilator --quiet --xml-only -Wno-WIDTHEXPAND -Wno-WIDTHTRUNC "..v)
  local goldver = verilator.tranxml(xml)
  print("gold = "..goldver)
  local goldmap=gatemap(maplib,goldver)
  sta(maplib,goldmap)
  return goldver, goldmap
end

function verible_parse(maplib,v)
  local goldver = verible.tranlst(v)
  local goldmap=gatemap(maplib,goldver)
  sta(maplib,goldmap)
  return goldver, goldmap
end

function rtlil_parse(v)
  local gold = pipe.rtlil("yosys -q -q -p 'read_verilog -sv "..v.."; synth; write_ilang'")
  print(gold)
  return gold
end

function eqv(gold,rev)
  local topmod=itms.nam(rev)
  itms.dump("_gold",gold)
  itms.dump("_rev",rev)
  print(external.eqv(topmod))
end

function z3sat(gold,rev)
  local goldsat=verible.satitm(gold)
  local revsat=verible.satitm(rev)
  print(z3.cmp(goldsat,revsat))
end

function minisat(gold,rev)
  local goldsat=verible.satitm(gold)
  local revsat=verible.satitm(rev)
  print(verible.cmpitm(goldsat,revsat))
end

function sta(maplib,itmmap)
itms.dump("_map",itmmap)
local topmod=itms.nam(itmmap)
print(topmod)
print(external.sta(topmod.."_map.v",topmod,maplib))
end

function allsat(goldver,revver)
  local satlib=readlib()
  local gold=convert(map(satlib,goldver))
  local rev=convert(map(satlib,revver))
  z3sat(gold,rev)
  minisat(gold,rev)
end

function rtlilsat(v,revver)
  local ilang=rtlil_parse(v)
  local satlib=readlib()
  local rev=convert(map(satlib,revver))
  eqv(ilang,rev)
  z3sat(ilang,rev)
  --minisat(ilang,rev)
end
