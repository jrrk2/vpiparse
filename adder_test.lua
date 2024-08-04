print(Sys.test("Hello","World"))

v = "test/hardcaml/adder_test.sv"
lib = "liberty/simcells"
-- verible.tran(v)
ver = verible.tranlst(v)
print(ver)
gold = yosys.gold(v)
print(gold)
lib = liberty.read(lib)
print(lib)
cnv = hardcaml.cnv(ver)
print(cnv)
print(itms.itm())
print(verible.tranitm(lib,gold,cnv))
