function factorial(n)
  local x = 1
  i = 2
  while i <= n do
    x = x * i
    i = i + 1
  end
  return x
end

print("fact=",factorial(6))

v = "test/hardcaml/multiplier_test.sv"
lib = "liberty/simcells"
-- verible.tran(v)
print(verible.tranlst(v))
print(yosys.gold(v))
print(liberty.read(lib))
print(itms.itm())
print(verible.tranitm(""))
