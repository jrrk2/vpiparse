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


