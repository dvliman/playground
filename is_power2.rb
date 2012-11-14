# how can you quickly determine whether a number is a power of 2?
# Check whether x & (x-1) = 0. If x is not an even power of 2,
# the highest position of x with 1 will have a 1 in x - 1; otherwise
# x will be 100...0 and (x-1) will be 011...1; and-ing them return 0
def is_power2(x)
  x & (x-1) == 0
end

puts is_power2(0)
puts is_power2(4)
puts is_power2(6)
