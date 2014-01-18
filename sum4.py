# Given A, an array of integers, find out if there are any four numbers in the
# array that sum up to zero (the same element can be used multiple times).
# For example given A = [2,3,1,0,-4,-1] a solution can be 3 + 1 + 0 + -4 = 0
# or 0 + 0 + 0 + 0 = 0

# O(n4) solution: brute force all four number combinations
# O(n2) solution: rewrite a + b + c + d = 0 as a + b = -(c + d)
#                 and store n ^ 2 sums in a hashset
def sum4(A):
  sums= {}
  for a in A:
    for b in A:
      sums[a + b] = (a, b)

  for c in A:
    for d in A:
      if -(c + d) in sums:
        print(sums[-(c + d)][0], sums[-(c + d)][1], c, d)
        return

  print "no solution"


sum4([2,3,1,0,-4,-1])
