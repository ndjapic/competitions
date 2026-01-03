from math import gcd, lcm

s = set()
l = []
for a in range(1, 10):
    for b in range(1, 10):
        for c in range(1, 10):
            x = lcm(a, b) + lcm(b, c) + lcm(c, a)
            if x % 3 > 0 and x % 2 == 0:
                s.add(x)
                l.append((a, b, c))

print(sorted(l))
