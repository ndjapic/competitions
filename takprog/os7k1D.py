a = int(input())
b = int(input())
c = int(input())
d = int(input())
[a, b, c, d] = sorted([a, b, c, d])
# Najbliži broj broju a je broj b.
# Najbliži broj broju d je broj c.
r1 = b-a
r2 = min(b-a, c-b)
r3 = min(c-b, d-c)
r4 = d-c
m = max(r1, r2, r3, r4)
if r1 == m: print(a)
if r2 == m: print(b)
if r3 == m: print(c)
if r4 == m: print(d)
