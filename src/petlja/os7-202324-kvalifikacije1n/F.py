from math import isqrt

def sqr(x):
    return x*x

p = int(input())

x = 1
a = 2
aa = 4

while sqr(isqrt(p)) < p and aa <= p:
    while p % aa == 0:
        x *= a
        p //= aa
    if p % a == 0:
        p //= a
    a += 1
    aa = a*a

if sqr(isqrt(p)) == p:
    x *= isqrt(p)

print(x)
