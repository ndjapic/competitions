from math import isqrt

n = int(input())
p = int(input())

p10 = 100**(p+4)
ans = 0

for i in range(1, n+1):
    if isqrt(i)**2 < i:
        x = isqrt(i * p10)
        d = []
        while x > 0:
            d.append(x % 10)
            x //= 10
        ans += sum(d[-p:])

print(ans)
