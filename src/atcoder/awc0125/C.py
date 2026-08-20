from math import gcd, lcm

n = int(input())
t = list(map(int, input().split()))

p = 1
for i in range(1, n):
	p = lcm(p, t[i] // gcd(t[0], t[i]))

print(str(p) + '/1')
