from math import isqrt

p, s = map(int, input().split())
s //= 2
dd = s*s - 4*p

if dd < 0:
	print(-1)
else:
	d = isqrt(dd)
	if dd > d*d or (s-d) % 2 == 1:
		print(-1)
	else:
		a, b = (s-d) // 2, (s+d) // 2
		print(a, b)
