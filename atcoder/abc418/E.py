from math import gcd
from collections import defaultdict, Counter
from random import seed, randrange

seed()
h0 = randrange(2**30)
slopes, centers = Counter(), Counter()

n = int(input())
x, y, ans = [0]*n, [0]*n, 0

for a in range(n):
	x[a], y[a] = map(int, input().split())
	for b in range(a):
		dx, dy = x[a] - x[b], y[a] - y[b]

		g = gcd(abs(dx), abs(dy))
		if dx < 0 or dx == 0 and dy < 0:
			g = -g
		dx, dy = (dx // g) ^ h0, (dy // g) ^ h0
		sx, sy = (x[a] + x[b]) ^ h0, (y[a] + y[b]) ^ h0

		ans += slopes[(dx, dy)] - centers[(sx, sy)]
		slopes[(dx, dy)] += 1
		centers[(sx, sy)] += 1

print(ans)
