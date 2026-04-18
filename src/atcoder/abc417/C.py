from collections import Counter
from random import seed, randrange

seed()
h0 = randrange(2**30)

n = int(input())
a = list(map(int, input().split()))

c, ans = Counter(), 0

for i, x in enumerate(a):
	ans += c[(i-x)^h0]
	c[(i+x)^h0] += 1

print(ans)
