from collections import Counter

n, k = map(int, input().split())
c = Counter()
s = 0
c[0] += 1
ans = 0

for x in map(int, input().split()):
	s += x
	ans += c[s-k]
	c[s] += 1

print(ans)
