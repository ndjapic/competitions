from heapq import *

n, m = map(int, input().split())
cars = [[] for l in range(n)]

for j in range(m):
	l, r = map(int, input().split())
	cars[l-1].append(r-1)

pq = []
ans = True
for l in range(n):
	if ans:
		for r in cars[l]:
			heappush(pq, r)
		if len(pq) > 0:
			r = heappop(pq)
			ans = l <= r

print('Yes' if ans and len(pq) == 0 else 'No')
