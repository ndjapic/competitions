from heapq import *

n, a = int(input()), []
for i in range(n):
	w, d = map(int, input().split())
	a.append((w, d))

a.sort(key=lambda x: (x[1] +x[0]))

pq, s = [], 0
for w, d in a:
	if d >= s:
		heappush(pq, -w)
		s += w
	elif len(pq) > 0 and w < -pq[0]:
		s -= -heappop(pq)
		heappush(pq, -w)
		s += w

print(len(pq))
