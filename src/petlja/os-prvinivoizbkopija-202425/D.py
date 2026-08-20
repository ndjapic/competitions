from collections import deque
import sys
input = sys.stdin.readline

n, m = map(int, input().strip().split())

prime, inf = 10**9+7, 2**60
fact = [1]*(n+1)
for i in range(1, n+1):
	fact[i] = i * fact[i-1] % prime

adj = [[] for v in range(n)]
dist = [inf]*n
dist[0] = 0
bfs = deque([0])
found = False
cw, cd = [0]*2, [0]*2

for e in range(m):
	u, v = map(int, input().strip().split())
	adj[u-1].append(v-1)
	adj[v-1].append(u-1)

for v in range(n):
	w = int(input().strip())
	cw[w%2] += 1

while len(bfs) > 0:
	u = bfs.popleft()
	cd[dist[u]] += 1
	for v in adj[u]:
		if dist[v] == inf:
			dist[v] = 1 - dist[u]
			bfs.append(v)
		elif dist[u] == dist[v]:
			found = True

ans = 0
if not found:
	if cw[0] == cd[0]:
		ans += fact[cw[0]] * fact[cw[1]]
	if cw[0] == cd[1]:
		ans += fact[cw[0]] * fact[cw[1]]

print(ans % prime)
