from collections import Counter
from sys import setrecursionlimit
setrecursionlimit(10**6)

n = int(input())
a = list(map(int, input().split()))

adj = [[] for v in range(n)]
par = [0]*n
ans = [False]*n
seen = Counter()

for i in range(n-1):
	u, v = map(int, input().split())
	u -= 1
	v -= 1
	adj[u].append(v)
	adj[v].append(u)

def dfs(u):
	seen[a[u]] += 1
	for v in adj[u]:
		if par[u] != v:
			par[v] = u
			ans[v] = ans[u] or seen[a[v]] > 0
			dfs(v)
	seen[a[u]] -= 1

dfs(0)
for k in range(n):
	print('Yes' if ans[k] else 'No')
