n, m = map(int, input().split())
a = [list(map(int, input().split())) for i in range(m)]

adj = [set() for i in range(n)]
for i in range(m):
	for j in range(n-1):
		x, y = a[i][j] - 1, a[i][j+1] - 1
		adj[x].add(y)
		adj[y].add(x)

ans = sum(n-1 - len(adj[x]) for x in range(n))
print(ans // 2)
