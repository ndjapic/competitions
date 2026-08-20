for tci in range(int(input())):
	n, m, x, y = map(int, input().split())
	x -= 1
	y -= 1

	adj, seen, par = [[] for v in range(n)], [False]*n, [-1]*n

	for i in range(m):
		u, v = map(int, input().split())
		adj[u-1].append(v-1)
		adj[v-1].append(u-1)

	for v in range(n):
		adj[v].sort(reverse=True)

	dfs, seen[x] = [x], True
	seen[x] = True

	while dfs[-1] != y:
		u = dfs.pop()
		seen[u] = False
		for v in adj[u]:
			dfs.append(v)
			par[v] = u
			seen[v] = True

	ans = [str(y+1)]
	v = y
	while v != x:
		v = par[v]
		ans.append(str(v+1))

	ans.reverse()
	print(' '.join(ans))
