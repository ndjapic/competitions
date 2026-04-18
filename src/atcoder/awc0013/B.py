n, m = map(int, input().split())
adj = [set() for a in range(n)]

for i in range(m):
	a, b = map(int, input().split())
	adj[a-1].add(b-1)

c = 0
for a in range(n):
	for b in adj[a]:
		if a in adj[b]:
			c += 1

print(c // 2)
