n = int(input())
adj = [[] for u in range(n)]
for i in range(n-1):
    u, v = input().split()
    u, v = int(u)-1, int(v)-1
    adj[u].append(v)
    adj[v].append(u)

mx = 0
for u in range(n):
    for x, (y, v) in enumerate(sorted([(len(adj[v]), v) for v in adj[u]], reverse=True)):
        mx = max(mx, (x+1)*y)
print(n-1-mx)
