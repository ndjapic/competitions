from sys import setrecursionlimit
maxn = 10**5
log2n = 17
setrecursionlimit(maxn)

adj = [[] for v in range(maxn)]
anc = [[0]*log2n for v in range(maxn)]
d, h, c = [0]*maxn, [0]*maxn, [0]*maxn

def dfs(u):

    for e in range(log2n - 1):
        anc[u][e+1] = anc[anc[u][e]][e]

    c[u] = 0
    arr = []
    for v in adj[u]:
        if anc[u][0] != v:
            anc[v][0] = u
            d[v] = d[u] + 1
            dfs(v)
            arr.append(h[v])
            c[u] += c[v]
    if len(arr) == 0:
        h[u] = 0
        c[u] = 1
    else:
        h[u] = min(arr) + 1

for tcase in range(int(input())):
    n = int(input())
    for i in range(n-1):
        u, v = [int(x) - 1 for x in input().split()]
        adj[u].append(v)
        adj[v].append(u)

    d[0] = 0
    anc[0][0] = 0
    dfs(0)

    arr = []
    for v in range(n):
        if h[v] == 0:
            u = v
            for e in reversed(range(log2n)):
                if 2*d[anc[u][e]] >= d[v]:
                    u = anc[u][e]
            arr.append(c[u])

    print(max(arr))
