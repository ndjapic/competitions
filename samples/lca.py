class Solution:
    def assignEdgeWeights(self, edges: List[List[int]], queries: List[List[int]]) -> List[int]:
        n, prime = len(edges) + 1, 10**9+7

        tree = [[] for v in range(n+1)]
        for u, v in edges:
            tree[u].append(v)
            tree[v].append(u)

        def dfs(u):
            e = 0
            while n >> e > 0:
                anc[u].append(anc[anc[u][e]][e])
                e += 1

            time1[u] = time[0]
            time[0] += 1
            for v in tree[u]:
                if dist[v] == -1:
                    dist[v] = dist[u] + 1
                    anc[v].append(u)
                    dfs(v)
            time2[u] = time[0]
            time[0] += 1

        def isanc(u, v):
            return time1[u] <= time1[v] and time2[v] <= time2[u]

        def lca(u, v):
            if not isanc(u, v):
                e = len(anc[u]) - 1
                while e >= 0:
                    if not isanc(anc[u][e], v):
                        u = anc[u][e]
                    e -= 1
                u = anc[u][0]
            return u

        anc = [[] for v in range(n+1)]
        anc[1].append(1)
        dist = [-1]*(n+1)
        dist[1] = 0
        time1, time2 = [0]*(n+1), [0]*(n+1)
        time = [0]
        dfs(1)

        p = 1
        pow2 = [p]
        for i in range(n):
            p <<= 1
            if p >= prime:
                p -= prime
            pow2.append(p)

        answer = []
        for u, v in queries:
            d = dist[u] - 2*dist[lca(u, v)] + dist[v]
            answer.append(pow2[d-1] if d > 0 else 0)
        return answer
