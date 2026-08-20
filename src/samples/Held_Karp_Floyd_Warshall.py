def solve_tsp_cyclic(graph_matrix):
    n = len(graph_matrix)
    # 1. Floyd-Warshall: Најкраћи путеви између свих чворова
    dist = [row[:] for row in graph_matrix]
    for k in range(n):
        for i in range(n):
            for j in range(n):
                dist[i][j] = min(dist[i][j], dist[i][k] + dist[k][j])

    # 2. Динамичко програмирање (Bitmask DP)
    # dp[mask][i] = мин цена да се посете чворови у 'mask' и заврши у 'i'
    dp = [[float('inf')] * n for _ in range(1 << n)]
    
    # Полазимо из чвора 0
    dp[1 << 0][0] = 0

    # Попуњавање DP табеле
    for mask in range(1 << n):
        for u in range(n):
            if dp[mask][u] == float('inf'):
                continue
            
            # Покушај преласка у следећи непосећени чвор 'v'
            for v in range(n):
                if not (mask & (1 << v)):
                    new_mask = mask | (1 << v)
                    dp[new_mask][v] = min(dp[new_mask][v], dp[mask][u] + dist[u][v])

    # 3. Затварање циклуса: Повратак у почетни чвор (0)
    full_mask = (1 << n) - 1
    min_cycle = float('inf')
    for i in range(1, n):
        min_cycle = min(min_cycle, dp[full_mask][i] + dist[i][0])

    return min_cycle

# Пример графа (матрица суседства, користите велики број за непостојеће гране)
INF = float('inf')
example_graph = [
    [0, 10, 15, 20],
    [10, 0, 35, 25],
    [15, 35, 0, 30],
    [20, 25, 30, 0]
]

result = solve_tsp_cyclic(example_graph)
print(f"Минимална цена циклуса: {result}")
