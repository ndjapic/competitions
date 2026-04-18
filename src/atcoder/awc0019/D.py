n, m, t = map(int, input().split())

dp = [[0]*(m+1) for i in range(n+1)]
ans = 0

for i in range(n):
	a, b, c = map(int, input().split())
	if b >= t:
		c = 0
	for j in range(m+1):
		dp[i+1][j] = dp[i][j]
	for j in range(c, m+1):
		dp[i+1][j] = max(dp[i+1][j], dp[i][j-c] + a)
		ans = max(ans, dp[i+1][j])

print(ans)
