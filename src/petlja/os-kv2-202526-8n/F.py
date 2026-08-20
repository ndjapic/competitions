n = int(input())
a = input().split()
dp = [0]*(n+1)

for i in range(n):
	b = 1 + int(a[i])
	if i+b <= n:
		dp[i+b] = max(dp[i+b], dp[i] + b)
	dp[i+1] = max(dp[i+1], dp[i])

print(n - dp[n])
