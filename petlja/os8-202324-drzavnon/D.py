n = int(input())
a = [int(x) for x in input().split()]
prime = 10**9 + 7

dp = [0]*(n+1)
dp[0] = 1

for i in range(n):
    a[i] = min(a[i], n-1-i)
    if i > 1: dp[i] += dp[i-1]
    dp[i] %= prime
    dp[i+1] += dp[i]
    dp[i+a[i]+1] -= dp[i]

print(dp[n-1])
