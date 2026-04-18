n, m = map(int, input().split())
c = list(map(int, input().split()))

ans = 0
for i in range(n):
	a, b = map(int, input().split())
	a -= 1
	b = min(b, c[a])
	ans += b
	c[a] -= b

print(ans)
