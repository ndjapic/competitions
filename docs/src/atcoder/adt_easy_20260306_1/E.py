n, m = map(int, input().split())

c = [n-1]*n
for i in range(m):
	a, b = map(int, input().split())
	c[a-1] -= 1
	c[b-1] -= 1

ans = []
for a in range(n):
	k = c[a]
	ans.append(k * (k-1) * (k-2) // 6)

print(' '.join(map(str, ans)))
