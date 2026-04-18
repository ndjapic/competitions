n, m = map(int, input().split())

mat = []
for i in range(n):
	row = list(map(int, input().split()))
	mat.append(row)

ans = 0
for j in range(m):
	c0 = 0
	for i in range(n):
		if mat[i][j] == 0:
			c0 += 1

	if c0 <= 2:
		ans += 1

print(ans)
