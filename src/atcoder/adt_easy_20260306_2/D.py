n = int(input())

a = [list(map(int, input())) for i in range(n)]

ans = 0
for dx in range(-1, 2):
	for dy in range(-1, 2):
		if not dx == dy == 0:
			for x in range(n):
				for y in range(n):
					i, j = x, y
					z = 0
					for k in range(n):
						z = 10*z + a[i][j]
						i += dx
						j += dy
						i %= n
						j %= n
					ans = max(ans, z)

print(ans)
