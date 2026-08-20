# RE

n, m = map(int, input().split())
s = [input() for i in range(n)]
t = [input() for i in range(m)]

a, found = 0, False
while a < n-m+1 and not found:
	b = 0
	while b < n-m+1 and not found:
		i, found = 0, True
		while i < m and found:
			j = 0
			while j < m and found:
				found = s[a+i][b+j] == t[i][j]
				j += 1
			i += 1
		b += 1
	a += 1

print(a, b)
