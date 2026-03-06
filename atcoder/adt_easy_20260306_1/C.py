h, w, n = map(int, input().split())

s = [set(map(int, input().split())) for i in range(h)]

c = [0]*h
for k in range(n):
	b = int(input())
	for i in range(h):
		if b in s[i]:
			c[i] += 1

print(max(c))
