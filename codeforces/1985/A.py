t = int(input())
for i in range(t):
	a, b = input().split()
	a, b = b[:1] + a[1:], a[:1] + b[1:]
	print(a, b)
