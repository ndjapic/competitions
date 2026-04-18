t = int(input())
for i in range(t):
	x, y = input().split()
	x, y = int(x), int(y)
	x, y = min(x, y), max(x, y)
	print(x, y)
