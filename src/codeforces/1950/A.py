t = int(input())
for _ in range(t):
	a, b, c = input().split()
	a, b, c = int(a), int(b), int(c)
	if a < b < c:
		print("STAIR")
	elif a < b > c:
		print("PEAK")
	else:
		print("NONE")
