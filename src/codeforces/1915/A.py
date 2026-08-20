t = int(input())
for _ in range(t):
	a, b, c = input().split()
	a, b, c = int(a), int(b), int(c)

	if a == b:
		print(c)
	elif a == c:
		print(b)
	else:
		print(a)
