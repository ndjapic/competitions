n, x = map(int, input().split())

for a in map(int, input().split()):
	if a < x:
		x = a
		print(1)
	else:
		print(0)
