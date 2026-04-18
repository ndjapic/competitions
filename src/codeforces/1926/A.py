t = int(input())
for _ in range(t):
	a = b = 0

	for ch in input():
		if ch == 'A':
			a += 1
		else:
			b += 1

	print('A' if a > b else 'B')
