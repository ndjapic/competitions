r, x = map(int, input().split())

if r == 1:
	print('Yes' if 1600 <= r < 3000 else 'No')
else:
	print('Yes' if 1200 <= r < 2400 else 'No')
