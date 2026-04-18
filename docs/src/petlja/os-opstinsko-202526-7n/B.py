a, b, c = [int(tx) for tx in input().split()]
x, y, z = [int(tx) for tx in input().split()]

if x < a:
	print('ne')
elif x > a:
	print('da')
elif y < b:
	print('ne')
elif y > b:
	print('da')
elif z < c:
	print('ne')
else:
	print('da')
