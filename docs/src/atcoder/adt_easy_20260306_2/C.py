s = input()

x, y = 0, 7
while s[x] != 'B':
	x += 1
while s[y] != 'B':
	y -= 1
ans = (x+y) % 2 == 1

if ans:
	x, y = 0, 7
	while s[x] != 'R' and s[x] != 'K':
		x += 1
	while s[y] != 'R' and s[y] != 'K':
		y -= 1
	ans = s[x] == 'R' and s[y] == 'R'

print('Yes' if ans else 'No')
