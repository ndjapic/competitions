n = int(input())
t = input()

x = y = d = 0
for ch in t:
	if ch == 'R':
		d = (d+1) % 4
	elif d == 0:
		x += 1
	elif d == 1:
		y -= 1
	elif d == 2:
		x -= 1
	elif d == 3:
		y += 1

print(x, y)
