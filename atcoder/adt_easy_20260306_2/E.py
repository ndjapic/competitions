n, t = int(input()), 0

for h in map(int, input().split()):
	d, h = divmod(h, 5)
	t += 3*d
	while h > 0:
		t += 1
		h -= 3 if t % 3 == 0 else 1

print(t)
