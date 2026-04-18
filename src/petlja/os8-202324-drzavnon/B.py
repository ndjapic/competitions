smerovi = set()

def sled(x, y):
	smer, duzina = input().split()
	duzina = int(duzina)
	smerovi.add(smer)

	if smer == 'gore': y += duzina
	if smer == 'dole': y -= duzina
	if smer == 'levo': x -= duzina
	if smer == 'desno': x += duzina

	return x, y

ax, ay = 0, 0
bx, by = sled(ax, ay)
cx, cy = sled(bx, by)
dx, dy = sled(cx, cy)
ex, ey = sled(dx, dy)

if (ex, ey) != (ax, ay):
	print('ne')
else:
	p = abs(cx * cy)
	if (bx, by) == (dx, dy): p = 0
	if len(smerovi) < 4: p = 0
	print('da', p)
