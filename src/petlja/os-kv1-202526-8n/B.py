a, b, t = map(int, input().split())

if t < a:
	print('ISPOD MINIMUMA')
elif t > b:
	print('IZNAD MAKSIMUMA')
elif t == a or t == b:
	print('NA GRANICI')
elif a < t < b:
	print('IZMEDJU')