m = float(input())
n = float(input())
p = float(input())

if m > max(n, p):
	print('M')
elif n > max(m, p):
	print('N')
elif p > max(n, m):
	print('P')
