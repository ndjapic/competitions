for _ in range(int(input())):
	s = input()

	if s == 'abc':
		print('YES')
	elif s == 'acb':
		print('YES')
	elif s == 'cba':
		print('YES')
	elif s == 'bac':
		print('YES')
	else:
		print('NO')
