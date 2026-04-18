for tcase in range(int(input())):
	n, k = map(int, input().split())

	casinos = []
	for i in range(n):
		l, r, real = map(int, input().split())
		casinos.append((l, real))
	casinos.sort()

	for l, r in casinos:
		if l <= k <= r:
			k = r

	print(k)
