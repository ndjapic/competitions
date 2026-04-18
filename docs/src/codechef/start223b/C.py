for tcase in range(int(input())):
	n = int(input())
	a = list(map(int, input().split()))
	b = list(map(int, input().split()))

	if any(a[i] > b[i] for i in range(n)):
		print('No')
	else:
		mn = 1 << 60
		ans = True
		i = n-1
		while i >= 0 and ans:
			if a[i] < b[i]:
				ans = a[i] < mn
			mn = min(mn, a[i])
			i -= 1

		print('Yes' if ans else 'No')
