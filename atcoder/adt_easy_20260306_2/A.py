n, l, r = map(int, input().split())

a = list(range(n+1))
while l < r:
	a[l], a[r] = a[r], a[l]
	l += 1
	r -= 1

print(' '.join(map(str, a[1:])))
