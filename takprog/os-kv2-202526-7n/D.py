n = int(input())
a = list(map(int, input().split()))

l, r, precutano = 0, n-1, 0
while l < r:
	mn, mx = min(a[l], a[r]), max(a[l], a[r])
	precutano += mx - mn
	a[l] = a[r] = mn
	l += 1
	r -= 1

print(precutano)
print(' '.join(map(str, a)))
