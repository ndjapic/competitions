n, k = map(int, input().split())
a = list(map(int, input().split()))

s = current_sum = max_so_far = l = ans = 0
for r in range(n):
	s += a[r]
	if r-l+1 > k:
		s -= a[l]
		l += 1
	if s <= 0:
		l = r+1
		s = 0
	current_sum = max(current_sum, s)
	max_so_far = max(max_so_far, current_sum)

print(max_so_far)
