from sortedcontainers import SortedList

n, q = map(int, input().split())
sl = SortedList([(0, 0), (n+1, n+1)])
white = n

for i in range(q):
	l, r = map(int, input().split())

	i1 = sl.bisect_left((l, l))
	i2 = sl.bisect_left((r+2, r+2))
	if sl[i1-1][1] >= l-1:
		i1 -= 1

	if i1 < i2:
		l = min(l, sl[i1][0])
		r = max(r, sl[i2-1][1])

		for i in range(i1, i2):
			white += sl[i][1] - sl[i][0] + 1

		del(sl[i1:i2])

	white -= r-l+1
	sl.add((l, r))
	print(white)
