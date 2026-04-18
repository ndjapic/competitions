n, m = map(int, input().split())
ans, seen = 0, set()

for i in range(m):
	r, c = map(int, input().split())

	s = set()
	s.add((r, c))
	s.add((r, c+1))
	s.add((r+1, c))
	s.add((r+1, c+1))

	if seen.isdisjoint(s):
		ans += 1
		seen.update(s)

print(ans)
