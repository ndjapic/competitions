n = int(input())
a = sorted(map(int, input().split()))

ans = 1
s = []
s.append(a[0])

for x in a[1:]:
	if x-1 != s[-1]:
		s = []
		ans += 1
	s.append(x)

print(ans)
