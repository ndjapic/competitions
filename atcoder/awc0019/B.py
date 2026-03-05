n, k = map(int, input().split())

ans = 0
for i in range(n):
	s = input()
	if sum(1 for x in s if x == '!') >= k:
		ans += 1

print(ans)
