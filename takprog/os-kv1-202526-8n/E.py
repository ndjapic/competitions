n, x = map(int, input().split())
a = list(map(int, input().split()))

cx = ans = 0
for j in range(n):
	if a[j] == 0:
		ans += cx
	if a[j] == x:
		cx += 1

print(ans)