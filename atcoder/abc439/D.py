from collections import Counter

n = int(input())
a = list(map(int, input().split()))
ans = 0

c3, c7 = Counter(), Counter()
for x in a:
	d, m = divmod(x, 5)
	if m == 0:
		ans += c3[d] * c7[d]
	d, m = divmod(x, 3)
	if m == 0:
		c3[d] += 1
	d, m = divmod(x, 7)
	if m == 0:
		c7[d] += 1

c3, c7 = Counter(), Counter()
for x in reversed(a):
	d, m = divmod(x, 5)
	if m == 0:
		ans += c3[d] * c7[d]
	d, m = divmod(x, 3)
	if m == 0:
		c3[d] += 1
	d, m = divmod(x, 7)
	if m == 0:
		c7[d] += 1

print(ans)
