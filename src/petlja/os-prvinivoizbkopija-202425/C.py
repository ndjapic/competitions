from re import sub
import sys
input = sys.stdin.readline

n = int(input().strip())
p = map(int, sub(r'\s+', ' ', input()).strip().split())
p0 = next(p)
dp0, dp1 = 0, p0

for p1 in p:
	dp0, dp1 = dp1, max(dp0 + max(p0, p1) * 2, dp1 + p1)
	p0 = p1

print(dp1)
