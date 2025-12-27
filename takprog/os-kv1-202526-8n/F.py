from functools import cmp_to_key

def cmp(l, r):
	lr, rl = l+r, r+l
	if lr < rl:
		return -1
	elif lr > rl:
		return 1
	else:
		return 0

n = int(input())
a = sorted(input().split(), key=cmp_to_key(cmp), reverse=True)
ans = ''.join(map(str, a))

print(ans)
