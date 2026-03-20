from collections import Counter

s = input()
if len(s) % 2 == 1:
	print('No')
else:
	h = len(s) // 2
	if any(s[i] != s[2*i] for i in range(h)):
		print('No')
	else:
		c = Counter(s)
		if any(c[x] != 2 for x in c.keys()):
			print('No')
		else:
			print('Yes')
