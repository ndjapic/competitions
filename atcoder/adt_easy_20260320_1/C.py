from collections import Counter

s = input()
h, m = divmod(len(s), 2)
c = Counter(s)

if m % 2 == 1:
	print('No')
elif any(s[2*i] != s[2*i+1] for i in range(h)):
	print('No')
elif any(c[x] != 2 for x in c.keys()):
	print('No')
else:
	print('Yes')
