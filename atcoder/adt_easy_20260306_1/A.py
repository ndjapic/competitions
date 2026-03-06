from collections import Counter

s = input()
c = Counter(s)

i = 0
while i < 3 and c[s[i]] > 1:
	i += 1

print(s[i] if i < 3 else -1)
