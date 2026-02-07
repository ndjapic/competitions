from collections import Counter

n = int(input())

ukrasi = 0
for i in range(n):
	c = Counter(input())
	ukrasi += c['o']

print(ukrasi)
