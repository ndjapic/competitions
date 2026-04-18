from secrets import randbelow
wrap = randbelow(1<<18)

h, w, n = map(int, input().split())
row = [set() for x in range(h)]
col = [set() for y in range(w)]

for i in range(n):
	x, y = [int(t)-1 for t in input().split()]
	row[x].add(y ^ wrap)
	col[y].add(x ^ wrap)

q = int(input())
for k in range(q):
	query = input().split()
	if query[0] == '1':

		x = int(query[1]) - 1
		print(len(row[x]))
		for y in row[x]:
			col[y ^ wrap].discard(x ^ wrap)
		row[x].clear()

	else:

		y = int(query[1]) - 1
		print(len(col[y]))
		for x in col[y]:
			row[x ^ wrap].discard(y ^ wrap)
		col[y].clear()
