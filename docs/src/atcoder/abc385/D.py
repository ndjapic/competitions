from bisect import bisect

n, m, sx, sy = [int(_) for _ in input().split()]
dx, dy = {}, {}
houses = []

for i in range(n):
    x, y = input().split()
    houses.append( ( int(x), int(y) ) )

for i in range(m):
    d, c = input().split()
    c = int(c)

    if (d == 'U' or d == 'D') and sx not in dx:
        dx[sx] = []

    if (d == 'L' or d == 'R') and sy not in dy:
        dy[sy] = []

    if d == 'U':
        dx[sx].append([sy, sy+c])
        sy += c
    elif d == 'D':
        dx[sx].append([sy-c, sy])
        sy -= c
    elif d == 'L':
        dy[sy].append([sx-c, sx])
        sx -= c
    elif d == 'R':
        dy[sy].append([sx, sx+c])
        sx += c

for x in dx:
    dx[x].sort()
    for i in range(1, len(dx[x])):
        dx[x][i][1] = max(dx[x][i][1], dx[x][i-1][1])

for y in dy:
    dy[y].sort()
    for i in range(1, len(dy[y])):
        dy[y][i][1] = max(dy[y][i][1], dy[y][i-1][1])

ans = 0
for x, y in houses:
    visited = False

    if not visited and x in dx:
        i = bisect(dx[x], [y, y])
        visited = (i < len(dx[x])) and (dx[x][i][0] <= y) or (0 < i) and (y <= dx[x][i-1][1])

    if not visited and y in dy:
        i = bisect(dy[y], [x, x])
        visited = (i < len(dy[y])) and (dy[y][i][0] <= x) or (0 < i) and (x <= dy[y][i-1][1])
    
    if visited:
        ans += 1

print(sx, sy, ans)
