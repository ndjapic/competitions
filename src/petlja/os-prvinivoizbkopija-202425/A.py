n, m = input().split()
n, m = int(n), int(m)
map = [input() for i in range(n)]
perimeter = 0

for i in range(n):
    for j in range(m):
        if map[i][j] == '#':
            perimeter += 4

for i in range(n):
    for j in range(m-1):
        if map[i][j:j+2] == '##':
            perimeter -= 2

for i in range(n-1):
    for j in range(m):
        if map[i][j] + map[i+1][j] == '##':
            perimeter -= 2

print(perimeter)
