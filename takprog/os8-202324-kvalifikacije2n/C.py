r, k = [int(x) for x in input().split()]

m = []
for i in range(r):
    m.append(input().split())

for i in range(r):
    for j in range(k):
        if m[i][j] == 'S': si, sj = i, j
        if m[i][j] == 'C': ci, cj = i, j

sd, cd = r+k, r+k

for i in range(r):
    for j in range(k):
        if m[i][j] == 'T':
            sd = min(sd, abs(si-i) + abs(sj-j))
            cd = min(cd, abs(ci-i) + abs(cj-j))

d = min(sd+cd, abs(si-ci) + abs(sj-cj))
print(d)
