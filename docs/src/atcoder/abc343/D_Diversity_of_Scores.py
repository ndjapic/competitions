n, t = map(int, input().split())
score = [0]*n
d = dict()
d[0] = n

for i in range(t):
    a, b = map(int, input().split())
    a -= 1

    s = score[a]
    d[s] = d.get(s, 0) - 1
    if d[s] == 0:
        del d[s]
    s += b
    d[s] = d.get(s, 0) + 1
    score[a] = s
    print(len(d))
