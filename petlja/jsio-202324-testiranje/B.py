n, k = [int(x) for x in input().split()]
a = [int(x) for x in input().split()]

e = 0
while (n >> e) > 1: e += 1
t = [[0, 0] for i in range(4 << e)]

def build(v, l, r):
    if r-l > 1:
        m = (l+r) // 2
        build(2*v+1, l, m)
        build(2*v+2, m, r)
        t[v][:] = sorted(t[2*v+1] + t[2*v+2])[2:]
    else:
        t[v][:] = [0, a[l]]
build(0, 0, n)

def query(v, l, r, x, y):
    if y <= l or r <= x:
        return [0, 0]
    elif x <= l and r <= y:
        return t[v]
    else:
        m = (l+r) // 2
        q1 = query(2*v+1, l, m, x, y)
        q2 = query(2*v+2, m, r, x, y)
        return sorted(q1 + q2)[2:]

q = int(input())
for j in range(q):
    c1, c2 = [int(x) for x in input().split()]
    ans = 0

    for i in range(max(0, c1-k), min(c1+k+1, n-1)):
        k1 = abs(i-c1)
        k2 = k - k1
        q2 = query(0, 0, n, max(i+1, c2-k2), min(c2+k2+1, n))
        ans = max(ans, a[i] + q2[1])

    print(ans)

    """
    ans = []

    for r1 in range(k+1):
        for r2 in range(k-r1+1):
            ans.append(f(c1-r1, c2-r2))
            ans.append(f(c1-r1, c2+r2))
            ans.append(f(c1+r1, c2-r2))
            ans.append(f(c1+r1, c2+r2))

    if k < r-l:
        for i in range(k+1):
            q1 = query(0, 0, n, l-i, l+i+1)
            q2 = query(0, 0, n, r-k+i, r+k-i+1)
            ans = max(ans, q1[1] + q2[1])
    else:
        for i in range(k+1):
            for j in range((r-k+i), (l+i+1)):
                q1 = query(0, 0, n, l-i, j)
                q2 = query(0, 0, n, j, r+k-i+1)
                ans = max(ans, q1[1] + q2[1])
    """
