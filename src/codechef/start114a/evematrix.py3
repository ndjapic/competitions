for tcase in range(int(input())):
    n = int(input())
    nn = n*n

    s = [[], []]
    for x in range(1, nn+1):
        s[x%2].append(x)

    a = []
    for i in range(n):
        a.append([])
        for j in range(n):
            if i < j or i == j and i % 2 == 0:
                a[i].append(s[1].pop())
            else:
                a[i].append(s[0].pop())

    if n > 2:
        a[1][0], a[2][2] = a[2][2], a[1][0]

    if n == 2:
        print(-1)
    else:
        for i in range(n):
            print(*a[i])
