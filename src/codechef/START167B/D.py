for tcase in range(int(input())):
    n = int(input())
    h = (n+1) // 2

    a = [[0]*n for i in range(n)]

    for i in range(n):
        for j in range(n):
            a[i][j] = (h+i+j) % n + 1

    for i in range(n):
        print(' '.join(map(str, a[i])))
