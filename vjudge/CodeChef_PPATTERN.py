for tcase in range(int(input())):
    n = int(input())
    a = [[0]*n for i in range(n)]

    x = 0
    for s in range(2*n-1):
        for i in range(max(0, s-n+1), min(s+1, n)):
            j = s-i
            x += 1
            a[i][j] = x

    for i in range(n):
        print(' '.join(map(str, a[i])))
