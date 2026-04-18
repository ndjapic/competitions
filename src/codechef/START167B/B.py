for tcase in range(int(input())):
    n = int(input())
    a = list(map(int, input().split()))

    l, r = 1, 10**6
    for x in a[1:]:
        if x > a[0]:
            r = min(r, (a[0]+x) // 2)
        elif x < a[0]:
            l = max(l, (a[0]+x+1) // 2)

    print(r-l+1)
