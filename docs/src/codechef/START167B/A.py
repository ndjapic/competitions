for tcase in range(int(input())):
    n = int(input())
    s = input()

    x = sum(map(int, s))
    print(1 if 0 < x < n else n)
