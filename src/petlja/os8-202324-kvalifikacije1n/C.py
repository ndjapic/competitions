from bisect import bisect

x1, y1 = [int(t) for t in input().split()]
x2, y2 = [int(t) for t in input().split()]

n = int(input())
v = [int(t) for t in input().split()]
v.sort()
vodg = bisect(v, max(x1, x2)) - bisect(v, min(x1, x2) - 1)

m = int(input())
h = [int(t) for t in input().split()]
h.sort()
hodg = bisect(h, max(y1, y2)) - bisect(h, min(y1, y2) - 1)

print(vodg + hodg)
