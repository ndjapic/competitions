n = int(input())
l = n - n%5
r = l+5
print(l if n-l < r-n else r)
