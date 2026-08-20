n, k = [int(x) for x in input().split()]
a = [int(x) for x in input().split()]
a.sort()

i0 = 0
for i in range(n-k+1):
    if a[i+k-1] - a[i] < a[i0+k-1] - a[i0]:
        i0 = i

podskup = a[i0:i0+k]
podskup = [str(x) for x in podskup]
print(' '.join(podskup))
