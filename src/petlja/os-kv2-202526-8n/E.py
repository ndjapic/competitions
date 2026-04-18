n = int(input())
a = input().split()
odg = sum(int(a[i]) * (i+1) * (n-i) for i in range(n))
print(odg)
