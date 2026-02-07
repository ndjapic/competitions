h1 = int(input())
h2 = int(input())
a = int(input())
b = int(input())
c = int(input())

if h1 > h2:
    h1, h2 = h2, h1

if h1 < c:
    odg = 0
elif h2 < c+a:
    odg = 0
elif h2 < h1+b:
    odg = h2
else:
    odg = h1+b

print(odg)
