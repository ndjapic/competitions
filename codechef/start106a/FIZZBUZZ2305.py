n = 60
w = [0]*n

for p in [3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 49, 53, 59]:
    i = p
    while i < n:
        if w[i-p] == 0:
            w[i] = 1
        i += p

for i in range(n):
    if w[i] == 0:
        print(i, end=' ')
print('Bob')

for i in range(n):
    if w[i] == 1:
        print(i, end=' ')
print('Alice')
