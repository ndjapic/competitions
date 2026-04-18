potrosnja = input()
n = len(potrosnja)

r = 0
for i in range(n-1):
    if potrosnja[i] != potrosnja[i+1]:
        r += 1

racun = [r]*(n+1)
for k in range(2, n+1):
    for i in range(k-1, n, k):

        if potrosnja[i-1] == potrosnja[i]:
            racun[k] += 1
        else:
            racun[k] -= 1

        if i < n-1:
            if potrosnja[i] == potrosnja[i+1]:
                racun[k] += 1
            else:
                racun[k] -= 1

r = min(racun)
k = 1
while racun[k] > r: k += 1
print(k, r)
