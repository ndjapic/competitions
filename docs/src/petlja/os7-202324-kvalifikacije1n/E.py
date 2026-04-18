m = int(input())
kuce = [int(x) for x in input().split()]
kuce.sort()

n = int(input())
pred = [int(x) for x in input().split()]
pred.sort()

signal = 0
i = 0

for x in kuce:
    d = abs(pred[i] - x)
    while i+1 < n and abs(pred[i+1] - x) < d:
        i += 1
        d = abs(pred[i] - x)
    signal = max(signal, d)

print(signal)
