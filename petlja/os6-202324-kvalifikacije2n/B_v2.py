dom, gos = [0]*5, [0]*5
odg = 0
for i in range(1, 5):
    dom[i], gos[i] = input().split()
    dom[i], gos[i] = int(dom[i]), int(dom[i])
    odg = max(odg, dom[i] - gos[i-1])
print(odg)
