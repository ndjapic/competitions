nmp = 0 # najveca moguca prednost

for i in range(4):
    dom, gos = input().split()
    dom, gos = int(dom), int(gos)
    nmp = max(nmp, dom - gos)
print(nmp)
