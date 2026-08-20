a = input()
b = input()

odg = a[0] != b[0] and a[1] != b[1] and a[2] != b[2]
if odg:
    c = 0
    if a[0] in b: c += 1
    if a[1] in b: c += 1
    if a[2] in b: c += 1
    odg = c == 1

print('da' if odg else 'ne')
