n = int(input())
odg = 0

for i in range(n):
    m = v = c = s = False
    lozinka = input()
    d = len(lozinka) >= 8

    for znak in lozinka:
        if 'a' <= znak <= 'z':
            m = m or True
        elif 'A' <= znak <= 'Z':
            v = v or True
        elif '0' <= znak <= '9':
            c = c or True
        else:
            s = s or True

    if all([d, m, v, c, s]):
        odg += 1

print(odg)
