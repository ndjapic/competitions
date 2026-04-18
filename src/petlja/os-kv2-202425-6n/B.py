vreme, x = input().split()
h, m = vreme.split(':')
h, m, x = int(h), int(m), int(x)

m += h*60
if m > 180:
    m += 60

m -= x
if m > 180:
    m -= 60

h, m = divmod(m, 60)
print(f'{h:02d}:{m:02d}')
