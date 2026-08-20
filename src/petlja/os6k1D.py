rec = input()
recenica = []
while rec != 'gotovo':
    recenica.append(rec)
    rec = input()
rec = recenica.pop()
odg = ', '.join(recenica) + ' i ' + rec
print(odg)
