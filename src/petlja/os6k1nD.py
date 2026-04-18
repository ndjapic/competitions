rec = input()
recenica = []
while rec != 'gotovo':
    recenica.append(rec)
    rec = input()
odg = ', '.join(recenica[:-1])
if len(recenica) > 1:
    odg += ' i '
odg += recenica[-1]
print(odg)
