reci = [input()]

while reci[-1] != 'gotovo':
    reci.append(input())

kraj = reci.pop()
kraj = reci.pop()

if len(reci) > 0:
    recenica = ', '.join(reci) + ' i ' + kraj
else:
    recenica = kraj

print(recenica)
