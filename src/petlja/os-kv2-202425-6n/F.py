n = int(input())

lista = []
for i in range(n):
    ime, poeni = input().split()
    lista.append((int(poeni), ime))
lista.sort(reverse=True)

for i in range(n):
    poeni, ime = lista[i]
    if i == 0 or poeni < poeni1:
        plasman = i+1
    print(f'{plasman}. {ime} {poeni}')
    poeni1, ime1 = poeni, ime
