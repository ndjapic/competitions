zbir = 0
zbir += int(input())
zbir += int(input())
zbir += int(input())
zbir += int(input())

broj = 4
while zbir / broj < 4.50:
    zbir += 5
    broj += 1

print(broj - 4)
