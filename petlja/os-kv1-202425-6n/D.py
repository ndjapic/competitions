n = int(input())

# Rezervišemo memoriju za n+1 brojeva za poene u danima od 0 do n.
# Dan 0 se ne koristi, ali se mora rezervisati, jer indeksi počinju od 0.
x = [0]*(n+1)

odg = 1
for dan in range(1, n+1):
    x[dan] = int(input())
    if x[dan] >= x[odg]:
        odg = dan

print(odg)
