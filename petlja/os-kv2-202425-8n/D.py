n = int(input())

adj = {}
for i in range(n):
    pol, kra = input().split()
    adj[pol] = kra

grad = 'Kraljevo'
print(grad)
while grad in adj:
    grad = adj[grad]
    print(grad)
