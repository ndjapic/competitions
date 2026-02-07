n = int(input())
s = input()

sam_lista = ['a', 'e', 'i', 'o', 'u']
sam_skup = set(sam_lista)
# Proveravanje pripadnosti skupu je brže nego proveravanje pripadnosti listi.
# Zato smo listu pretvorili u skup.

sugl = [x for x in s if x not in sam_skup]
print(len(sugl))
