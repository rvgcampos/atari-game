import music21
parsed = music21.converter.parse('doom.mid')
lista = []

for p in parsed.pitches:
    lista.append(p.name)

mapa = {
    'A': '6',
    'B': '31',
    'C': '29',
    'D': '26',
    'E': '23',
    'F': '22',
    'G': '3'
}

atari = []
for l in lista:
    for k, v in mapa.items():
        if k == l[0]:
            for _ in range(12):
                atari.append(v)


arq = open('sfx_f.txt', 'w')
arq.write('SFX_F\n')
contador = 0
for a in atari:
    arq.write(f'    .byte #{a}\n')
    contador = contador+1

# print(lista)
# print(atari)
# print(f'Contador: {contador}')
