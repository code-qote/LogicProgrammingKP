import sys

if len(sys.argv) != 2:
    print('Input format:\n'+ sys.argv[0] + ' <file.ged>')
    sys.exit(1) 

words = ['NAME', 'SEX', 'FAMS', 'FAMC', 'MARNM']
with open(sys.argv[1], 'r', encoding='cp1251') as file:
    lines = [line.strip('\n') for line in file.readlines() if line.split()[1] in words]
    data = [i.replace('\iiin', '').split(' ')[1:] for i in lines]

for i in range(len(data)):
    if data[i][0] == '_MARNM' and '?' not in data[i][1] and '//' not in data[i][1]:
        data[i - 1][2] = data[i][1]
    data[i] = ' '.join(data[i]).replace('/', '').split(' ')
    if data[i][0] == 'NAME':
        data[i][1] = ' '.join(data[i][1:]).strip().replace("'", '')
        data[i] = data[i][:2]
data = [e for e in data if e[0] in words[:-1]]

families = dict()
person = [str(), str()]
for elem in data:
    if elem[0] in words[:2]:
        person[elem[0] == 'SEX'] = elem[1]
    if elem[0] == 'FAMS':
        if elem[1] not in families:
            families[elem[1]] = [set(), set()]
        families[elem[1]][0].add(tuple(person))
    elif elem[0] == 'FAMC':
        if elem[1] not in families:
            families[elem[1]] = [set(), set()]
        families[elem[1]][1].add(tuple(person))

with open('tree.pl', 'w') as file:
    res = []
    for family in families:
        for parent in families[family][0]:
            for child in families[family][1]:
                if parent[1] == 'M':
                    res.append(f"father('{parent[0]}', '{child[0]}').")
                else:
                    res.append(f"mother('{parent[0]}', '{child[0]}').")
    res.sort(key=lambda x: x[0])
    for i in res:
        print(i, file=file)

