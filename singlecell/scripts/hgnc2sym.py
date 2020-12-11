map = {}
f = open('HGNC_to_symbols.txt')
f.readline()
while True:
    line = f.readline()
    if not line:
        f.close()
        break
    fields = line.split('\t')
    map[fields[0]] = fields[2]

fr = open('genes_of_interest.txt')
fo = open('gene_symbols_of_interest.txt', 'w')
while True:
    line = fr.readline()
    if not line:
        fr.close()
        fo.close()
        break
    fo.write(map[line.strip()])
    fo.write('\n')
