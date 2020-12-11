f = open('../../gene_symbols_of_interest.txt')
interest = f.read().split('\n')
f.close()

f = open('combined.csv')
fo = open('limited.csv', 'w')
fo.write(f.readline())
while True:
    line = f.readline()
    if not line:
        f.close()
        fo.close()
        break
    i = line.index('\t')
    gene = line[0:i]
    if gene in interest:
        fo.write(line)
