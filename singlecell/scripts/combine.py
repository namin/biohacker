fo = open('combined.csv', 'w')
fh = open('header.csv')
fo.write(fh.readline())
fh.close()

fv = open('var.csv')
fv.readline()
fx = open('data.csv')

while True:
    lv = fv.readline().strip()
    lx = fx.readline()
    if not lv or not lx:
        assert(not lv)
        assert(not lx)
        fv.close()
        fx.close()
        fo.close()
        break
    fo.write(lv+'\t'+lx)
