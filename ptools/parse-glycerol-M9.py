class Glycerol_M9_parser:
    def __init__( self ):
        pass
    def parse(self, filename ):
        stream = open(filename)
        title = stream.readline()
        print title
        header = self.parse_header( stream.readline() )
        legend = stream.readline()
        genes = []
        for line in stream:
            genes.append( self.parse_columns( line.strip().split('\t'), header ) )
        stream.close()
        return genes
    
    def parse_header( self, header ):
        return header.strip().split('\t')

    def parse_columns( self, columns, header ):
        gene = {}
        for i in range( len( columns ) ):
            if i < len( header ):
                gene[header[i]] = columns[i]
            else:
                print columns
        return gene

def parse_gene_names( filename ):
    stream = open( filename )
    header = stream.readline().strip().split('\t')
    genes = {}
    for line in stream:
        gene, blattner = line.strip().split('\t')
        genes[blattner] = gene
    return genes


def parse_compound_mappings( filename ):
    stream = open( filename )
    header = stream.readline().strip().split('\t')
    compounds = {}
    for line in stream:
        columns = {}
        i  = 0
        for column in line.strip().split('\t'):
            columns[header[i]] = column
            i += 1
        compounds[columns[header[0]]] = columns 
    return compounds

def convert_nutrients( compounds, nutrient_set ):
    ecocyc_nutrients = []
    for nutrient in nutrient_set:
        if nutrient in compounds:
            if 'ecocyc-id' in compounds[nutrient] and compounds[nutrient]['ecocyc-id'] != '':
                ecocyc_nutrients.append( compounds[nutrient]['ecocyc-id'] )
            elif 'metacyc-id' in compounds[nutrient] and compounds[nutrient]['metacyc-id'] != '':
                ecocyc_nutrients.append( compounds[nutrient]['metacyc-id'] )
            else:
                ecocyc_nutrients.append( nutrient )
    return ecocyc_nutrients

def write_experiment( stream, nutrients, gene, growth_p ):
    stream.write( '(experiment %s (nutrients %s) (off %s))\n' % (growth_p, ' '.join( nutrients ), gene) )

if __name__ == '__main__':
    infilenames = ['essential-on-glycerol-M9.txt','nonessential-on-glycerol-M9.txt']
    outfilename = 'glycerol-M9.lisp'
    gene_names = parse_gene_names( 'gene-blattner.txt')
    glycerol_M9_parser = Glycerol_M9_parser()
    glycerol_M9 = []
    M9 = ['h2o','h','glyc', 'nh4', 'pi', 'co2', 'so4', 'o2']
    ecocyc_compound_mappings = parse_compound_mappings( 'iAF1260-ecocyc-cpd-mappings.txt' )
    for filename in infilenames:
        glycerol_M9.append( glycerol_M9_parser.parse( filename ) )
    stream = open( outfilename, 'w')
    nutrients = convert_nutrients( ecocyc_compound_mappings, M9 )
    for gene in glycerol_M9[0]:
        if 'b number' in gene and gene['b number'] != '' and gene['b number'] in gene_names:
            write_experiment( stream, nutrients, gene_names[gene['b number']], 'nogrowth')
    for gene in glycerol_M9[1]:
        if 'b number' in gene and gene['b number'] != '' and gene['b number'] in gene_names:
            write_experiment(stream, nutrients, gene_names[gene['b number']], 'growth' )
    stream.close()
        
        
        
