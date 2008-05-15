#biohacker/trunk/ptools/parse-biolog.py
import sys
class Biolog_parser:
    def __init__( self ):
        pass

    def parse(self, filename ):
        stream = open( filename )
        title = self.parse_title( stream.readline() )
        header = self.parse_header( stream.readline() )
        biolog = []
        for line in stream:
            biolog.append( self.parse_experiment( line, header ) )
        stream.close()
        return biolog

    def parse_title(self, title ):
        return title.strip()

    def parse_header(self, header ):
        return header.strip().split('\t')

    def parse_column(self, header, column ):
        if header == 'Data Definition':
            return column
        if header == 'Medium':
            return self.parse_medium( column )
        else:
            return self.parse_growth_phenotype( column )

    def parse_data_definition(self, data_definition):
        return data_definition

    def parse_medium(self, column ):
        media = []
        for medium in column.split('(e)-'):
            media.append( medium.strip('(e)') )
        return media

    def parse_growth_phenotype(self, phenotype ):
        predictions = ['in vivo','FBA', 'rFBA']
        i = 0
        growth_phenotype = {}
        for growth in phenotype.split('/'):
            growth_phenotype[predictions[i]] = growth
            i += 1
        return growth_phenotype
    
    def parse_experiment(self, line, header ):
        experiment = {}
        i = 0
        for col in line.split('\t'):
            experiment[header[i]] = self.parse_column( header[i], col )
            i += 1
        return experiment


class Biolog_PM_parser:
    def parse(self, filename ):
        pm = {}
        stream = open(filename)
        header = self.parse_header( stream.readline() )
        for line in stream:
            columns = self.parse_columns( line.strip().split('\t'), header ) 
            pm[columns[header[0]]] = columns
        stream.close()
        return pm
    def parse_header( self, header ):
        header = header.split('\t')
        new_header = []
        for head in header:
            new_header.append( head.strip('[] \n(e)') )
        return new_header
    
    def parse_columns( self, columns, header ):
        i = 0
        pm = {}
        for column in columns:
            if i == 1:
                carbon_source = self.parse_column( column, i )
            elif i == 2:
                pm[carbon_source] = self.parse_column( column, i )
            else:
                pm[header[i]] = self.parse_column( column, i )
            i+= 1
        return pm

    def parse_column( self, column, column_number ):
        if column_number == 0:
            return self.parse_biolog_plate( column )
        if column_number == 1:
            return self.parse_carbon_source( column )
        else:
            return self.parse_influx( column )
        
    def parse_biolog_plate( self, column ):
        return column

    def parse_carbon_source( self, column ):
        return column.strip('(e) ')

    def parse_influx( self, column ):
        return int( column )

    
class Experiment:
    def toSexp( self, pylist ):
        if pylist:
            return "( %s )" % ' '.join( pylist )
        else:
            return "Nil"

    def toBoolean( self, pybool ):
        if pybool:
            return "T"
        else:
            return "Nil"

    def __init__( self, experiment_name, nutrients, growth_p, essential_compounds, ko,  ki=[], toxins=[], bootstrap_compounds=[]  ):
        self.experiment = {}
        self.experiment['name'] = experiment_name
        self.experiment['nutrients'] = self.toSexp( nutrients )
        self.experiment['growth?'] = self.toBoolean( growth_p )
        self.experiment['ko'] = self.toSexp( ko )
        self.experiment['ki'] = self.toSexp( ki )
        self.experiment['toxins'] = self.toSexp( toxins )
        self.experiment['bootstrap-compounds'] = self.toSexp( bootstrap_compounds )
        self.experiment['essential-compounds'] = self.toSexp( essential_compounds )


    def write( self, stream ):
        #print self.experiment
        stream.write( """
(experiment %(name)s %(nutrients)s
   :growth?  %(growth?)s
   :essential-compounds %(essential-compounds)s
   :knock-outs %(ko)s
   :knock-ins %(ki)s
   :toxins %(toxins)s
   :bootstrap-compounds %(bootstrap-compounds)s)\n"""   % self.experiment)

class ExperimentMaker:
    def __init__( self, biolog, pm, compounds, gene_names, essential_compounds ):
        self.biolog = biolog
        self.pm = pm
        self.compounds = compounds
        self.gene_names = gene_names
        self.essential_compounds = essential_compounds

    def convert_nutrients( self, nutrient_set ):
        ecocyc_nutrients = []
        for nutrient in nutrient_set:
            if nutrient in compounds:
                if 'ecocyc-id' in self.compounds[nutrient] and self.compounds[nutrient]['ecocyc-id'] != '':
                    ecocyc_nutrients.append( self.compounds[nutrient]['ecocyc-id'] )
                elif 'metacyc-id' in self.compounds[nutrient] and compounds[nutrient]['metacyc-id'] != '':
                    ecocyc_nutrients.append( self.compounds[nutrient]['metacyc-id'] )
                else:
                    ecocyc_nutrients.append( nutrient )
        return ecocyc_nutrients

    def get_nutrient_media( self, od, medium ):
        nutrient_media = []
        if 'nh4' in medium:
            for carbon_source in [0, 1]:
                if od in self.pm[carbon_source]:
                    for nutrient in self.pm[carbon_source][od]:
                        if self.pm[carbon_source][od][nutrient] > 0:
                            nutrient_media.append( nutrient )
                            
        elif 'succ' in medium:
            nitrogen_source = 2
            if od in self.pm[nitrogen_source]:
                for nutrient in self.pm[nitrogen_source][od]:
                    if self.pm[nitrogen_source][od][nutrient]  > 0:
                        nutrient_media.append( nutrient )
        else:
            sys.stderr.write("Error: Can't find appropriate Biolog Plate for experiment %s and medium %s\n" % (od, medium ) )
        
        return self.convert_nutrients( nutrient_media )

    def get_mutant_growth( self, ko, model):
        mutant_growth = {}
        for mutant in ko:
            if mutant != 'Data Definition' and mutant != 'Medium':
                gene = self.gene_names[mutant[0:5]]
                if ko[mutant][model] == '+':
                    mutant_growth[gene] = True
                elif ko[mutant][model] == '-':
                    mutant_growth[gene] = False
                else:
                    sys.stderr.write( "Mutant %s growth %s not well defined in model %s\n" % (mutant, ko[mutant][model], model ) )
        return mutant_growth
    
    def make_experiment_name( self, media, mutant, model ):
        return model.replace(" ", "_") + "_" +  mutant + "_" + '_'.join( media )
    def make_experiments(self, model):
        experiments = []
        for nutrient_condition in self.biolog:
            if 'Data Definition' in nutrient_condition and 'Medium' in nutrient_condition:
                media = self.get_nutrient_media( nutrient_condition['Data Definition'], nutrient_condition['Medium'] )
                mutant_growth = self.get_mutant_growth( nutrient_condition, model)
                for mutant in mutant_growth:
                    experiment_name = self.make_experiment_name( nutrient_condition['Medium'], mutant, model )
                    experiments.append( Experiment( experiment_name, media, mutant_growth[mutant], self.essential_compounds, [mutant] ) )
            else: 
                sys.stderr.write('ko %s is not the correct format\n' % nutrient_condition )
        return experiments

def write_experiments( filename, experiments ):
    stream = open( filename, 'w' )
    for experiment in experiments:
        experiment.write( stream )
    stream.close()


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


def parse_gene_names( filename ):
    stream = open( filename )
    header = stream.readline().strip().split('\t')
    genes = {}
    for line in stream:
        gene, blattner = line.strip().split('\t')
        genes[blattner] = gene
    return genes

if __name__ == '__main__':
    filename = 'biolog-model-comp.txt' 
    biolog_parser = Biolog_parser()
    biolog = biolog_parser.parse( filename )
    pm_filename = 'biolog-pm%d.txt'
    pm = []
    i = 0
    pm_parser = Biolog_PM_parser()
    for i in range(3):
        pm.append( pm_parser.parse( pm_filename % (i+1) ) )
        
    essential_compounds = ['L-ALPHA-ALANINE','ARG','ASN','L-ASPARTATE','CYS','GLN','GLT','GLY','HIS','ILE','LEU','LYS','MET','PHE','PRO','SER','THR','TRP','TYR','VAL','DATP','TTP','DGTP','DCTP','ATP','UTP','GTP','CTP','L-1-PHOSPHATIDYL-ETHANOLAMINE','CARDIOLIPIN','L-1-PHOSPHATIDYL-GLYCEROL','C6','BISOHMYR-GLC','ADP-L-GLYCERO-D-MANNO-HEPTOSE','KDO','UDP-GLUCOSE','UDP-GALACTOSE','DTDP-RHAMNOSE','GDP-MANNOSE','N-ACETYL-D-GLUCOSAMINE',]

    gene_names = parse_gene_names( 'gene-blattner.txt' )
    compounds = parse_compound_mappings( 'iAF1260-ecocyc-cpd-mappings.txt' )
    em = ExperimentMaker( biolog, pm, compounds, gene_names, essential_compounds )
    experiments = em.make_experiments( 'in vivo' )
    experiment_file = 'data/biolog-experiments.lisp'
    write_experiments( experiment_file, experiments )
    
    
