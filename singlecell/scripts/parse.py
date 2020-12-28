import collections
import os

import GEOparse
import numpy as np
import pandas as pd
import scipy.io
import scipy.sparse as sp_sparse
import tables


def b2a(fg, attr):
    return [gene.decode("utf-8") for gene in getattr(fg, attr).read().tolist()]


CountMatrix = collections.namedtuple('CountMatrix', ['feature_ref', 'barcodes', 'matrix'])


def get_matrix_from_h5(filename):
    with tables.open_file(filename, 'r') as f:
        mat_group = f.get_node(f.root, 'matrix')
        barcodes = [barcode.decode('utf-8') for barcode in f.get_node(mat_group, 'barcodes').read().tolist()]
        data = getattr(mat_group, 'data').read()
        indices = getattr(mat_group, 'indices').read()
        indptr = getattr(mat_group, 'indptr').read()
        shape = getattr(mat_group, 'shape').read()
        matrix = sp_sparse.csc_matrix((data, indices, indptr), shape=shape)

        feature_ref = {}
        feature_group = f.get_node(mat_group, 'features')
        feature_ids = b2a(feature_group, 'id')
        feature_names = b2a(feature_group, 'name')
        feature_types = b2a(feature_group, 'feature_type')
        feature_ref['id'] = feature_ids
        feature_ref['name'] = feature_names
        feature_ref['feature_type'] = feature_types
        tag_keys = b2a(feature_group, '_all_tag_keys')
        for key in tag_keys:
            if type(key) is not str:
                key = key.decode("utf-8")
                feature_ref[key] = b2a(feature_group, key)
            else:
                print(type(key), key)
        cmx = CountMatrix(feature_ref, barcodes, matrix)
        gene_names = cmx.feature_ref['name']
        barcodes = cmx.barcodes
        mat = cmx.matrix
        rows, cols = mat.shape
        if len(gene_names) == rows and len(barcodes) == cols:
            return pd.DataFrame.sparse.from_spmatrix(data=mat, columns=barcodes, index=gene_names)
        elif len(gene_names) == cols and len(barcodes) == rows:
            return pd.DataFrame.sparse.from_spmatrix(data=mat, index=barcodes, columns=gene_names)
        else:
            raise Exception("Rows, cols =", rows, cols, "genes, barcodes=", len(gene_names), len(barcodes))


def parse_mtx(*, barcodes_path, features_path, mtx_path):
    """Return a dataframe with barcodes and features and matrix data"""
    mat = scipy.io.mmread(mtx_path)
    rows, cols = mat.shape
    features = pd.read_csv(features_path, delimiter='\t', compression='gzip', header=None,
                           names=['feature_id', 'gene_name', 'feature_type'])

    barcodes = pd.read_csv(barcodes_path, delimiter="\t",
                           compression='gzip',
                           header=None,
                           names=['barcode']
                           )
    if len(features.index) == rows and len(barcodes.index) == cols:
        return pd.DataFrame.sparse.from_spmatrix(data=mat, columns=barcodes['barcode'], index=features['gene_name'])
    elif len(features.index) == cols and len(barcodes.index) == rows:
        return pd.DataFrame.sparse.from_spmatrix(data=mat, index=barcodes['barcode'], columns=features['gene_name'])
    else:
        raise Exception("Rows, cols =", rows, cols, "genes, barcodes=", len(barcodes))


def get_celltype_barcodes_from_patient(*, patient, celltype, all_cells):
    return set(all_cells[(all_cells['sample_new'] == patient) &
                         (all_cells['celltype'] == celltype)]['barcode']
               .values.tolist())


def extract_celltype_from_patient(patient, raw, celltype, all_cells):
    old_barcodes = raw[patient].columns
    raw[patient].columns = [bc.split('-')[0] for bc in old_barcodes]
    celltype_barcodes = get_celltype_barcodes_from_patient(patient=patient, celltype=celltype, all_cells=all_cells)
    return raw[patient][sorted(celltype_barcodes)]


def get_metadata_barcodes_from_patient(patient, raw, all_cells):
    return set(all_cells[all_cells['sample_new'] == patient]['ID']
               .str.split('_').str.get(0)
               .values.tolist())


def get_raw_barcodes_for_patient(raw_patient):
    return set([raw_barcode.split('-')[0] for raw_barcode in raw_patient.columns])


def get_all_samples(balf_files, sample_to_patient, balf_dir):
    raw = {}
    geo = {}
    raw_barcodes = {}
    for gsm_file in balf_files:
        gsm_id, sample = gsm_file.split('_')[0:2]
        patient = sample_to_patient[sample]
        geo[patient] = GEOparse.get_GEO(geo=gsm_id, destdir=balf_dir)
        if patient == 'HC4':
            barcodes = os.path.basename(geo[patient].get_metadata_attribute('supplementary_file_1'))
            genes = os.path.basename(geo[patient].get_metadata_attribute('supplementary_file_2'))
            matrix = os.path.basename(geo[patient].get_metadata_attribute('supplementary_file_3'))
            supp_dir = 'Supp_' + barcodes.split('_barcodes')[0]
            geo[patient].download_supplementary_files(balf_dir, download_sra=False)
            raw[patient] = parse_mtx(
                barcodes_path=os.path.join(balf_dir, supp_dir, barcodes),
                features_path=os.path.join(balf_dir, supp_dir, genes),
                mtx_path=os.path.join(balf_dir, supp_dir, matrix),
            )
        else:
            raw[patient] = get_matrix_from_h5(os.path.join(balf_dir, gsm_file))
        raw[patient].columns = [bc.split('-')[0] + '_' + patient for bc in raw[patient].columns]
        raw_barcodes[patient] = raw[patient].columns
    return geo, raw, raw_barcodes


def get_model_genes(bel_graph):
    import pybel
    model_genes = []
    for node in bel_graph:
        if not isinstance(node, pybel.dsl.ComplexAbundance):
            if node.namespace == 'HGNC':
                model_genes.append(node.name)
        else:
            print('\n'.join([m.name for m in node.members]))
    return model_genes


def library_size_factors(df, celltypes, patients, all_cells):
    library_size = {}
    library_sum = 0
    for patient in patients:
        for celltype in celltypes:
            patient_celltype_barcodes = get_celltype_barcodes_from_patient(
                patient=patient, celltype=celltype, all_cells=all_cells,
            )
            library_size[patient, celltype] = df[patient][patient_celltype_barcodes].sum(axis=1)
            library_sum += library_size[patient, celltype].sum()
    library_size_factor = {}
    for patient in patients:
        for celltype in celltypes:
            library_size_factor[patient, celltype] = library_size[patient, celltype] / library_sum
    return library_size_factor


def log_norm_counts(df, size_factors, pseudocounts=1, base=2):
    if base == 2:
        return np.log2(df + pseudocounts).subtract(np.log2(size_factors + pseudocounts), axis='index')
    elif base:
        return np.log(df + pseudocounts).subtract(np.log(size_factors + pseudocounts) - np.log(base), axis='index')
    else:
        return np.log(df + pseudocounts).subtract(np.log(size_factors + pseudocounts), axis='index')
