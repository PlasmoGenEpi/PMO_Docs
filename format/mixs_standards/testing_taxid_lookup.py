#!/usr/bin/env python3

from Bio import Entrez


tax_ids = [9606, 5833]

Entrez.email = 'example@gmail.com'  # Put your email here
handle = Entrez.efetch('taxonomy', id=tax_ids, rettype='xml')
response = Entrez.read(handle)

for entry in response:
    sci_name = entry.get('ScientificName')
    lineage_taxa = entry.get('Lineage').split(';')
    print(sci_name, ' > '.join(lineage_taxa), sep=',')
