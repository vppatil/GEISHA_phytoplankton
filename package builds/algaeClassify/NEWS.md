# algaeClassify 1.3.1

## Major changes

- package now includes ability to classify species to MFG using a literature-derived library
of species-MFG assignments.
- The package also includes the mfg_to_csr function, which allows for CSR classification based
on a cross-mapping of morpho-functional traits that define MFG and CSR functional groups
- algaebase database search functions (algae_search and spp_list_algaebase) have been removed to 
comply with new terms of use for the algaebase online database.
-traits_to_mfg and traits_to_csr functions have been modified to allow users to supply their own trait database

## Minor changes
- species_to_mfg function now includes extra columns to indicate if an mfg classification was ambiguous, based on genus only, or based on a partial (fuzzy) taxonomic name match.
