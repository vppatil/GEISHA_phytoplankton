# algaeClassify 1.4

## Major changes

- algae_search() function removed in 1.3.0 and later versions
- Version 1.4.x includes alternative tools for standardizing taxonomic names
and extracting higher taxonomy using the Geographic Names Resolution Service and 
the ITIS taxonomic database.

## Minor changes
- when functions call the aggregate() function, the firs argument is no longer named. This eliminates errors caused by a change in the first argument name from 'formula' to 'x'.
- the reference for nordicmicroalgae.org has been updated in the docs for
species_mfg_library