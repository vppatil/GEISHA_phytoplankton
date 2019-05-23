#' Database of phytoplankton functional trait files compiled by Rimet et al. 2019. see original reference for data sources and calculations
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{Kingdom}{genus name}
#'  \item{Phylum}{species name}
#'  #' Trait-based MFG classifications for common Eurasion/North American phytoplankton species.
#' See accompanying manuscript for sources
#'
#' @format A data frame with columns:
#' \describe{
#'  \item{Kingdom}{Taxonomic kingdom}
#'  \item{Phylum}{Taxonomic phylum}
#'  \item{Class}{Taxonomic class}
#'  \item{Order}{Taxonomic order}
#'  \item{Family}{Taxonomic family}
#'  \item{Genus}{Taxonomic genus}
#'  \item{Species}{Taxonomic species}
#'  \item{Cell.surface.area}{surface area in µm²}
#'  \item{Cell.biovolume}{volume in µm3}
#'  \item{MLD}{maximum linear dimension of algal object in µm}
#'  \item{FG}{Functional Groups from Reynolds et al. 2002}
#'  \item{FG_Padisak}{Functional groups from Padisak et al. 2009}
#'  \item{FG_Kruk}{Functional groups from Kruk et al. 2010}
#'  \item{Colony.surface.area.with.mucilage}{µm²}
#'  \item{Colony.biovolume.without.mucilage.}{biovolume in µm3}
#'  \item{Colony.biovolume}{biovolume in µm3}
#'  \item{Mobility apparatus}{binary 1/0; 1=presence of flagella or raphe}
#'  \item{Colonial}{binary 1/0; 1 means that colonial growth is common}
#'  \item{Heterotrophic}{binary 1/0; 1 means capacity for heterotrophy}
#'  \item{Mixotrophy}{binary 1/0; 1 means capacity for mixotrophy}
#'  \item{Filament}{binary 1/0; 1 means Filamentous growth form}
#'  \item{Nano.microphytoplankton}{Nanoplankton if < 20 µm  diameter; else microphytoplankton}
#'  \item{Mucilage}{binary 1/0; 1= presence of mucilage}
#'  
#' }

#' @docType data
#'
#' @usage data(algaetraits)
#'
#' @keywords datasets
#'
"algaetraits"