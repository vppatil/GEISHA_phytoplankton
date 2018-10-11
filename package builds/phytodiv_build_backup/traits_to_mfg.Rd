\name{traits_to_mfg}
\alias{traits_to_mfg}

\title{phytoplankton MFG assignment}

\description{Assign a morphofunctional group based on binary functional traits and higher taxonomy}

\usage{traits_to_mfg(flagella,size,colonial,filament,centric, gelatinous,aerotopes,class,order)}

\arguments{
  \item{flagella}{1 if flagella are present, 0 if they are absent.}
  \item{size}{Character string: 'large' or 'small'. Classification criteria is left to the user.}
  \item{colonial}{1 if typically colonial growth form, 0 if typically unicellular.}
  \item{filament}{1 if dominant growth form is filamentous, 0 if not.}
  \item{centric}{1 if diatom with centric growth form, 0 if not. NA for non-diatoms.}
  \item{gelatinous}{1 if mucilagenous sheath is typically present, 0 if not.}
  \item{aerotopes}{1 if aerotopes allowing buoyancy regulation are typically present, 0 if not.}
  \item{class}{The taxonomic class of the species; We recommend algaebase as a standard reference.}
  \item{order}{The taxonomic order of the species; We recommend algaebase as a standard reference.}
}

\value{
A character string of the species' morphofunctional group
}

\references{
Salmaso, Nico, Luigi Naselli-Flores, and Judit Padisak. "Functional classifications and their application in phytoplankton ecology." Freshwater Biology 60.4 (2015): 603-619
}

\author{
Vijay Patil
Alaska Science Center
USGS
vpatil@usgs.gov
}

\note{
This software is preliminary or provisional and is subject to revision. It is being provided to meet
the need for timely best science. The software has not received final approval by the
U.S. Geological Survey (USGS). No warranty, expressed or implied, is made by the USGS or the U.S. Government as to the functionality of the software and related material nor shall the fact of
release constitute any such warranty. The software is provided on the condition that neither the
USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or
unauthorized use of the software.
}

\examples{
traits_to_mfg(1,"large",1,0,NA,0,0,"Euglenophyceae","Euglenales")
}
