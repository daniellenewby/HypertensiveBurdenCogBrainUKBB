
## HypertensiveBurdenCogBrainUKBB ----

#load up packages
rm(list = ls())
library(data.table)
library(ggplot2)
library(RSQLite)
library(sqldf)
library(vroom)
library(tictoc)
library(forcats)
library(dplyr)
library(gtools)
library(devtools)
library(ukbtools)
library(lubridate)
library(twang)
library(tableone)
library(knitr)
library(kableExtra)
library(VennDiagram)
library(patchwork)
library(lmtest)
library(QuantPsyc)
library(compareGroups)
library(textutils)
library(lavaan)
library(tidyr)
library(here)

#where to save data
saving_directory <- here::here("Github", "HypertensiveBurdenCogBrainUKBB", "DataOutputs")

ReadInDataAndClean <- TRUE # if already read in data and cleaned it put TRUE if not put false


#FUNCTIONS

# Gets the populations of people with the diseases listed in codes_jD
findCases = function( codes_jD, epi_iSiC ) {
  
  # Build populations
  print( "Building populations for disease list A" );
  library( parallel )
  bCase_iSiD <- array( data = NA,
                       dim = c( dim( epi_iSiC )[1], length( codes_jD ) ),
                       dimnames = list( subject = dimnames( epi_iSiC )[[1]],
                                        disease = names( codes_jD ) ) );
  cl <- makeCluster( getOption( "cl.cores", 
                                4 ) );
  clusterExport( cl = cl,
                 varlist = c( "codes_jD" ),
                 envir = environment() );
  for( iD in 1:length( codes_jD ) ){
    # Select population
    print( "Extracting population with ", names( codes_jD )[iD]) #names( codes_jD )[iD] ) );
    clusterExport( cl = cl,
                   varlist = c( "iD" ),
                   envir = environment() );
    bCase_iS <- parApply( cl = cl,
                          X = epi_iSiC,
                          MARGIN = 1,
                          FUN = function( e_iC ){
                            any( !is.na( match( codes_jD[[iD]],
                                                e_iC ) ) );
                          } );
    bCase_iSiD[ ,iD] <- bCase_iS;
  }
  stopCluster( cl );
  
  # Return results
  return( bCase_iSiD );
  
}


# other functions here






#source(here("Github","HypertensiveBurdenCogBrainUKBB", "CleaningData.R"))

#source(here("Github","HypertensiveBurdenCogBrainUKBB", "RunStudy.R"))





