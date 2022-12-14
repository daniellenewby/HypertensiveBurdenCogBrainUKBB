
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

model_2L_brain <- function(
    outcome,
    exposure,
    data,
    brainvol
){
  
  model <- glm(outcome ~ exposure + 
                 Age_at_assessment +
                 as.factor(Education) +
                 as.factor(smoking_status) +
                 as.factor(Gender)*Age_at_assessment +
                 as.factor(Gender)*poly(Age_at_assessment, 2) +
                 as.factor(UKBB_centre) +
                 BMI +
                 as.factor(Ethnicity) +
                 as.factor(diabetes) +
                 as.factor(high_chol) +
                 f.25000_Volumetric_scaling_T1_head_space +
                 
                 `25756-2_ScannerXposition`      +                    
                 `25757-2.ScannerYposition`            +               
                 `25758-2_ScannerZposition`     +                      
                 `25759-2_Scannerposition` ,
               
               data = data , family = gaussian
  )
  
  model1 <- summary(model)$coefficients
  model1_confint <- na.omit(confint(model))
  model1 <- cbind(model1, model1_confint)
  model1 <- as.data.frame(model1)
  
  ## add in FDR correction
  model1$`Pr(>|t|)` <- p.adjust(model1$`Pr(>|t|)`, method = "fdr", n = nrow(model1) )
  model1$n <- rep(nobs(model), nrow(model1))
  model1$`t value` <- NULL
  model1$`Std. Error` <- NULL
  model1 <- model1[2,]
  
  model1$Description <- c(paste(brainvol, " (n = ",model1$n[1],")", sep = "")
  )
  
  model1 <- model1[,c(6,1,3,4,2)]
  
  rownames(model1) <- NULL
  
  return(model1)
  
}


model_2L_brain_interaction <- function(
    outcome,
    exposure,
    data,
    brainvol
){
  
  model <- glm(outcome ~ exposure*Age_at_assessment + 
                 as.factor(Education) +
                 as.factor(smoking_status) +
                 as.factor(Gender)*Age_at_assessment +
                 as.factor(Gender)*poly(Age_at_assessment, 2) +
                 as.factor(UKBB_centre) +
                 BMI +
                 as.factor(Ethnicity) +
                 as.factor(diabetes) +
                 as.factor(high_chol) +
                 f.25000_Volumetric_scaling_T1_head_space +
                 
                 `25756-2_ScannerXposition`      +                    
                 `25757-2.ScannerYposition`            +               
                 `25758-2_ScannerZposition`     +                      
                 `25759-2_Scannerposition` ,
               
               data = data , family = gaussian
  )
  
  model1 <- summary(model)$coefficients
  model1_confint <- na.omit(confint(model))
  model1 <- cbind(model1, model1_confint)
  model1 <- as.data.frame(model1)
  
  ## add in FDR correction
  model1$`Pr(>|t|)` <- p.adjust(model1$`Pr(>|t|)`, method = "fdr", n = nrow(model1) )
  model1$n <- rep(nobs(model), nrow(model1))
  model1$`t value` <- NULL
  model1$`Std. Error` <- NULL
  model1 <- model1[c(2,19),]
  
  model1$Description <- c(paste(brainvol, " : Main Effect (n = ",model1$n[1],")", sep = ""), paste(brainvol, " : Age Interaction" , sep = "")
  )
  
  model1 <- model1[,c(6,1,3,4,2)]
  
  rownames(model1) <- NULL
  
  return(model1)
  
}


model_3L_brain <- function(
    outcome,
    exposure,
    exposurelevels,
    data,
    brainvol
){
  
  model <- glm(outcome ~ exposure + 
                 Age_at_assessment +
                 as.factor(Education) +
                 as.factor(smoking_status) +
                 as.factor(Gender)*Age_at_assessment +
                 as.factor(Gender)*poly(Age_at_assessment, 2) +
                 as.factor(UKBB_centre) +
                 BMI +
                 as.factor(Ethnicity) +
                 as.factor(diabetes) +
                 as.factor(high_chol) +
                 f.25000_Volumetric_scaling_T1_head_space +
                 
                 `25756-2_ScannerXposition`      +                    
                 `25757-2.ScannerYposition`            +               
                 `25758-2_ScannerZposition`     +                      
                 `25759-2_Scannerposition` ,
               
               data = data , family = gaussian
  )
  
  model1 <- summary(model)$coefficients
  model1_confint <- na.omit(confint(model))
  model1 <- cbind(model1, model1_confint)
  model1 <- as.data.frame(model1)
  
  ## add in FDR correction
  model1$`Pr(>|t|)` <- p.adjust(model1$`Pr(>|t|)`, method = "fdr", n = nrow(model1) )
  model1$n <- rep(nobs(model), nrow(model1))
  model1$`t value` <- NULL
  model1$`Std. Error` <- NULL
  model1 <- model1[2:3,]
  model1[3,] <- c(NA, NA, NA, NA, model1$n[2])
  
  model1$Description <- c(exposurelevels, paste(brainvol, " (n =",model1$n[1],")", sep = "")
  )
  
  model1 <- model1[c(3,1,2),]
  model1 <- model1[,c(6,1,3,4,2)]
  
  rownames(model1) <- NULL
  
  return(model1)
  
}


model_5L_brain <- function(
    outcome,
    exposure,
    exposurelevels,
    data,
    brainvol
){
  
  model <- glm(outcome ~ exposure + 
                 Age_at_assessment +
                 as.factor(Education) +
                 as.factor(smoking_status) +
                 as.factor(Gender)*Age_at_assessment +
                 as.factor(Gender)*poly(Age_at_assessment, 2) +
                 as.factor(UKBB_centre) +
                 BMI +
                 as.factor(Ethnicity) +
                 as.factor(diabetes) +
                 as.factor(high_chol) +
                 f.25000_Volumetric_scaling_T1_head_space +
                 
                 `25756-2_ScannerXposition`      +                    
                 `25757-2.ScannerYposition`            +               
                 `25758-2_ScannerZposition`     +                      
                 `25759-2_Scannerposition` ,
               
               data = data , family = gaussian
  )
  
  model1 <- summary(model)$coefficients
  model1_confint <- na.omit(confint(model))
  model1 <- cbind(model1, model1_confint)
  model1 <- as.data.frame(model1)
  
  ## add in FDR correction
  model1$`Pr(>|t|)` <- p.adjust(model1$`Pr(>|t|)`, method = "fdr", n = nrow(model1) )
  model1$n <- rep(nobs(model), nrow(model1))
  model1$`t value` <- NULL
  model1$`Std. Error` <- NULL
  model1 <- model1[2:5,]
  model1[5,] <- c(NA, NA, NA, NA, model1$n[2])
  
  model1$Description <- c(exposurelevels, paste(brainvol, " (n =",model1$n[1],")", sep = "")
  )
  
  model1 <- model1[c(5,1,2,3,4),]
  model1 <- model1[,c(6,1,3,4,2)]
  
  rownames(model1) <- NULL
  
  return(model1)
  
}




#source(here("Github","HypertensiveBurdenCogBrainUKBB", "CleaningData.R"))

#source(here("Github","HypertensiveBurdenCogBrainUKBB", "RunStudy.R"))





