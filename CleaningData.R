
# READING IN ALL DATA FOR ANALYSIS AND CLEANING DATA VARIABLES

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), header=TRUE, nrows = 0)
  
# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

##########################################################################
# BRAIN IMAGING VARIABLES #
###########################################################################

colwewant <- c("eid",   ## id
               "^53\\-",
               "^25000\\-"   ,      #Volumetric scaling from T1 head image to standard space
               "^25010\\-"   ,      #Volume of brain, grey+white matter
               "^25008\\-"   ,      #Volume of white matter
               "^25006\\-"   ,      #Volume of grey matter
               "^25004\\-"   ,      #Volume of ventricular cerebrospinal fluid
               "^25023\\-"   ,      #Volume of accumbens (left)
               "^25024\\-"   ,      #Volume of accumbens (right)
               "^25021\\-"   ,      #Volume of amygdala (left)
               "^25022\\-"   ,      #Volume of amygdala (right)
               "^25013\\-"   ,      #Volume of caudate (left)
               "^25014\\-"   ,      #Volume of caudate (right)
               "^25019\\-"   ,      #Volume of hippocampus (left)
               "^25020\\-"   ,      #Volume of hippocampus (right)
               "^25017\\-"   ,      #Volume of pallidum (left)
               "^25018\\-"   ,      #Volume of pallidum (right)
               "^25015\\-"   ,      #Volume of putamen (left)
               "^25016\\-"   ,      #Volume of putamen (right)
               "^25011\\-"   ,      #Volume of thalamus (left)
               "^25012\\-"       #Volume of thalamus (right)

               
)



bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))



tic()
# read in dataset with columns needed
brain_imaging_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), 
                               header=TRUE, 
                               sep=",", 
                               verbose = T,
                               select = bdtest3)

toc()   


# subset to only include those with brain imgaging data
brain_imaging_rawdata1 <- subset(brain_imaging_rawdata, as.character(brain_imaging_rawdata$`53-2.0`) != "" & !is.na(brain_imaging_rawdata$`25000-2.0`) )

##############################################
# labelling the brain imaging data variables #
##############################################

names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25000-2.0"] <- "f.25000_Volumetric_scaling_T1_head_space"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25010-2.0"] <- "f.25010_Volbrain_grey+white matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25006-2.0"] <- "f.25006_Volgrey matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25004-2.0"] <- "f.25004_vol_ventricular_CSF"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25023-2.0"] <- "f.25023_vol_accumbens_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25024-2.0"] <- "f.25024_vol_accumbens_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25021-2.0"] <- "f.25021_vol_amygdala_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25022-2.0"] <- "f.25022_vol_amygdala_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25019-2.0"] <- "f.25019_vol_hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25020-2.0"] <- "f.25020_vol_hippocampus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25017-2.0"] <- "f.25017_vol_pallidum_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25018-2.0"] <- "f.25018_vol_pallidum_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25015-2.0"] <- "f.25015_vol_putamen_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25016-2.0"] <- "f.25016_vol_putamen_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25013-2.0"] <- "f.25013_vol_caudate_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25014-2.0"] <- "f.25014_vol_caudate_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25011-2.0"] <- "f.25011_vol_thalamus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25012-2.0"] <- "f.25012_vol_thalamus_R"

##########
# date of visit #
##########
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "53-2.0"] <- "Date_of_visit"


##############################################################################
# scanner position imaging variables #
#############################################################################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41533.csv"), header=TRUE, nrows = 0)


# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant <- c("eid",   ## id
               "^54\\-", # assessment centre
               "^25756\\-"   ,      #Scanner lateral (X) brain position space
               "^25757\\-"   ,      #Scanner transverse (Y) brain position
               "^25758\\-"   ,      #Scanner longitudinal (Z) brain position
               "^25759\\-"        #Scanner table position
)


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))


tic()

scanner_position_brain_imaging <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41533.csv"), 
                                        header=TRUE, 
                                        sep=",", 
                                        verbose = T,
                                        select = bdtest3)

toc()          


#rename the variables
names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25756-2.0"] <- "25756-2_ScannerXposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25757-2.0"] <- "25757-2.ScannerYposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25758-2.0"] <- "25758-2_ScannerZposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25759-2.0"] <- "25759-2_Scannerposition"

scanner_position_brain_imaging <- subset(scanner_position_brain_imaging, scanner_position_brain_imaging$eid %in% brain_imaging_rawdata1$eid)


###################################
### white matter hypertensities ###
###################################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41321.csv"), header=TRUE, nrows = 0)


# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant <- c("eid",   ## id
               "^25781\\-"  # white matter hyperintensity,
              )


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))


tic()

brain_imaging_rawdata_WMH <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41321.csv"), 
                                   header=TRUE, 
                                   sep=",", 
                                   verbose = T,
                                   select = bdtest3)

toc()          


brain_imaging_rawdata_WMH1 <- subset(brain_imaging_rawdata_WMH, brain_imaging_rawdata_WMH$eid %in% brain_imaging_rawdata1$eid)

names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "25781-2.0"] <- "f.25781_WMH"


# merge brain imaging data together
Brain_imaging_subset <- merge(brain_imaging_rawdata1, brain_imaging_rawdata_WMH1,  by = "eid")
Brain_imaging_subset <- merge(Brain_imaging_subset, scanner_position_brain_imaging,  by = "eid")


save(Brain_imaging_subset, file = paste(saving_directory, "/brain_imaging_compiled_13122020.Rdata", sep=""))

###############################
# demographics and cognition
###############################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), header=TRUE, nrows = 0)

bdtest <- bd[,grep("[0-9]{1,10}\\-[0|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant1 <- c("eid",   ## id
                "^31\\-0",          ## sex
                "^21003\\-2",       ## age at assessment 
                "^399\\-2",         ## memory test pairs matching second round of 6 cards
                "^400\\-2",         ## time to complete memory test we need 400.02 for 6 card test
                "^404\\-2",         ## duration to first press of snap button in each round
                "^398\\-2",         ## pairs matching Number of correct matches in round
                "^20016\\-2",       ## fluid intelligence score number of correct answers 0-13
                "^20023\\-2",       ## Mean reaction time
                "^189\\-" ,         # Townsend deprivation index at recruitment
                "^6138\\-2",        ## Qualifications
                "^20116\\-2",       ## smoking status
                "^21000\\-2",       ## ethnic background imaging
                "^21000\\-0",       ## ethnic background baseline
                "^21001\\-2"   ,   ## BMI
                "^54\\-2"    ,     ## UK BB assessment centre
                "^2966\\-2"  ,# age high blood pressure diagnosed
                "^2976\\-2"  ,# Age diabetes diagnosed
                "^2443\\-2"  ,# Diabetes diagnosed by doctor
                "^4079\\-2"  ,# diastolic blood pressure automated reading
                "^4080\\-2", # systolic blood pressure automated reading
                "^6177\\-2", # medication for cholesterol, blood pressure or diabetes
                "^6153\\-2", # medication for cholesterol, blood pressure or diabetes or exogenous hormones
                "^2966\\-2"  # age high blood pressure diagnosed
)



bdtest4 <- unique(grep(paste(colwewant1, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE
))



tic()
# read in dataset 
demographics_cog_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), 
                                  header=TRUE, 
                                  sep=",", 
                                  verbose = T,
                                  select = bdtest4)

toc()

#subset to those that have imaging data
demographics_cog_rawdata1 <- subset(demographics_cog_rawdata, demographics_cog_rawdata$eid %in% Brain_imaging_subset$eid)


#####################################
# extra cognitive variables
######################################

# trail making, matrix and digits

setDTthreads(0L)
bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb43218.csv"), header=TRUE, nrows = 0)

# get column names for imaging

bdtest2a <- bd[,grep("[0-9]{1,10}\\-[0|1|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

# select the columns we need
colwewant1 <- c("eid",   ## id
                "^6348\\-2",         ## Duration to complete numeric path (trail #1)
                "^6350\\-2",         ## Duration to complete numeric path (trail #2)
                "^6373\\-2" ,      ## Number of puzzles correctly solved (Matrix pattern completion)
                "^23324\\-2"         ## Number of symbol digit matches made correctly
                
)


bdtest4a <- unique(grep(paste(colwewant1, collapse ="|"),
                        bdtest2a, value = TRUE, fixed = FALSE
))


tic()
# read in dataset 
extra_cog_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb43218.csv"), 
                           header=TRUE,
                           sep=",",
                           verbose = T,
                           select = bdtest4a)

toc()

newcogtests_rawdata <- subset(extra_cog_rawdata, extra_cog_rawdata$eid %in% Brain_imaging_subset$eid)

names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6348-2.0" ] <- "f6348_Duration_trailA"
names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6350-2.0" ] <- "f6350_Duration_trailB" 
names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6373-2.0" ] <- "f6373_matrix_pattern_puzzles_solved" 
names(newcogtests_rawdata)[names(newcogtests_rawdata) == "23324-2.0" ] <- "f23324_correct_symbol_digit_matches" 

#####################################
# tower rearranging
######################################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb43579.csv"), header=TRUE, nrows = 0)

# get column names for imaging
bdtest2ab <- bd[,grep("[0-9]{1,10}\\-[0|1|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

# to select the columns we need
colwewant8 <- c("eid",   ## id      
                "21004\\-2"      ## Number of puzzles correctly solved (tower)
                
)

bdtest4ab <- unique(grep(paste(colwewant8, collapse ="|"),
                         bdtest2ab, value = TRUE, fixed = FALSE
))


tic()
# read in dataset 
extra_cog_tower_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb43579.csv"), 
                                 header=TRUE,
                                 sep=",",
                                 verbose = T,
                                 select = bdtest4ab)

toc()

newcogtests_tower_rawdata <- subset(extra_cog_tower_rawdata , extra_cog_tower_rawdata$eid %in% Brain_imaging_subset$eid)

names(newcogtests_tower_rawdata)[names(newcogtests_tower_rawdata) == "21004-2.0" ] <- "21004_correct_tower_arranging"

###############################
# first occurence data
setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41117.csv"), header=TRUE, nrows = 0)

# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[0]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

##########################################################################
# first occurrence for diabetes
###########################################################################

colwewant <- c("eid",   ## id
               "^130706\\-" ,	#Date E10 first reported (insulin-dependent diabetes mellitus)
               "^130707\\-" ,	#Source of report of E10 (insulin-dependent diabetes mellitus)
               "^130708\\-" ,	#Date E11 first reported (non-insulin-dependent diabetes mellitus)
               "^130709\\-" ,	#Source of report of E11 (non-insulin-dependent diabetes mellitus)
               "^130710\\-" ,	#Date E12 first reported (malnutrition-related diabetes mellitus)
               "^130711\\-" ,	#Source of report of E12 (malnutrition-related diabetes mellitus)
               "^130712\\-" ,	#Date E13 first reported (other specified diabetes mellitus)
               "^130713\\-" ,	#Source of report of E13 (other specified diabetes mellitus)
               "^130714\\-" ,	#Date E14 first reported (unspecified diabetes mellitus)
               "^130715\\-" 	#Source of report of E14 (unspecified diabetes mellitus)
)


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))

tic()

first_occurence_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41117.csv"), 
                                 header=TRUE, 
                                 sep=",", 
                                 verbose = T,
                                 select = bdtest3)

toc()   

first_occurence_rawdata <- subset(first_occurence_rawdata, first_occurence_rawdata$eid %in% Brain_imaging_subset$eid)

# remove dates that have these codes
# 1901-01-01	Code has event date before participant's date of birth
# 1902-02-02	Code has event date matching participant's date of birth
# 1903-03-03	Code has event date after participant's date of birth and falls in the same calendar year as date of birth
# 2037-07-07	Code has event date in the future and is presumed to be a place-holder or other system default

first_occurence_rawdata[first_occurence_rawdata == "1901-01-01"] <- NA
first_occurence_rawdata[first_occurence_rawdata == "1902-02-02"] <- NA
first_occurence_rawdata[first_occurence_rawdata == "1903-03-03"] <- NA
first_occurence_rawdata[first_occurence_rawdata == "2037-07-07"] <- NA

#rename the variables
#Date E10 first reported (insulin-dependent diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130706-0.0" ] <- "f130706-0-0_Date_insulin_dependentDM"
#Source of report of E10 (insulin-dependent diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130707-0.0" ] <- "f130707-0-0_source__insulin_dependentDM"

#Date E11 first reported (non insulin-dependent diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130708-0.0" ] <- "f130708-0-0_Date_noninsulin_dependentDM"
#Source of report of E11 (non insulin-dependent diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130709-0.0" ] <- "f130709-0-0_source_noninsulin_dependentDM"

#Date E12 first reported (malnutrition-related diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130710-0.0" ] <- "f130710-0-0_Date_malnutrition_dependentDM"
#Source of report of E12 (malnutrition-related diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130711-0.0" ] <- "f130711-0-0_source_malnutrition_dependentDM"

#Date E13 first reported (other specified diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130712-0.0" ] <- "f130712-0-0_Date_other_specifiedDM"
#Source of report of E13 (other specified diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130713-0.0" ] <- "f130713-0-0_source_other_specifiedDM"

#Date E14 first reported (unspecified diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130714-0.0" ] <- "f130714-0-0_Date_unspecified_DM"
#Source of report of E14 (unspecified diabetes mellitus)
names(first_occurence_rawdata)[names(first_occurence_rawdata) == "130715-0.0" ] <- "f130715-0-0_source_unspecified_DM"

save(first_occurence_rawdata, file = paste(saving_directory, "/first_occurence_subset_compiled_13122020.Rdata", sep=""))

#################
# DATA CLEANING #
#############################################################################################
# in this section we are extracting the number of people taking bp meds, diabetics etc
# 6177 FOR MALES ############################################################

demographics_cog_rawdata2 <- demographics_cog_rawdata1[,c("6177-2.0" ,
                                                          "6177-2.1" ,
                                                          "6177-2.2")]


diseases_UKBB <- unique(c(as.matrix(demographics_cog_rawdata2[,paste("6177-2.",c(0:2),sep="")])))
# 
diseases_UKBB <- diseases_UKBB[!is.na(diseases_UKBB)]
stopifnot(length(diseases_UKBB) == 6)
# 

# # Using function over disease columns create a boolean matrix for each cancer disease for each subject in UKBB
bCase_iSiD_diseases = findCases( diseases_UKBB , demographics_cog_rawdata2[,paste("6177-2.",c(0:2),sep="")])
colnames(bCase_iSiD_diseases) <- diseases_UKBB
#dim(bCase_iSiD_diseases) # should be 6 diseases
rownames(bCase_iSiD_diseases) <- demographics_cog_rawdata1$eid

# turn into a integer matrix
bCase_iSiD_diseases <- 1 * bCase_iSiD_diseases

# subset to only include the ones we need (BP, high chol, insulin etc)
bCase_iSiD_diseases <- bCase_iSiD_diseases[,c(2,3,6)]


# rename columns 
colnames(bCase_iSiD_diseases) <- c("High_chol_M",
                                   "BP_medications_M",
                                   "Insulin_M"
)


#################################################################################################
#############################################################################################
# in this section we are extracting the number of people taking bp meds, diabetics etc
#  6153 FEMALES ############################################################

demographics_cog_rawdata2a <- demographics_cog_rawdata1[,c("6153-2.0" ,
                                                           "6153-2.1" ,
                                                           "6153-2.2")]


diseases_UKBBa <- unique(c(as.matrix(demographics_cog_rawdata2a[,paste("6153-2.",c(0:2),sep="")])))

diseases_UKBBa <- diseases_UKBBa[!is.na(diseases_UKBBa)]
stopifnot(length(diseases_UKBBa) == 8)

# # Using function over disease columns create a boolean matrix for each cancer disease for each subject in UKBB
bCase_iSiD_diseasesa = findCases( diseases_UKBBa , demographics_cog_rawdata2a[,paste("6153-2.",c(0:2),sep="")])


colnames(bCase_iSiD_diseasesa) <- diseases_UKBBa
#dim(bCase_iSiD_diseasesa) # should be 8 diseases
rownames(bCase_iSiD_diseasesa) <- demographics_cog_rawdata1$eid


# turn into a integer matrix
bCase_iSiD_diseasesa <- 1 * bCase_iSiD_diseasesa

# subset to only include the ones we need (BP, high chol, insulin etc)
bCase_iSiD_diseasesa <- bCase_iSiD_diseasesa[,c(4,2,7)]


# rename columns 
colnames(bCase_iSiD_diseasesa) <- c("High_chol_F",
                                    "BP_medications_F",
                                    "Insulin_F"
)


# join the two together
bCase_iSiD_diseases_both <- cbind(bCase_iSiD_diseases,
                                  bCase_iSiD_diseasesa)

bCase_iSiD_diseases_both <- as.data.frame(bCase_iSiD_diseases_both)

#sum up BP meds, chol mes and insulin meds
bCase_iSiD_diseases_both$BP_medications <- bCase_iSiD_diseases_both$BP_medications_M + bCase_iSiD_diseases_both$BP_medications_F

bCase_iSiD_diseases_both$Cholesterol_medications <- bCase_iSiD_diseases_both$High_chol_M + bCase_iSiD_diseases_both$High_chol_F

bCase_iSiD_diseases_both$Insulin_medications <- bCase_iSiD_diseases_both$Insulin_M + bCase_iSiD_diseases_both$Insulin_F

bCase_iSiD_diseases_both$eid <- rownames(bCase_iSiD_diseases_both)


#susbet this to include combined male and female
bCase_iSiD_diseases_both <- bCase_iSiD_diseases_both[,7:10]
bCase_iSiD_diseases_both$eid <- as.integer(bCase_iSiD_diseases_both$eid)


#############################################################################
#merge back into cognitive tests and demographics 
Cognition_demographics_subset <- merge(newcogtests_tower_rawdata,  newcogtests_rawdata,  by = "eid")
Cognition_demographics_subset <- merge(Cognition_demographics_subset, demographics_cog_rawdata1,  by = "eid")
Cognition_demographics_subset <- merge(Cognition_demographics_subset,  bCase_iSiD_diseases_both,  by = "eid")


# add in the rest of the data cleaning here
save(Cognition_demographics_subset, file = paste(saving_directory, "/Cognition_demographics_subset_compiled_13122020.Rdata", sep=""))


# Reading  in the self reported diseases from UKBB
# In this we will extract:
# 1) the self reported measures that will be used for the confounders
# 2) self reported measures for exclusion criteria for excluding people with neurological conditions at imaging visit

# setDTthreads(0L)
# 
# bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), header=TRUE, nrows = 0)
# 
# bdtest <- bd[,grep("[0-9]{1,10}\\-[0|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]
# 
# # to select the columns we need
# colwewant2 <- c("eid",   ## id
#                   "20002\\-2"   # ,     ## non illness code self reported
# 
# 
# )
# 
# bdtest5 <- unique(grep(paste(colwewant2, collapse ="|"),
#                        bdtest, value = TRUE, fixed = FALSE
# ))
# 
# tic()
# 
# # read in dataset 
# self_reported_disease <- ffread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), 
#                                header=TRUE,
#                                sep=",",
#                               verbose = T,
#                                select = bdtest5)
# 
# toc()
# 
# 
# 
# #subset to those that have imaging data
# self_reported_disease1 <- subset(self_reported_disease, self_reported_disease$eid %in% Brain_imaging_subset$eid)
# 
# 
# # # ##################################################################################
# # # this next part of the code take the self diagnosed information and converts it into a single column for each disease saying y or n if a person has reported that diagnosis
# #
# self_reported_disease2 <- self_reported_disease1[,c(35, 34, 33, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1)]
# 
# diseases_UKBB <- unique(c(as.matrix(self_reported_disease2[,paste("20002-2.",c(0:33),sep="")])))
# 
# diseases_UKBB <- diseases_UKBB[!is.na(diseases_UKBB)]
# stopifnot(length(diseases_UKBB) == 435 )
# 
# # # # this takes ages!!
# # # # Using function over disease columns create a boolean matrix for each cancer disease for each subject in UKBB
# bCase_iSiD_diseases = findCases( diseases_UKBB ,self_reported_disease2[,paste("20002-2.",c(0:33),sep="")])
# 
# colnames(bCase_iSiD_diseases) <- diseases_UKBB
# dim(bCase_iSiD_diseases) # should be 435 diseases
# rownames(bCase_iSiD_diseases) <- self_reported_disease2$eid

# # #save the matrix for all diseases for all subjects and save output (can use columns for certain diseases at a later date)
#save(bCase_iSiD_diseases, file = paste(saving_directory, "/self_diagnosed_disease20002_for_imaging_visit_20102020.Rdata", sep=""))
load(file = paste(saving_directory, "/self_diagnosed_disease20002_for_imaging_visit_20102020.Rdata", sep=""))

bCase_iSiD_diseases <- as.data.frame(bCase_iSiD_diseases)

bCase_iSiD_diseases1 <- 1 * bCase_iSiD_diseases # to turn from true/false to 0 and 1

rm(bCase_iSiD_diseases)

hypertension <- bCase_iSiD_diseases1$`1065` + bCase_iSiD_diseases1$`1072`

hypertension[hypertension == 2 ] <- 1

# diabetes
diabetes <-  bCase_iSiD_diseases1$`1220` + bCase_iSiD_diseases1$`1222` + bCase_iSiD_diseases1$`1223` 

diabetes[diabetes == 2 ] <- 1

# renal disease
renal_disease <-   bCase_iSiD_diseases1$`1519` + bCase_iSiD_diseases1$`1192` + bCase_iSiD_diseases1$`1193` + bCase_iSiD_diseases1$`1194`

# history of coronary heart disease symptoms
his_coronary_heart_disease_symptoms <- bCase_iSiD_diseases1$`1075` + bCase_iSiD_diseases1$`1074` + 
  bCase_iSiD_diseases1$`1076` 

his_coronary_heart_disease_symptoms[his_coronary_heart_disease_symptoms == 2 ] <- 1

# atrial fibulation/atrial flutter
Atrial_fibrillation <- bCase_iSiD_diseases1$`1483` + bCase_iSiD_diseases1$`1471`

Atrial_fibrillation[Atrial_fibrillation == 2 ] <- 1

# high cholesterol/ hypercholesterolemia
high_chol <- bCase_iSiD_diseases1$`1473`

#depression/depressive symptoms

depression <- bCase_iSiD_diseases1$`1286` + bCase_iSiD_diseases1$`1291`
depression[depression == 2 ] <- 1


# extract confounders and additional variables

self_reported_disease3 <- cbind(
  
  hypertension,
  diabetes,
  renal_disease,
  his_coronary_heart_disease_symptoms,
  Atrial_fibrillation,
  high_chol,
  depression
  
)

rownames(self_reported_disease3) <- rownames(bCase_iSiD_diseases1)
self_reported_disease3 <- as.data.frame(self_reported_disease3)
self_reported_disease3$eid <- rownames(self_reported_disease3)
self_reported_disease3$eid <- as.integer(self_reported_disease3$eid)


############################################################
# variables for exclusion criteria

#Dementia or Alzheimer's disease	1263
Dementia   <- bCase_iSiD_diseases1$`1263`

#Parkinson's disease	1262
PD<- bCase_iSiD_diseases1$`1262`

#Chronic degenerative neurological 	1258
Chronic_degenerative_neurological<- bCase_iSiD_diseases1$`1258`

#Guillain-Barré syndrome	1256
Guillain_Barre<- bCase_iSiD_diseases1$`1256`

#Multiple Sclerosis	1261
MS <- bCase_iSiD_diseases1$`1261`

#Other demyelinating disease	1397
Other_demyelinating_disease <- bCase_iSiD_diseases1$`1397`

#Stroke or ischaemic stroke	1081
stroke <- bCase_iSiD_diseases1$`1081`

#Brain cancer	1032
Brain_cancer <- bCase_iSiD_diseases1$`1032`

#Brain haemorrhage 	1491
Brain_haemorrhage <- bCase_iSiD_diseases1$`1491`

#Brain/intracranial abscess	1245
Brain_abscess<- bCase_iSiD_diseases1$`1245`

#Cerebral aneurysm	1425
Cerebral_aneurysm <- bCase_iSiD_diseases1$`1425`

#Cerebral palsy	1433
Cerebral_palsy <- bCase_iSiD_diseases1$`1433`

#Encephalitis	1246
Encephalitis <- bCase_iSiD_diseases1$`1246`

#Epilepsy	1264
Epilepsy <- bCase_iSiD_diseases1$`1264`

#Head injury	1266
Head_injury <- bCase_iSiD_diseases1$`1266`

#Infections of the nervous system	1244
Infections_nervous_system <- bCase_iSiD_diseases1$`1244`

#Ischaemic stroke	1583
Ischaemic_stroke <- bCase_iSiD_diseases1$`1583`

#Meningeal cancer	1031
Meningeal_cancer <- bCase_iSiD_diseases1$`1031`

#Meningioma (benign)	1659
Meningioma_benign <- bCase_iSiD_diseases1$`1659`

#Meningitis	1247
Meningitis <- bCase_iSiD_diseases1$`1247`

#Motor Neuron Disease	1259
Motor_Neuron_Disease <- bCase_iSiD_diseases1$`1259`

#Neurological injury/trauma	1240
Neurological_injury_trauma <- bCase_iSiD_diseases1$`1240`

#Spina bifida	1524
Spina_bifida <- bCase_iSiD_diseases1$`1524`

#Subdural haematoma	1083
Subdural_haematoma <- bCase_iSiD_diseases1$`1083`

#Subarachnoid haemorrhage	1086
Subarachnoid_haemorrhage <- bCase_iSiD_diseases1$`1086`

#Transient ischaemic attack	1082
Transient_ischaemic_attack <- bCase_iSiD_diseases1$`1082`


########################################
# put them into one dataframe
########################################
diseases_for_exclusion <- cbind(
  
  Dementia,
  PD,
  Chronic_degenerative_neurological,
  Guillain_Barre,
  MS ,
  Other_demyelinating_disease ,
  stroke,
  Brain_cancer,
  Brain_haemorrhage ,
  Brain_abscess,
  Cerebral_aneurysm ,
  Cerebral_palsy ,
  Encephalitis ,
  Epilepsy ,
  Head_injury ,
  Infections_nervous_system ,
  Ischaemic_stroke ,
  Meningeal_cancer ,
  Meningioma_benign ,
  Meningitis ,
  Motor_Neuron_Disease ,
  Neurological_injury_trauma ,
  Spina_bifida ,
  Subdural_haematoma ,
  Subarachnoid_haemorrhage ,
  Transient_ischaemic_attack
  
)


rownames(diseases_for_exclusion) <- rownames(bCase_iSiD_diseases1)

diseases_for_exclusion <- as.data.frame(diseases_for_exclusion)

# create variable to exlucde
diseases_for_exclusion$people2exlcude <- rowSums(diseases_for_exclusion)

diseases_for_exclusion$people2exlcude[diseases_for_exclusion$people2exlcude > 1] <- 1

diseases_for_exclusion$eid <- rownames(diseases_for_exclusion)

diseases_for_exclusion$eid <- as.integer(diseases_for_exclusion$eid)

# merge into cognition/demographic file
Cognition_demographics_subset <- merge(Cognition_demographics_subset, diseases_for_exclusion,  by = "eid")

Cognition_demographics_subset <- merge(Cognition_demographics_subset,  self_reported_disease3,  by = "eid")

save(Cognition_demographics_subset, file = paste(saving_directory, "/Cognition_demographics_subset_compiled_13122020.Rdata", sep=""))

##############################################################################
# cleaning the cognitive and demographics 
##############################################################################
rawdata <- Cognition_demographics_subset

##########
# ID #
##########
rownames(rawdata) <- rawdata$eid


##########
# GENDER #
##########
rawdata$`31-0.0` <- ifelse(rawdata$`31-0.0` == 0, "Female", "Male")
names(rawdata)[names(rawdata) == "31-0.0"] <- "Gender"
rawdata$Gender <- as.factor(rawdata$Gender) # create a factor
stopifnot(is.factor(rawdata$Gender))


######################
# AGE OF ASSESSMENT # 
######################
names(rawdata)[names(rawdata) == "21003-2.0"] <- "Age_at_assessment"


##########################
# UKBB ASSESSMENT CENTRE #
##########################
names(rawdata)[names(rawdata) == "54-2.0"] <- "UKBB_centre"
rawdata$UKBB_centre <- as.factor(rawdata$UKBB_centre)
stopifnot(is.factor(rawdata$UKBB_centre))


###############################################
# smoking status                 ##############
###############################################
names(rawdata)[names(rawdata) == "20116-2.0"] <- "smoking_status"

rawdata$smoking_status[ rawdata$smoking_status == -3 ] = NA
rawdata$smoking_status[ rawdata$smoking_status == 2 ] = 1 # changes smoking status to never versus pervious and current categorical bin

rawdata$smoking_status <- factor(rawdata$smoking_status)


####################################################
# town and deprivation index
####################################################

names(rawdata)[names(rawdata) == "189-0.0"] <- "deprivation_index"

#greater the value the greater the deprivation # 

#split townsend into deciles

rawdata$Townsend_deciles <- quantcut(rawdata$deprivation_index , q = 10, na.rm=TRUE)

rawdata$Townsend_deciles <- as.numeric(rawdata$Townsend_deciles)


##########################
# educational attainment #
##########################

# As with publication Hagenaars et al 2016 converted education in a binary variable
# 0 = do not have college/uni degree OR 1 = those that have a uni degree
names(rawdata)[names(rawdata) == "6138-2.0"] <- "Education"

# there are multiple columns as people could specifiy multiple levels of education
rawdata$Education[ rawdata$Education == -3 ] = NA
rawdata$Education[ rawdata$Education == -7 ] = 7
rawdata$Education <- ifelse(rawdata$Education == 1, 1, 0)

# remove other educational variables as we no longer need these

rawdata <- subset( rawdata, 
                   select = -c(`6138-2.1` , 
                               `6138-2.2` , 
                               `6138-2.3` ,
                               `6138-2.4` , 
                               `6138-2.5`
                   ) )



######################################
# age high bp diagnosed ############ 
####################################
# collected from participants who indicated they were told by a doctor that they have had high blood pressure

names(rawdata)[names(rawdata) == "2966-2.0"] <- "Age_highBP_diagnosed"
rawdata$Age_highBP_diagnosed[ rawdata$Age_highBP_diagnosed == -3 ] = NA


######################################
# age diabetes diagnosed ############ 
####################################
# collected from participants who indicated they were told by a doctor they had diabetes

names(rawdata)[names(rawdata) == "2976-2.0"] <- "Age_diabetes_diagnosed"
rawdata$Age_diabetes_diagnosed[ rawdata$Age_diabetes_diagnosed == -3 ] = NA


######################################
# diabetes diagnosed by a doctor ### 
####################################
# collected from participants who indicated they were told by a doctor that they have had high blood pressure

names(rawdata)[names(rawdata) == "2443-2.0"] <- "Diabetes_diagnosed_doctor"

rawdata$Diabetes_diagnosed_doctor[ rawdata$Diabetes_diagnosed_doctor == -3 ] = NA
rawdata$Diabetes_diagnosed_doctor[ rawdata$Diabetes_diagnosed_doctor == -1 ] = NA


######################################
# automated blood pressure ############ 
####################################
# The individal measures will be kept as well as the average dys and sys calculated
names(rawdata)[names(rawdata) == "4079-2.0"] <- "Diastolic_bp_1"
names(rawdata)[names(rawdata) == "4079-2.1"] <- "Diastolic_bp_2"

rawdata$Diastolic_bp_average <- rowMeans(rawdata[, c("Diastolic_bp_1","Diastolic_bp_2")
], na.rm = TRUE )

names(rawdata)[names(rawdata) == "4080-2.0"] <- "Systolic_bp_1"
names(rawdata)[names(rawdata) == "4080-2.1"] <- "Systolic_bp_2"


rawdata$Systolic_bp_average <- rowMeans(rawdata[, c("Systolic_bp_1", "Systolic_bp_2") ], na.rm = TRUE
)

rawdata$Systolic_bp_average[rawdata$Systolic_bp_average == "NaN"] <- NA
rawdata$Diastolic_bp_average[rawdata$Diastolic_bp_average == "NaN"] <- NA


###########################################
# memory pairs matching round 2 - 6 pairs #
###########################################
names(rawdata)[names(rawdata) == "399-2.2"] <- "f.399.2.2_Memory_pm_6pairs"
# if any subject has > 30 errors this was capped at 30 as with Hagenaars et al 2016 paper
rawdata$f.399.2.2_Memory_pm_6pairs[rawdata$f.399.2.2_Memory_pm_6pairs > 30.99 ] <- 30
# check that there are no values > 30 for this variable
z <- rawdata$f.399.2.2_Memory_pm_6pairs > 30.99
stopifnot(sum(z, na.rm=TRUE) == 0)

#remove other memory scores variables from other rounds
rawdata <- subset( rawdata, 
                   select = -c(`399-2.1` , 
                               `399-2.3`
                   ) )


############################################################
# TIME TO COMPLETE memory pairs matching round 2 - 6 pairs #
############################################################
names(rawdata)[names(rawdata) == "400-2.2"] <- "f.400.2.2_Memory_pm_6pairs_TIME"

#remove other memory scores variables from other rounds
rawdata <- subset( rawdata, 
                   select = -c(`400-2.1` , 
                               `400-2.3`
                   ) )


##############################################################################
# Number of correct matches in round memory pairs matching round 2 - 6 pairs #
##############################################################################
# if this does not equal 6 then the person did not complete the test therefore replace with NA
names(rawdata)[names(rawdata) == "398-2.2"] <- "f.398.2.2_Memory_pm_6pairs_nMATCHES"

#remove other memory scores variables from other rounds
rawdata <- subset( rawdata,
                   select = -c(`398-2.1` ,
                               `398-2.3`
                   ) )


#################################################################################################
# # replacing those who did not complete pairs matching due to time to complete test set at zero
#################################################################################################

# some ppts failed to complete test however this was set to 0 for pairs matching
# if time to undertake pairs matching was 0 we made the memory pairs matching results NA

# replace pairs matching with a zero value with NA
rawdata$f.400.2.2_Memory_pm_6pairs_TIME[rawdata$f.400.2.2_Memory_pm_6pairs_TIME == 0] <- NA

# replace where people did not get all 6 matches with NA
rawdata$f.398.2.2_Memory_pm_6pairs_nMATCHES[rawdata$f.398.2.2_Memory_pm_6pairs_nMATCHES < 6] <- NA

# number of people not completing all 6 matches 
#table(rawdata$f.398.2.2_Memory_pm_6pairs_nMATCHES == 6) ~1.75% didnt complete all 6

#replace pairs matching with NA where NA for TIME to take test was zero
rawdata$f.399.2.2_Memory_pm_6pairs = ifelse(is.na(rawdata$f.400.2.2_Memory_pm_6pairs_TIME), NA, rawdata$f.399.2.2_Memory_pm_6pairs)

#replace pairs matching with NA where NA for n matches <6 indicating they didnt complete the test
rawdata$f.399.2.2_Memory_pm_6pairs = ifelse(is.na(rawdata$f.398.2.2_Memory_pm_6pairs_nMATCHES), NA, rawdata$f.399.2.2_Memory_pm_6pairs)

###########################################
# Fluid intelligence #
###########################################
names(rawdata)[names(rawdata) == "20016-2.0"] <- "f.20016.2.0_fluid_intelligence"
stopifnot(is.numeric(rawdata$f.20016.2.0_fluid_intelligence))

###########################################
# reaction time #
###########################################
names(rawdata)[names(rawdata) == "20023-2.0"] <- "f.20023.2.0_reaction_time"
stopifnot(is.numeric(rawdata$f.20023.2.0_reaction_time))

################################
# ethnicity ##
###################################
names(rawdata)[names(rawdata) == "21000-2.0"] <- "Ethnicity"

names(rawdata)[names(rawdata) == "21000-0.0"] <- "Ethnicity_baseline"

rawdata$Ethnicity[ rawdata$Ethnicity == -3 ] = NA
rawdata$Ethnicity[ rawdata$Ethnicity == -1 ] = NA

rawdata$Ethnicity[ rawdata$Ethnicity == 1001 ] = 1
rawdata$Ethnicity[ rawdata$Ethnicity == 1002 ] = 1
rawdata$Ethnicity[ rawdata$Ethnicity == 1003 ] = 1

rawdata$Ethnicity[ rawdata$Ethnicity > 1 ] = 0
#missing alot of ethnicity in imaging visit but as ethnicity is crystallised can use from baseline


rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline == -3 ] = NA
rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline == -1 ] = NA

rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline == 1001 ] = 1
rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline == 1002 ] = 1
rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline == 1003 ] = 1

rawdata$Ethnicity_baseline[ rawdata$Ethnicity_baseline > 1 ] = 0

#replace ethnicity at wave 3 with baseline information (wont change over time and even if some misclassfication this will not effect the results greatly as majority of cohort is white)
rawdata$Ethnicity <- rawdata$Ethnicity_baseline

rawdata$Ethnicity[rawdata$Ethnicity_baseline == 1 & is.na(rawdata$Ethnicity)] <- 1
rawdata$Ethnicity[rawdata$Ethnicity_baseline == 0 & is.na(rawdata$Ethnicity)] <- 0


################################
# BMI ##
###################################
names(rawdata)[names(rawdata) == "21001-2.0"] <- "BMI"


###########################################
# for trails makers A and B anyone with a value of 0 is set to missing i.e. NA
#Those with a score coded as 0 (denoting "Trail not completed") had their score set to missing.

#table(rawdata$f6348_Duration_trailA == 0)
rawdata$f6348_Duration_trailA[ rawdata$f6348_Duration_trailA == 0 ] = NA

#table(rawdata$f6350_Duration_trailB == 0)
rawdata$f6350_Duration_trailB[ rawdata$f6350_Duration_trailB == 0 ] = NA

rawdata$f6350_Duration_trailB[ rawdata$f6350_Duration_trailB >= 2500 ] = NA

#########################################
# creating a cogntive test measure which takes TMT from TMTa
rawdata$TMTB_MINUS_TMTA <- rawdata$f6350_Duration_trailB - rawdata$f6348_Duration_trailA

rawdata$TMTB_MINUS_TMTA[rawdata$TMTB_MINUS_TMTA < 0 ] <- NA

rawdata$TMTB_MINUS_TMTA[rawdata$TMTB_MINUS_TMTA > 1500 ] <- NA


#########################################
#remove columns we dont need

rawdata_cog <- subset( rawdata,
                       select = -c(Ethnicity_baseline,
                                   f.398.2.2_Memory_pm_6pairs_nMATCHES,
                                   f.400.2.2_Memory_pm_6pairs_TIME,
                                   Ethnicity_baseline,
                                   `6153-2.0`,
                                   `6153-2.1`,                                  
                                   `6153-2.2` ,
                                   `6153-2.3`  ,                              
                                   `6177-2.0` ,
                                   `6177-2.1`  ,                               
                                   `6177-2.2`
                       )
)

Cognition_demographics_subset_CLEANED <- rawdata_cog

save(Cognition_demographics_subset_CLEANED, file = paste(saving_directory, "/CLEANED_cog_demo_imaging_people_only_131220.Rdata", sep=""))






# MERGE ALL THE DATASETS TOGETHER
Full_dataset_v1 <- merge(Brain_imaging_subset, Cognition_demographics_subset_CLEANED,  by = "eid")

Full_dataset_v1 <- merge(Full_dataset_v1, first_occurence_rawdata,  by = "eid", all = T)

# only include people with blood pressure for second reading
Full_dataset_v1 <- subset(Full_dataset_v1, !is.na(Full_dataset_v1$Systolic_bp_2) & !is.na(Full_dataset_v1$Diastolic_bp_2))
# goes down to 31249


# remove people who have who have withdrawn
withdrawnlist <- paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data", "/w43309_20210201.csv"))

withdrawn <- read.table(withdrawnlist, sep = ",", header = FALSE)

withdrawn <- withdrawn$V1

#subset out of main database
Full_dataset_v1 <- subset(Full_dataset_v1, !(Full_dataset_v1$eid %in% withdrawn) )

save(Full_dataset_v1, file = paste(saving_directory, "/Full_dataset_13122020_v1.Rdata", sep=""))












