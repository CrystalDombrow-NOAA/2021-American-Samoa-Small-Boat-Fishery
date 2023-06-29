##############
# Title: 2021 American Samoa small boat fishery cost-earnings survey
# Author: Crystal Dombrow
# Date: March 2023
##############

#----------------------------
# DESCRIPTION
#----------------------------
  # The purpose of this program is to clean the data, analyze the data, & 
  # generate the results for the 2021 American Samoa small boat survey report.
  # This readme script runs the entire program (code below).


#----------------------------
# R PROJECT FOLDER ORGANIZATION
#----------------------------
   #1. Main folder
     # Contains the analyses scripts, the R project file, and the folders 
       # supporting the program. 
     # The scripts 01-09 run the analyses, produce output tables for most
        # questions, and generate lists or R objects for all calculations.
        # In each script, the outputs are identified by survey question number.

   #2. "Data" folder 
     # Contains data cleaning script, data cleaning documentation, & all data sets:
       # AS PIFSC Data File_v2_ORIGINAL.xlsx: original raw survey data
       # AS PIFSC Data File_v2.xlsx: raw survey data edited to match hard copies
       # AS PIFSC Data File_v2_manual.csv: manually cleaned raw survey data as 
         # input for this R program. Data cleaning steps are described in: 
         # AS_DataCleaningDocumentation.pdf
       # AS_FishNames.pdf: Details the decisions made to translate fish names
         # from the survey questionnaire.
       # AS_SBF_2021_DataCleaned.csv: cleaned survey data output from this R 
         # program.
       # "2020 Census data" folder: contains select data from the 2020 Census &
         # calculations for our Admin report demographics section.
   
   #3. "Functions" folder
     # Contains scripts that run the functions for the data analysis scripts,
       # labeled by the description of their task.

   #4. "Preliminary analyses" folder
     # Contains the code chunks used for the preliminary analyses of the 
       # survey data, labeled by project task. These are for reference only;
       # they do not run with the program below. Some chunks are used for the
       # project brochures.

   #5. "Tables" folder
     # Contains .csv files of output tables generated for most survey questions.
       # The type of calculation is indicated next to the survey question number.
     # Also contains several tables created in Excel, noted by the .xlsx extension.
    


#----------------------------
# INSTRUCTIONS
#----------------------------
# To run the program, run the following code in this order:

#Clear workspace
rm(list = ls())

#Run data cleaning script
source("Data/AS_SBF_DataCleaning.R")


#Run analyses, by section 
source("01_Demographics.R")
source("02_VesselCharacteristics.R")
source("03_FishingActivity.R")
source("04_MarketParticipation.R")
source("05_TripCosts.R")
source("06_AnnualFishingExpenditures.R")
source("07_CrewConsiderations.R")
source("08_SocialAspectsOfFishing.R")
source("09_FisherPerspectives.R")

