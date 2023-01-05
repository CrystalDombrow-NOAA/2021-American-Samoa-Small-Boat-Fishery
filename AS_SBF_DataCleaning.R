#------------------------------------------------------------------------------
#This script cleans the data for the 2021 American Samoa small boat fishery
#cost-earnings survey.
#------------------------------------------------------------------------------

#------------------
#SET UP WORKSPACE
#------------------

#Load libraries
library(tidyverse)
library(dplyr)
library(doBy)
library(naniar)


#Import data set. Originally, data set was an .xlsx file; author saved the .xlsx 
  #as a .csv, & added manual data cleaning for individual observations. Steps are 
  #documented in AS_DataCleaningDocumentation.docx in the "Data" folder for this
  #R program.
as.sbs.data.raw <- read_csv("Data/AS PIFSC Data File_v2_manual.csv")


#--------------------
#CLEAN DATA
#--------------------
#Each object name describes the work being done on it

#Remove unneeded columns.
remove.cols <- as.sbs.data.raw %>%
  select(-SurvDate, -SurvNumber, -Surveyor)


#Remove unneeded rows.
  #NA rows not appearing every time, this is necessary when they do.
remove.na.rows <- remove.cols %>%
  rowwise() %>%
  filter(!all(is.na(across(everything())))) %>%
  ungroup()


#Remove longliners.
remove.longliners <- remove.na.rows %>%
  filter(Q4G != "Longline")


#Remove survey responses of non-boat fishers.
remove.nonboat.rows <- remove.longliners %>%
  filter(Q2 != "3")


#Drop duplicate survey responses, removing versions that are less complete
remove.duplicate.surveys <- remove.nonboat.rows %>%
  filter(Survey != "PPP_SCAN_0133") %>%
  filter(Survey != "PPP_SCAN_0134")


#Rename misnumbered columns with survey question number from codebook.
rename.cols <- remove.duplicate.surveys %>% 
  rename(Q31.2J = Q31.2J...81,
         Q31.2N = Q31.2J...85,
         Q32.2J = Q32.2J...99,
         Q32.2N = Q32.2J...103) 


#Assume 77 and 99 are missing values and recode to NA. Repeated below; this 
  #repetition reduces the chance of errors in chunks below.
recode.na <- rename.cols %>% 
  na_if(99) %>%
  na_if(77) %>%
  na_if("N/A") %>%
  na_if("n/a")


#Recoding numeric and some character values for data cleaning.
    #Took median of ranges, took stated numeric value of other responses, & made 
    #open-ended responses uniform where possible.
recode.values <- recode.na %>%
  mutate(Q4A = ifelse(Q4A == "99", "1", Q4A),
         Q4B = ifelse(Q4B == "99", "1", Q4B),
         Q4C = ifelse(Q4C == "99", "1", Q4C),
         Q4D = ifelse(Q4D == "99", "1", Q4D),
         Q4E = ifelse(Q4E == "99", "1", Q4E),
         Q4F = ifelse(Q4F == "99", "1", Q4F),
         Q4G = ifelse(Q4G == "99", "77", Q4G),
         Q5B = ifelse(Q5A == "0", "1", Q5B),
         Q8 = ifelse(Q8 == "99", "1", Q8),
         Q36 = recode(Q36, "Iliili" = "Ili'ili",
                      "Ofu Manua" = "Ofu",
                      "Tau" = "Ta'u", 
                      "Ta'u, Manu'a" = "Ta'u",
                      "Ta'u, Manu'a Islands" = "Ta'u"))


#Create new columns that will be recoded later with midpoint values, to maintain
  #both codebook values and midpoint values for analyses.
columns.for.midpoint.recodes <- recode.values %>%
  mutate(Q3B.mid = Q3B, 
         Q4A.mid = Q4A,
         Q4B.mid = Q4B,
         Q4C.mid = Q4C,
         Q4D.mid = Q4D,
         Q4E.mid = Q4E,
         Q4F.mid = Q4F,
         Q5B.mid = Q5B,
         Q6A.mid = Q6A,
         Q6B.mid = Q6B,
         Q6C.mid = Q6C,
         Q6D.mid = Q6D,
         Q7A.mid = Q7A,
         Q7B.mid = Q7B,
         Q9A.mid = Q9A,
         Q10A.mid = Q10A,
         Q11A.mid = Q11A,
         Q12A.mid = Q12A,
         Q13.mid = Q13,
         Q16A.mid = Q16A,
         Q16B.mid = Q16B,
         Q16C.mid = Q16C,
         Q16D.mid = Q16D,
         Q19A.mid = Q19A,
         Q20A.mid = Q20A,
         Q20B.mid = Q20B,
         Q20C.mid = Q20C,
         Q20D.mid = Q20D,
         Q21.mid = Q21,
         Q23.mid = Q23,
         Q35.mid = Q35,
         Q40.mid = Q40)
         

#Create new columns that will be recoded later with character values, to maintain
  #both codebook values and character values for analyses.
columns.for.chr.recodes <- columns.for.midpoint.recodes %>%
  mutate(Q1.combo = Q1,
         Q7.combo = Q7A, 
         Q14.combo = Q14A.1,
         Q17.chr = Q17,
         Q18A.chr = Q18A,
         Q18B.chr = Q18B,
         Q18C.chr = Q18C,
         Q18D.chr = Q18D,
         Q18E.chr = Q18E,
         Q22.chr = Q22,
         Q35.combo = Q35,
         Q38.combo = Q38.1A,
         Q38.1.chr = Q38.1,
         Q50.chr = Q50,
         Q52.chr = Q52)

  
#Recode codebook ranges to midpoint values & character values.
recode.midpoints.chrs <- columns.for.chr.recodes %>%
  mutate(Q1.combo = recode(Q1.combo, "1" = "boat owner",
                           "2" = "boat owner",
                           "3" = "77",
                           "4" = "crew")) %>%
  mutate_at(vars(Q3B.mid, Q5B.mid), 
            list(~recode(., `1` = 0,
                         `2` = 6, 
                         `3` = 18,
                         `4` = 37,
                         `5` = 74.5,
                         `6` = 150))) %>%
  mutate_at(vars(Q4A.mid:Q4F.mid, Q6A.mid:Q6D.mid, Q7A.mid:Q7B.mid, Q13.mid, 
                 Q16A.mid:Q16D.mid, Q20A.mid:Q20D.mid, Q21.mid, Q23.mid), 
            list(~recode(., `1` = 0,
                         `2` = 5,
                         `3` = 24.5,
                         `4` = 49.5,
                         `5` = 74.5,
                         `6` = 95))) %>%
  mutate(Q7.combo = recode(Q7.combo, "1" = "2", #federal only
                            "2" = "3", #federal and local
                            "3" = "3", #federal and local
                            "4" = "3", #federal and local
                            "5" = "3", #federal and local
                            "6" = "1")) %>% #local only
  mutate_at(vars(Q9A.mid, Q10A.mid, Q11A.mid, Q12A.mid),
            list(~recode(., `1` = 0,
                         `2` = 25.5,
                         `3` = 75.5,
                         `4` = 300.5,
                         `5` = 750.5,
                         `6` = 6))) %>%
  mutate(Q14.combo = recode(Q14.combo, "1" = "non-commercial",
                           "2" = "non-commercial",
                           "3" = "non-commercial",
                           "4" = "non-commercial",
                           "5" = "commercial",
                           "6" = "commercial",
                           "7" = "non-commercial",
                           "8" = "non-commercial")) %>%
  mutate_at(vars(Q17.chr, Q18A.chr:Q18E.chr, Q22.chr, Q38.1.chr, Q50.chr, 
                 Q52.chr), 
            list(~recode(., `1` = "yes",
                         `2` = "no"))) %>%
  mutate_at(vars(Q19A.mid),
            list(~recode(., `1` = 50.5,
                         `2` = 300.5,
                         `3` = 750.5,
                         `4` = 1500.5,
                         `5` = 3500.5,
                         `6` = 7500.5,
                         `7` = 15000.5,
                         `8` = 35000.5,
                         `9` = 50000))) %>% 
  mutate(Q35.combo = recode(Q35.combo, "1" = "44 and under",
                           "2" = "44 and under",
                           "3" = "44 and under",
                           "4" = "45 and over",
                           "5" = "45 and over",
                           "6" = "45 and over")) %>%
  mutate_at(vars(Q35.mid),
            list(~recode(., `1` = 25,
                         `2` = 29.5,
                         `3` = 39.5,
                         `4` = 49.5,
                         `5` = 59.5,
                         `6` = 65))) %>%
  mutate(Q38.combo = recode(Q38.combo, "1" = "samoan",
                            "2" = "non-samoan",
                            "3" = "non-samoan",
                            "4" = "non-samoan",
                            "5" = "non-samoan",
                            "6" = "non-samoan")) %>%
  mutate(Q40.mid = recode(Q40, "1" = "10000",
                          "2" = "17499.5",
                          "3" = "37499.5",
                          "4" = "74999.5",
                          "5" = "174999.5",
                          "6" = "250000"))


#Coerce columns with numbers to numeric.
cols.to.numeric <- recode.midpoints.chrs %>% 
  mutate_at(vars(Q7.combo, Q8, Q24, Q31.2A, Q31.2B, Q31.2D, Q31.2K, Q31.2M, 
                 Q32.2A, Q32.2B, Q32.2D:Q32.2F, Q32.2L, Q33A:Q33H, Q40.mid), 
            as.numeric)


#Create new columns for combinations of variables, using ifelse statements
columns.for.ifelses <- cols.to.numeric %>%
  mutate(Q3.mid.ifelse = ifelse(is.na(Q3A), Q3B.mid, Q3A),
       #Create bottomfish variable, anyone reporting Q4 >= 40% bottomfish fishing
       Q4B.ifelse = ifelse(Q4B >= 4, Q4B, 77),
       Q4C.ifelse = ifelse(Q4C >= 4, Q4C, 77),
       Q4.bottomfish = ifelse(Q4B.ifelse <= 6, Q4B.ifelse, Q4C.ifelse),
       #Regular ifelse's
       Q4A.yesno = ifelse(Q4A > 1, "yes", "no"),
       Q4B.yesno = ifelse(Q4B > 1, "yes", "no"),
       Q4C.yesno = ifelse(Q4C > 1, "yes", "no"),
       Q4D.yesno = ifelse(Q4D > 1, "yes", "no"),
       Q4E.yesno = ifelse(Q4E > 1, "yes", "no"),
       Q4F.yesno = ifelse(Q4F > 1, "yes", "no"),
       Q9.ifelse = ifelse(!is.na(Q9B), Q9B, Q9A.mid), 
       Q10.ifelse = ifelse(!is.na(Q10B), Q10B, Q10A.mid),
       Q11.ifelse = ifelse(!is.na(Q11B), Q11B, Q11A.mid),
       Q12.ifelse = ifelse(!is.na(Q12B), Q12B, Q12A.mid),
       #Create ifelse variables for those who sold fish only
       Q18A.ifelse = ifelse(Q17.chr == "no", 77, Q18A),
       Q18B.ifelse = ifelse(Q17.chr == "no", 77, Q18B),
       Q18C.ifelse = ifelse(Q17.chr == "no", 77, Q18C),
       Q18D.ifelse = ifelse(Q17.chr == "no", 77, Q18D),
       Q18E.ifelse = ifelse(Q17.chr == "no", 77, Q18E),
       Q19A.mid.ifelse = ifelse(Q17.chr == "no", 77, Q19A.mid),
       Q20A.mid.ifelse = ifelse(Q17.chr == "no", 77, Q20A.mid),
       Q20B.mid.ifelse = ifelse(Q17.chr == "no", 77, Q20B.mid),
       Q20C.mid.ifelse = ifelse(Q17.chr == "no", 77, Q20C.mid),
       Q20D.mid.ifelse = ifelse(Q17.chr == "no", 77, Q20D.mid),
       Q21.ifelse = ifelse(Q17.chr == "no", 77, Q21),
       Q21.mid.ifelse = ifelse(Q17.chr == "no", 77, Q21.mid),
       #Create ifelse variables for boat owners only
       Q23.ifelse = ifelse(Q22.chr == "no", 77, Q23),
       Q23.mid.ifelse = ifelse(Q22.chr == "no", 77, Q23.mid),
       Q24.ifelse = ifelse(Q22.chr == "no", 77, Q24),
       Q25.ifelse = ifelse(Q22.chr == "no", 77, Q25),
       Q26.ifelse = ifelse(Q22.chr == "no", 77, Q26),
       Q27.ifelse = ifelse(Q22.chr == "no", 77, Q27),
       Q28.ifelse = ifelse(Q22.chr == "no", 77, Q28),
       Q29.ifelse = ifelse(Q22.chr == "no", 77, Q29),
       Q30.ifelse = ifelse(Q22.chr == "no", 77, Q30))


#Remove 0's for catch quantity questions
remove.zeros.catch <- columns.for.ifelses %>%
  replace_with_na(recode.values, 
                  replace = list(Q9.ifelse = 0,
                                 Q10.ifelse = 0,
                                 Q11.ifelse = 0, 
                                 Q12.ifelse = 0))


#Replace character strings to make single-species open-ended responses uniform.
  #NOTE: There is a specific order of these mutate functions to ensure species
  #are properly renamed.
recode.strings <- remove.zeros.catch %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Snaper|Mala'i|Palu Malau|Red Snapper|Red snapper|Yellowtail|Yellow Tail|Savane|Savage|Silver snapper",
                replacement = "Snapper")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Surgeon Fish|Surgeon fish|Surgeon Fishes|Alogo|Line surgeonfish", 
                replacement = "Surgeonfish")) %>% 
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Misc.", 
                replacement = "77")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Yellow Fin|AsiAsi", 
                replacement = "Yellowfin tuna")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Bottom Fish|Small bottomfish misc species|Muatu",
                replacement = "Bottomfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Filoa|Emperor|Red-Gill Emperor|Red Gill Emperor|Filoa ulutele|Pink ear emperor", 
                replacement = "Emperors")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Parrot Fish|Parrotfish", 
                replacement = "Parrot fish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Palu Siliva", 
                replacement = "Lehi/Yelloweye opakapaka")) %>% 
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Papa|Velo", 
                replacement = "Grouper")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Blotcheye soldier|Blotcheye Soldier", 
                replacement = "Blotcheye soldierfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Mahi Mahi|Mahimahi|Masimasi|Masi Masi", 
                replacement = "Dolphinfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Shallow Bottom Fish|Shallow Bottomfish", 
                replacement = "Shallow bottomfish")) %>% 
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Atu", 
                replacement = "Mackerels and jacks")) %>% 
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Skip Jack Tuna|Aku|Skip jack|Skip Jack", 
                replacement = "Skipjack tuna")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Dog Tooth|Dogtooth",
                replacement = "Dogtooth tuna")) %>%
  mutate(across(Q43A:Q43I, gsub, 
              pattern = "All Reef fishes",  
              replacement = "Reef fish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Sea Cucumber", 
                replacement = "Sea cucumber")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Sea Urchins", 
                replacement = "Sea urchin")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Unicorn Fish", 
                replacement = "Unicornfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Palu enaena", 
                replacement = "Opakapaka")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Bill Fish", 
                replacement = "Billfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Masi", 
                replacement = "Dolphinfish")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Palu", 
                replacement = "Snapper")) %>%
  mutate(across(Q43A:Q43I, gsub, 
                pattern = "Malau", 
                replacement = "Squirrelfish/soldierfish"))


#For 77's added since above step, assume 77 is a missing value & recode to NA.
recode.na.again <- recode.strings %>% 
  na_if(77) 


#Make bottomfish variable uniform for section breakdowns later
make.bottomfish.uniform <- recode.na.again %>%
  mutate(Q4.bottomfish = recode(Q4.bottomfish, 
                                "4" = "bottomfish",
                                "5" = "bottomfish"))


#Create species groupings column from species names.
create.species.groups <- make.bottomfish.uniform %>%
  mutate(Q43A.spp = Q43A,
         Q43B.spp = Q43B,
         Q43C.spp = Q43C,
         Q43D.spp = Q43D,
         Q43E.spp = Q43E,
         Q43F.spp = Q43F,
         Q43G.spp = Q43G,
         Q43H.spp = Q43H,
         Q43I.spp = Q43I) %>%
  mutate(across(Q43A.spp:Q43I.spp, gsub, 
                pattern = "Barracuda|Billfish|Dogtooth tuna|Dolphinfish|Mackerel|Mackerels and jacks|Marlin|Shark|Skipjack tuna|Trevally|Tuna|Wahoo|Yellowfin tuna", 
                replacement = "Pelagics")) %>%
  mutate(across(Q43A.spp:Q43I.spp, gsub, 
                pattern = "Bottomfish|Emperors|Grouper|Lehi/Yelloweye opakapaka|Opakapaka|Shallow bottomfish|Snapper", 
                replacement = "Bottomfish")) %>% 
  mutate(across(Q43A.spp:Q43I.spp, gsub, 
                pattern = "Reef fish|Blotcheye soldierfish|Octopus|Parrot fish|Sea cucumber|Sea urchin|Squirrelfish/soldierfish|Surgeonfish|Surgeonfish, Octopus|Unicornfish", 
                replacement = "Reef fish")) %>% 
  mutate(across(Q43A.spp:Q43I.spp, gsub, 
                pattern = "Lobster", 
                replacement = "Crustaceans"))


#Coerce species group columns to numeric to be able to use in distribution 
  #tables function later.
# spp.groups.to.numeric <- create.species.groups %>% 
#   mutate_at(vars(Q43A.spp:Q43I.spp), as.numeric)
  
         
#Create island column from respondent's village.
create.island.column <- create.species.groups %>%
  mutate(Island = Q36, .after = Q36) %>%
  mutate(Island = recode(Island, "Alega" = "Tutuila",
                         "Faleniu" = "Tutuila",
                         "Futiga" = "Tutuila",
                         "Ili'ili" = "Tutuila",
                         "Malaeimi" = "Tutuila",
                         "Malota" = "Tutuila",
                         "Ofu" = "Manua Islands",
                         "Pago Pago" = "Tutuila",
                         "Tafuna" = "Tutuila",
                         "Ta'u" = "Manua Islands",
                         "Utulei" = "Tutuila"))


#Create full sample column for calculations.
create.full.sample.column <- create.island.column %>%
  mutate(Full.sample = Q1) %>%
  mutate(Full.sample = recode(Full.sample, 
                              "1" = "full sample",
                              "2" = "full sample",
                              "3" = "full sample",
                              "4" = "full sample"))


#Change object name to reflect that data cleaning is finished.
as.sbs.data.cleaned <- create.full.sample.column 


#Save as a CSV in the Data folder.
write_csv(as.sbs.data.cleaned, "Data/AS_SBF_2021_DataCleaned.csv")

