#=================================================================
# Load Libraries (Install packages if not found)
#=================================================================
library(haven)
library(data.table)
library(ggplot2)
library(summarytools)
library(dplyr)
library(gridExtra)
library(corrplot)
library(tidyr)
library(forcats)
library(stringr)
library(caTools)
library(olsrr)
library(car)
library(rpart)
library(caret)
library(rpart.plot)
library(olsrr)
library(randomForest)
library(scales)
library(nnet)

#=================================================================
# Set Working Directory
#=================================================================
working_path <- getwd()
setwd(working_path)

# ++==========================++
# ||   Phase 1: Data Cleaning ||
# ++==========================++

#=================================================================
# Data Import and Conversion
#=================================================================
# Converting from XPT to CSV format
xpt_data <- read_xpt("BFRSS2022.XPT")
write.csv(xpt_data, "BFRSS_Uncleaned.csv", row.names = FALSE)

cvd <- fread("BFRSS_Uncleaned.csv")
dim(cvd)
view(dfSummary(cvd))

#=================================================================
# Data Cleaning: selecting relevant columns
#=================================================================

# Narrowed down to 31 relevant columns
selected <- cvd[, c(
  "_SEX", "_AGE_G", "GENHLTH", "PHYSHLTH", "MENTHLTH",
  "MEDCOST1", "CHECKUP1", "_TOTINDA", "SLEPTIM1", 
  "CVDCRHD4", "CVDSTRK3", "_CASTHM1", "CHCSCNC1", "CHCOCNC1", 
  "CHCCOPD3", "ADDEPEV3", "CHCKDNY2", "_DRDXAR2", "DIABETE4", 
  "_EDUCAG", "INCOME3", "WTKG3", "HTM4", "_BMI5", 
  "_BMI5CAT", "_RFBMI5", "_RFSMOK3", "_PACKDAY", "ALCDAY4", "DRNKANY6", "_RFDRHV8"
)]

# Write the selected data to a new CSV file
write.csv(selected, "BFRSS_Uncleaned_RelevantColumns.csv", row.names = FALSE)

# Importing new file
cvd_uncleaned <- fread("BFRSS_Uncleaned_RelevantColumns.csv")
dim(cvd_uncleaned)
view(dfSummary(cvd_uncleaned))

#=================================================================
# Data Cleaning: renaming columns for easier understanding
#=================================================================

setnames(cvd_uncleaned, old = c("_SEX", "_AGE_G","GENHLTH", "PHYSHLTH", "MENTHLTH", 
                                "MEDCOST1", "CHECKUP1", "_TOTINDA", "SLEPTIM1", 
                                "CVDCRHD4", "CVDSTRK3", "_CASTHM1", "CHCSCNC1", "CHCOCNC1", 
                                "CHCCOPD3", "ADDEPEV3", "CHCKDNY2", "_DRDXAR2", "DIABETE4", 
                                "_EDUCAG", "INCOME3", "WTKG3", "HTM4", "_BMI5", 
                                "_BMI5CAT", "_RFBMI5", "_RFSMOK3", "_PACKDAY", "ALCDAY4", "DRNKANY6", "_RFDRHV8"), 
         new = c("SEX" ,"AGE_GROUP","GENERAL_HEALTH", "PHYSICAL_HEALTH", "MENTAL_HEALTH",
                 "UNABLE_TO_AFFORD_MED", "CHECKUP_DURATION", "EXERCISE", "SLEEP_TIME",
                 "HEART_DISEASE", "STROKE", "ASTHMA", "SKIN_CANCER", "OTHER_CANCER",
                 "LUNG_DISEASE", "DEPRESSION", "KIDNEY_DISEASE", "ARTHRITIS", "DIABETES",
                 "EDUCATION", "INCOME", "WEIGHT", "HEIGHT", "BMI",
                 "BMI_CATEGORY", "OVERWEIGHT_OR_OBESE", "SMOKING", "NO_OF_PACKS", "ALC_AMOUNT", "DRINKING", "HEAVY_DRINKING"))

#=================================================================
# Data Cleaning: handle empty string
#=================================================================
# For columns with empty string, replace with NA
cvd_uncleaned[cvd_uncleaned == ''] <- NA

# Check NA values
colSums(is.na(cvd_uncleaned))

#=================================================================
# Data Cleaning: converting values to categories
#=================================================================

# According to codebook, other than data collected, there are 3 types of special labels:
# "Don't know/Not Sure" -- Respondent was unsure of the answer to the question (usually represented with 7 or 77)
# "Refused" -- Respondent refused to answer the question (usually represented with 8, 88, 9 or 99)
# "Not asked or Missing" -- Respondent was not asked the question or there was an error of omission (usually represented with 9 or 99)

# Only "Not asked or Missing" is represented with Null value
# While the other two labels are represented with specific numerical values
# Since all these labels mean data was not collected,
# we will convert values representing "Don't know/Not Sure", "Refused" to Null value
# This is consistent with how CDC handled the data for some columns.

# Furthermore, each column consists of numerical values, 
# including those intended to represent categorical variables. 
# To facilitate EDA and modelling, these numerical representations 
# will be converted back into their respective categorical formats.

# Therefore, in this step, we will:
# 1. Convert values to Null value 
# 2. Convert values back to respective categories and factorising them (for categorical)

#-----------------------------------------------------------------
# SEX (CATEGORICAL)
cvd_uncleaned[, .N, by = SEX] 

#    SEX      N
# 1:   2 235894
# 2:   1 209238

# Replacing values with categories
cvd_uncleaned[, SEX := factor(SEX, levels = c(1, 2), labels = c("Male", "Female"))]

#-----------------------------------------------------------------
# AGE_GROUP (CATEGORICAL)
cvd_uncleaned[, .N, by = AGE_GROUP] 

#    AGE_GROUP      N
# 1:         6 161451
# 2:         5  82740
# 3:         3  59174
# 4:         4  66984
# 5:         2  47840
# 6:         1  26943

# Replacing values with categories
cvd_uncleaned[, AGE_GROUP := factor(AGE_GROUP, levels = c(1, 2, 3, 4, 5, 6), labels = c("[18,24]", "[25,34]", "[35,44]", "[45,54]", "[55,64]", "[65,Inf)"))]

#-----------------------------------------------------------------
# GENERAL_HEALTH (CATEGORICAL)
cvd_uncleaned[, .N, by = GENERAL_HEALTH]

#    GENERAL_HEALTH      N
# 1:              1  71878
# 2:              2 148447
# 3:              3 143598
# 4:              4  60273
# 5:              5  19741
# 6:              7    810
# 7:              9    385
# 8:             NA      3

# Replacing 7 and 9 with NA
cvd_uncleaned[GENERAL_HEALTH %in% c(7, 9), GENERAL_HEALTH := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, GENERAL_HEALTH := factor(GENERAL_HEALTH, levels = c(1, 2, 3, 4, 5), labels = c("Excellent", "Very good", "Good", "Fair", "Poor"))]

#-----------------------------------------------------------------
# PHYSICAL_HEALTH (finding values above 30 since question asks for days of poor health out of 30 days)
cvd_uncleaned[is.na(PHYSICAL_HEALTH) | PHYSICAL_HEALTH > 30, .N, by = PHYSICAL_HEALTH]

#    PHYSICAL_HEALTH      N
# 1:              88 267819
# 2:              77   8875
# 3:              99   2047
# 4:              NA      5

# Replacing 88: "None" with 0 days as it represents no days of poor physical health
cvd_uncleaned[PHYSICAL_HEALTH == 88, PHYSICAL_HEALTH := 0]

# Replacing 7 and 9 with NA
cvd_uncleaned[PHYSICAL_HEALTH %in% c(77, 99), PHYSICAL_HEALTH := NA_integer_]

#-----------------------------------------------------------
# MENTAL_HEALTH (finding values above 30 since question asks for days of poor health out of 30 days)
cvd_uncleaned[is.na(MENTAL_HEALTH) | MENTAL_HEALTH > 30, .N, by = MENTAL_HEALTH]

#    MENTAL_HEALTH      N
# 1:            88 265229
# 2:            77   6589
# 3:            99   2475
# 4:            NA      3

# Replacing 88: "None" with 0 days as it represents no days of poor mental health
cvd_uncleaned[MENTAL_HEALTH == 88, MENTAL_HEALTH := 0]

# Replacing 7 and 9 with NA
cvd_uncleaned[MENTAL_HEALTH %in% c(77, 99), MENTAL_HEALTH := NA_integer_]

#-----------------------------------------------------------
# UNABLE_TO_AFFORD_MED (CATEGORICAL)
cvd_uncleaned[, .N, by = UNABLE_TO_AFFORD_MED]

#    UNABLE_TO_AFFORD_MED      N
# 1:                  2 406300
# 2:                  1  37227
# 3:                  7   1157
# 4:                  9    448
# 5:                 NA      4

# Replacing 7 and 9 with NA
cvd_uncleaned[UNABLE_TO_AFFORD_MED %in% c(7, 9), UNABLE_TO_AFFORD_MED := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, UNABLE_TO_AFFORD_MED := factor(UNABLE_TO_AFFORD_MED, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# CHECKUP_DURATION (CATEGORICAL)
cvd_uncleaned[, .N, by = CHECKUP_DURATION]

#    CHECKUP_DURATION      N
# 1:                1 350944
# 2:                8   2509
# 3:                7   5063
# 4:                2  41919
# 5:                3  24882
# 6:                4  19079
# 7:               NA      3
# 8:                9    733

# Replacing 7, 8 and 9 with NA 
cvd_uncleaned[CHECKUP_DURATION %in% c(7, 8, 9), CHECKUP_DURATION := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, CHECKUP_DURATION := factor(CHECKUP_DURATION, levels = c(1, 2, 3, 4), labels = c("[0,1)", "[1,2)", "[2,5)", "[5, Inf)" ))]

#-----------------------------------------------------------
# EXERCISE (CATEGORICAL)
cvd_uncleaned[, .N, by = EXERCISE]

#    EXERCISE      N
# 1:        2 106480
# 2:        1 337559
# 3:        9   1093

# Replacing 9 with NA
cvd_uncleaned[EXERCISE == 9, EXERCISE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, EXERCISE := factor(EXERCISE, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# SLEEP_TIME (finding values above 24 since question asks for hours of sleep within 24h)
cvd_uncleaned[is.na(SLEEP_TIME) | SLEEP_TIME > 24, .N, by = SLEEP_TIME]

#    SLEEP_TIME    N
# 1:         77 4792
# 2:         99  658
# 3:         NA    3

# Replacing 77 and 99 with NA 
cvd_uncleaned[SLEEP_TIME %in% c(77, 99), SLEEP_TIME := NA_integer_]

#-----------------------------------------------------------
# HEART_DISEASE (CATEGORICAL)
cvd_uncleaned[, .N, by = HEART_DISEASE]

#    HEART_DISEASE      N
# 1:             2 414176
# 2:             1  26551
# 3:             7   4044
# 4:             9    359
# 5:            NA      2

# Replacing 7 and 9 with NA
cvd_uncleaned[HEART_DISEASE %in% c(7, 9), HEART_DISEASE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, HEART_DISEASE := factor(HEART_DISEASE, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# STROKE (CATEGORICAL)
cvd_uncleaned[, .N, by = STROKE]

#    STROKE      N
# 1:      2 424336
# 2:      1  19239
# 3:      7   1274
# 4:     NA      2
# 5:      9    281

# Replacing 7 and 9 with NA 
cvd_uncleaned[STROKE %in% c(7, 9), STROKE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, STROKE := factor(STROKE, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# ASTHMA (CATEGORICAL)
cvd_uncleaned[, .N, by = ASTHMA]

#    ASTHMA      N
# 1:      1 395613
# 2:      2  45659
# 3:      9   3860

# Replacing 9 with NA 
cvd_uncleaned[ASTHMA == 9, ASTHMA := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, ASTHMA := factor(ASTHMA, levels = c(1, 2), labels = c("No", "Yes"))]

#-----------------------------------------------------------
# SKIN_CANCER (CATEGORICAL)
cvd_uncleaned[, .N, by = SKIN_CANCER]

#    SKIN_CANCER      N
# 1:           2 406504
# 2:           1  35485
# 3:           7   2822
# 4:          NA      2
# 5:           9    319

# Replacing 7 and 9 with NA 
cvd_uncleaned[SKIN_CANCER %in% c(7, 9), SKIN_CANCER := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, SKIN_CANCER := factor(SKIN_CANCER, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# OTHER_CANCER (CATEGORICAL)
cvd_uncleaned[, .N, by = OTHER_CANCER]

#    OTHER_CANCER      N
# 1:            2 392442
# 2:            1  50269
# 3:            7   2016
# 4:           NA      3
# 5:            9    402

# Replacing 7 and 9 with NA 
cvd_uncleaned[OTHER_CANCER %in% c(7, 9), OTHER_CANCER := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, OTHER_CANCER := factor(OTHER_CANCER, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# LUNG_DISEASE (CATEGORICAL)
cvd_uncleaned[, .N, by = LUNG_DISEASE]

#    LUNG_DISEASE      N
# 1:            2 407257
# 2:            1  35656
# 3:           NA      2
# 4:            7   1885
# 5:            9    332

# Replacing 7 and 9 with NA 
cvd_uncleaned[LUNG_DISEASE %in% c(7, 9), LUNG_DISEASE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, LUNG_DISEASE := factor(LUNG_DISEASE, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# DEPRESSION (CATEGORICAL)
cvd_uncleaned[, .N, by = DEPRESSION]

#    DEPRESSION      N
# 1:          2 350910
# 2:          1  91410
# 3:          9    665
# 4:          7   2140
# 5:         NA      7

# Replacing 7 and 9 with NA 
cvd_uncleaned[DEPRESSION %in% c(7, 9), DEPRESSION := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, DEPRESSION := factor(DEPRESSION, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# KIDNEY_DISEASE (CATEGORICAL)
cvd_uncleaned[, .N, by = KIDNEY_DISEASE]

#    KIDNEY_DISEASE      N
# 1:              2 422891
# 2:              1  20315
# 3:              7   1581
# 4:              9    343
# 5:             NA      2

# Replacing 7 and 9 with NA 
cvd_uncleaned[KIDNEY_DISEASE %in% c(7, 9), KIDNEY_DISEASE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, KIDNEY_DISEASE := factor(KIDNEY_DISEASE, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# ARTHRITIS (CATEGORICAL)
cvd_uncleaned[, .N, by = ARTHRITIS]

#    ARTHRITIS      N
# 1:         2 291351
# 2:         1 151148
# 3:        NA   2633

# Replacing values with categories
cvd_uncleaned[, ARTHRITIS := factor(ARTHRITIS, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# DIABETES (CATEGORICAL)
cvd_uncleaned[, .N, by = DIABETES]

#    DIABETES      N
# 1:        1  61158
# 2:        3 368722
# 3:        4  10329
# 4:        9    321
# 5:        2   3836
# 6:        7    763
# 7:       NA      3

# Replacing 7 and 9 with NA 
cvd_uncleaned[DIABETES %in% c(7, 9), DIABETES := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, DIABETES := factor(DIABETES, levels = c(1, 2, 3, 4), labels = c("Yes", "Yes, but during pregnancy", "No", "No"))]

#-----------------------------------------------------------
# EDUCATION (CATEGORICAL)
cvd_uncleaned[, .N, by = EDUCATION]

#    EDUCATION      N
# 1:         4 187496
# 2:         2 108990
# 3:         3 120252
# 4:         1  26011
# 5:         9   2383

# Replacing 9 with NA 
cvd_uncleaned[EDUCATION == 9, EDUCATION := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, EDUCATION := factor(EDUCATION, levels = c(1, 2, 3, 4), labels = c("Attended Some High School", "Graduated High School", "Attended Some College", "Graduated College"))]

#-----------------------------------------------------------
# INCOME (CATEGORICAL)
cvd_uncleaned[, .N, by = INCOME]

#     INCOME     N
# 1:      99 47001
# 2:       5 42294
# 3:      10 22553
# 4:      77 36114
# 5:       8 48436
# 6:       7 59148
# 7:       9 50330
# 8:       6 46831
# 9:       4 20343
# 10:      2 11031
# 11:     11 23478
# 12:     NA 12932
# 13:      3 14300
# 14:      1 10341

# Replacing 77 and 99 with NA 
cvd_uncleaned[INCOME %in% c(77, 99), INCOME := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, INCOME := factor(INCOME, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), labels = c("(-Inf, 10,000)", "[10,000, 15,000)", "[15,000, 20,000)", "[20,000, 25,000)", "[25,000, 35,000)", "[35,000, 50,000)", "[50,000, 75,000)", "[75,000, 100,000)", "[100,000, 150,000)", "[150,000, 200,000)", "[200,000, Inf)"))]
#-----------------------------------------------------------
# BMI_CATEGORY (CATEGORICAL)
cvd_uncleaned[, .N, by = BMI_CATEGORY]

#    BMI_CATEGORY      N
# 1:           NA  48806
# 2:            3 139995
# 3:            2 116976
# 4:            4 132577
# 5:            1   6778

# Replacing values with categories
cvd_uncleaned[, BMI_CATEGORY := factor(BMI_CATEGORY, levels = c(1, 2, 3, 4), labels = c("Underweight", "Normal Weight", "Overweight", "Obese"))]

#-----------------------------------------------------------
# OVERWEIGHT_OR_OBESE (CATEGORICAL)
cvd_uncleaned[, .N, by = OVERWEIGHT_OR_OBESE]

#    OVERWEIGHT_OR_OBESE      N
# 1:                   9  48806
# 2:                   2 272572
# 3:                   1 123754

# Replacing 9 with NA 
cvd_uncleaned[OVERWEIGHT_OR_OBESE == 9, OVERWEIGHT_OR_OBESE := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, OVERWEIGHT_OR_OBESE := factor(OVERWEIGHT_OR_OBESE, levels = c(1, 2), labels = c("No", "Yes"))]

#-----------------------------------------------------------
# SMOKING (CATEGORICAL)
cvd_uncleaned[, .N, by = SMOKING]

#    SMOKING      N
# 1:       1 359729
# 2:       2  49941
# 3:       9  35462

# Replacing 9 with NA 
cvd_uncleaned[SMOKING == 9, SMOKING := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, SMOKING := factor(SMOKING, levels = c(1, 2), labels = c("No", "Yes"))]

#-----------------------------------------------------------
# ALC_AMOUNT
cvd_uncleaned[, .N, by = ALC_AMOUNT]

# Value 101 - 199 means 1__ days per week
# To standardise, we will convert days per week to days per month by multiplying days per week by 4. 
# This is because one month is around 4 weeks 

# Value 201 - 299 means 2__ days per month

# Replacing 88: "None" with 0 days per month as it represents no days of alcohol consumption

# Replacing 777 and 999 with NA 

cvd_uncleaned[, ALC_AMOUNT := 
                fifelse(ALC_AMOUNT >= 101 & ALC_AMOUNT <= 199, (ALC_AMOUNT - 100) * 4,
                        fifelse(ALC_AMOUNT >= 201 & ALC_AMOUNT <= 299, ALC_AMOUNT - 200,
                                fifelse(ALC_AMOUNT %in% c(777, 999), NA_integer_,
                                        fifelse(ALC_AMOUNT == 888, 0, ALC_AMOUNT))))]

#-----------------------------------------------------------
# DRINKING (CATEGORICAL)
cvd_uncleaned[, .N, by = DRINKING]

#    DRINKING      N
# 1:        2 187667
# 2:        1 210891
# 3:        9  43127
# 4:        7   3447

# Replacing 7 and 9 with NA 
cvd_uncleaned[DRINKING %in% c(7, 9), DRINKING := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, DRINKING := factor(DRINKING, levels = c(1, 2), labels = c("Yes", "No"))]

#-----------------------------------------------------------
# HEAVY_DRINKING (CATEGORICAL)
cvd_uncleaned[, .N, by = HEAVY_DRINKING]

#    HEAVY_DRINKING      N
# 1:              1 369122
# 2:              9  49705
# 3:              2  26305

# Replacing 9 with NA 
cvd_uncleaned[HEAVY_DRINKING == 9, HEAVY_DRINKING := NA_integer_]

# Replacing values with categories
cvd_uncleaned[, HEAVY_DRINKING := factor(HEAVY_DRINKING, levels = c(1, 2), labels = c("No", "Yes"))]

#-----------------------------------------------------------

#=================================================================
# Data Cleaning: handle NULL values
#=================================================================

colSums(is.na(cvd_uncleaned))
# Variables are split into categories as variables in some categories can be cleaned similarly

# Personal Particulars ------------------------------------------------------------------------------

# EDUCATION
cvd_uncleaned[is.na(EDUCATION),.N]    # 2316 Null values

rows <- cvd_uncleaned[is.na(EDUCATION)]   
#View(rows) 

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(EDUCATION)]

#-----------------------------------------------------------------
# INCOME
cvd_uncleaned[is.na(INCOME),.N]    # 91664 Null values

# In context, INCOME is a sensitive topic 
# Hence a large number of individuals might have been unwilling to disclose

# Investigate Missing values in INCOME
cvd_uncleaned[UNABLE_TO_AFFORD_MED == 'Yes' & is.na(INCOME), .N] # 7364 

# No cleaning performed as missing values appear to be random and not part of any particular subgroup
# As a placeholder, null values will be filled with 'Missing'.
cvd_uncleaned[, INCOME := factor(INCOME, levels = c(levels(INCOME), "Missing"))]
cvd_uncleaned[is.na(INCOME), INCOME := "Missing"]

# Two Options after performing EDA:
# > A -- Remove rows with missing income
# > B -- Ignore income as a variable

#-----------------------------------------------------------------
# UNABLE_TO_AFFORD_MED
cvd_uncleaned[is.na(UNABLE_TO_AFFORD_MED),.N]    # 1592 Null values

rows <- cvd_uncleaned[is.na(UNABLE_TO_AFFORD_MED)]   
#View(rows)

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(UNABLE_TO_AFFORD_MED)]

#---------------------------------------------------------------------------------------------------------

# Physical Characteristics -------------------------------------------------------------------------------
# These are variables that are rather personal and sensitive,
# Hence the large number of rows with null values might be due to unwillingness to reveal

# WEIGHT
cvd_uncleaned[is.na(WEIGHT),.N]    # 37674 Null values

# It is inaccurate to estimate a person's height or weight using mean or median
# Despite the large number of null values, the best choice is to remove them
# as an inaccurate estimation affects other variables, such as BMI and OVERWEIGHT_OR_OBESE

cvd_uncleaned <- cvd_uncleaned[!is.na(WEIGHT)]

#-----------------------------------------------------------------
# HEIGHT
cvd_uncleaned[is.na(HEIGHT),.N]    # 24371 Null values

# Choice: remove rows (same reason)
cvd_uncleaned <- cvd_uncleaned[!is.na(HEIGHT)]

#-----------------------------------------------------------------
# BMI, BMI_CATEGORY & OVERWEIGHT_OR_OBESE (same exact rows of null values as they are all derived from BMI variable)
cvd_uncleaned[is.na(BMI),.N]    # 44401 Null values (2238 after removing HEIGHT & WEIGHT null values)

# If HEIGHT & WEIGHT are not null, BMI can be calculated
cvd_uncleaned[!is.na(HEIGHT) & !is.na(WEIGHT) & is.na(BMI),.N] # 2238
cvd_uncleaned[!is.na(HEIGHT) & !is.na(WEIGHT) & is.na(BMI), BMI := (WEIGHT / (HEIGHT * HEIGHT))] # BMI
cvd_uncleaned[!is.na(HEIGHT) & !is.na(WEIGHT), BMI_CATEGORY := fifelse(BMI < 1850, 'Underweight',
                                                                       fifelse(BMI >= 1850 & BMI < 2500, 'Normal Weight',
                                                                               fifelse(BMI >= 2500 & BMI < 3000, 'Overweight',
                                                                                       fifelse(BMI >= 3000, 'Obese', NA_character_))))] # BMI_CATEGORY
cvd_uncleaned[!is.na(HEIGHT) & !is.na(WEIGHT), OVERWEIGHT_OR_OBESE := fifelse(BMI >= 2500, 'Yes', 'No')] # OVERWEIGHT_OR_OBESE
# Check: reduced null values to 0

#---------------------------------------------------------------------------------------------------------

# General Health Status ----------------------------------------------------------------------------------
# GENERAL_HEALTH
cvd_uncleaned[is.na(GENERAL_HEALTH),.N]    # 1182 Null values

rows <- cvd_uncleaned[is.na(GENERAL_HEALTH)]   
#View(rows)

# No cleaning performed as missing values appear to be random and not part of any particular subgroup
# The number of rows with NA are a very small fraction of the dataset. 
# Hence, their impact on our analysis is negligible
cvd_uncleaned <- cvd_uncleaned[!is.na(GENERAL_HEALTH)]

#-----------------------------------------------------------------
# PHYSICAL_HEALTH
cvd_uncleaned[is.na(PHYSICAL_HEALTH),.N]    # 10906 Null values

rows <- cvd_uncleaned[is.na(PHYSICAL_HEALTH)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows 
cvd_uncleaned <- cvd_uncleaned[!is.na(PHYSICAL_HEALTH)]

#-----------------------------------------------------------------
# MENTAL_HEALTH
cvd_uncleaned[is.na(MENTAL_HEALTH),.N]    # 9046 Null values

rows <- cvd_uncleaned[is.na(MENTAL_HEALTH)]   
#View(rows)
cvd_uncleaned[is.na(MENTAL_HEALTH) & is.na(DEPRESSION), .N]  
cvd_uncleaned[is.na(MENTAL_HEALTH) & DEPRESSION == 'Yes', .N] 

# Missing values appear to be random 
# Choice: Remove rows 
cvd_uncleaned <- cvd_uncleaned[!is.na(MENTAL_HEALTH)]

#---------------------------------------------------------------------------------------------------------

# Health Issues and Illness History ----------------------------------------------------------------------
# All of these are categorical variables (either remove row or impute with mode)
# Since many factors that can affect their diagnoses (e.g. genetics, by chance) can be difficult to predict/discover,
# if the null values appear to be random, we will remove the rows

# HEART_DISEASE
cvd_uncleaned[is.na(HEART_DISEASE),.N]    # 4387 Null values

rows <- cvd_uncleaned[is.na(HEART_DISEASE)]   
#View(rows)

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(HEART_DISEASE)]

#-----------------------------------------------------------------
# STROKE
cvd_uncleaned[is.na(STROKE),.N]    # 1540 Null values

rows <- cvd_uncleaned[is.na(STROKE)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(STROKE)]

#-----------------------------------------------------------------
# ASTHMA
cvd_uncleaned[is.na(ASTHMA),.N]    # 3843 Null values

rows <- cvd_uncleaned[is.na(ASTHMA)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(ASTHMA)]

#-----------------------------------------------------------------
# SKIN_CANCER 
cvd_uncleaned[is.na(SKIN_CANCER),.N]    # 3126 Null values

rows <- cvd_uncleaned[is.na(SKIN_CANCER)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(SKIN_CANCER)]

#-----------------------------------------------------------------
# OTHER_CANCER
cvd_uncleaned[is.na(OTHER_CANCER),.N]    # 2404 Null values

rows <- cvd_uncleaned[is.na(OTHER_CANCER)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(OTHER_CANCER)]

#-----------------------------------------------------------------
# LUNG_DISEASE
cvd_uncleaned[is.na(LUNG_DISEASE),.N]    # 2202 Null values

rows <- cvd_uncleaned[is.na(LUNG_DISEASE)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(LUNG_DISEASE)]

#-----------------------------------------------------------------
# DEPRESSION
cvd_uncleaned[is.na(DEPRESSION),.N]    # 2795 Null values

cvd_uncleaned[is.na(DEPRESSION) & is.na(MENTAL_HEALTH),.N] 

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(DEPRESSION)]

#-----------------------------------------------------------------
# KIDNEY_DISEASE
cvd_uncleaned[is.na(KIDNEY_DISEASE),.N]    # 1909 Null values

rows <- cvd_uncleaned[is.na(KIDNEY_DISEASE)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(KIDNEY_DISEASE)]

#-----------------------------------------------------------------
# ARTHRITIS
cvd_uncleaned[is.na(ARTHRITIS),.N]    # 2616 Null values

rows <- cvd_uncleaned[is.na(ARTHRITIS)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(ARTHRITIS)]

#-----------------------------------------------------------------
# DIABETES
cvd_uncleaned[is.na(DIABETES),.N]    # 1070 Null values

rows <- cvd_uncleaned[is.na(DIABETES)]   
#View(rows)

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(DIABETES)]

#---------------------------------------------------------------------------------------------------------

# Health Habits and Behaviours ---------------------------------------------------------------------------

# CHECKUP_DURATION
cvd_uncleaned[is.na(CHECKUP_DURATION),.N]    # 8268 Null values

cvd_uncleaned[is.na(CHECKUP_DURATION) & UNABLE_TO_AFFORD_MED == 'Yes',.N]

# Missing values appear to be random 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(CHECKUP_DURATION)]

#-----------------------------------------------------------------
# EXERCISE
cvd_uncleaned[is.na(EXERCISE),.N]    # 1076 Null values

rows <- cvd_uncleaned[is.na(EXERCISE)]   
#View(rows)

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(EXERCISE)]

#-----------------------------------------------------------------
# SLEEP_TIME
cvd_uncleaned[is.na(SLEEP_TIME),.N]    # 5434 Null values

rows <- cvd_uncleaned[is.na(SLEEP_TIME)]   
#View(rows)

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(SLEEP_TIME)]

#-----------------------------------------------------------------
# SMOKING
cvd_uncleaned[is.na(SMOKING),.N]    # 31433 Null values

cvd_uncleaned[NO_OF_PACKS>0, SMOKING := 'Yes']
# Check: no reduction in null values

# It is not reliable to estimate SMOKING with the mode 
# as it might impact our analysis since there is quite a large fraction of null values
# It will affect the NO_OF_PACKS variable as well.
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(SMOKING)]

#-----------------------------------------------------------------
# NO_OF_PACKS
cvd_uncleaned[is.na(NO_OF_PACKS),.N]    # 289683 Null values

# Check if SMOKING = No, then NO_OF_PACKS should be 0
cvd_uncleaned[SMOKING == 'No', NO_OF_PACKS := 0]
# Check: reduced null values to 2755

# If Smoking = Yes, NO_OF_PACKS should be > 0 and not NA
cvd_uncleaned[SMOKING == 'Yes' & is.na(NO_OF_PACKS),.N] # 2755 rows
# Choice: Replace with mean
mean <- mean(cvd_uncleaned[SMOKING == 'Yes',]$NO_OF_PACKS, na.rm = TRUE)
cvd_uncleaned[SMOKING == 'Yes' & is.na(NO_OF_PACKS), NO_OF_PACKS := mean]
# Check: reduced null values to 0

#-----------------------------------------------------------------
# DRINKING & ALC_AMOUNT (same exact rows of null values)
cvd_uncleaned[is.na(DRINKING),.N]    # 42522 Null values

cvd_uncleaned[ALC_AMOUNT>0, DRINKING := 'Yes']
cvd_uncleaned[HEAVY_DRINKING == 'Yes', DRINKING := 'Yes']
# Check: no reduction in null values 

# Choice: Remove rows (same reason as smoking)
cvd_uncleaned <- cvd_uncleaned[!is.na(DRINKING)]

#-----------------------------------------------------------------
# HEAVY_DRINKING
cvd_uncleaned[is.na(HEAVY_DRINKING),.N]    # 45653 -> 2137 Null values after removing null values in DRINKING

rows <- cvd_uncleaned[is.na(HEAVY_DRINKING)]   
#View(rows)

# Missing values appear to be random 
# The number of rows with NA are a very small fraction of the dataset. 
# Choice: Remove rows
cvd_uncleaned <- cvd_uncleaned[!is.na(HEAVY_DRINKING)]

# Check
colSums(is.na(cvd_uncleaned)) 

#=================================================================
# Further Data Processing: transformation of existing columns
#=================================================================

# 1. Standardising Height/Weight columns
cvd_uncleaned[, HEIGHT := HEIGHT / 100]
cvd_uncleaned[, WEIGHT := WEIGHT / 100]
cvd_uncleaned[, BMI := BMI / 100]


# 2. Further handling values in columns
cvd_uncleaned[, NO_OF_PACKS := round(NO_OF_PACKS * 20)] # converting to number of cigarettes to get integer

# 3. Renaming of columns with units
setnames(cvd_uncleaned, old = c("CHECKUP_DURATION", "SLEEP_TIME", "NO_OF_PACKS", "ALC_AMOUNT", "HEIGHT", "WEIGHT"), 
         new = c("CHECKUP_DURATION_YR", "SLEEP_DURATION_HR", "CIGARETTES_PER_DAY", "DRINKING_DAYS_PER_MONTH","HEIGHT_M", "WEIGHT_KG"))


# 4. Check if values make sense for continuous variables
cvd_uncleaned[SLEEP_DURATION_HR < 0 | SLEEP_DURATION_HR > 24, .N]
cvd_uncleaned[HEIGHT_M < 0.5 | HEIGHT_M > 2.5, .N]
cvd_uncleaned[WEIGHT_KG < 20 | WEIGHT_KG > 300, .N]
cvd_uncleaned[CIGARETTES_PER_DAY < 0 | CIGARETTES_PER_DAY > 100, .N]
cvd_uncleaned[DRINKING_DAYS_PER_MONTH < 0 | DRINKING_DAYS_PER_MONTH > 31, .N]


# 5. Recalculation of BMI, Recategorisation of BMI_CATEGORY and OVERWEIGHT_OR_OBESE according to Singapore's standards
cvd_uncleaned[, BMI := round(WEIGHT_KG / (HEIGHT_M * HEIGHT_M),2)]
cvd_uncleaned[, BMI_CATEGORY := fifelse(BMI < 18.50, 'Underweight',
                                        fifelse(BMI >= 18.50 & BMI < 23.00, 'Normal Weight',
                                                fifelse(BMI >= 23.00 & BMI < 30.00, 'Overweight',
                                                        'Obese')))]
cvd_uncleaned[, OVERWEIGHT_OR_OBESE := fifelse(BMI >= 23.00, 'Yes', 'No')]

cvd_uncleaned$BMI_CATEGORY <- factor(cvd_uncleaned$BMI_CATEGORY)
cvd_uncleaned$OVERWEIGHT_OR_OBESE <- factor(cvd_uncleaned$OVERWEIGHT_OR_OBESE)

#=================================================================
# Data Cleaning: remove duplicates
#=================================================================

duplicates <- cvd_uncleaned[duplicated(cvd_uncleaned)]
cvd_uncleaned <- cvd_uncleaned[!duplicated(cvd_uncleaned)]

# view(dfSummary(cvd_uncleaned)) - Check: 0 duplicates

#=================================================================
# Final Check 
#=================================================================
view(dfSummary(cvd_uncleaned))
dim(cvd_uncleaned)

#Please output the file as this original dataframe will be read in again for model building later
store_path <- getwd()
write.csv(cvd_uncleaned, paste(store_path, "/cvd_cleaned.csv",sep=""), row.names=FALSE)

#---------------------------------------End of Phase 1-------------------------------------------


# ++=================++
# ||   Phase 2: EDA  ||
# ++=================++

cvd_cleaned <- fread("cvd_cleaned.csv", stringsAsFactors = T)
dim(cvd_cleaned) # 333374 rows, 31 columns
view(dfSummary(cvd_cleaned))

# ================================================================
# EDA: Check for Outliers
#=================================================================
summary(cvd_cleaned$CIGARETTES_PER_DAY) #MAX is 300

#Check for outliers
#function to count the number of outliers
count_outliers <- function(data, column_name) {
  quartiles <- quantile(data[[column_name]], na.rm = TRUE, probs = c(0.25, 0.75))
  IQR_val <- IQR(data[[column_name]],na.rm=TRUE)
  
  lower_bound <- quartiles[1] - 1.5 * IQR_val #Lower whisker
  upper_bound <- quartiles[2] + 1.5 * IQR_val #Upper whisker
  
  outliers_count <- sum(data[[column_name]] < lower_bound | data[[column_name]] > upper_bound, na.rm = TRUE) #IQR Method to identify outliers
  
  return(outliers_count)
}

ggplot(cvd_cleaned,aes(x=factor(0),CIGARETTES_PER_DAY))+geom_boxplot() #from the boxplot, there are indeed a lot of outliers which skewed the data significantly
count_outliers(cvd_cleaned,"CIGARETTES_PER_DAY") # 13534 outliers

# ================================================================
# EDA: Removal of Extreme Values
# To avoid skewness in our data, we decide to omit extremely high values in our EDA phase. 
#=================================================================
# Set a Z-score threshold
z_threshold <- 3 

# Calculate Z-scores for the dependent variable
cvd_cleaned$Zscore <- scale(cvd_cleaned$CIGARETTES_PER_DAY)
# Identify outliers based on the Z-score
outliers <- cvd_cleaned[cvd_cleaned$Zscore > z_threshold | cvd_cleaned$Zscore < -z_threshold, ]
cvd_cleaned_without_extreme_values <- cvd_cleaned[!(cvd_cleaned$Zscore > z_threshold | cvd_cleaned$Zscore < -z_threshold), ] #outliers/extreme values are removed

# Check number of outliers removed
dim(outliers) # 8095 rows are deleted
dim(cvd_cleaned_without_extreme_values)

cvd_cleaned <- cvd_cleaned_without_extreme_values #overwrite cvd_cleaned to exclude extreme values

# To check
view(dfSummary(cvd_cleaned)) # 333374 rows -> 325279 rows 

# Used for PowerBI Dashboard
# store_path <- getwd()
# write.csv(cvd_cleaned, paste(store_path, "/cvd_cleaned_no_outliers.csv",sep=""), row.names=FALSE)

#=================================================================
# Uni-variate EDA 
#=================================================================

# Response Variable ------------------------------------------------------------
# HEART_DISEASE
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Heart Disease", x="Heart Disease", y="Count") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme_minimal()


# Personal Particulars ---------------------------------------------------------
# AGE_GROUP
cvd_cleaned$AGE_GROUP <- factor(cvd_cleaned$AGE_GROUP, levels = c("[18,24]", "[25,34]", "[35,44]", "[45,54]", "[55,64]", "[65,Inf)"))
ggplot(cvd_cleaned, aes(x=AGE_GROUP, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Age Group", x="Age Group", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1))   

# EDUCATION
cvd_cleaned$EDUCATION <- factor(cvd_cleaned$EDUCATION, levels = c("Attended Some High School", "Graduated High School", "Attended Some College", "Graduated College"))
ggplot(cvd_cleaned, aes(x=EDUCATION, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Education Level", x="Education Level", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(angle = 45, hjust = 1)) 

# INCOME
# Replacing values with categories
cvd_cleaned$INCOME <- factor(cvd_cleaned$INCOME, levels = c("Missing", "(-Inf, 10,000)", "[10,000, 15,000)", "[15,000, 20,000)", "[20,000, 25,000)", "[25,000, 35,000)", "[35,000, 50,000)", "[50,000, 75,000)", "[75,000, 100,000)", "[100,000, 150,000)", "[150,000, 200,000)", "[200,000, Inf)"))
ggplot(cvd_cleaned, aes(x=INCOME, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Income", x="Income", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) 

# UNABLE_TO_AFFORD_MED
ggplot(cvd_cleaned, aes(x=UNABLE_TO_AFFORD_MED, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Inability to visit a doctor in the past year due to costs", x="Inability to visit a doctor in the past year due to costs", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Physical Characteristics -----------------------------------------------------
# BMI_CATEGORY
cvd_cleaned$BMI_CATEGORY = factor(cvd_cleaned$BMI_CATEGORY, levels = c("Underweight","Normal Weight","Overweight","Obese"))
ggplot(cvd_cleaned, aes(x=BMI_CATEGORY, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of BMI Category", x="BMI Category", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# General Health Status --------------------------------------------------------
# GENERAL_HEALTH
cvd_cleaned$GENERAL_HEALTH = factor(cvd_cleaned$GENERAL_HEALTH, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
ggplot(cvd_cleaned, aes(x=GENERAL_HEALTH, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of General Health", x="General Health", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# PHYSICAL_HEALTH
ggplot(cvd_cleaned, aes(y=PHYSICAL_HEALTH, x='', fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Box Plot of Number of Days in 30 days with Poor Physical Health", y="Number of Days in 30 days with Poor Physical Health") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# MENTAL_HEALTH
ggplot(cvd_cleaned, aes(y=MENTAL_HEALTH, x='', fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Box Plot of Number of Days in 30 days with Poor Mental Health", y="Number of Days in 30 days with Poor Mental Health") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Health Habits & Behaviours ---------------------------------------------------
# CHECKUP_DURATION_YR
cvd_cleaned$CHECKUP_DURATION_YR = factor(cvd_cleaned$CHECKUP_DURATION_YR, levels = c("[0,1)", "[1,2)", "[2,5)", "[5, Inf)"))
ggplot(cvd_cleaned, aes(x=CHECKUP_DURATION_YR, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Years since the last doctor's routine checkup", x="Years since the last doctor's routine checkup", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# EXERCISE
ggplot(cvd_cleaned, aes(x=EXERCISE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Exercise", x="Exercise", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SLEEP_DURATION_HR
ggplot(cvd_cleaned, aes(y=SLEEP_DURATION_HR, x='', fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Box Plot of Average Hours of Sleep per 24hr period", y="Average Hours of Sleep per 24hr period") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SMOKING
ggplot(cvd_cleaned, aes(x=SMOKING, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Smoking", x="Smoking", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# CIGARETTES_PER_DAY
ggplot(cvd_cleaned, aes(y=CIGARETTES_PER_DAY, x='', fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Box Plot of Number of Cigarettes Per Day", y="Number of Cigarettes per Day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# DRINKING
ggplot(cvd_cleaned, aes(x=DRINKING, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Drinking", x="Drinking", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# DRINKING_DAYS_PER_MONTH
ggplot(cvd_cleaned, aes(y=DRINKING_DAYS_PER_MONTH, x='', fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Box Plot of Number of days of alcohol consumption in the past 30 days", y="Number of days of alcohol consumption in the past 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# HEAVY_DRINKING
ggplot(cvd_cleaned, aes(x=HEAVY_DRINKING, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Heavy Drinking", x="Heavy Drinking", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Health Issues & Illness History ----------------------------------------------
# STROKE
ggplot(cvd_cleaned, aes(x=STROKE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Stroke Diagnosis", x="Stroke Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ASTHMA
ggplot(cvd_cleaned, aes(x=ASTHMA, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Asthma Diagnosis", x="Asthma Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SKIN_CANCER
ggplot(cvd_cleaned, aes(x=SKIN_CANCER, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Skin Cancer Diagnosis", x="Skin Cancer Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# OTHER_CANCER
ggplot(cvd_cleaned, aes(x=OTHER_CANCER, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Other types of Cancer Diagnosis", x="Other types of Cancer Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# LUNG_DISEASE
ggplot(cvd_cleaned, aes(x=LUNG_DISEASE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Lung Disease Diagnosis", x="Lung Disease Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# DEPRESSION
ggplot(cvd_cleaned, aes(x=DEPRESSION, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Depressive Disorder Diagnosis", x="Depressive Disorder Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# KIDNEY_DISEASE
ggplot(cvd_cleaned, aes(x=KIDNEY_DISEASE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Kidney Disease Diagnosis", x="Kidney Disease Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# ARTHRITIS
ggplot(cvd_cleaned, aes(x=ARTHRITIS, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Arthritis Diagnosis", x="Arthritis Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# OVERWEIGHT_OR_OBESE
ggplot(cvd_cleaned, aes(x=OVERWEIGHT_OR_OBESE, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Respondent being Overweight or Obese", x="Respondent being Overweight or Obese", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# DIABETES
ggplot(cvd_cleaned, aes(x=DIABETES, fill='darkseagreen')) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-0.5) + 
  scale_fill_identity() +
  labs(title="Bar Graph of Diabetes Diagnosis", x="Diabetes Diagnosis", y="Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#================================================================
# Bi-variate EDA 
## Response variable: Heart Disease
## Predictor variable: Categorical
#=================================================================
# Personal Particulars ---------------------------------------------------------
# Proportion of Heart Disease by Age Group
cvd_cleaned$AGE_GROUP = factor(cvd_cleaned$AGE_GROUP, levels = c("[18,24]", "[25,34]", "[35,44]", "[45,54]", "[55,64]", "[65,Inf)"))
ggplot(cvd_cleaned, aes(x = AGE_GROUP, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Age Group",
       x = "Age Group",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format()) 

# Proportion of Heart Disease by Education
cvd_cleaned$EDUCATION = factor(cvd_cleaned$EDUCATION, levels = c("Attended Some High School", "Graduated High School", "Attended Some College", "Graduated College"))
ggplot(cvd_cleaned, aes(x = EDUCATION, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Education",
       x = "Education",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Income
cvd_plotting <- cvd_cleaned %>%
  filter(INCOME != "Missing")
cvd_plotting$INCOME = factor(cvd_plotting$INCOME, levels = c("(-Inf, 10,000)", "[10,000, 15,000)", "[15,000, 20,000)", "[20,000, 25,000)", "[25,000, 35,000)", "[35,000, 50,000)", "[50,000, 75,000)", "[75,000, 100,000)", "[100,000, 150,000)", "[150,000, 200,000)", "[200,000, Inf)"))
ggplot(cvd_plotting, aes(x = INCOME, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Income",
       x = "Income",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Inability to Visit a Doctor in the Past Year due to Costs
ggplot(cvd_cleaned, aes(x = UNABLE_TO_AFFORD_MED, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Inability to Visit a Doctor in the Past Year due to Costs",
       x = "Inability to Visit a Doctor in the Past Year due to Costs",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Physical Characteristics -----------------------------------------------------
# Proportion of Heart Disease by BMI Category
cvd_cleaned$BMI_CATEGORY = factor(cvd_cleaned$BMI_CATEGORY, levels = c("Underweight", "Normal Weight", "Overweight", "Obese"))
ggplot(cvd_cleaned, aes(x = BMI_CATEGORY, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by BMI Category",
       x = "BMI Category",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# General Health Status --------------------------------------------------------
# Proportion of Heart Disease by General Health
cvd_cleaned$GENERAL_HEALTH = factor(cvd_cleaned$GENERAL_HEALTH, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))
ggplot(cvd_cleaned, aes(x = GENERAL_HEALTH, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by General Health",
       x = "General Health",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Health Habits & Behaviours ---------------------------------------------------
# Proportion of Heart Disease by Checkup Duration (Years) 
cvd_cleaned$CHECKUP_DURATION_YR = factor(cvd_cleaned$CHECKUP_DURATION_YR, levels = c("[0,1)", "[1,2)", "[2,5)", "[5, Inf)" ))
ggplot(cvd_cleaned, aes(x = CHECKUP_DURATION_YR, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Checkup Duration (Years)",
       x = "Checkup Duration (Years)",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Exercise 
ggplot(cvd_cleaned, aes(x = EXERCISE, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Exercise",
       x = "Exercise",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Smoking
ggplot(cvd_cleaned, aes(x = SMOKING, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Smoking",
       x = "Smoking",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Drinking
ggplot(cvd_cleaned, aes(x = DRINKING, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Drinking",
       x = "Drinking",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Health Issues & Illness History ----------------------------------------------
# Proportion of Heart Disease by Stroke 
ggplot(cvd_cleaned, aes(x = STROKE, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Stroke",
       x = "Stroke",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Asthma 
ggplot(cvd_cleaned, aes(x = ASTHMA, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Asthma",
       x = "Asthma",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Skin Cancer 
ggplot(cvd_cleaned, aes(x = SKIN_CANCER, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Skin Cancer",
       x = "Skin Cancer",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Other Cancer 
ggplot(cvd_cleaned, aes(x = OTHER_CANCER, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Other Cancer",
       x = "Other Cancer",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Lung Disease
ggplot(cvd_cleaned, aes(x = LUNG_DISEASE, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Lung Disease",
       x = "Lung Disease",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Depression
ggplot(cvd_cleaned, aes(x = DEPRESSION, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Depression",
       x = "Depression",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Kidney Disease
ggplot(cvd_cleaned, aes(x = KIDNEY_DISEASE, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Kidney Disease",
       x = "Kidney Disease",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Arthritis
ggplot(cvd_cleaned, aes(x = ARTHRITIS, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Arthritis",
       x = "Arthritis",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Diabetes
cvd_cleaned$DIABETES = factor(cvd_cleaned$DIABETES, levels = c("No", "Yes", "Yes, but during pregnancy" ))
ggplot(cvd_cleaned, aes(x = DIABETES, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Diabetes",
       x = "Diabetes",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Proportion of Heart Disease by Being Overweight or Obese
ggplot(cvd_cleaned, aes(x = OVERWEIGHT_OR_OBESE, fill = HEART_DISEASE)) +
  geom_bar(aes(y = ..prop.., group = HEART_DISEASE), position = "fill") +
  scale_fill_manual(values = c("Yes" = "darkred", "No" = "darkseagreen")) +
  labs(title = "Proportion of Heart Disease by Being Overweight or Obese",
       x = "Being Overweight or Obese",
       y = "Proportion",
       fill = "Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

#=================================================================
# Bi-variate EDA
## Response variable: Heart Disease
## Predictor variable: Continuous
#=================================================================
# PHYSICAL_HEALTH --------------------------------------------------------------
# Days with Poor Physical Health by Heart Disease
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, y=PHYSICAL_HEALTH, fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Days with Poor Physical Health in 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Days with Poor Physical Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Days with Poor Physical Health by Heart Disease
avg_physical_health <- aggregate(PHYSICAL_HEALTH ~ HEART_DISEASE, data=cvd_cleaned, FUN=base::mean)
ggplot(avg_physical_health, aes(x=HEART_DISEASE, y=PHYSICAL_HEALTH, fill='darkseagreen')) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(PHYSICAL_HEALTH, 2)), vjust=-0.3) + 
  scale_fill_identity() +
  labs(title="Average Days with Poor Physical Health in 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Days with Poor Physical Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# MENTAL_HEALTH ----------------------------------------------------------------
# Days with Poor Mental Health by Heart Disease
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, y=MENTAL_HEALTH, fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Days with Poor Mental Health in 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Days with Poor Mental Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Days with Poor Mental Health by Heart Disease
avg_mental_health <- aggregate(MENTAL_HEALTH ~ HEART_DISEASE, data=cvd_cleaned, FUN=base::mean)
ggplot(avg_mental_health, aes(x=HEART_DISEASE, y=MENTAL_HEALTH, fill='darkseagreen')) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(MENTAL_HEALTH, 2)), vjust=-0.3) + 
  scale_fill_identity() +
  labs(title="Average Days with Poor Mental Health in 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Days with Poor Mental Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# SLEEP_DURATION_HR ------------------------------------------------------------
# Average Sleep Duration by Heart Disease
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, y=SLEEP_DURATION_HR, fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Average Hours of Sleep per 24hr period by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Hours of Sleep per 24hr period") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Sleep Duration across Respondents by Heart Disease
avg_sleep_duration <- aggregate(SLEEP_DURATION_HR ~ HEART_DISEASE, data=cvd_cleaned, FUN=base::mean)
ggplot(avg_sleep_duration, aes(x=HEART_DISEASE, y=SLEEP_DURATION_HR, fill='darkseagreen')) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(SLEEP_DURATION_HR, 2)), vjust=-0.3) + 
  scale_fill_identity() +
  labs(title="Average Hours of Sleep per 24hr period across Respondents by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Hours of Sleep per 24hr period across Respondents") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# CIGARETTES_PER_DAY -----------------------------------------------------------
# Cigarettes per Day by Heart Disease
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, y=CIGARETTES_PER_DAY, fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Cigarettes per Day by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Cigarettes per Day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Cigarettes per Day by Heart Disease
avg_cigarettes_per_day <- aggregate(CIGARETTES_PER_DAY ~ HEART_DISEASE, data=cvd_cleaned, FUN=base::mean)
ggplot(avg_cigarettes_per_day, aes(x=HEART_DISEASE, y=CIGARETTES_PER_DAY, fill='darkseagreen')) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(CIGARETTES_PER_DAY, 2)), vjust=-0.3) + 
  scale_fill_identity() +
  labs(title="Average Cigarettes per Day by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Cigarettes per Day") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# DRINKING_DAYS_PER_MONTH ------------------------------------------------------
# Days of Alcohol Consumption per Month by Heart Disease
ggplot(cvd_cleaned, aes(x=HEART_DISEASE, y=DRINKING_DAYS_PER_MONTH, fill='darkseagreen')) +
  geom_boxplot() +
  scale_fill_identity() +
  labs(title="Number of days of alcohol consumption in the past 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Number of days of alcohol consumption in the past 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Days of Alcohol Consumption per Month by Heart Disease
avg_drinking_days <- aggregate(DRINKING_DAYS_PER_MONTH ~ HEART_DISEASE, data=cvd_cleaned, FUN=base::mean)
ggplot(avg_drinking_days, aes(x=HEART_DISEASE, y=DRINKING_DAYS_PER_MONTH, fill='darkseagreen')) +
  geom_bar(stat="identity") +
  geom_text(aes(label=round(DRINKING_DAYS_PER_MONTH, 2)), vjust=-0.3) + 
  scale_fill_identity() +
  labs(title="Average Number of days of alcohol consumption in the past 30 days by Heart Disease Diagnosis",
       x="Heart Disease Diagnosis", y="Average Number of days of alcohol consumption in the past 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#=================================================================
# Bi-variate EDA between two predictor variables
#=================================================================
# Diabetes by BMI Category
ggplot(cvd_cleaned, aes(x = BMI_CATEGORY, fill = DIABETES)) +
  geom_bar(aes(y = ..prop.., group = DIABETES), position = "fill") +
  scale_fill_manual(values = c("Yes, but during pregnancy" = "darkslateblue", "No" = "darkseagreen", "Yes" = "darkred")) +
  labs(title = "Proportion of Diabetes by BMI Category",
       x = "BMI Category",
       y = "Proportion",
       fill = "Diabetes") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::percent_format())

# Average Days with Poor Physical Health by Exercise
ggplot(cvd_cleaned, aes(x = EXERCISE, y = PHYSICAL_HEALTH, group = EXERCISE)) +
  stat_summary(fun = function(x) mean(x), geom = "col", fill = "darkseagreen") +
  labs(title = "Average Days with Poor Physical Health in 30 days by Exercise Category",
       x = "Exercise",
       y = "Average Days with Poor Physical Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Days with Poor Mental Health by Exercise
ggplot(cvd_cleaned, aes(x = EXERCISE, y = MENTAL_HEALTH, group = EXERCISE)) +
  stat_summary(fun = function(x) mean(x), geom = "col", fill = "darkseagreen") +
  labs(title = "Average Days with Poor Mental Health in 30 days by Exercise Category",
       x = "Exercise",
       y = "Average Days with Poor Mental Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Average Days with Poor Mental Health by Income
ggplot(cvd_plotting, aes(x = INCOME, y = MENTAL_HEALTH, group = INCOME)) +
  stat_summary(fun = function(x) mean(x), geom = "col", fill = "darkseagreen") +
  labs(title = "Average Days with Poor Mental Health in 30 days by Income Category",
       x = "Income Category",
       y = "Average Days with Poor Mental Health in 30 days") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

#=================================================================
# Multi-variate EDA
#=================================================================

# Heart Disease by Sleep Duration and Age Group --------------------------------
# Define the breaks for the intervals
breaks <- c(1, 4, 5, 6, 7, 8, 9, Inf)

# Define the labels for the intervals
labels <- c("[1 - 4]", "[5]", "[6]", "[7]", "[8]", "[9]", "[10+]")

# Add a new column to clean_dt with the grouped sleep durations
clean_dt <- cvd_cleaned
clean_dt <- clean_dt %>%
  mutate(SLEEP_DURATION_GROUP = cut(SLEEP_DURATION_HR, breaks = breaks, labels = labels, include.lowest = TRUE))

# Calculate total counts for each sleep duration group
total_counts <- clean_dt %>%
  group_by(SLEEP_DURATION_GROUP) %>%
  summarise(total = n())

# Calculate counts of people with heart disease for each sleep duration group and age group
heart_disease_counts <- clean_dt %>%
  filter(HEART_DISEASE == "Yes") %>%
  group_by(SLEEP_DURATION_GROUP, AGE_GROUP) %>%
  summarise(heart_disease_count = n())

# Merge the two datasets to calculate the percentage of people with heart disease in each sleep duration group
merged_data <- merge(total_counts, heart_disease_counts, by = "SLEEP_DURATION_GROUP", all.x = TRUE)

# Calculate the percentage of people with heart disease in each sleep duration group
merged_data$percentage <- (merged_data$heart_disease_count / merged_data$total) * 100

# Create the plot
ggplot(data = merged_data, aes(x = SLEEP_DURATION_GROUP, y = percentage, fill = AGE_GROUP)) +
  geom_bar(stat = 'identity', position = "dodge") +
  theme_minimal() +
  labs(x = 'Hours of Sleep', y = 'Percentage with Heart Disease', title = 'Percentage of People with Heart Disease by Sleep Duration and Age Group') +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("darkseagreen", "darkred", "darkblue", "darkorange","darkmagenta", "darkcyan")) 

# Heart Disease by Cigarettes per Day and Age Group ----------------------------
# Define the breaks and labels for the intervals
cigg <- c(0, 1, 5, 10, 15, 20, Inf)
label2 <- c("[0]", "[1 - 5]", "[6 - 10]", "[11 - 15]", "[16 - 20]", "[21 - 25]")

clean_dt <- clean_dt %>%
  mutate(CIGARETTES_PER_DAY = cut(CIGARETTES_PER_DAY, breaks = cigg, labels = label2, include.lowest = TRUE))

# Calculate total counts for each cigarette group
total_counts <- clean_dt %>%
  group_by(CIGARETTES_PER_DAY) %>%
  summarise(total = n())

# Calculate counts of people with heart disease for each cigarette group and age group
heart_disease_counts <- clean_dt %>%
  filter(HEART_DISEASE == "Yes") %>%
  group_by(CIGARETTES_PER_DAY, AGE_GROUP) %>%
  summarise(heart_disease_count = n())

# Merge the two datasets to calculate the percentage of people with heart disease in each cigarette group
merged_cigg <- merge(total_counts, heart_disease_counts, by = "CIGARETTES_PER_DAY", all.x = TRUE)

# Calculate the percentage of people with heart disease in each cigarette group
merged_cigg$percentage <- (merged_cigg$heart_disease_count / merged_cigg$total) * 100

# Create the plot
ggplot(data = merged_cigg, aes(x = CIGARETTES_PER_DAY, y = percentage, fill = AGE_GROUP)) +
  geom_bar(stat = 'identity', position = "dodge") +
  theme_minimal() +
  labs(x = 'Cigarettes Per Day', y = 'Percentage with Heart Disease', title = 'Percentage of People with Heart Disease by Cigarettes per Day and Age Group') +
  scale_x_discrete(labels = label2) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_fill_manual(values = c("darkseagreen", "darkred", "darkblue", "darkorange","darkmagenta", "darkcyan"))

# Heart Disease by Drinking Days per Month and Age Group -----------------------
# Define the breaks and labels for the intervals
drinking <- c(0, 1, 5, 10, 15, 20, 25, 30)

label3 <- c("[0]", "[1 - 5]", "[6 - 10]", "[11 - 15]", "[16 - 20]", "[21 - 25]", "[26 - 30]") 

clean_dt <- clean_dt %>%
  mutate(DRINKING_DAYS_PER_MONTH = cut(DRINKING_DAYS_PER_MONTH, breaks = drinking, labels = label3, include.lowest = TRUE))

# Calculate total counts for each drinking group
total_counts2 <- clean_dt %>%
  group_by(DRINKING_DAYS_PER_MONTH) %>%
  summarise(total = n())

# Calculate counts of people with heart disease for each drinking group and age group
heart_disease_counts <- clean_dt %>%
  filter(HEART_DISEASE == "Yes") %>%
  group_by(DRINKING_DAYS_PER_MONTH, AGE_GROUP) %>%
  summarise(heart_disease_count = n())

# Merge the two datasets to calculate the percentage of people with heart disease in each drinking group
merged_drink <- merge(total_counts2, heart_disease_counts, by = "DRINKING_DAYS_PER_MONTH", all.x = TRUE)

# Calculate the percentage of people with heart disease in each drinking group
merged_drink$percentage <- (merged_drink$heart_disease_count / merged_drink$total) * 100

# Create the plot
ggplot(data = merged_drink, aes(x = DRINKING_DAYS_PER_MONTH, y = percentage, fill = AGE_GROUP)) +
  geom_bar(stat = 'identity', position = "dodge") +
  theme_minimal() +
  labs(x = 'Drinking Days Per Month', y = 'Percentage with Heart Disease', title = 'Percentage of People with Heart Disease by Drinking Days Per Month and Age Group') +
  scale_x_discrete(labels = label3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("darkseagreen", "darkred", "darkblue", "darkorange","darkmagenta", "darkcyan"))

# Heart Disease by General Health and Exercise ----------------------------
# Calculate the overall proportion of individuals with heart disease within each general health level
general_health_proportions <- cvd_cleaned %>%
  group_by(GENERAL_HEALTH) %>%
  summarise(Total = n(),
            HeartDisease = sum(HEART_DISEASE == "Yes", na.rm = TRUE),
            ProportionWithDisease = HeartDisease / Total) %>%
  ungroup()

# Calculate the distribution of exercise habits among those with heart disease for each general health level
exercise_distribution_within_disease <- cvd_cleaned %>%
  filter(HEART_DISEASE == "Yes") %>%
  count(GENERAL_HEALTH, EXERCISE) %>%
  group_by(GENERAL_HEALTH) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup()

final_data_exercise <- exercise_distribution_within_disease %>%
  left_join(general_health_proportions, by = "GENERAL_HEALTH") %>%
  mutate(AdjustedProportion = Proportion * ProportionWithDisease)

final_data_exercise$GENERAL_HEALTH = factor(final_data_exercise$GENERAL_HEALTH, levels = c("Excellent", "Very good", "Good", "Fair", "Poor"))

ggplot(final_data_exercise, aes(x = GENERAL_HEALTH, y = AdjustedProportion, fill = EXERCISE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2", name = "Exercise") +
  labs(title = "Proportion of Heart Disease by General Health and Exercise",
       x = "General Health Status",
       y = "Proportion with Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Heart Disease by BMI Category and Exercise -----------------------------------
# Calculate the overall proportion of individuals with heart disease within each BMI category
bmi_category_proportions <- cvd_cleaned %>%
  group_by(BMI_CATEGORY) %>%
  summarise(Total = n(),
            HeartDisease = sum(HEART_DISEASE == "Yes", na.rm = TRUE),
            ProportionWithDisease = HeartDisease / Total) %>%
  ungroup()

# Calculate the distribution of exercise habits among those with heart disease for each BMI category
exercise_distribution_within_disease_bmi <- cvd_cleaned %>%
  filter(HEART_DISEASE == "Yes") %>%
  count(BMI_CATEGORY, EXERCISE) %>%
  group_by(BMI_CATEGORY) %>%
  mutate(Proportion = n / sum(n)) %>%
  ungroup()

final_data_exercise_bmi <- exercise_distribution_within_disease_bmi %>%
  left_join(bmi_category_proportions, by = "BMI_CATEGORY") %>%
  mutate(AdjustedProportion = Proportion * ProportionWithDisease)

final_data_exercise_bmi$BMI_CATEGORY = factor(final_data_exercise_bmi$BMI_CATEGORY, levels = c("Underweight", "Normal Weight", "Overweight", "Obese"))

ggplot(final_data_exercise_bmi, aes(x = BMI_CATEGORY, y = AdjustedProportion, fill = EXERCISE)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "Set2", name = "Exercise") +
  labs(title = "Proportion of Heart Disease by BMI Category and Exercise",
       x = "BMI Category",
       y = "Proportion with Heart Disease") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Average Days with Poor Physical Health by Age Group and Heart Disease -------------------------------
avg_physical_health <- cvd_cleaned %>%
  filter(HEART_DISEASE == "Yes") %>%
  group_by(AGE_GROUP) %>%
  summarise(AveragePoorPhysicalHealth = mean(PHYSICAL_HEALTH, na.rm = TRUE)) %>%
  ungroup()

ggplot(avg_physical_health, aes(x = AGE_GROUP, y = AveragePoorPhysicalHealth, fill = AGE_GROUP)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Spectral", name = "Age Group") +
  labs(title = "Average Days with Poor Physical Health by Age Group among Individuals with Heart Disease",
       x = "Age Group",
       y = "Average Days with Poor Physical Health") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

#---------------------------------------End of Phase 2------------------------------------


# ++=======================++
# ||   Phase 3: Modelling  ||
# ++=======================++

dim(cvd_cleaned) # 325279 rows, 32 columns
view(dfSummary(cvd_cleaned))
#=================================================================
# Train-Test Split
#=================================================================
set.seed(200) # For reproducibility
train <- sample.split(Y = cvd_cleaned$HEART_DISEASE, SplitRatio = 0.7)
trainset <- subset(cvd_cleaned, train == T)
testset <- subset(cvd_cleaned, train == F)

summary(trainset$HEART_DISEASE) # 214627 NO, 13068 YES
summary(testset$HEART_DISEASE) # 91983 NO, 5601 YES
# This is an imbalanced data in the trainset and testset as there are too many "NO", the recommended balance ratio is at most 2:1

# Balance the trainset
trainyes <- sum(trainset$HEART_DISEASE == "Yes")
desiredtrainno <- trainyes * 2

trainno <- subset(trainset, HEART_DISEASE == "No")
sampled_no_train <- trainno[sample(nrow(trainno), desiredtrainno), ]
trainData <- rbind(sampled_no_train, subset(trainset, HEART_DISEASE == "Yes"))

# Balance the testset
testyes <- sum(testset$HEART_DISEASE == "Yes")
desiredtestno <- testyes * 2

testno <- subset(testset, HEART_DISEASE == "No")
sampled_no_test <- testno[sample(nrow(testno), desiredtestno), ]
testData <- rbind(sampled_no_test, subset(testset, HEART_DISEASE == "Yes"))

# Check trainset is balanced
summary(trainData$HEART_DISEASE)

# Check testset is balanced
summary(testData$HEART_DISEASE)

#=================================================================
# Modelling
#=================================================================

# +---------------------------------+
# | 1 Logistic Regression           |
# +---------------------------------+

set.seed(200)
# Step 1: Fit the Logistic Regression Model with all the relevant predictor variables
m.glm.1 <- glm(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED +
                 BMI_CATEGORY + GENERAL_HEALTH + MENTAL_HEALTH + PHYSICAL_HEALTH +
                 EXERCISE + SMOKING + CIGARETTES_PER_DAY + DRINKING + DRINKING_DAYS_PER_MONTH +
                 STROKE + ASTHMA + SKIN_CANCER + OTHER_CANCER + LUNG_DISEASE + DEPRESSION +
                 KIDNEY_DISEASE + ARTHRITIS + OVERWEIGHT_OR_OBESE + DIABETES, 
               family = binomial, data = trainData, na.action = na.omit)

# Summary of First Model
summary(m.glm.1)

# Step 2: Remove statistically less significant predictor variables using backward elimination
m.glm.2 <- step(m.glm.1, direction = "backward") 

# Summary of model after backward elimination
summary(m.glm.2)

# Step 3: Check VIF Values for multicollinearity
# Multicollinearity: VIF (continuous vars) >10 / adj-GVIF(categorical vars) >2
vif(m.glm.2) # No multicollinearity

# Step 4: Remove DRINKING (not significant (>0.05))
m.glm.3 <- glm(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED + 
                 BMI_CATEGORY + GENERAL_HEALTH + PHYSICAL_HEALTH + EXERCISE +
                 CIGARETTES_PER_DAY + STROKE + SKIN_CANCER + OTHER_CANCER + 
                 LUNG_DISEASE + KIDNEY_DISEASE + ARTHRITIS + DIABETES, 
               family = binomial, data = trainData, na.action = na.omit)

# Summary of model
summary(m.glm.3)

# Step 5: Remove EXERCISE (not significant (>0.01))
m.glm.4 <- glm(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED + 
                 BMI_CATEGORY + GENERAL_HEALTH + PHYSICAL_HEALTH + 
                 CIGARETTES_PER_DAY + STROKE + SKIN_CANCER + OTHER_CANCER + 
                 LUNG_DISEASE + KIDNEY_DISEASE + ARTHRITIS + DIABETES, 
               family = binomial, data = trainData, na.action = na.omit)

# Summary of model
summary(m.glm.4)
# Final model: m.glm.4

OR.m.glm.4 <- exp(coef(m.glm.4))
round(OR.m.glm.4, 3)

OR.CI.m.glm.4 <- exp(confint(m.glm.4))
round(OR.CI.m.glm.4,3)

# Step 6: Model Evaluation
# Variable Importance
caret::varImp(m.glm.4)
# Higher values indicate more importance. These results match up nicely with the p-values from the model.
# Variables with higher importance: GENERAL_HEALTH, AGE_GROUP, STROKE, KIDNEY_DISEASE and DIABETES

# Step 7: Model Performance Metrics
# Make predictions on the test data
m.prob <- predict(m.glm.4, newdata = testData, type = 'response')

# Finding optimal threshold for threshold moving
# ROC curve and AUC: higher the AUC, the better the model is at predicting (https://towardsdatascience.com/understanding-auc-roc-curve-68b2303cc9c5)
library(pROC)
roc_obj <- roc(response = testData$HEART_DISEASE, predictor = m.prob)
auc <- auc(roc_obj)
plot(roc_obj, print.thres = "best", print.thres.best.method = "closest.topleft", main = paste("AUC =", round(auc, 2))) # AUC = 0.84: rather high predictive power
coords <- coords(roc_obj, "best", best.method = "closest.topleft", ret = c("threshold", "sensitivity", "specificity"))
optimal_threshold <- coords$threshold
m.predict.optimal <- ifelse(m.prob > optimal_threshold, "Yes", "No")

# Confusion matrix
table1 <- table(Testset.Actual = testData$HEART_DISEASE, Predicted = m.predict.optimal)
table1
#               Predicted
# Testset.Actual   No  Yes
#            No  8365 2837
#            Yes 1319 4282
round(prop.table(table1), 3)

# Overall Accuracy
accuracy.optimal <- mean(m.predict.optimal == testData$HEART_DISEASE)
print(accuracy.optimal) # Accuracy = 0.7526632

# Calculate predictions for training data
train.prob <- predict(m.glm.4, newdata = trainData, type = 'response')
train.predict.optimal <- ifelse(train.prob > optimal_threshold, "Yes", "No")

# Confusion matrix for training data
confusion_matrix_train <- table(Trainset.Actual = trainData$HEART_DISEASE, Predicted = train.predict.optimal)
confusion_matrix_train
#                Predicted
# Trainset.Actual    No   Yes
#              No  19355  6781
#              Yes  3120  9948

# Overall accuracy for training data
train_accuracy <- mean(train.predict.optimal == trainData$HEART_DISEASE)
print(paste("Training Accuracy:", train_accuracy)) # Accuracy = 0.747449239873482

# +---------------------------------+
# | 2 CART - Classification Tree    |
# +---------------------------------+

set.seed(200) # for 10-fold CV
# Step 1: Grow tree to max and set cp to 0
# use method 'class' instead of 'anova': response variable is categorical
cart1 <- rpart(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED +
                 BMI_CATEGORY + GENERAL_HEALTH + MENTAL_HEALTH + 
                 PHYSICAL_HEALTH + EXERCISE + SMOKING + CIGARETTES_PER_DAY + 
                 DRINKING + DRINKING_DAYS_PER_MONTH + STROKE + ASTHMA +
                 SKIN_CANCER + OTHER_CANCER + LUNG_DISEASE + DEPRESSION +
                 KIDNEY_DISEASE + ARTHRITIS + OVERWEIGHT_OR_OBESE + DIABETES,
               data = trainData, method = 'class',
               control = rpart.control(cp = 0))


printcp(cart1)
plotcp(cart1)

# Step 2: Prune Tree to min
# Find Optimal cp:
# store the cp table
dt<-data.table(cart1$cptable)

# Number the sequence of the trees
dt[,index:=1:nrow(dt)]

# Find out minimum index where xerror is min
min_cp_index<-min(dt[(xerror+xstd) == min(xerror+xstd),index])

# Find the CV error cap
cv_error_cap<-dt[min_cp_index,xerror+xstd]     # 0.718206

# Determine the optimal index for the cp
optimal_cp_index <- min(dt[(xerror < cv_error_cap), index])

# Find the geometric mean of the cp for that index and one cp appearing before it
cp.opt = sqrt(dt[index == optimal_cp_index,CP]*
                dt[index == optimal_cp_index-1,CP]) # 0.00118497

# Step 3: Prune the max tree using optimal.cp
cart_optimal <- prune(cart1, cp = cp.opt)

print(cart_optimal)
rpart.plot(cart_optimal)

summary(cart_optimal)

# Step 4: Model Evaluation
# Variable Importance
# Higher scores indicate that the predictor has a greater impact on the model's performance
cart_optimal$variable.importance
# Variables with higher importance: AGE_GROUP, GENERAL_HEATLH, PHYSICAL_HEALTH

# Step 5: CART Model Performance Metrics
# Make predictions on the test data using cart_optimal
cart_predict <- predict(cart_optimal, newdata = testData, type = "class")

# Confusion Matrix
table2 <- table(Testset.Actual = testData$HEART_DISEASE, cart_predict, deparse.level = 2)
table2

#                 cart_predict
# Testset.Actual    No   Yes
#            No   9524  1678
#            Yes  2261  3340

# calculate the proportions of counts in the confusion matrix, providing a normalised view of the results.
round(prop.table(table2), 3)

#                cart_predict
# Testset.Actual    No   Yes
#            No  0.567 0.100
#            Yes 0.135 0.199


# Overall Accuracy
mean(cart_predict == testData$HEART_DISEASE) # 0.7655776


# +---------------------------------+
# | 3 Random Forest                 |
# +---------------------------------+

set.seed(200)  # for Bagging & RSF selection.

# Step 1: Selecting of relevant predictor variables for modelling
m.RF.1 <- randomForest(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED +
                         BMI_CATEGORY + GENERAL_HEALTH + MENTAL_HEALTH + PHYSICAL_HEALTH +
                         EXERCISE + SMOKING + CIGARETTES_PER_DAY + DRINKING + DRINKING_DAYS_PER_MONTH +
                         STROKE + ASTHMA + SKIN_CANCER + OTHER_CANCER + LUNG_DISEASE + DEPRESSION +
                         KIDNEY_DISEASE + ARTHRITIS + OVERWEIGHT_OR_OBESE + DIABETES, 
                       data = trainData, na.action = na.omit, importance = T)

m.RF.1

plot(m.RF.1)
# OOB error has stabilised after 100-150 trees

# Step 2: Model Evaluation
# Variable Importance
var.impt <- importance(m.RF.1)
varImpPlot(m.RF.1, type = 1)
# Variables with higher importance are AGE_GROUP, GENERAL_HEALTH and STROKE

# Step 3: Model Performance Metrics
# 3.1: Using Out-of-Bag (OOB) samples
m.RF.1
# OOB Confusion matrix:
#        No  Yes class.error
# No  22254 3882   0.1485308
# Yes  5742 7326   0.4393939

# FPR: 3882/3882+22254 = 0.1485308
# FNR: 5742/5742+7326 = 0.4393939

# OOB Error
round(m.RF.1$err.rate[nrow(m.RF.1$err.rate), "OOB"]*100,2)
# OOB error = 24.55%

# 3.2: Make predictions on the test data
testpred <- predict(m.RF.1, testData)

# Test Data Confusion matrix:
test.table <- table(Actual = testData$HEART_DISEASE, Predicted = testpred)
test.table
#       Predicted
# Actual    No   Yes
#    No   9659  1543
#    Yes  2399  3202
round(prop.table(test.table), 3)

# Overall Accuracy
mean(testpred == testData$HEART_DISEASE) # 0.765399


# +---------------------------------+
# | 4 Neural Network                |
# +---------------------------------+

set.seed(200)

# Step 1: Grid search for hyper parameter tuning
# Initialise empty dataframe to store fine tuning results
gridsearch.df <- data.frame(size=integer(), 
                            decay=double(),
                            accuracy=double(),
                            recall=double(),
                            total=double())

# Grid search values
# Size is the number of units in hidden layer (nnet fit a single hidden layer neural network)
gridsearch.sizes <- c(5,7,9,11,13,15,17)
# Decay is the regularization parameter to avoid over-fitting
gridsearch.decay <- c(1e-1, 1e-2, 1e-3, 1e-4, 1e-5)
accuracies <- rep(0, length(gridsearch.sizes) * length(gridsearch.decay))
recalls <- rep(0, length(accuracies))
threshold <- 0.5
idx <- 1

# Max iterations of 200 to be used consistently throughout grid search & actual modelling
maxit <- 200

# Grid search every combination of size & decay
for (size in gridsearch.sizes) {
  for (decay in gridsearch.decay) {
    set.seed(200)
    nn <- nnet(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED +
                 BMI_CATEGORY + GENERAL_HEALTH + MENTAL_HEALTH + PHYSICAL_HEALTH +
                 EXERCISE + SMOKING + CIGARETTES_PER_DAY + DRINKING + DRINKING_DAYS_PER_MONTH +
                 STROKE + ASTHMA + SKIN_CANCER + OTHER_CANCER + LUNG_DISEASE + DEPRESSION +
                 KIDNEY_DISEASE + ARTHRITIS + OVERWEIGHT_OR_OBESE + DIABETES, 
               data=trainData, size=size, decay=decay, maxit=maxit)
    predictions <- predict(nn, testData)
    y.hat.nn <- ifelse(predictions < threshold, 0, 1)
    
    # Convert actual values to factor with levels matching predictions
    actual_values <- factor(ifelse(testData$HEART_DISEASE == "Yes", "1", "0"), levels = c("0", "1"))
    
    # Calculate confusion matrix
    confusion_matrix <- table(actual_values, y.hat.nn, deparse.level=2)
    
    # Calculate accuracy
    accuracy <- sum(actual_values==y.hat.nn) / nrow(testData)
    
    # Calculate recall
    recall <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
    
    gridsearch.df[idx, ] = c(size, decay, accuracy, recall, accuracy+recall)
    idx <- idx+1
  }  
}

# Visualise grid search results
wireframe(recall~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
wireframe(accuracy~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
wireframe(total~size+decay, data=gridsearch.df, drape=TRUE, scales=list(arrows=FALSE))
opt.param <- gridsearch.df[which.max(gridsearch.df$total),]

# Step 2: Actual model based on optimal hyper parameters
set.seed(200)
nn <- nnet(HEART_DISEASE ~ AGE_GROUP + EDUCATION + UNABLE_TO_AFFORD_MED +
             BMI_CATEGORY + GENERAL_HEALTH + MENTAL_HEALTH + PHYSICAL_HEALTH +
             EXERCISE + SMOKING + CIGARETTES_PER_DAY + DRINKING + DRINKING_DAYS_PER_MONTH +
             STROKE + ASTHMA + SKIN_CANCER + OTHER_CANCER + LUNG_DISEASE + DEPRESSION +
             KIDNEY_DISEASE + ARTHRITIS + OVERWEIGHT_OR_OBESE + DIABETES, 
           data=trainData, size=opt.param$size, decay=opt.param$decay, maxit=maxit) # to skip grid search, use size=17, decay=0.00001

# Step 3: Model Performance Metrics
# Make predictions on the test data
predictions <- predict(nn, testData)

# Finding optimal threshold for threshold moving
n <- 60
recalls = rep(0, n)
accuracies = rep(0, n)
threshold <- 0.1

for (i in 1:n) {
  threshold <- threshold + 0.01
  testpred <- ifelse(predictions < threshold, 0, 1)
  
  # Convert actual values to factor with levels matching predictions
  actual_values <- factor(ifelse(testData$HEART_DISEASE == "Yes", "1", "0"), levels = c("0", "1"))
  
  # Calculate confusion matrix
  confusion_matrix <- table(Actual = actual_values, Predicted = testpred, deparse.level=2)
  
  # Calculate accuracy
  accuracies[i] <- sum(actual_values==testpred) / nrow(testData)
  
  # Calculate recall
  recalls[i] <- confusion_matrix[4]/(confusion_matrix[4]+confusion_matrix[2]) # recall = TP/(TP+FN) 
}

# Confusion matrix
confusion_matrix
#       Predicted
# Actual    No   Yes
#    No  10823   379
#    Yes  4168  1433
round(prop.table(confusion_matrix), 3)

# Overall accuracy
sum(diag(confusion_matrix)) / sum(confusion_matrix) # 0.7293936

#---------------------------------------End of Phase 3------------------------------------





