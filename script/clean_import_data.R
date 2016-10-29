library(readxl)
library(readr)
library(stringr)
library(plyr)
library(dplyr)
library(reshape2)
library(ggmap)

setwd("C:/Users/tboom_000/Documents/Personal/Projects/RioultSurvey")
source('./script/remap_char.R') # Function to clean up dance co names.

# Function to map zip codes to state
map_state <- function(x) {
     prefix <- as.numeric(substr(x, 1, 3))
     if(is.na(prefix))                      {state <- NA}
     else if(prefix >= 10 & prefix <= 27)   {state  <- "massachusetts"}
     else if(prefix >= 60 & prefix <= 69)   {state  <- "connecticut"}
     else if(prefix >= 70 & prefix <= 89)   {state  <- "new jersey"}
     else if(prefix >= 100 & prefix <= 149) {state  <- "new york"}
     else                                   {state  <- "Other"}
     return(state)
}

as_binary <- function(x) {
     x[x == 'x'] <- 1 # For 2013 encoded data
     x[x == 'T'] <- 1 # For 2016 encoded data
     x <- as.integer(x)
     x[is.na(x)] <- 0
     return(x)
}

# Load and clean data
# Load data from Excel
types <- c(rep('text', 26), 'numeric', rep('text', 29))
data_2016 <- read_excel('./SourceData/SurveyResults2016import.xls', 
                        col_types = types, sheet = 1, skip = 9)

data_2016 <- data_2016[1:394, c(1, 5:55)] # Filter out unneeded cols & blank rows

colnames(data_2016) <- c('Performance', 'Rioult_Direct', 'Joyce_Direct', 
                         'Any_Print_Ad', 'Times', 'TimeOut', 'MetroNY', 
                         'AMNY_Ad', 'Article_Blog', 'Any_Social_Media', 'Facebook',
                         'Twitter', 'Instagram', 'Personal_Recommendation', 
                         'Other_Source', 'Source_Notes', 'Number_Performances',
                         'Like_Company', 'Like_Specific_Dances', 'Like_Modern_Dance', 
                         'Like_Dance', 'Like_Music', 'Like_Joyce', 'Joyce_Years_Member', 
                         'Am_Dancer', 'Am_FriendOrFamily', 'Am_Guest', 'Other_Reason',
                         '92Y', 'BAM', 'Dixon', 'Pillow',
                         'Judson', 'Joyce', 'LincolnCtr',
                         'CityCtr', 'NYLiveArts', 'Triskelion',
                         'Other_Venue', 'Dance_Company1', 'Dance_Company2',
                         'Dance_Company3', 'Interested_InSchool',
                         'Interested_Children', 'Interested_Pre-Prof',
                         'Interested_Adult', 'Gender', 'Zip_Code', 'Age', 'Ethnicity',
                         'Income', 'Education')

# Modify variable types
data_2016[, c(2:15, 18:23, 25:27, 43:46)] <- 
     lapply(data_2016[, c(2:15, 18:23, 25:27, 43:46)], as_binary)
data_2016$Joyce_Years_Member <- as.numeric(data_2016$Joyce_Years_Member)
data_2016[, c(1, 17, 29:38, 47, 49:52)] <- 
     lapply(data_2016[, c(1, 17, 29:38, 47, 49:52)], as.factor)
data_2016[, 28] <- as.integer(!is.na(data_2016[,28])) # Field contains text and "T"

# Resequence factor levels
data_2016$Income <- factor(data_2016$Income, 
                           levels = c("<25", "25-50", "50-75", "75-100",
                                      "100-150", "150-200", "200+"))
data_2016$Number_Performances <- factor(data_2016$Number_Performances,
                                        levels = c("First time", "1-4 times", "5+ times"))
data_2016$Education <- factor(data_2016$Education, 
                              levels = c("Some high school", "Some college",
                                         "College grad", "Some graduate school",
                                         "Grad/prof degree"))
data_2016$Ethnicity <- relevel(data_2016$Ethnicity, "White")

# Change "Any_" variables so they are true if any subcategory is true
data_2016$Any_Print_Ad = apply(data_2016[, 4:9], 1, sum)
data_2016$Any_Social_Media = apply(data_2016[, 10:13], 1, sum)

# Create Program variable
data_2016$Program = as.factor(substr(data_2016$Performance, 9,9))

# Create text file of unique dance company names for mapping
# !!! Only run when there is a change in the table !!! Only run when there is a change in the table
# company <- sort(unique(c(data_2016$Dance_Company1, data_2016$Dance_Company2, data_2016$Dance_Company3)))
# write(company, file = './cache/dance_co.txt')

# Remap Dance Company names
map_table <- read_csv('./cache/dance_co_mapping.csv')
data_2016$Dance_Company1_norm <- map_char(data_2016$Dance_Company1, map_table, "Input", "Mapped_To")
data_2016$Dance_Company2_norm <- map_char(data_2016$Dance_Company2, map_table, "Input", "Mapped_To")
data_2016$Dance_Company3_norm <- map_char(data_2016$Dance_Company3, map_table, "Input", "Mapped_To")

# Aggregate data by zip code and map zip codes to state
data_2016$State <- sapply(data_2016$Zip_Code, map_state)
zip_count_2016 <- data_2016 %>% group_by(region = Zip_Code) %>% summarize(value = n())
state_count_2016 <- data_2016 %>% group_by(region = State) %>% summarize(value = n())


#-------------------------------------------------------------------------------
# 2013 Data
#-------------------------------------------------------------------------------
data_2013 <- read_excel('./SourceData/SurveyImport2013.xls', sheet = 2)
# Normalize zip codes
data_2013$Zip <- str_replace(data_2013$Zip, ".000000", "")
# Filter out columns not required for period to period comparison
data_2013 <- data_2013[, c(12,33:38)] 
colnames(data_2013) <- c('Number_Performances','Gender', 'Zip_Code', 'Age', 
                         'Ethnicity', 'Income', 'Education')

# Modify variable types
data_2013 <- data.frame(sapply(data_2013, function(x) gsub("^$|^ $", NA, x)))
data_2013 <- data.frame(sapply(data_2013, as.factor))

# Rename and resequence factor levels
levels(data_2013$Age) <- c("<18", "18-24", "25-29", "30-44", "45-64", "45-64", "65+")

levels(data_2013$Income) <- c("<25", "100-150", "150-200", "200+", "25-50", "50-75", "75-100")
data_2013$Income <- factor(data_2013$Income, 
                           levels = c("<25", "25-50", "50-75", "75-100",
                                      "100-150", "150-200", "200+"))

levels(data_2013$Number_Performances) <- c("1-4 times", "1-4 times", "5+ times", "First time")
data_2013$Number_Performances <- factor(data_2013$Number_Performances,
                                        levels = c("First time", "1-4 times", "5+ times"))

levels(data_2013$Education) <- c("College grad", "Grad/prof degree", "Grad/prof degree",
                                 "Grad/prof degree", "Some high school", "Some college",
                                 "Some college", "Some graduate school", "Some graduate school",
                                 "Some high school", "Some high school")
data_2013$Education <- factor(data_2013$Education, 
                              levels = c("Some high school", "Some college",
                                         "College grad", "Some graduate school",
                                         "Grad/prof degree"))

data_2013$Ethnicity[data_2013$Ethnicity == "Prefer not to say"] <- NA
levels(data_2013$Ethnicity) <- c("Afro-American", "Am Indian", "Asian", "White",
                                 "Hispanic", "Multiracial", "Multiracial",
                                 "White", "White")
data_2013$Ethnicity <- relevel(data_2013$Ethnicity, "White")


# Combine data with 2016
data_combined <- cbind(Year = "2013", data_2013)
temp_2016 <- cbind(Year = "2016", data_2016)
temp_2016 <- select(temp_2016, c(18, 48:53))
temp_2016 <- cbind(Year = "2016", temp_2016)
data_combined <- rbind(data_combined, temp_2016)
rm(temp_2016)
