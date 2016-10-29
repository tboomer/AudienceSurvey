# Analysis of RIOULT audience survey results

library(readxl)
library(readr)
library(stringr)
library(dplyr)
library(reshape2)
library(ggmap)
library(ggplot2)
library(RColorBrewer)

source('./script/remap_char.R') # Function to clean up dance co names.

# Function to map zip codes to state
map_state <- function(x) {
     prefix <- as.numeric(substr(x, 1, 3))
     if(is.na(prefix))                      {state <- NA}
     else if(prefix >= 10 & prefix <= 27)   {state  <- "MA"}
     else if(prefix >= 60 & prefix <= 69)   {state  <- "CT"}
     else if(prefix >= 70 & prefix <= 89)   {state  <- "NJ"}
     else if(prefix >= 100 & prefix <= 149) {state  <- "NY"}
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
data_2016 <- read_excel('./SourceData/SurveyResults2016import.xls', 
                        sheet = 1, skip = 9)

data_2016 <- data_2016[1:395, c(1, 5:55)] # Filter out unneeded cols & blank rows

colnames(data_2016) <- c('Performance', 'Rioult_Direct', 'Joyce_Direct', 
                         'Any_Print_Ad', 'Times_Ad', 'TimeOut_Ad', 'MetroNY_Ad', 
                         'AMNY_Ad', 'Article_Blog', 'Any_Social_Media', 'Facebook',
                         'Twitter', 'Instagram', 'Personal_Recommendation', 
                         'Other_Source', 'Source_Notes', 'Number_Performances',
                         'Company', 'Specific_Dances', 'Modern_Dance', 
                         'Dance_General', 'Music', 'Joyce', 'Joyce_Years', 
                         'Dancer', 'FriendOrFamily', 'Guest', 'Other_Reason',
                         '92Y_Venue', 'BAM_Venue', 'Dixon_Venue', 'Pillow_Venue',
                         'Judson_Venue', 'Joyce_Venue', 'LincolnCtr_Venue',
                         'CityCtr_Venue', 'NYLiveArts_Venue', 'Triskelion_Venue',
                         'Other_Venue', 'Dance_Company1', 'Dance_Company2',
                         'Dance_Company3', 'Interested_InSchool',
                         'Interested_Children', 'Interested_Pre-Prof',
                         'Interested_Adult', 'Gender', 'Zip_Code', 'Age', 'Ethnicity',
                         'Income', 'Education')

# Modify variable types
data_2016[, c(2:15, 18:23, 25:27, 43:46)] <- 
     lapply(data_2016[, c(2:15, 18:23, 25:27, 43:46)], as_binary)
data_2016$Joyce_Years <- as.numeric(data_2016$Joyce_Years)
data_2016[, c(1, 17, 29:38, 47, 49:52)] <- 
     lapply(data_2016[, c(1, 17, 29:38, 47, 49:52)], as.factor)

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

# Change "Any_" variables so they are true if any subcategory is true
data_2016$Any_Print_Ad = apply(data_2016[, 4:9], 1, sum)
data_2016$Any_Social_Media = apply(data_2016[, 10:13], 1, sum)

# Create Program variable
data_2016$Program = as.factor(substr(data_2016$Performance, 9,9))

# Create text file of unique dance company names for mapping !!!!!!!!!!! Only run when there is a change in the table
# company <- sort(unique(c(data_2016$Dance_Company1, data_2016$Dance_Company2, data_2016$Dance_Company3)))
# write(company, file = './cache/dance_co.txt')

# Remap Dance Company names
map_table <- read_csv('./cache/dance_co_mapping.csv')
data_2016$Dance_Company1_norm <- map_char(data_2016$Dance_Company1, map_table, "Input", "Mapped_To")
data_2016$Dance_Company2_norm <- map_char(data_2016$Dance_Company2, map_table, "Input", "Mapped_To")
data_2016$Dance_Company3_norm <- map_char(data_2016$Dance_Company3, map_table, "Input", "Mapped_To")

# Aggregate data by zip code and map zip codes to state
zip_count_2016 <- data_2016 %>% group_by(region = Zip_Code) %>% summarize(value = n())
zip_count_2016$state <- sapply(zip_count_2016$region, map_state)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Make Graphs

# Demographics by Program
plot_data <-  group_by(data_2016, Program, Gender) %>%
     filter(!is.na(Gender)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(percent = n / sum(n))
ggplot(plot_data, aes(x = Program, y = percent, fill = Gender)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer(type = "qual", palette = 'Set3')

plot_data <-  group_by(data_2016, Program, Income) %>%
     filter(!is.na(Income)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Income)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer("Income", type = "seq", palette = 'YlGnBu', direction = -1)

plot_data <-  group_by(data_2016, Program, Ethnicity) %>%
     filter(!is.na(Ethnicity)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Ethnicity)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer("Gender", type = "qual", palette = 'Set2', direction = -1)

plot_data <-  group_by(data_2016, Program, Education) %>%
     filter(!is.na(Education)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Education)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer("Education", type = "seq", palette = 'YlGnBu', direction = -1)

plot_data <-  group_by(data_2016, Program, Age) %>%
     filter(!is.na(Age)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Age)) + 
     geom_bar(stat = "identity") + 
     scale_fill_brewer("Income", type = "seq", palette = 'YlGnBu', direction = -1)

# Number of Performances by Program
plot_data <-  group_by(data_2016, Program, Number_Performances) %>%
     filter(!is.na(Number_Performances)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Number_Performances)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer("Income", type = "seq", palette = 'YlGnBu', direction = -1)

# Age by Number of Performances
plot_data <-  group_by(data_2016, Number_Performances, Age) %>%
     filter(!is.na(Age), !is.na(Number_Performances)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Number_Performances, y = Percent, fill = Age)) + 
     geom_bar(stat = "identity") +
     scale_fill_brewer("Age", type = "seq", palette = 'YlGnBu', direction = -1)

# Aggregate Source Data
plot_data <- melt(data_2016[, 2:15]) %>% group_by(Source = variable) %>% 
     summarize(Mentions = sum(value))
# Plot Sources
plot1 <- filter(plot_data, as.integer(Source) %in% c(1:3, 8, 9, 13:14))
plot1$Source <- factor(plot1$Source, 
                           levels = plot1$Source[order(plot1$Mentions)])
ggplot(plot1, aes(x = Source, y = Mentions)) + geom_bar(stat = "identity") +
     coord_flip()

# Plot Ad Detail
plot2 <- filter(plot_data, as.integer(Source) %in% 4:7)
ggplot(plot2, aes(x = Source, y = Mentions)) + geom_bar(stat = "identity")

# Plot Social Media Detail
plot3 <- filter(plot_data, as.integer(Source) %in% 10:12)
ggplot(plot3, aes(x = Source, y = Mentions)) + geom_bar(stat = "identity")

# Aggregate and plot Reasons
plot_data <- melt(data_2016[, c(18:23, 25:27)]) %>% group_by(Source = variable) %>% 
     summarize(Mentions = sum(value))
plot_data$Source <- factor(plot_data$Source, 
                           levels = plot_data$Source[order(plot_data$Mentions)])
ggplot(plot_data, aes(x = Source, y = Mentions)) + geom_bar(stat = "identity") +
     coord_flip()

# Plot Company Mentions
plot_data <- sort(table(unlist(data_2016[,54:56])), decreasing = FALSE)
plot_data <- filter(as.data.frame(plot_data), Freq > 7)
names(plot_data) <- c("Company", "Mentions")
ggplot(plot_data, aes(x = Company, y = Mentions)) + geom_bar(stat = "identity") +
     coord_flip()

# Plot Interest in Education
plot_data <- melt(data_2016[, 43:46]) %>% group_by(Type = variable) %>% 
     summarize(Mentions = sum(value))
levels(plot_data$Type) <- c('In_School', 'Children', 'Pre-professional', 'Adult')
ggplot(plot_data, aes(x = Type, y = Mentions)) + geom_bar(stat = "identity")
