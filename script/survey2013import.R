# Analysis of RIOULT audience survey results

require(readxl)
require(stringr)
require(dplyr)
require(ggmap)


# Load and clean data
# Load data from Excel
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


# Combine data with 2016
data_combined <- cbind(Year = "2013", data_2013)
temp_2016 <- cbind(Year = "2016", data_2016)
temp_2016 <- select(temp_2016, c(18, 48:53))
temp_2016 <- cbind(Year = "2016", temp_2016)
data_combined <- rbind(data_combined, temp_2016)

#-----------------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------------
# Period to Period Comparisons
# Number of Performances
plot_data <-  group_by(data_combined, Year, Number_Performances) %>%
     filter(!is.na(Number_Performances)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Number_Performances, color = Number_Performances)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")

# Gender
plot_data <-  group_by(data_combined, Year, Gender) %>%
     filter(!is.na(Gender)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Gender, color = Gender)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")

# Age
plot_data <-  group_by(data_combined, Year, Age) %>%
     filter(!is.na(Age)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Age, color = Age)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")

# Income
plot_data <-  group_by(data_combined, Year, Income) %>%
     filter(!is.na(Income)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Income, color = Income)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")

# Education
plot_data <-  group_by(data_combined, Year, Education) %>%
     filter(!is.na(Education)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Education, color = Education)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")

# Ethnicity
plot_data <-  group_by(data_combined, Year, Ethnicity) %>%
     filter(!is.na(Ethnicity)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Ethnicity, color = Ethnicity)) + 
     geom_line(stat = "identity") + geom_point(stat = "identity")


#-----------------------------------------------------------------------------------------------
# Aggregate data by zip code and map zip codes to state
zip_count_2013 <- data_2013 %>% group_by(region = Zip_Code) %>% summarize(value = n())
zip_count_2013$state <- sapply(zip_count_2013$region, map_state)

# Geocode zip codes
zip_lookup <- data.frame(Zip_Code = unique(data_2013$Zip_Code), lon = NA, lat = NA)
zip_lookup <- filter(zip_lookup, !is.na(zip)) %>% arrange(zip)

# for(i in 1:nrow(zip_lookup)) {
#      zip_lookup[i, 2:3] <- geocode(as.character(zip_lookup[i,1]))
# }

zip_lookup <- readRDS('zip_lookup.RDS')

# Get base map
center <- geocode("Joyce Theater NYC")
metro_map <- get_map(location=center, source = 'google', maptype = 'terrain')
zip_count <- left_join(zip_count, zip_lookup, by = 'Zip_Code')

ggmap(metro_map) +
     geom_point(aes(x=lon, y=lat, color = 'darkred', size = frequency), data = zip_count)

