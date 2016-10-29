library(tidyr)
library(reshape2)
library(ggplot2)
library(RColorBrewer)

# Standard Color Palettes
pal_class <- scale_fill_brewer(type = "qual", palette = 'Pastel1')
pal_seq <- scale_fill_brewer(type = "seq", palette = 'BuGn')

# Period to Period Comparisons
# Number of Performances
plot_data <-  group_by(data_combined, Year, Number_Performances) %>%
     filter(!is.na(Number_Performances)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Number_Performances, fill = Number_Performances)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Number of RIOULT Performances by Survey Period")

# Gender
plot_data <-  group_by(data_combined, Year, Gender) %>%
     filter(!is.na(Gender)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Gender, fill = Gender)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_class + 
     ggtitle("Gender by Survey Period")

# Age
plot_data <-  group_by(data_combined, Year, Age) %>%
     filter(!is.na(Age)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Age, fill = Age)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Age Distribution by Survey Period")

# Income
plot_data <-  group_by(data_combined, Year, Income) %>%
     filter(!is.na(Income)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Income, fill = Income)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Income by Survey Period")

# Education
plot_data <-  
     group_by(data_combined, Year, Education) %>%
     filter(!is.na(Education)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Education, fill = Education)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Education by Survey Period")

# Ethnicity
plot_data <-  group_by(data_combined, Year, Ethnicity) %>%
     filter(!is.na(Ethnicity)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Year, y = Percent, group = Ethnicity, fill = Ethnicity)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_class +
     ggtitle("Ethnicity by Survey Period")


# Comparisons by Program
# Demographics by Program
plot_data <-  group_by(data_2016, Program, Gender) %>%
     filter(!is.na(Gender)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(percent = n / sum(n))
ggplot(plot_data, aes(x = Program, y = percent, fill = Gender)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_class +
     ggtitle("Gender by Program")

plot_data <-  group_by(data_2016, Program, Income) %>%
     filter(!is.na(Income)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Income)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Income by Program")

plot_data <-  group_by(data_2016, Program, Ethnicity) %>%
     filter(!is.na(Ethnicity)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Ethnicity)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_class +
     ggtitle("Ethnicity by Program")

plot_data <-  group_by(data_2016, Program, Education) %>%
     filter(!is.na(Education)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Education)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Education by Program")

plot_data <-  group_by(data_2016, Program, Age) %>%
     filter(!is.na(Age)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Age)) + 
     geom_bar(stat = "identity", width = 0.5) + 
     pal_seq +
     ggtitle("Age by Program")

# Number of Performances by Program
plot_data <-  group_by(data_2016, Program, Number_Performances) %>%
     filter(!is.na(Number_Performances)) %>%
     summarize(n = n(), na.exclude = TRUE) %>%
     mutate(Percent = 100* n / sum(n))
ggplot(plot_data, aes(x = Program, y = Percent, fill = Number_Performances)) + 
     geom_bar(stat = "identity", width = 0.5) +
     pal_seq +
     ggtitle("Number of RIOULT Performances by Program")


# Advertising, Promotion and Reason for Attending
# Aggregate Source Data
plot_data <- melt(data_2016[, 2:15]) %>% group_by(Source = variable) %>% 
     summarize(Mentions = sum(value))
# Plot Sources
# # Source of Awareness
plot1 <- filter(plot_data, as.integer(Source) %in% c(1:3, 8, 9, 13:14))
plot1$Source <- factor(plot1$Source, 
                       levels = plot1$Source[order(plot1$Mentions)])
ggplot(plot1, aes(x = Source, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Source of Awareness: Number of Mentions")

# Source of Awareness by Company familiarity *****************************
plot_data <- melt(data_2016[, 2:15]) %>% group_by(Source = variable) %>% 
     summarize(Mentions = sum(value))
# Plot Sources
plot1 <- filter(plot_data, as.integer(Source) %in% c(1:3, 8, 9, 13:14))
plot1$Source <- factor(plot1$Source, 
                       levels = plot1$Source[order(plot1$Mentions)])
ggplot(plot1, aes(x = Source, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Source of Awareness: Number of Mentions")

# Plot Ad Detail
plot2 <- filter(plot_data, as.integer(Source) %in% 4:7)
ggplot(plot2, aes(x = Source, y = Mentions)) + 
     geom_bar(stat = "identity", width = 0.5) +
     ggtitle("Source of Awareness: Advertising Detail")

# Plot Social Media Detail
plot3 <- filter(plot_data, as.integer(Source) %in% 10:12)
ggplot(plot3, aes(x = Source, y = Mentions)) + 
     geom_bar(stat = "identity", width = 0.5) +
     ggtitle("Source of Awareness: Social Media Detail")

# Aggregate and plot Reasons
plot_data <- melt(data_2016[, c(18:23, 25:28)]) %>% group_by(Reason = variable) %>% 
     summarize(Mentions = sum(value))
plot_data$Reason <- factor(plot_data$Reason, 
                           levels = plot_data$Reason[order(plot_data$Mentions)])
ggplot(plot_data, aes(x = Reason, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Reason for Attending")

# Dance Companies and Venues
# Dance Company Mentions: Total
plot_data <- sort(table(unlist(data_2016[,54:56])), decreasing = FALSE)
plot_data <- filter(as.data.frame(plot_data), Freq > 7)
names(plot_data) <- c("Company", "Mentions")
ggplot(plot_data, aes(x = Company, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Dance Company Mentions: Total")

# Dance Company Mentions: Exclude First Time Audience
plot_data <- filter(data_2016, Number_Performances != 'First time')
plot_data <- sort(table(unlist(plot_data[,54:56])), decreasing = FALSE)
plot_data <- filter(as.data.frame(plot_data), Freq > 7)
names(plot_data) <- c("Company", "Mentions")
ggplot(plot_data, aes(x = Company, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Dance Company Mentions: Know RIOULT")

# Dance Company Mentions: First Time Only
plot_data <- filter(data_2016, Number_Performances == 'First time')
plot_data <- sort(table(unlist(plot_data[,54:56])), decreasing = FALSE)
plot_data <- filter(as.data.frame(plot_data), Freq > 7)
names(plot_data) <- c("Company", "Mentions")
ggplot(plot_data, aes(x = Company, y = Mentions)) + 
     geom_bar(stat = "identity") +
     coord_flip() +
     ggtitle("Dance Company Mentions: Know RIOULT")

# Dance Venues
plot_data <- gather(data_2016[,29:38]) %>% 
     filter(!is.na(value)) %>%
     group_by(key, value) %>%
     summarize(n())
names(plot_data) <- c("Venue", "Performances", "Mentions")
venue_seq <- group_by(plot_data, Venue) %>%
     summarize(total = sum(Mentions)) %>%
     arrange(total)
plot_data$Venue <- factor(plot_data$Venue, levels = venue_seq$Venue, ordered = TRUE)
plot_data <- plot_data[order(plot_data$Venue, decreasing = FALSE), ]
ggplot(plot_data, aes(x = Venue, y = Mentions, fill = Performances)) + 
     geom_bar(stat = "identity") + 
     coord_flip() +
     pal_seq +
     ggtitle("Dance Venues Attended")

# Interest in Education
plot_data <- melt(data_2016[, 43:46]) %>% group_by(Type = variable) %>% 
     summarize(Mentions = sum(value))
levels(plot_data$Type) <- c('In_School', 'Children', 'Pre-professional', 'Adult')
ggplot(plot_data, aes(x = Type, y = Mentions)) + 
     geom_bar(stat = "identity", width = 0.5) +
     ggtitle("Interest in Dance Programs")