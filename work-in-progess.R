require(tigerstats)
library(readr)
library(tidyverse)
library(dplyr)
library(ggplot2)
TurkishAspect <- read_csv("TurkishAspect.csv")
normVerbs <- read_csv("normVerbs.csv")
#view(TurkishAspect)

str(TurkishAspect)

xtabs(~appropriate, data = TurkishAspect)

xtabs(~periphrasis, data = TurkishAspect)

TurkishAspect %>%
  filter(appropriate == "yes"|appropriate == "no") -> TurkishAspect

TurkishAspect$participant <- substr(TurkishAspect$doc,1,8)

TurkishAspect <- TurkishAspect %>%  mutate(country = 
                                             case_when(
  startsWith(doc, "DE") ~ "Germany",
  startsWith(doc, "De") ~ "Germany",
  startsWith(doc, "US") ~ "USA",
  startsWith(doc, "Us") ~ "USA",
  startsWith(doc, "TU") ~ "Turkiye",
  startsWith(doc, "Tu") ~ "Turkiye"
)) %>% 
  mutate(register = case_when(
    grepl("_f", doc) ~ "formal",
    grepl("_i", doc) ~ "informal",
  ))%>%
  mutate(mode = case_when(
    grepl("_fs", doc) ~ "spoken",
    grepl("_is", doc) ~ "spoken",
    grepl("_fw", doc) ~ "written",
    grepl("_iw", doc) ~ "written",
  ))%>% mutate(
    country = as.factor(country),
    register = as.factor(register),
    mode = as.factor(mode),
    participant = as.factor(participant),
  )

# Task 1: Count raw number of periphrasis cases per country
result <- TurkishAspect %>%
  group_by(country) %>%
  summarise(yes_count = sum(periphrasis == "yes"))

print(result)

result1 <- TurkishAspect %>%
  group_by(participant) %>%
  summarise(yes_count = sum(periphrasis == "yes"))
print(result1)

# Task 2: Visualize raw number of periphrasis cases per country 
ggplot(result, aes(x = country, y = yes_count)) +
  geom_bar(stat = "identity", fill = "pink") +
  labs(title = "Number of periphrasis cases per country", x = "Country", y = "Number of periphrases cases") +
  theme_minimal()


# Task 3: Visualize raw number of periphrasis cases per country and/or register and mode
TurkishAspectPlot <- TurkishAspect %>%
  count(country,periphrasis, register,mode) #%>% filter(periphrasis!='no')
ggplot(TurkishAspectPlot, aes(x = country, y = n, col=periphrasis)) +
  geom_col(fill = "pink") +
  labs(title = "Number of periphrasis cases per country", x = "Country", y = "Number of periphrases cases") +
  theme_minimal()+
  facet_grid(register~mode)



# Desired plot types using ggplot as a function
  # Bar plot (geom_bar, geom_col)
  # Box plot (geom_boxplot)

# Do Task 3 by participant, not by mode and register




# Task
# Task X: Normalize the results for Task 1 and 2 by a variable of your choice
# Define regex pattern:
beginning <- 'salt.*[(0-9)]/'
ending <- '#.*'

# normalization verbs
NormVerbs <- normVerbs %>% # load verbs for normalizazion
  rename(doc = "1_id") %>% # rename variables for clearer names
  mutate(doc = str_replace_all(doc, beginning, '')) %>%
  mutate(doc = str_replace_all(doc, ending, '')) %>% 
  mutate(participant = str_replace_all(doc, '_.*', '')) %>%
  mutate(country = case_when(
    startsWith(doc, "DE") ~ "Germany",
    startsWith(doc, "De") ~ "Germany",
    startsWith(doc, "US") ~ "USA",
    startsWith(doc, "Us") ~ "USA",
    startsWith(doc, "TU") ~ "Turkiye",
    startsWith(doc, "Tu") ~ "Turkiye",
    startsWith(doc, "RU") ~ "Russia",
    startsWith(doc, "Ru") ~ "Russia",
    startsWith(doc, "GR") ~ "Greece",
    startsWith(doc, "Gr") ~ "Greece",
  ))%>%
  mutate(register = case_when(
    grepl("_f", doc) ~ "formal",
    grepl("_i", doc) ~ "informal",
  ))%>%
  mutate(mode = case_when(
    grepl("_fs", doc) ~ "spoken",
    grepl("_is", doc) ~ "spoken",
    grepl("_fw", doc) ~ "written",
    grepl("_iw", doc) ~ "written",
  ))%>%
  mutate(Language = case_when(
    endsWith(participant, "G") ~ "Greek",
    endsWith(participant, "R") ~ "Russian",
    endsWith(participant, "T") ~ "Turkish",
    endsWith(participant, "D") ~ "German"
  ))%>% mutate(
    country = as.factor(country),
    register = as.factor(register),
    mode = as.factor(mode),
    participant = as.factor(participant),
  ) %>% filter(Language == "Turkish")
TurkishNorm <- NormVerbs %>% count(country)

# Task 3b: Visualize raw number of periphrasis cases per country and/or register and mode
#and normalize
TurkishAspectPlot <- TurkishAspect %>%
  count(country , periphrasis) %>% filter(periphrasis!='no')
TurkishAspectPlot$n <- TurkishAspectPlot$n / TurkishNorm$n * 100
ggplot(TurkishAspectPlot, aes(x = country, y = n, col=periphrasis)) +
  geom_col(fill = "pink") +
  labs(title = "Number of periphrasis cases per country", x = "Country", y = "Number of periphrases cases") +
  theme_minimal()

# Second try Task 3 participant

TurkishAspectPlot <- TurkishAspect %>%
  count(country,periphrasis, participant) #%>% filter(periphrasis!='no')
ggplot(TurkishAspectPlot, aes(x = country, y = n, col=periphrasis)) +
  geom_col(fill = "pink") +
  labs(title = "Number of periphrasis cases per country", x = "Country", y = "Number of periphrases cases") +
  theme_minimal()


  