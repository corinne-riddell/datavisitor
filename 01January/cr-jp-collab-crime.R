
crime.dat <- read.csv("/Users/corinneriddell/Documents/repos/datavisitor/01January/cansim.csv")
View(crime.dat)
str(crime.dat)

library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)

dat.clean <- crime.dat %>% filter(Statistics == "Rate per 100,000 population")
dat.clean <- dat.clean %>% gather(year, crime.rate, -Province, -Violations, -Statistics)
dat.clean$crime.rate <- as.numeric(dat.clean$crime.rate)
dat.clean$year <- substr(dat.clean$year, 2, 5)
dat.clean$year <- as.numeric(dat.clean$year)

dat.clean$Violations <- as.character(dat.clean$Violations)
dat.clean$Violations[dat.clean$Violations == "Total violent Criminal Code violations [100]  (16)"]  <- "Violent Crime"
dat.clean$Violations[dat.clean$Violations == "Total other Criminal Code violations [300] "]  <- "Other Crime"
dat.clean$Violations[dat.clean$Violations == "Total property crime violations [200] "]  <- "Property Crime"

dat.clean$Province <- as.character(dat.clean$Province)

dat.clean$Province[dat.clean$Province == "British Columbia (28,41,49)"] <- "British Columbia" 
dat.clean$Province[dat.clean$Province == "Newfoundland and Labrador"] <- "Newfoundland & Labrador" 
dat.clean$Province[dat.clean$Province == "Quebec (50,62)"] <- "Quebec" 
dat.clean$Province[dat.clean$Province == "Northwest Territories (15)"] <- "Northwest Territories"
dat.clean$Province[dat.clean$Province == "Nunavut (15)"] <- "Nunavut" 
dat.clean$Province[dat.clean$Province == "Saskatchewan (11)"] <- "Saskatchewan" 
dat.clean$Province[dat.clean$Province == "Ontario (7,67)"] <- "Ontario" 
dat.clean$Province[dat.clean$Province == "Prince Edward Island (51)"] <- "Prince Edward Island" 

dat.clean$Province2 <- factor(dat.clean$Province, levels = c("British Columbia", "Alberta", "Saskatchewan",
                                                             "Manitoba", "Ontario", "Quebec", "New Brunswick",
                                                             "Nova Scotia", "Prince Edward Island", 
                                                             "Newfoundland & Labrador"))

dat.clean$Violations2 <- factor(dat.clean$Violations, levels = c("Property Crime", "Violent Crime"))

ggplotly(ggplot(data = subset(dat.clean, is.na(Province2) == F & is.na(Violations2) == F), aes(x = year, y = crime.rate)) + geom_line(aes(col = Violations)) +
  facet_grid(Violations ~ Province2, scales = "free_y") + theme_bw() + ylab("Crime Rate (per 100,000 population)") +
  xlab("Year") + theme(legend.position = "none"))

ggplot(data = subset(dat.clean, is.na(Province2) == F & is.na(Violations2) == F), aes(x = year, y = crime.rate)) + geom_line(aes(col = Violations)) +
  facet_grid(Violations ~ Province2, scales = "free_y") + theme_bw() + ylab("Crime Rate (per 100,000 population)") +
  xlab("Year") + theme(legend.position = "none")