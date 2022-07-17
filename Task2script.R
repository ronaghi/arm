#ASDM Assignment Task 2: Association Rules Mining
#Programmed by S. Mehran Ronaghi

#Setting working directory
mypath = 'C:/ASDM/Assignment/Task2'
setwd(mypath)
getwd()

#Importing Greater Manchester Road Safety Dataset
GMacc <- read.csv('gm-accident.csv', header= TRUE, colClasses='factor')

dim(GMacc)
names(GMacc)
head(GMacc)
tail(GMacc)
summary(GMacc)
str(GMacc)

#Cleaning Variable Names
install.packages('janitor')
library(janitor)

GMacc <- GMacc %>% janitor::clean_names()

#Data Wrangling
install.packages('tidyverse')
library(tidyverse)

GMacc <- GMacc %>% rename('year' = "i_accident_year")

names(GMacc)

#Missing Values
install.packages('skimr')
library(skimr)

skim(GMacc)

#Outliers
boxplot(GMacc$accident_severity)

#Normalisation
#install.packages('caret')
#library(caret)

#preproc<- preProcess(GMacc, method = 'range')
#GMacc <- predict(preproc,GMacc)

#summary(GMacc)

#Skew
#plot(density(GMacc$accident_severity))
#abline(v=mean(GMacc$accident_severity), col='red')
#abline(v=median(GMacc$accident_severity), col='blue')

#Installing and activating "arules" package
install.packages('arules')
library(arules)

#Creating frequent itemsets
rules <- apriori(GMacc, parameter= list(minlen= 2, maxlen= 4, supp= 0.0001, conf= 0.95),
                 appearance= list(rhs=c('accident_severity=1'), default='lhs'))

summary(rules)

#inspecting the rules
inspect(rules)

#Visualizing the rules
install.packages('arulesViz')
library(arulesViz)

plot(rules)
plot(rules, method="grouped")
plot(rules@quality)

ruleExplorer(rules)

#Preparing the dataset for SAS
GMacc$ID <- seq.int(nrow(GMacc))
GMacc <- GMacc %>% pivot_longer(!ID, names_to= 'Attributes', values_to= 'Codes')
GMacc$Lables <- paste(GMacc$Attributes,GMacc$Codes,sep="=")
head(GMacc)

write.csv(GMacc, 'gm-accident-SAS.csv', row.names = FALSE)
