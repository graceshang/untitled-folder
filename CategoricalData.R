##
## =============  Categorical Data Analysis ==============
##
## R Tutorial
## Katia Oleinik, 2013
## Scientific Computing and Visualization
## Boston University
##
## =======================================================

# Read CSV file
# Notice that the spaces in the variable names will be changed to periods
dt <- read.csv("Categorical.csv")

# Explore the dataset
names(dt)  # get header names
head(dt)   # View first 6 lines of the file
tail(dt)   # View last 6 lines of the file
str(dt)    # structure of the dataset
dim(dt)    # get dimensions of the loaded data

# Analyze SEX variable
table(dt$sex)
table(dt$sex)/length(dt$sex)  ## proportion list
summary(dt$sex)

barplot(table(dt$sex))
barplot(table(dt$sex)/length(dt$sex))  # see graphics tutorial for an enhanced graphs
pie(table(dt$Math.grade))

# Analyze Math.grade variable
table(dt$Math.grade)
table(dt$Math.grade)/length(dt$Math.grade)  ## proportion list
summary(dt$Math.grade)

## ======= Frequency tables for 2 variables  ==============

# We would like to explore relationship between sex and Math.grade
# Construct a coningency Table
(ctable <- table(dt$sex,dt$Math.grade))

# cell percentages
prop.table(ctable)

# Crosstabulation using formula style
(xtable <- xtabs(~sex+Math.grade, data=dt))
ftable(xtable)

# chi-square test of independence
summary(xtable)


## ======= Frequency tables for 3 variables  ==============

# We would like to explore relationship between sex and Math.grade and Science.Grade
# Construct a coningency Table
ctable <- table(dt$sex, dt$Math.grade, dt$Science.Grade)
ftable(ctable)

# cell percentages
prop.table(ctable)

# Crosstabulation using formula style
(xtable <- xtabs(~sex+Math.grade+Science.Grade, data=dt))
ftable(xtable)

# chi-square test of independence
summary(xtable)

## =======  Use gmodels library for Cross tabulation tables ===
library(gmodels)

