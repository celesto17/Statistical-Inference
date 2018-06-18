# setwd("F:/01. Data Science/06. Statistical Inference/04. Week 4/01. Course Project")

# PART 2: Basic inferential data analysis
library (ggplot2)
library (dplyr)

# Load ToothGrowth data set
library (datasets)
data (ToothGrowth)

# 1. Some basic exploratory data analyses
head (ToothGrowth)
tail (ToothGrowth)
str (ToothGrowth)

# Sample Size
length (ToothGrowth)
# Number of Rows and Columns
dim (ToothGrowth)

# Boxplot graph of the tooth length vs the does
p <- ggplot (ToothGrowth, aes (x = factor(dose), y = len, fill = factor(dose))) +
  geom_boxplot () +
  facet_grid (.~supp) +
  labs (title = "Length of Tooth vs. Dose by for OJ & VC",
       x = "Doses", y = "Length of Tooth")

print (p)

# 2. Provide a basic summary of the data
summary (ToothGrowth)

table (ToothGrowth$supp, ToothGrowth$dose)

# Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. 
# (Only use the techniques from class, even if there's other approaches worth considering)

# Dose 0.5
doseOJ0.5 <- ToothGrowth %>% filter (supp == "OJ" & dose == "0.5")
doseVC0.5 <- ToothGrowth %>% filter (supp == "VC" & dose == "0.5")
t.test(doseOJ0.5$len,doseVC0.5$len)

# Dose 1
doseOJ1 <- ToothGrowth %>% filter (supp == "OJ" & dose == "0.5")
doseVC1 <- ToothGrowth %>% filter (supp == "VC" & dose == "0.5")
t.test(doseOJ1$len,doseVC1$len)

# Dose 2
doseOJ2 <- ToothGrowth %>% filter (supp == "OJ" & dose == "0.5")
doseVC2 <- ToothGrowth %>% filter (supp == "VC" & dose == "0.5")
t.test(doseOJ2$len,doseVC2$len)

# State your conclusions and the assumptions needed for your conclusions.
print ("At 95% confidence, we can have following conclusions: ")
print ("1. Dose 0.5 of OJ results in longer tooth than dose 0.5 of VC")
print ("2. Dose 1 of OJ results in longer tooth than dose 0.5 of VC")
print ("3. However, Dose 2 of OJ and VC result in almost similar tooth lenght")

