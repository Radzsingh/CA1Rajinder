# Load dataset on dataframe

covid_dataset = read.csv("covid.csv", na = "")
head(covid_dataset)
str(covid_dataset)


complete.cases(covid_dataset)

pairs(covid_dataset, labels = colnames(covid_dataset), main = "SARS Covid-19 dataset correlation plot")
na_records <- covid_dataset[!complete.cases(covid_dataset),]
nrow(na_records)
library(mice)
# 15 records have missing NI address
# 10 have missing type
md.pattern(covid_dataset)

library(VIM)

# Looks like the missing values are all in this 1 column
# for "Leave". We're not going to use it for now
# so we'll leave it as it is
missing_values <- aggr(covid_dataset, prop = FALSE, numbers = TRUE)

#Hypothesis 1
# H0 Vaccination has no effect on icu occupancies
# H1 vaccination affect the number of icu occupancies

us_covid = subset(covid_dataset, iso_code == "USA", select = c("icu_patients", "people_fully_vaccinated"))
us_covid
md.pattern(us_covid)
summary(us_covid)

pairs(us_covid, labels = colnames(us_covid), main = "SARS Covid-19 dataset correlation plot")

library(psych)
pairs.panels(us_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals


us_covid$people_fully_vaccinated[is.na(us_covid$people_fully_vaccinated)] <- 0
us_covid

na.omit(us_covid)
us_covid <- na.omit(us_covid)
us_covid

pairs.panels(us_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
md.pattern(us_covid)


# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(us_covid)
plot(icu_patients, people_fully_vaccinated, pch = 19, col = "lightblue")

histogram(~icu_patients | people_fully_vaccinated, 
          data = us_covid, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(icu_patients, people_fully_vaccinated, median)


# check normality of data

qqnorm(icu_patients)
# this line represents normal distribution
qqline(icu_patients, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(us_covid$icu_patients)
normality_test$p.value


hist(icu_patients)

# not normally distributed

cor.test(us_covid$people_fully_vaccinated, us_covid$icu_patients,  method = "spearman")

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypthesis
# There is a strong negative corlation between icu pateints and people who are fully vaccinated

detach(us_covid)



#Hypothesis 2
# H0 Positivity rate is not affected by population desnity
# H1 Positivity rate is affected by population desnity

positive_dataset = subset(covid_dataset, !is.na(covid_dataset$population_density), select = c("total_cases_per_million", "population_density"))
positive_dataset

na_records <- covid_dataset[!complete.cases(positive_dataset),]
nrow(na_records)
library(mice)
# 15 records have missing NI address
# 10 have missing type
md.pattern(positive_dataset)

library(VIM)

# Looks like the missing values are all in this 1 column
# for "Leave". We're not going to use it for now
# so we'll leave it as it is
missing_values <- aggr(positive_dataset, prop = FALSE, numbers = TRUE)
summary(positive_dataset)

#create a categorical column to determine country is with high desnity or low
positive_dataset$Density[positive_dataset$population_density >= 1101] <- "Extreme"
positive_dataset$Density[positive_dataset$population_density >= 701 & positive_dataset$population_density <= 1100] <- "High"
positive_dataset$Density[positive_dataset$population_density >= 101 & positive_dataset$population_density <= 700] <- "Average"
positive_dataset$Density[positive_dataset$population_density <= 100] <- "Low"
positive_dataset
summary(positive_dataset)


density <- factor(positive_dataset$Density, order = TRUE, levels = c("Low", "Average", "High", "Extreme"))
positive_dataset$Density <- density
str(positive_dataset)

positive_dataset <- na.omit(positive_dataset)
positive_dataset

#dataset is ready

#normality test

# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(positive_dataset)
plot(Density, total_cases_per_million, pch = 19, col = "lightblue")

histogram(~total_cases_per_million | Density, 
          data = positive_dataset, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(total_cases_per_million, Density, median)


# check normality of data

qqnorm(total_cases_per_million)
# this line represents normal distribution
qqline(total_cases_per_million, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
library(dplyr)
sample_n(positive_dataset, 5)
temp <- sample_n(positive_dataset, 5000)
temp
normality_test <- shapiro.test(temp$total_cases_per_million)
normality_test$p.value


hist(total_cases_per_million)

# not normally distributed
# After consulting the chart, I am examining
# a dependent continuous variable (temp)
# with an independent categorical variable (activity)
# so I use the Mann-Whitney test
# this is also known as the "Kruskal-Wallis test"
# Format = wilcox.test(dependent~independent)
kruskal.test(total_cases_per_million ~ Density, data = positive_dataset)

# P value is smaller than 0.05 thus we can reject the null hypothesis



######################################################################################################
# Hypothesis 3
#H0 New cases is not affected by stringency_index
#H1 new cases is affected by stringency_index

india_covid = subset(covid_dataset, iso_code == "IND", select = c("new_cases", "stringency_index"))
india_covid
md.pattern(india_covid)
summary(india_covid)

pairs(india_covid, labels = colnames(india_covid), main = "SARS Covid-19 dataset correlation plot")

library(psych)
pairs.panels(india_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



na.omit(india_covid)
india_covid <- na.omit(india_covid)
india_covid

pairs.panels(india_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
md.pattern(india_covid)


# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(india_covid)
plot(new_cases, stringency_index, pch = 19, col = "lightblue")

histogram(~new_cases | stringency_index, 
          data = india_covid, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(new_cases, stringency_index, median)


# check normality of data

qqnorm(new_cases)
# this line represents normal distribution
qqline(new_cases, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
normality_test <- shapiro.test(india_covid$new_cases)
normality_test$p.value


hist(new_cases)

# not normally distributed

cor.test(india_covid$stringency_index, india_covid$new_cases,  method = "spearman")

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypothesis
# There is a strong negative corlation between icu pateints and people who are fully vaccinated

detach(india_covid)

##################################################################################################3
#Hypothesis 4
# H0:Countries with high GDP has nothing to do with number of tests
# H1: Countries with GDP affect the testing
age_covid = subset(covid_dataset,  !(is.na(covid_dataset$median_age)), select = c("hosp_patients", "median_age"))
age_covid
md.pattern(age_covid)
summary(age_covid)

attach(age_covid)
age_covid$AgeCat[median_age >= 36] <- "Elder"
age_covid$AgeCat[median_age >= 21 & age_covid$median_age <= 35] <- "Middle Aged"
age_covid$AgeCat[median_age <= 20] <- "Young"

age_cat <- factor(age_covid$AgeCat, order = TRUE, levels = c("Young", "Middle Aged", "Elder"))
age_covid$AgeCat <- age_cat
str(age_covid)

age_covid <- na.omit(age_covid)
age_covid
pairs(age_covid, labels = colnames(age_covid), main = "SARS Covid-19 dataset correlation plot")

library(psych)
pairs.panels(age_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals




# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")

plot(hosp_patients, AgeCat, pch = 19, col = "lightblue")

histogram(~hosp_patients | AgeCat, 
          data = age_covid, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(hosp_patients, AgeCat, median)


# check normality of data

qqnorm(hosp_patients)
# this line represents normal distribution
qqline(hosp_patients, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
temp <- sample_n(age_covid, 5000)
normality_test <- shapiro.test(temp$hosp_patients)
normality_test$p.value


hist(hosp_patients)

# not normally distributed

kruskal.test(hosp_patients ~ AgeCat, data = age_covid)

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypothesis
# There is a strong negative corelation between icu pateints and people who are fully vaccinated

detach(age_covid)


# Hypothesis 5

#H0: Handwashing facilities has less covid infections
#H1: Handwashing facilites channelise covid infections

hand_wash_covid <- subset(covid_dataset, !is.na(covid_dataset$handwashing_facilities),select = c("handwashing_facilities", "total_cases"))
hand_wash_covid
md.pattern(hand_wash_covid)
summary(hand_wash_covid)

pairs(hand_wash_covid, labels = colnames(hand_wash_covid), main = "SARS Covid-19 dataset correlation plot")

library(psych)
pairs.panels(hand_wash_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence intervals



na.omit(hand_wash_covid)
hand_wash_covid <- na.omit(hand_wash_covid)
hand_wash_covid

pairs.panels(hand_wash_covid,
             smooth = TRUE,      # If TRUE, draws loess smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = TRUE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the LOESS (smoothed) fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)     
md.pattern(hand_wash_covid)


# Icu patients is a continous variable and so is people_fully_vaccinated(interval)
library("lattice")
attach(hand_wash_covid)
plot(total_cases, handwashing_facilities, pch = 19, col = "lightblue")

histogram(~total_cases | handwashing_facilities, 
          data = hand_wash_covid, 
          main = "Distribution of beaver activity data", 
          xlab = "Temperature (degrees)", 
          ylab = "Activity %")
tapply(total_cases, handwashing_facilities, median)


# check normality of data

qqnorm(total_cases)
# this line represents normal distribution
qqline(total_cases, col = "red")


# Formal test of normality
# provided through widely used Shapiro-Wilks test
temp <- sample_n(hand_wash_covid, 5000)
normality_test <- shapiro.test(hand_wash_covid$total_cases)
normality_test$p.value


hist(total_cases)

# not normally distributed

cor.test(handwashing_facilities, total_cases,  method = "spearman")

# Spearman’s Correlation Coefficient
#since p value is smaller than cuto 0.05 thus we can reject the null hypthesis
# There is a strong negative corlation between icu pateints and people who are fully vaccinated

detach(hand_wash_covid)





