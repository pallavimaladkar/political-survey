
# Ling 460 final project
rm(list = ls())

# library(tidyverse)
library(ggplot2)
# library(stringr)
library(dplyr)
# library(fastDummies)
# library(tidyr)
# library(stargazer)
# library(ggrepel)
setwd("/Users/PallaviMaladkar/Documents/UNC/Classes/2022-2023/LING 460 POLI 209")
data <- read.csv("SurveyData.csv")


data <- data[-c(1:2),]

### HYPOTHESES ###
# 1. We hypothesize that women will be more likely than men to have answers closer to the actual statistic for female representation in Congress.
# 2. We hypothesize that Democrats will be more likely than Republicans to have answers closer to the actual statistic for female representation in Congress.
# 3. We hypothesize that the more politically aware an individual is, the more likely they will be to have answers closer to the actual statistic for female representation in Congress.



# DEPENDENT VARIABLE = difference from statistic
# creating variable that shows distance between the guess and the actual number for female representation in Congress
data$Q16_1 <- as.numeric(data$Q16_1)
data$diff <- abs(data$Q16_1 - 28) # absolute value of the difference between the response and correct answer
summary(data$diff)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   0.000   3.000   7.000   7.882  11.000  51.000       5 
sd(data$diff, na.rm = T) # 6.348453


# INDEPENDENT VARIABLES = female indicator, party indicator, political awareness
# creating a female indicator variable (1 = female, 0 = male)
data$female <- recode(data$Q3,
                      "Male" = 0,
                      "Female" = 1)
sum(data$female == 0, na.rm = T) # 39 men responded
sum(data$female == 1, na.rm = T) # 61 women responded
men <- data$diff[data$female == 0]
women <- data$diff[data$female == 1]
summary(men, na.rm = T)
summary(women, na.rm = T)
sd(men, na.rm = T) # 4.725765
sd(women, na.rm = T) # 4.45199

mean(men, na.rm = T) # 5.789
mean(women, na.rm = T) # 8.525
t.test(men, women, mu = 0)

# Men seem to be more likely than women to have answers closer to the actual statistic for female representation in Congress.
# p-value = 0.005436, so statistically significant, true difference in means is not = 0



# subsetting data into Democrat/Republican
dem_data <- subset(data, Q5 == "Democrat" | Q6 == "Closer to the Democratic Party")
rep_data <- subset(data, Q5 == "Republican" | Q6 == "Closer to the Republican Party")
dem <- dem_data$diff
rep <- rep_data$diff
mean(dem, na.rm = T) # 8.373
mean(rep, na.rm = T) # 6.609
sd(dem, na.rm = T) # 6.927761
sd(rep, na.rm = T) # 4.261233
t.test(dem, rep, mu = 0)
# Republicans seem to be more likely than Democrats to have answers closer to the actual statistic for female representation in Congress.
# however, p-value = 0.1452, so this difference is not statistically significant. possible that more data is necessary to make more concrete conclusions

# creating an indicator variable for Democrat so that it can be used in the regression model
data$isDemocrat <- ifelse(data$Q5 == "Democrat" | data$Q6 == "Closer to the Democratic Party", 1,
                          ifelse(data$Q5 == "Republican" | data$Q6 == "Closer to the Republican Party", 0, NA))


# creating POLITICAL AWARENESS SCORE
# how high you rate yourself gives you a specific number of points from 1-7
data$self_aware <- recode(data$Q10,
                         "Not Aware at All" = 1,
                         "Not Very Aware" = 2,
                         "Somewhat Unaware" = 3,
                         "Moderately Aware" = 4,
                         "Somewhat Aware" = 5,
                         "Aware" = 6,
                         "Very Aware" = 7)

# Here I just made an edit so each correct score is 1
data$q1 <- recode(data$Q12,
                  "Kevin McCarthy" = 1,
                  "Paul Ryan" = 0,
                  "Hakeem Jeffries" = 0,
                  "Nancy Pelosi" = 0)
data$q2 <- recode(data$Q13,
                  "California" = 1,
                  "Illinois" = 0,
                  "New York" = 0,
                  "New Jersey" = 0)
data$q3 <- recode(data$Q14,
                  "Jeannette Rankin" = 1,
                  "Paula Hawkins" = 0,
                  "Dianne Feinstein" = 0,
                  "Barbara A. Mikulski" = 0)

# Here I just create a seperate variable called polknow that includes the three political knowledge questions. 
data$polknow <- data$q1 + data$q2 + data$q3
data$awareness <- data$polknow + data$self_aware

summary(data$self_aware)
sd(data$self_aware, na.rm = T) # 1.389407
summary(data$polknow)
sd(data$polknow, na.rm = T) # 0.8977525
summary(data$awareness)
sd(data$awareness, na.rm = T) # 1.919043


########################
# Seven point Party ID #
########################

# Code Democats as 1, Republicans as 2, and Independents and Don't Know/Other as 3
data$pid <- ifelse(data$Q5 == "Democrat", 1, 
                   ifelse(data$Q5 == "Republican", 2, 
                          ifelse(data$Q5 == "Independent" | data$Q5 == "Don't know/Other", 3, NA)))
# Code Democratic leaners as 1, Republican leaners as 2, and true independents as 3 
data$pidlean <- ifelse(data$Q6 == "Closer to the Democratic Party", 1, 
                       ifelse(data$Q6 == "Closer to the Republican Party", 2, 
                              ifelse(data$Q6 == "I am not closer to either party"  , 3, NA)))

# Code strong partisans as 1 and Not strong partisans as 0 
data$pidstrength <- ifelse(data$Q8 == "Strong ${q://QID5/ChoiceGroup/SelectedChoices}", 1,  
                           ifelse(data$Q8 == "Not very strong ${q://QID5/ChoiceGroup/SelectedChoices}", 0, NA))


#Strong Democrat
data$pid7[data$pid == 1 & data$pidstrength == 1] <- 1
# Not very strong Democrat
data$pid7[data$pid == 1 & data$pidstrength == 0] <- 2
# Lean Democrat
data$pid7[data$pidlean == 1] <- 3
# Pure Independent 
data$pid7[data$pidlean == 3] <- 4
# Lean Republican 
data$pid7[data$pidlean == 2] <- 5
# Not very strong Republican 
data$pid7[data$pid == 2 & data$pidstrength == 0] <- 6
# Strong Republican 
data$pid7[data$pid == 2 & data$pidstrength == 1] <- 7



### Linear Regressions ###

# first regression on polknow variable
plot(data$polknow, data$diff)
reg <- lm(diff ~ polknow, data)
abline(reg)
summary(reg)





# want to remove outliers
cooksd <- cooks.distance(reg)
cooksd
sample_size <- length(data)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")
abline(h = 4/sample_size, col="red") # shows you the cutoff for outliers
outliers <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))]) # picking out outliers - the index is  off because we removed the first 2 rows of the dataframe, so i will remove the outlier by hand
which(data$diff >= 40) # this is the true index of the outlier with the massive difference
outliers <- 82


# Second regression on same polknow variable
data2 <- data[-outliers, ] # removing outliers from data
plot(data2$polknow, data2$diff)
reg2 <- lm(diff ~ polknow, data2)
abline(reg2)
summary(reg2)

# slope on polknow variable is -0.8181, but NOT statistically significant
# p-value is 0.11, which is not in the significant range
# it is telling that the slope is negative, which supports our predicted relationship between the variables
# we had predicted that the more politically aware you were, the more accurate your answer would be, which a negative slope supports


# third regression on self_aware variable
plot(data2$self_aware, data2$diff)
reg3 <- lm(diff ~ self_aware, data2)
abline(reg3)
summary(reg3)

# slope on self_aware variable is -0.7142, and it IS statistically significant at the 0.05 level
# p-value is 0.04094, which is significant at the 0.05 level
# slope is negative, which supports our predicted relationship between the variables
# the more politically aware you are, the more accurate your answer should be

# graph of second regression with line of best fit
plot2 <- ggplot(data = data2, aes(x = polknow, y = diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  xlab("Political Knowledge Score") +
  ylab("Difference from Correct Percentage") +
  theme_bw()
plot2

# graph of third regression with line of best fit
plot3 <- ggplot(data = data2, aes(x = self_aware, y = diff)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = 'lm') +
  xlab("Self Political Awareness Score") +
  ylab("Difference from Correct Percentage") +
  theme_bw()
plot3



# linear regression including ALL variables 
reg4 <- lm(diff ~ polknow + self_aware + female + pid7 + isDemocrat, data2)
summary(reg4)
# the slope that was statistically significant was the slope on the female indicator variable (significant at 0.001 level)
# shows that being female makes you statistically more likely to have an answer that is 3.0272 away from the actual statistic for female representation in Congress. this is NOT what we expected in our first hypothesis.

# polknow had a negative slope, as expected in our third hypothesis. however, the slope is statistically insignificant.
# self_aware had a negative slope, as expected in our third hypothesis. however, the slope is statistically insignificant.
# pid7 had a positive slope, showing that the closer you were to being a "strong republican" (and the further you were from being a "strong democrat"), the more likely you are to be further away from the actual statistic for female representation in Congress. This actually DOES support our second hypothesis, unlike the slope on the isDemocrat variable. however, this slope is also statistically insignificant.
# isDemocrat had a negative slope, not as expected in our second hypothesis. however, the slope is statistically insignificant. 
# the p-value for this regression was 0.007287

# library(stargazer)
# stargazer(reg4, title="Results", align=TRUE)



