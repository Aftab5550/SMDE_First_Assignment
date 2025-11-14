install.packages("DescTools")
install.packages("lmtest")
install.packages("agricolae")
library(DescTools)
library(car)
library(lmtest)
library(agricolae)

##################### QUESTION1 #####################
#a
data <- read.csv("Tech_Use_Stress_Wellness.csv", stringsAsFactors=TRUE)
summary(data)

#b
data$age_cat <- cut(
  data$age,
  breaks = c(15, 28, 44, 60, 79),
  labels = c("Gen Z", "Gen Y", "Gen X", "Baby Boomers"),
  right = TRUE,
  include.lowest = TRUE
)

data$stress_cat <- cut(
  data$stress_level,
  breaks = c(1, 5, 10),
  labels = c("Low", "High"),
  right = TRUE,
  include.lowest = TRUE
)

summary(data$age_cat)
summary(data$stress_cat)

#c
## location - age generation
loc_age <- table(data$location_type, data$age_cat)
loc_age

### Joint Probabilities
jp_loc_age <- prop.table(loc_age)
jp_loc_age

### Marginal Probabilities
row_marg_loc_age <- margin.table(jp_loc_age, 1)
col_marg_loc_age <- margin.table(jp_loc_age, 2)
row_marg_loc_age
col_marg_loc_age

### Conditional Probabilities
sweep(jp_loc_age, 1, row_marg_loc_age, '/')
sweep(jp_loc_age, 2, col_marg_loc_age, '/')

### Bar Plot
barplot(loc_age,
        beside = TRUE,
        # legend = TRUE,
        main = "Distribution of Age Generations by Location",
        xlab = "Location",
        ylab = "Count")

### Using Chi-Squared because of categorical variables
chisq.test(loc_age)
### We have no evidence to reject the null hypothesis that is "the variables location_type and age_cat are independent"

## stress level - age generation
stress_age <- table(data$stress_cat, data$age_cat)
stress_age

### Joint Probabilities
jp_stress_age <- prop.table(stress_age)
jp_stress_age

### Marginal Probabilities
row_marg_stress_age <- margin.table(jp_stress_age, 1)
col_marg_stress_age <- margin.table(jp_stress_age, 2)
row_marg_stress_age
col_marg_stress_age

### Conditional Probabilities
sweep(jp_stress_age, 1, row_marg_stress_age, '/')
sweep(jp_stress_age, 2, col_marg_stress_age, '/')

### Bar Plot
barplot(stress_age,
        beside = TRUE,
        #legend = TRUE,
        main = "Distribution of Age Generations by Stress level",
        xlab = "Stress",
        ylab = "Count")

### Using Chi-Squared because of categorical variables
chisq.test(stress_age)
### With high probability, we reject the null hypothesis, that means it shows a statistically significant association between stress level and age generation. From the barplot, we can see that Younger generations (especially Gen Z) report a much higher proportion of high stress compared to older generations.

#d
numeric_vars <- data[sapply(data, is.numeric)]
# Dropping the variables "user_id", "sleep_quality" and "stress_level"
numeric_vars <- numeric_vars[ , !(names(numeric_vars) %in% c("user_id"))]

## For each variable, we plot the histogram and apply Shapiro test
for (var in names(numeric_vars)) {
  hist(numeric_vars[[var]], main=var)
  print(var)
  print(shapiro.test(numeric_vars[[var]]))
}
## Interpretation:
## Histogram: "age" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "daily_screen_time_hours" seems to follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value = 1.877e-12)
## Histogram: "phone_usage_hours" seems to follow a normal distribution/slightly right skewed; Shapiro: Rejects the null hypothesis (p-value = 3.068e-15)
## Histogram: "laptop_usage_hours" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "tablet_usage_hours" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "tv_usage_hours" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "social_media_hours" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "work_related_hours" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "entertainment_hours" seems to follow a normal distribution/slightly right skewed; Shapiro: Rejects the null hypothesis (p-value = 1.144e-11)
## Histogram: "gaming_hours" seems to follow a normal distribution(*); Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "sleep_duration_hours" seems to follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value = 3.413e-09)
## Histogram: "mood_rating" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "physical_activity_hours_per_week" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "mental_health_score" seems to follow a normal distribution(*); Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "caffeine_intake_mg_per_day" seems to follow a normal distribution; Shapiro: No evidence to reject the null hypothesis (p-value = 0.1083)
## Histogram: "weekly_anxiety_score" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "weekly_depression_score" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "mindfulness_minutes_per_day" doesn't follow a normal distribution/seems to be right skewed; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)

#e
group_low <- data$mental_health_score[data$stress_cat=="Low"]
group_high <- data$mental_health_score[data$stress_cat=="High"]

## Density curves
plot(density(group_low), main="Density curves of 2 Groups", xlim=c(20,100))
lines(density(group_high), col=2)

## Checking Assumptions
### Assumption 1 (Testing Normality)
#### Histogram
hist(group_low, main="mental_health_score_low")
hist(group_high, main="mental_health_score_high")
#### The interpretation from the histogram shows that even though mental_health_score doesn't follow a Normal distribution on its own, it follows a Normal distribution when seen for the subgroups of Low and High stress level

#### Shapiro test
shapiro.test(group_low)
shapiro.test(group_high)
#### Shapiro test rejects the null hypothesis in both of the cases
#### Conclusion: Even though Shapiro test rejects the null hypothesis, we see that the Assumption 1 fulfills with the help of histograms

### Assumption 2 (Homogeneity of variances)
#### F test
var.test(data$mental_health_score ~ data$stress_cat, data=data)
#### We have no evidence to reject the null hypothesis, i-e the Assumption 2 fulfills 

#### Confidence Intervals
VarCI(data$mental_health_score)
VarCI(group_low)
VarCI(group_high)

## T-test
t.test(data$mental_health_score ~ data$stress_cat, var.equal=TRUE)
## We reject the null hypothesis (that means their means are not equal)

## Box Plot
plot(data$mental_health_score ~ data$stress_cat)

##################### QUESTION2 #####################
#a
summary(data$sleep_quality)

data$sleep_cat <- cut(
  data$sleep_quality,
  breaks = c(1, 3, 4, 5),
  labels = c("Low", "Medium", "High"),
  right = TRUE,
  include.lowest = TRUE
)

summary(data$sleep_cat)

#b

## “daily screen time hours”, “mental health score” and “phone usage” for each age generation groups
Boxplot(data$daily_screen_time_hours ~ data$age_cat, id=FALSE, col=2:(nlevels(data$age_cat)+1))
### We can observe that the mean for Gen Y, Gen X and Baby Boomers is visually equal, but the mean for Gen Z is different (slightly higher than the others)
Boxplot(data$mental_health_score ~ data$age_cat, id=FALSE, col=2:(nlevels(data$age_cat)+1))
### We can observe that the mean for Gen Y, Gen X and Baby Boomers is approximately equal, but the mean for Gen Z is different (lower than the others)
Boxplot(data$phone_usage_hours ~ data$age_cat, id=FALSE, col=2:(nlevels(data$age_cat)+1))
### We can observe that the mean for Gen Z, Gen Y, Gen X and Baby Boomers is visually equal. 

## “daily screen time hours”, “mental health score” and “phone usage” for each sleep quality categories
Boxplot(data$daily_screen_time_hours ~ data$sleep_cat, id=FALSE, col=2:(nlevels(data$sleep_cat)+1))
### We can observe that all the 3 groups (Low, Medium and High) have different means: Low > Medium > High
Boxplot(data$mental_health_score ~ data$sleep_cat, id=FALSE, col=2:(nlevels(data$sleep_cat)+1))
### We can observe that all the 3 groups (Low, Medium and High) have different means: Low < Medium < High
Boxplot(data$phone_usage_hours ~ data$sleep_cat, id=FALSE, col=2:(nlevels(data$sleep_cat)+1))
### We can observe that all the 3 groups (Low, Medium and High) have slightly different means: Low > Medium > High

#c
## Normality of quantitative variables
### Histograms
hist(data$phone_usage_hours)
hist(data$daily_screen_time_hours)

### Test of Normality
shapiro.test(data$phone_usage_hours)
shapiro.test(data$daily_screen_time_hours)

### Both seem to follow a normal distribution, even though shapiro test rejects the null hypothesis

## First Model (phone_usage_hours ~ age_cat)
model1 <- aov(phone_usage_hours ~ age_cat, data=data)
summary(model1)
## p-value = 0.298, so we don't have evidence to reject the null hypothesis that is "the mean of phone_usage_hours is EQUAL for the 4 groups (Gen Z, Gen Y, Gen X and Baby Boomers)
## As previously concluded by seeing the histogram, they have visually same mean

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model1, alternative ="two.sided")
#### Durbin Watson: p-value = 0.7039, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model1))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable phone_usage_hours seems to follow a Normal Distribution
qqnorm(residuals(model1))
qqline(residuals(model1), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(model1))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(model1)
#### Breusch Pagan: p-value = 0.8905, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills
leveneTest(phone_usage_hours ~ age_cat, data=data)
#### Levene's Test: p-value = 0.9121, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### Conclusion: All the 3 Assumptions are fulfilled for the First Model and we don't have evidence to reject the null hypothesis that is "the mean of phone_usage_hours is EQUAL for the 4 groups (Gen Z, Gen Y, Gen X and Baby Boomers). We also confirmed its true by looking at the Boxplot that showed that they have the same mean

## Second Model (phone_usage_hours ~ sleep_cat)
model2 <- aov(phone_usage_hours ~ sleep_cat, data=data)
summary(model2)
## p-value < 2e-16, so we reject the null hypothesis, that means at least one group (Low, Medium or High) has a different mean from the rest 

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model2, alternative ="two.sided")
#### Durbin Watson: p-value = 0.8562, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model2))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable phone_usage_hours seems to follow a Normal Distribution
qqnorm(residuals(model2))
qqline(residuals(model2), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(model2))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(model2)
#### Breusch Pagan: p-value = 0.07419, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills
leveneTest(phone_usage_hours ~ sleep_cat, data=data)
#### Levene's Test: p-value = 0.3958, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### All the 3 Assumptions are fulfilled for the Second Model

### Post hoc tests
#### First Test (Tukey's HSD)
TukeyHSD(model2)
#### From the results, we conclude that all 3 groups have statistically significant difference because (Firstly, 0 is not included in the confidence interval of 95%. And secondly, p-value for all of them is 0). We can also compare the results obtained, to what we saw in section b (Boxplot) and see that the results match: Low > Medium > High

#### Second Test (Pairwise t-test)
with(data,
     {
       pairwise.t.test(phone_usage_hours, sleep_cat, p.adj="none")
     })
#### We see the same results, the pairwise t-tests show that all 3 groups have statistically significant difference because all of the p-values are < 2e-16

#### Second Test (Pairwise t-test, with Bonferroni Correction)
with(data,
     {
       pairwise.t.test(phone_usage_hours, sleep_cat, p.adj="bonf")
     })
#### Even after applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as before (all of the p-values are < 2e-16)

#### Third Test (LSD Test)
LSD <- LSD.test(model2, "sleep_cat")
LSD
#### After applying the LSD test, we observe that none of the 3 groups share any alphabet. That means that they are all significantly different and from the values, we see the same results: Low > Medium > High 

#### Third Test (LSD Test, with Bonferroni Correction)
LSD <- LSD.test(model2, "sleep_cat", p.adj = "bonferroni")
LSD
#### Even after applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as before: None of the 3 groups share any alphabet and Low > Medium > High

#### Conclusion: All 3 groups have statistically significant different mean. So for the second model, we reject the null hypothesis and accept the alternative hypothesis of "mean_Low != mean_Medium != mean_High" (more in concrete "mean_Low > mean_Medium > mean_High")

## Third Model (daily_screen_time_hours ~ age_cat)
model3 <- aov(daily_screen_time_hours ~ age_cat, data=data)
summary(model3)
## p-value < 2e-16, so we reject the null hypothesis, that means at least one group (Gen Z, Gen Y, Gen X and Baby Boomers) has a different mean from the rest

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model3, alternative ="two.sided")
#### Durbin Watson: p-value = 0.3474, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model3))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable daily_screen_time_hours seems to follow a Normal Distribution
qqnorm(residuals(model3))
qqline(residuals(model3), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(model3))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(model3)
#### Breusch Pagan: p-value = 0.4146, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills
leveneTest(daily_screen_time_hours ~ age_cat, data=data)
#### Levene's Test: p-value = 0.4423, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### All the 3 Assumptions are fulfilled for the Third Model

### Post hoc tests
#### First Test (Tukey's HSD)
TukeyHSD(model3)
#### From the results, we conclude that Gen Z has a statistically significant different (higher) mean compared to all the other groups (Gen Y, Gen X, Baby Boomers) because (Firstly, 0 is not included in the confidence interval of 95%. And secondly, p-value for all of them is 0). This was already seen in section b when we saw the Boxplot.  
#### And also Gen Y has a statistically significant different (higher) mean than Baby Boomers because (Firstly, 0 is not included in the confidence interval of 95%. And secondly, the p-value is below 0.05). This was not really visible in the Boxplot interpretation in section b 

#### Second Test (Pairwise t-test)
with(data,
     {
       pairwise.t.test(daily_screen_time_hours, age_cat, p.adj="none")
     })
#### The pairwise t-tests show that all groups have statistically significant different mean (because all of the p-values are < 0.05), except the pair Baby Boomers and Gen X (whose p-value is 0.3316)

#### Second Test (Pairwise t-test, with Bonferroni Correction)
with(data,
     {
       pairwise.t.test(daily_screen_time_hours, age_cat, p.adj="bonf")
     })
#### After applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as Tukey's HSD: 
#### - Gen Z has a statistically significant different mean compared to all the other groups (Gen Y, Gen X, Baby Boomers) because all of the p-values are < 2e-16
#### - Gen Y and Baby Boomers have statistically significant different means because the p-value = 0.011 (which is < 0.05)

#### Third Test (LSD Test)
LSD <- LSD.test(model3, "age_cat")
LSD
#### After applying the LSD test, we observe that Gen X and Baby Boomers have the same alphabet, meanwhile Gen Z and Gen Y have different alphabets than the rest. This means that Gen Z and Gen Y are significantly different from the rest (and from each other too because they have different alphabets). Meanwhile the difference of mean between Gen X and Baby Bloomers is not significant

#### Third Test (LSD Test, with Bonferroni Correction)
LSD <- LSD.test(model3, "age_cat", p.adj = "bonferroni")
LSD
#### After applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as Tukey's HSD:
#### - First of all, Gen Z has a significantly different mean (higher) than the rest because it doesn't share the alphabet with any of the other groups
#### - Secondly, Gen Y is significantly different (higher) from Baby Boomers

#### Conclusion: Gen Z has a statistically significant different (higher) mean compared to all the other groups (Gen Y, Gen X, Baby Boomers). And Gen Y has a statistically significant different (higher) mean than Baby Boomers. 
#### So for the third model, we reject the null hypothesis and accept the alternative hypothesis of "mean_GenZ != mean_GenY, mean_GenZ != mean_GenX, mean_GenZ != mean_BabyBoomers and mean_GenY != mean_BabyBoomers"

## Fourth Model (daily_screen_time_hours ~ sleep_cat)
model4 <- aov(daily_screen_time_hours ~ sleep_cat, data=data)
summary(model4)
## p-value < 2e-16, so we reject the null hypothesis, that means at least one group (Low, Medium or High) has a different mean from the rest

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model4, alternative ="two.sided")
#### Durbin Watson: p-value = 0.7082, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model4))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable daily_screen_time_hours seems to follow a Normal Distribution
qqnorm(residuals(model4))
qqline(residuals(model4), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(model4))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(model4)
#### Breusch Pagan: p-value = 1.81e-05
leveneTest(daily_screen_time_hours ~ sleep_cat, data=data)
#### Levene's Test: p-value = 0.0009863
#### Even though Breusch Pagan and Levene's Test give p-values < 0.05, visually the points are roughly scattered around the whole figure in a rectangular shape, i-e Assumption 3 fulfills

#### All the 3 Assumptions are fulfilled for the Fourth Model

### Post hoc tests
#### First Test (Tukey's HSD)
TukeyHSD(model4)
#### From the results, we conclude that all 3 groups have statistically significant difference because (Firstly, 0 is not included in the confidence interval of 95%. And secondly, p-value for all of them is 0). We can also compare the results obtained, to what we saw in section b (Boxplot) and see that the results match: Low > Medium > High

#### Second Test (Pairwise t-test)
with(data,
     {
       pairwise.t.test(daily_screen_time_hours, sleep_cat, p.adj="none")
     })
#### We see the same results, the pairwise t-tests show that all 3 groups have statistically significant difference because all of the p-values are < 2e-16

#### Second Test (Pairwise t-test, with Bonferroni Correction)
with(data,
     {
       pairwise.t.test(daily_screen_time_hours, sleep_cat, p.adj="bonf")
     })
#### Even after applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as before (all of the p-values are < 2e-16)

#### Third Test (LSD Test)
LSD <- LSD.test(model4, "sleep_cat")
LSD
#### After applying the LSD test, we observe that none of the 3 groups share any alphabet. That means that they are all significantly different and from the values, we see the same results: Low > Medium > High 

#### Third Test (LSD Test, with Bonferroni Correction)
LSD <- LSD.test(model4, "sleep_cat", p.adj = "bonferroni")
LSD
#### Even after applying the Bonferroni Correction (to reduce the Type I error), we obtain the same results as before: None of the 3 groups share any alphabet and Low > Medium > High

#### Conclusion: All 3 groups have statistically significant different mean. So for the second model, we reject the null hypothesis and accept the alternative hypothesis of "mean_Low != mean_Medium != mean_High" (more in concrete "mean_Low > mean_Medium > mean_High")

#d
## Fifth Model (daily_screen_time_hours ~ sleep_cat*age_cat)
model5 <- aov(daily_screen_time_hours ~ sleep_cat*age_cat, data=data)
summary(model5)
## These findings suggest that while both age and sleep quality independently influence daily screen time, their combined effect does not differ significantly across groups. Because p-value for both sleep_cat and age_cat is < 2e-16, but for the combined effect the p-value = 0.843

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model5, alternative ="two.sided")
#### Durbin Watson: p-value = 0.7153, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model5))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable daily_screen_time_hours seems to follow a Normal Distribution
qqnorm(residuals(model5))
qqline(residuals(model5), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(model5))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(model5)
#### Breusch Pagan: p-value = 0.05687, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills
leveneTest(daily_screen_time_hours ~ sleep_cat*age_cat, data=data)
#### Levene's Test: p-value = 0.2265, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### All the 3 Assumptions are fulfilled for the Third Model

### Post hoc tests

#### ........................................................ ####

#### Interaction plot
interaction.plot(data$sleep_cat, data$age_cat, data$daily_screen_time_hours, fun=mean, type="l", legend=TRUE, col = 1:4)
#### Conclusion: The interaction of the two term is not significant 

##################### QUESTION3 #####################
#a
predictors <- numeric_vars[, !(names(numeric_vars) %in% "mental_health_score")]
## Remove 'mental_health_score' from numeric_vars
for (var in names(predictors)) {
  plot(predictors[[var]], numeric_vars$mental_health_score,
       xlab = var,
       ylab = "Mental Health Score",
       main = paste("Mental Health Score vs", var))
}
## Visually see the relation between the mental_health_score and other variables

cor(numeric_vars)[14,]
correlations <- cor(numeric_vars, use = "complete.obs")
mh_cor <- correlations["mental_health_score", ]
mh_cor_sorted <- sort(mh_cor, decreasing = TRUE)
mh_cor_sorted
cor_table <- data.frame(
  Variable = names(mh_cor_sorted),
  Correlation = mh_cor_sorted
)
print(cor_table, row.names = FALSE)
## The variable that is most correlated to mental_health_score is social_media_hours
cor.test(data$social_media_hours, data$mental_health_score)
plot(data$social_media_hours, data$mental_health_score)
scatterplot(data$social_media_hours, data$mental_health_score, smooth = FALSE)

## First Regression Model (mental_health_score ~ social_media_hours)
reg1 <- lm(mental_health_score ~ social_media_hours, data=data) 
summary(reg1)
## We reject the null hypothesis because the p-value for social_media_hours is < 2e-16, i-e The slope is != 0

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg1, alternative = "two.sided")
#### Durbin Watson: p-value = 0.5871, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg1))
#### Shapiro Test: p-value = 0.2103, so we don't have evidence to reject the null hypothesis, i-e Assumption 2 fulfills
qqnorm(residuals(reg1))
qqline(residuals(reg1), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg1))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg1)
#### Breusch Pagan: p-value = 0.249, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### All the 3 Assumptions are fulfilled for the First Regression Model

### Confidence interval for the response variable ###
y1 <- predict(reg1, interval = "confidence")
y1

### Need to interpret ..............................................

#b
## Second Regression Model (mental_health_score ~ social_media_hours+mood_rating)
reg2 <- lm(mental_health_score ~ social_media_hours+mood_rating, data=data) 
summary(reg2)
## We reject the null hypothesis because the p-values for social_media_hours and mood_rating are < 2e-16, i-e The slope is != 0

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg2, alternative = "two.sided")
#### Durbin Watson: p-value = 0.6779, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg2))
#### Shapiro Test: p-value = 0.3853, so we don't have evidence to reject the null hypothesis, i-e Assumption 2 fulfills
qqnorm(residuals(reg2))
qqline(residuals(reg2), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg2))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg2)
#### Breusch Pagan: p-value = 0.3319, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### Assumption 4 (Multicollinearity)
vif(reg2)
#### Both predictors, social_media_hours and mood_rating, showed high VIF values (~6.75), indicating substantial multicollinearity.
#### This suggests that these variables are strongly correlated and may be explaining overlapping variance in mental health scores.

#### Choose the next new most correlated variable
## Third Regression Model (mental_health_score ~ social_media_hours+physical_activity_hours_per_week)
reg3 <- lm(mental_health_score ~ social_media_hours+physical_activity_hours_per_week, data=data) 
summary(reg3)
## We reject the null hypothesis because the p-values for social_media_hours and physical_activity_hours_per_week are < 2e-16, i-e The slope is != 0

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg3, alternative = "two.sided")
#### Durbin Watson: p-value = 0.7587, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg3))
#### Shapiro Test: p-value = 0.001868
qqnorm(residuals(reg3))
qqline(residuals(reg3), col="red")
#### Finally, even though Shapiro test give us a p-value < 0.05, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg3))
#### Even though some extreme values can be observed, we don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg3)
#### Even though Breusch Pagan test's p-value = 9.38e-05, visually the points are roughly scattered around the whole figure in a rectangular shape, i-e Assumption 3 fulfills

#### Assumption 4 (Multicollinearity)
vif(reg3)
#### The multicollinearity for both the variables is approximately 2 which is < 5, so we can assume no correlation between the variables, i-e Assumption 4 fulfills

#### All the 4 Assumptions are fulfilled for the Third Regression Model

y3 <- predict(reg3, interval = "confidence")
y3

### Need to interpret ..............................................

### Comparing regression models
#### Using R_squared metrics
summary(reg1)$adj.r.squared
summary(reg3)$adj.r.squared
#### reg3 has better R squared value 80.58%

#### Using Anova
anova(reg1, reg3)
#### We obtain a lower value for RSS compared to the first model and p-value < 2.2e-16, so we reject the null hypothesis (which is that Beta_physical_activity_hours_per_week == 0). This indicates that physical_activity_hours_per_week is an important predictor of mental_health_score beyond the effect of social_media_hours alone.

#### Conclusion: We choose the Third model because multiple regression model provides a better fit and explains mental health score more accurately.

#c
## age_cat
reg3_age <- lm(mental_health_score ~ social_media_hours+physical_activity_hours_per_week+age_cat, data=data) 
summary(reg3_age)
### For every 1 additional hour spent on social media per day, mental health score decreases by about 6.39 points, holding age group and physical activity constant
### For every 1 hour increase in physical activity per week, mental health score increases by about 2.01 points, holding social media use and age group constant
### Gen Y scores about 2.15 points higher than Gen Z, controlling for SM hours + PA
### Gen X scores about 2.29 points higher than Gen Z
### Baby Boomers score about 2.13 points higher than Gen Z
### The model explains 81% of the variation in mental health score
### The overall model is highly significant since p-values < 2e-16

names(reg3_age)
coefficients(reg3_age)

cols <- c("red", "blue", "green", "yellow")
names(cols) <- c("Gen Z", "Gen Y", "Gen X", "Baby Boomers")

### Since we cannot plot a 3-variable regression in 2D, so we must choose one predictor for the x-axis
#### Holding physical_activity_hours_per_week constant
mean_PA <- mean(data$physical_activity_hours_per_week, na.rm = TRUE)
b0_GenZ = coefficients(reg3_age)[1] + (coefficients(reg3_age)[3]*mean_PA) 
b0_GenY = coefficients(reg3_age)[1] + coefficients(reg3_age)[4] + (coefficients(reg3_age)[3]*mean_PA)
b0_GenX = coefficients(reg3_age)[1] + coefficients(reg3_age)[5] + (coefficients(reg3_age)[3]*mean_PA)
b0_BabyBoomers = coefficients(reg3_age)[1] + coefficients(reg3_age)[6] + (coefficients(reg3_age)[3]*mean_PA)
slope = coefficients(reg3_age)[2]
#### Visual plot
plot(mental_health_score ~ social_media_hours, 
     data = data, 
     col = cols[data$age_cat],
     pch = 16)
abline(b0_GenZ,  slope, col = "red",    lwd = 2)
abline(b0_GenY,  slope, col = "blue",   lwd = 2)
abline(b0_GenX,  slope, col = "green",  lwd = 2)
abline(b0_BabyBoomers, slope, col = "yellow", lwd = 2)
legend("bottomleft",
       legend = c("Gen Z", "Gen Y", "Gen X", "Baby Boomers"),
       col = cols,
       lwd = 2)

#### Holding social_media_hours constant
mean_SM <- mean(data$social_media_hours, na.rm = TRUE)
b0_GenZ = coefficients(reg3_age)[1] + (coefficients(reg3_age)[2]*mean_SM) 
b0_GenY = coefficients(reg3_age)[1] + coefficients(reg3_age)[4] + (coefficients(reg3_age)[2]*mean_SM)
b0_GenX = coefficients(reg3_age)[1] + coefficients(reg3_age)[5] + (coefficients(reg3_age)[2]*mean_SM)
b0_BabyBoomers = coefficients(reg3_age)[1] + coefficients(reg3_age)[6] + (coefficients(reg3_age)[2]*mean_SM)
slope = coefficients(reg3_age)[3]
#### Visual plot
plot(mental_health_score ~ physical_activity_hours_per_week, 
     data = data, 
     col = cols[data$age_cat],
     pch = 16)
abline(b0_GenZ,  slope, col = "red",    lwd = 2)
abline(b0_GenY,  slope, col = "blue",   lwd = 2)
abline(b0_GenX,  slope, col = "green",  lwd = 2)
abline(b0_BabyBoomers, slope, col = "yellow", lwd = 2)
legend("bottomright",
       legend = c("Gen Z", "Gen Y", "Gen X", "Baby Boomers"),
       col = cols,
       lwd = 2)

## stress_cat
reg3_stress <- lm(mental_health_score ~ social_media_hours+physical_activity_hours_per_week+stress_cat, data=data) 
summary(reg3_stress)
### For every additional hour of social media, mental health decreases by 5.67 points, holding PA and stress constant.
### For every extra hour of physical activity per week, mental health increases by 1.88 points, holding SM and stress constant.
### High stress participants score 3.38 points lower on mental health than Low stress participants, controlling for SM and PA.
### The model explains 81.03% of the variation in mental health score
### The overall model is highly significant since p-values < 2e-16

names(reg3_stress)
coefficients(reg3_stress)

### Since we cannot plot a 3-variable regression in 2D, so we must choose one predictor for the x-axis
#### Holding physical_activity_hours_per_week constant
b0_Low = coefficients(reg3_stress)[1] + (coefficients(reg3_stress)[3]*mean_PA)
b0_High = coefficients(reg3_stress)[1] + coefficients(reg3_stress)[4] + (coefficients(reg3_stress)[3]*mean_PA)
slope = coefficients(reg3_stress)[2]

#### Visual plot
plot(mental_health_score ~ social_media_hours, data = data, col = c("black", "red")[data$stress_cat], pch=16)
abline(b0_Low, slope, col="black", lwd=2)
abline(b0_High, slope, col="red", lwd=2)
legend("topright", legend=c("Low Stress", "High Stress"), col=c("black","red"), lwd=2)

#### Holding social_media_hours constant
b0_Low = coefficients(reg3_stress)[1] + (coefficients(reg3_stress)[2]*mean_SM)
b0_High = coefficients(reg3_stress)[1] + coefficients(reg3_stress)[4] + (coefficients(reg3_stress)[2]*mean_SM)
slope = coefficients(reg3_stress)[3]

#### Visual plot
plot(mental_health_score ~ physical_activity_hours_per_week, data = data, col = c("black", "red")[data$stress_cat], pch=16)
abline(b0_Low, slope, col="black", lwd=2)
abline(b0_High, slope, col="red", lwd=2)
legend("topright", legend=c("Low Stress", "High Stress"), col=c("black","red"), lwd=2)

#d




