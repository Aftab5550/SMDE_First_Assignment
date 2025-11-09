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
# Dropping the variables "user_id", "sleep_quality", "mood_rating" and "stress_level"
numeric_vars <- numeric_vars[ , !(names(numeric_vars) %in% c("user_id", "sleep_quality", "mood_rating", "stress_level"))]

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
## Histogram: "physical_activity_hours_per_week" doesn't follow a normal distribution; Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
## Histogram: "mental_health_score" doesn't follow a normal distribution(*); Shapiro: Rejects the null hypothesis (p-value < 2.2e-16)
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
bptest(model4)
#### Breusch Pagan: p-value = 1.81e-05, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled
leveneTest(daily_screen_time_hours ~ sleep_cat, data=data)
#### Levene's Test: p-value = 0.0009863, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled

################################################################################################
#### Assumptions 1 and 2 are fulfilled, but Assumption 3 is not fulfilled (Should do something?)
################################################################################################

#### We apply log tranformation of the variable to see if we can resolve this issue 
data$daily_screen_time_hours_log <- log(data$daily_screen_time_hours)
summary(data$daily_screen_time_hours_log)
hist(data$daily_screen_time_hours_log)

model4_log <- aov(daily_screen_time_hours_log ~ sleep_cat, data=data)
summary(model4_log)
## p-value < 2e-16, so we reject the null hypothesis, that means at least one group (Low, Medium or High) has a different mean from the rest

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model4_log, alternative ="two.sided")
#### Durbin Watson: p-value = 0.5661, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model4_log))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable daily_screen_time_hours_log seems to follow a Normal Distribution
qqnorm(residuals(model4_log))
qqline(residuals(model4_log), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
bptest(model4_log)
#### Breusch Pagan: p-value < 2.2e-16, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled
leveneTest(daily_screen_time_hours_log ~ sleep_cat, data=data)
#### Levene's Test: p-value < 2.2e-16, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled

################################################################################################
#### Assumptions 1 and 2 are fulfilled, but Assumption 3 is not fulfilled (Should do something?)
################################################################################################

#### We apply sqrt tranformation of the variable to see if we can resolve this issue 
data$daily_screen_time_hours_sqrt <- sqrt(data$daily_screen_time_hours)
summary(data$daily_screen_time_hours_sqrt)
hist(data$daily_screen_time_hours_sqrt)

model4_sqrt <- aov(daily_screen_time_hours_sqrt ~ sleep_cat, data=data)
summary(model4_sqrt)
## p-value < 2e-16, so we reject the null hypothesis, that means at least one group (Low, Medium or High) has a different mean from the rest

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(model4_sqrt, alternative ="two.sided")
#### Durbin Watson: p-value = 0.5905, so we don't have evidence to reject the null hypothesis. i-e the Assumption 1 fulfills

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(model4_sqrt))
#### We have already seen the shapiro test rejects the null hypothesis even though the variable daily_screen_time_hours_log seems to follow a Normal Distribution
qqnorm(residuals(model4_sqrt))
qqline(residuals(model4_sqrt), col="red")
#### Finally, we apply Q-Q Plot and the residuals seem to be Normal, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
bptest(model4_sqrt)
#### Breusch Pagan: p-value = 1.407e-09, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled
leveneTest(daily_screen_time_hours_sqrt ~ sleep_cat, data=data)
#### Levene's Test: p-value = 4.488e-10, so we reject the null hypothesis. i-e the Assumption 3 is not fulfilled

################################################################################################
#### Assumptions 1 and 2 are fulfilled, but Assumption 3 is not fulfilled (Should do something?)
################################################################################################


