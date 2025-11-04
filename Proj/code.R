install.packages("DescTools")
library(DescTools)

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

##################### QUESTION1 #####################
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


