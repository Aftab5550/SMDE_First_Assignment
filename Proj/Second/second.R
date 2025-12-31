install.packages("corrplot")
install.packages("psych")
install.packages("FactoMineR")
install.packages("factoextra")
library(readxl)
library(corrplot)
library(psych)
library(FactoMineR)
library(factoextra)
library(lmtest)
library(car)

##################### EuroBasket2025 #####################
#a
data <- read_excel("data_Eurobasket_2025.xlsx")
summary(data)
data <- as.data.frame(data)
rownames(data) <- data$PLAYER
data$PLAYER <- NULL

#b
str(data)
data$Team <- as.factor(data$Team)
data$Position <- as.factor(data$Position)
summary(data)

#c
df_pca <- subset(data, select = c("Position", "MIN", "FG", "2PT FG", "3PT FG",
                                  "FT", "OREB", "DREB", "REB", "AST", "PF",
                                  "TO", "STL", "BLK", "EFF", "PTS"))
numerical_values <- df_pca[, -1]

plot(numerical_values)

R <- cor(numerical_values)
print(R)

corrplot(R, 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         tl.col = "black",
         number.cex = 0.6,
         diag = FALSE)

n<-nrow(numerical_values)
cortest.bartlett(R,n)

KMO(numerical_values)
KMO(numerical_values[,-14])

#d
index_quali <- which(colnames(df_pca) == "Position")
index_quanti_sup <- which(colnames(df_pca) == "EFF")

result_pca <- PCA(df_pca, quanti.sup = index_quanti_sup, quali.sup = index_quali)

#e
### Explained Variation ###
result_pca$eig
plot(result_pca$eig[,1], type="o", main="Scree Plot")

cumulative_var <- result_pca$eig[3, 3]
print(cumulative_var)

#f
round(result_pca$var$coord[, 1:3], 2)
plot(result_pca, choix = "var", axes = c(1, 2), title = "PCA Variables - Dim 1 & 2")
plot(result_pca, choix = "var", axes = c(1, 3), title = "PCA Variables - Dim 1 & 3")
plot(result_pca, choix = "var", axes = c(2, 3), title = "PCA Variables - Dim 2 & 3")

## Component Loadings ##
result_pca$var$coord
result_pca$var$coord[,1:3]

#g
## Dimensions 1 and 2
fviz_pca_ind(result_pca, geom.ind = "text", col.ind = df_pca$Position, axes = c(1, 2))

fviz_pca_ind(result_pca,
             axes = c(1, 2),
             geom.ind = "point",
             col.ind = df_pca$Position,
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Position",
             repel = TRUE,
             title = "Individuals: Performance (Dim1) vs. Role (Dim2)")

## Dimensions 1 and 3
fviz_pca_ind(result_pca, geom.ind = "text", col.ind = df_pca$Position, axes = c(1, 3))

fviz_pca_ind(result_pca,
             axes = c(1, 3),
             geom.ind = "point",
             col.ind = df_pca$Position,
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Position",
             repel = TRUE,
             title = "Individuals: Performance (Dim1) vs. Playmaking (Dim3)")

## Dimensions 2 and 3
fviz_pca_ind(result_pca, geom.ind = "text", col.ind = df_pca$Position, axes = c(2, 3))

fviz_pca_ind(result_pca,
             axes = c(2, 3),
             geom.ind = "point",
             col.ind = df_pca$Position,
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Position",
             repel = TRUE,
             title = "Individuals: Role (Dim2) vs. Playmaking (Dim3)")

## Scores of PC1, PC2 and PC3
result_pca$ind$coord[,1:3]

#h
## Principal Component Regression
df_pca$PC1<-result_pca$ind$coord[,1]
df_pca$PC2<-result_pca$ind$coord[,2]
df_pca$PC3<-result_pca$ind$coord[,3]
cor(df_pca[, -1])

## First Model
reg1 <- lm(EFF ~ PC1 + PC2 + PC3, data=df_pca)
summary(reg1)

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg1, alternative = "two.sided")
#### Durbin Watson: p-value < 0.05, so we reject the null hypothesis. i-e the Assumption 1 doesn't fulfill

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg1))
#### Shapiro Test: p-value = 0.2751, so we don't have evidence to reject the null hypothesis, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg1))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg1)
#### Breusch Pagan: p-value = 0.442, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### Assumption 4 (Multicollinearity)
vif(reg1)

## Second Model
reg2 <- lm(EFF ~ PC1 + PC3, data=df_pca)
summary(reg2)

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg2, alternative = "two.sided")
#### Durbin Watson: p-value < 0.05, so we reject the null hypothesis. i-e the Assumption 1 doesn't fulfill

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg2))
#### Shapiro Test: p-value = 0.3146, so we don't have evidence to reject the null hypothesis, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg2))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg2)
#### Breusch Pagan: p-value = 0.2743, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

#### Assumption 4 (Multicollinearity)
vif(reg2)

## Third Model
reg3 <- lm(EFF ~ PC1, data=df_pca)
summary(reg3)

### Confirming Assumptions
#### Assumption 1 (The observations within each sample must be independent)
dwtest(reg3, alternative = "two.sided")
#### Durbin Watson: p-value < 0.05, so we reject the null hypothesis. i-e the Assumption 1 doesn't fulfill

#### Assumption 2 (The populations from which the samples are selected must be normal)
shapiro.test(residuals(reg3))
#### Shapiro Test: p-value = 0.3968, so we don't have evidence to reject the null hypothesis, i-e Assumption 2 fulfills

#### Assumption 3 (The populations from which the samples are selected must have equal variances (homogeneity of variance))
plot(residuals(reg3))
#### We don’t see any shape and the points are roughly scattered around the whole figure in a rectangular shape
bptest(reg3)
#### Breusch Pagan: p-value = 0.08449, so we don't have evidence to reject the null hypothesis. i-e the Assumption 3 fulfills

