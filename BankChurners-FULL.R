library(dplyr)
library(nortest)
library(randomForest)
library(caret)
library(AICcmodavg)

dt <- read.csv("BankChurners.csv")
dt <- dt[dt$Attrition_Flag == "Existing Customer", ]
dt <- select(dt, -c("CLIENTNUM", "Attrition_Flag", 
                    "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_1", 
                    "Naive_Bayes_Classifier_Attrition_Flag_Card_Category_Contacts_Count_12_mon_Dependent_count_Education_Level_Months_Inactive_12_mon_2",
                    "Total_Amt_Chng_Q4_Q1", 
                    "Total_Ct_Chng_Q4_Q1"))
dt <- dt[dt$Total_Revolving_Bal != 0, ]
head(dt)
dim(dt)
# make sure no zero values for average utilization ratio
dim(dt[dt$Avg_Utilization_Ratio == 0,])

# ----- statistical tests -----

# ----- 1. INDIVIDUAL RELATIONSHIP -----

# 1-0 average_utilization_ratio (numerical)
# 1-0-1 confidence interval and two-sided t-test
x <- dt$Avg_Utilization_Ratio
hist(dt$Avg_Utilization_Ratio,
     main='Histogram of Avg_Utilization_Ratio',
     xlab='Avg_Utilization_Ratio',
     ylab='Frequency' )
n <- length(x)
xbar <- mean(x)
s <- sd(x)
alpha <- 0.05
t <- qt(1-alpha/2,df=n-1)
CI95 <- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n))
print(paste("n=",n, "; xbar=", xbar, "; s=", s))
print(paste("alpha=",alpha, "; t=", t))
print(paste("95% CI= [", CI95[1], CI95[2], "]"))
# two-sided
t.test(x, conf.level=0.95)


# 1-1 customer_age (numerical)
# 1-1-1 confidence interval and two-sided t-test
x <- dt$Customer_Age
n <- length(x)
xbar <- mean(x)
s <- sd(x)
alpha <- 0.05
t <- qt(1-alpha/2,df=n-1)
CI95 <- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n))
print(paste("n=",n, "; xbar=", xbar, "; s=", s))
print(paste("alpha=",alpha, "; t=", t))
print(paste("95% CI= [", CI95[1], CI95[2], "]"))
# two-sided
t.test(x, conf.level=0.95)

# 1-1-2 normalise data to be between 0 and 1
head(dt$Customer_Age)
dt$Customer_Age_norm <- (dt$Customer_Age-min(dt$Customer_Age))/(max(dt$Customer_Age)-min(dt$Customer_Age))
head(dt$Customer_Age_norm)


# 1-2 Gender
#count(dt[dt$Gender=="M",])
#count(dt[dt$Gender=="F",])
gender_class <- c("M",
                  "F")
obsfreq_ <- c(count(dt[dt$Gender=="M",]),
             count(dt[dt$Gender=="F",]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Gender)
count(dt[dt$Gender,])
barplot(obsfreq,names.arg=gender_class,main="Observed Frequency")
# goodness of fit test: check if the expected gender occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Gender,])/2,2))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=gender_class,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=gender_class,
        beside=TRUE,legend.text=TRUE, xlab="Gender")

gof_test <- chisq.test(table(dt$Gender),
                       p=c(1/2,1/2))
gof_test


# 1-3 Dependent_count
unique(dt$Dependent_count)
dep_count <- c(0,1,2,3,4,5)
obsfreq_ <- c(count(dt[dt$Dependent_count== 0,]),
              count(dt[dt$Dependent_count== 1,]),
              count(dt[dt$Dependent_count== 2,]),
              count(dt[dt$Dependent_count== 3,]),
              count(dt[dt$Dependent_count== 4,]),
              count(dt[dt$Dependent_count== 5,]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Dependent_count)

barplot(obsfreq,names.arg=dep_count,main="Observed Frequency")
# goodness of fit test: check if the dependent count occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Dependent_count,])/6,6))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=dep_count,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=dep_count,
        beside=TRUE,legend.text=TRUE, xlab="Dependency Count")

gof_test <- chisq.test(table(dt$Dependent_count),p=c(rep(1/6,6)))
gof_test


# 1-4 Education_Level
unique(dt$Education_Level)
ed <- c("High School",
               "Graduate",
               "Uneducated",
               "Post-Graduate",
               "Doctorate",
               "College",
               "Unknown")
obsfreq_ <- c(count(dt[dt$Education_Level== "High School",]),
              count(dt[dt$Education_Level== "Graduate",]),
              count(dt[dt$Education_Level== "Uneducated",]),
              count(dt[dt$Education_Level== "Post-Graduate",]),
              count(dt[dt$Education_Level== "Doctorate",]),
              count(dt[dt$Education_Level== "College",]),
              count(dt[dt$Education_Level== "Unknown",]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Education_Level)

barplot(obsfreq,names.arg=ed,main="Observed Frequency")
# goodness of fit test: check if the dependent count occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Education_Level,])/7,7))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=ed,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=ed,
        beside=TRUE,legend.text=TRUE, xlab="Education Level")

gof_test <- chisq.test(table(dt$Education_Level),p=c(rep(1/7,7)))
gof_test


# 1-5 Marital_Status
unique(dt$Marital_Status)
marital_class <- c("Married",
                  "Single",
                  "Divorced",
                  "Unknown")
obsfreq_ <- c(count(dt[dt$Marital_Status=="Married",]),
              count(dt[dt$Marital_Status=="Single",]),
              count(dt[dt$Marital_Status=="Divorced",]),
              count(dt[dt$Marital_Status=="Unknown",]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Marital_Status)
barplot(obsfreq,names.arg=marital_class,main="Observed Frequency")
# goodness of fit test: check if the expected gender occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Marital_Status,])/4,4))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=marital_class,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=marital_class,
        beside=TRUE,legend.text=TRUE, xlab="Marital Status")

gof_test <- chisq.test(table(dt$Marital_Status),
                       p=c(rep(1/4,4)))
gof_test

# 1-6 Income_Category
unique(dt$Income_Category)
income_cat <- c("Less than $40K",
                   "$40K - $60K",
                   "$60K - $80K",
                   "$80K - $120K",
                   "$120K +",
                   "Unknown" )

obsfreq_ <- c(count(dt[dt$Income_Category=="Less than $40K",]),
              count(dt[dt$Income_Category=="$40K - $60K",]),
              count(dt[dt$Income_Category=="$60K - $80K",]),
              count(dt[dt$Income_Category=="$80K - $120K",]),
              count(dt[dt$Income_Category=="$120K +",]),
              count(dt[dt$Income_Category=="Unknown",]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Income_Category)
barplot(obsfreq,names.arg=income_cat,main="Observed Frequency")
# goodness of fit test: check if the expected gender occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Income_Category,])/6,6))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=income_cat,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=income_cat,
        beside=TRUE,legend.text=TRUE, xlab="Marital Status")

gof_test <- chisq.test(table(dt$Income_Category),
                       p=c(rep(1/6,6)))
gof_test

# 1-7 Card_Category
unique(dt$Card_Category)
card_cat <- c("Blue",
                "Gold",
                "Silver",
                "Platinum" )

obsfreq_ <- c(count(dt[dt$Card_Category=="Blue",]),
              count(dt[dt$Card_Category=="Gold",]),
              count(dt[dt$Card_Category=="Silver",]),
              count(dt[dt$Card_Category=="Platinum",]))
obsfreq <- unlist(obsfreq_,use.names=FALSE)
n <- sum(obsfreq)
n
table(dt$Card_Category)
barplot(obsfreq,names.arg=card_cat,main="Observed Frequency")
# goodness of fit test: check if the expected gender occur in equal proportion
exp_freq_ <- c(rep(count(dt[dt$Card_Category,])/4,4))
exp_freq <- unlist(exp_freq_)
#exp_freq
barplot(exp_freq,names.arg=card_cat,main="Expected Frequency")

# plot 2 graphs together
barplot(rbind(exp_freq,obsfreq),names.arg=card_cat,
        beside=TRUE,legend.text=TRUE, xlab="Card Category")

gof_test <- chisq.test(table(dt$Card_Category),
                       p=c(rep(1/4,4)))
gof_test

# 1-8 Months_on_book (numerical)
# 1-8-1 t-test and confidence interval
x <- dt$Months_on_book
n <- length(x)
xbar <- mean(x)
s <- sd(x)
alpha <- 0.05
t <- qt(1-alpha/2,df=n-1)
CI95 <- c(xbar-t*s/sqrt(n), xbar+t*s/sqrt(n))
print(paste("n=",n, "; xbar=", xbar, "; s=", s))
print(paste("alpha=",alpha, "; t=", t))
print(paste("95% CI= [", CI95[1], CI95[2], "]"))
# two-sided
t.test(x, conf.level=0.95)

# 1-8-2 normalise data to be between 0 and 1 (same scale)
head(dt$Months_on_book)
dt$Months_on_book_norm <- (dt$Months_on_book-min(dt$Months_on_book))/(max(dt$Months_on_book)-min(dt$Months_on_book))
head(dt$Customer_Age_norm)


# ----- 2. RELATIONSHIPS BETWEEN 2 VARIABLES -----

# relation between avg_utilization_ratio and customer_age
# take avg_utilization_ratio vs log(customer_age)
fit1 <- lm(dt$Avg_Utilization_Ratio ~ (dt$Customer_Age_norm))

summary(fit1)
#plot(fit1)
plot(dt$Avg_Utilization_Ratio ~ (dt$Customer_Age_norm))
abline(fit1, col='red')

hist((dt$Avg_Utilization_Ratio), breaks=20)
hist((dt$Customer_Age_norm))

# assumption of one-way ANOVA takes place
# relation between avg_utilization_ratio and Gender
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Gender)
summary(one.way)

# relation between avg_utilization_ratio and Dependent_count
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Dependent_count)
summary(one.way)

# relation between avg_utilization_ratio and Education_Level
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Education_Level)
summary(one.way)

# relation between avg_utilization_ratio and Marital_Status
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Marital_Status)
summary(one.way)

# relation between avg_utilization_ratio and Income_Category
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Income_Category)
summary(one.way)

# relation between avg_utilization_ratio and Card_Category
one.way <- aov(dt$Avg_Utilization_Ratio ~ dt$Card_Category)
summary(one.way)

# relation between avg_utilization_ratio and Months_on_book
fit2 <- lm(dt$Avg_Utilization_Ratio ~ (dt$Months_on_book_norm))

summary(fit2)
#plot(fit2)
plot(dt$Avg_Utilization_Ratio ~ (dt$Months_on_book_norm))
abline(fit2, col='red')

hist((dt$Avg_Utilization_Ratio), breaks=20)
hist((dt$Months_on_book_norm))


# ----- 3. use random forest to choose most important features among the 17 predictor variables -----
rfc <- randomForest(Avg_Utilization_Ratio~., data=dt)
importance(rfc)
varImp(rfc)
varImpPlot(rfc)
# Nic's part -> sift out gender, income category, card category for 2-way ANOVA analysis
# based on random forest


# ----- 4. two-way ANOVA with gender, income category, card category (3 choose 2) -----

# gender - income category
two.way.gen_inc <- aov(dt$Avg_Utilization_Ratio ~ dt$Gender*dt$Income_Category)
summary(two.way.gen_inc)

# gender - card category
two.way.gen_card <- aov(dt$Avg_Utilization_Ratio ~ dt$Gender*dt$Card_Category)
summary(two.way.gen_card)

# income category - card category
two.way.inc_card <- aov(dt$Avg_Utilization_Ratio ~ dt$Income_Category*dt$Card_Category)
summary(two.way.inc_card)

# two way anova Pr(>F) below level of significance (or F value high) FOR THE INTERACTIONS
# => reject H0 => exists interactions

# find best fit two-way ANOVA model using Akaike Information Criterion (AIC)
model.set <- list(two.way.gen_inc, two.way.gen_card, two.way.inc_card)
model.names <- c("two.way.gen_inc", "two.way.gen_card", "two.way.inc_card")
aictab(model.set, modnames = model.names)

# two.way.gen_card has the lowest AICc value -> best model


# ----- 5. Tukey test to test significance between 2 levels of best model (post-hoc test)
tukey.two.way <- TukeyHSD(two.way.gen_card)
tukey.two.way
# look at p adj column, the lower the value 
# => there are statistically significant between levels