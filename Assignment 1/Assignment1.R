###############################################################################################
####################   Assignment 1    #####################
###############################################################################################
#import packages;
library(dplyr)
library(reshape2)
library(ggplot2)
library(Hmisc)
library(corrplot)
library(mice)
library(VIM)
library(pROC)
library(caret)
library(sqldf)
library(rpart)

#################################################################################
##########################    Data Exploration     ##############################
#################################################################################
getwd();
data <- read.table("data.txt", sep =",", header = FALSE, dec =".")


#check distribution of target variable

colnames(data)[colnames(data)=="V1"] <- "age"
colnames(data)[colnames(data)=="V2"] <- "workclass"
colnames(data)[colnames(data)=="V3"] <- "fnlwgt"
colnames(data)[colnames(data)=="V4"] <- "education"
colnames(data)[colnames(data)=="V5"] <- "education-num"
colnames(data)[colnames(data)=="V6"] <- "marStat"
colnames(data)[colnames(data)=="V7"] <- "occupation"
colnames(data)[colnames(data)=="V8"] <- "relationship"
colnames(data)[colnames(data)=="V9"] <- "race"
colnames(data)[colnames(data)=="V10"] <- "sex"
colnames(data)[colnames(data)=="V11"] <- "capital-gain"
colnames(data)[colnames(data)=="V12"] <- "capital-loss"
colnames(data)[colnames(data)=="V13"] <- "hrs-per-week"
colnames(data)[colnames(data)=="V14"] <- "native-country"
colnames(data)[colnames(data)=="V15"] <- "income"
data$ID <- seq.int(nrow(data))

str(data)
summary(data)



#search for missing data
is.na(data) = data=='?'
is.na(data) = data==' ?'
sum(is.na(data))
mean(is.na(data))
#erase those rows
data = na.omit(data)

#outliers
boxplot(data, horizontal = T)
boxplot(data$`capital-gain`, horizontal = T)
boxplot(data$`capital-loss`, horizontal = T)


attach(data)

freq_tbl=table(workclass)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$workclass, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(education)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$education, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(`education-num`)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$education-`education-num`, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(marStat)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$marStat, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(occupation)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$occupation, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(relationship)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$relationship, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(race)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$race, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(sex)
head(freq_tbl)
barplot(freq_tbl)
pie(freq_tbl)
ggplot(data) + aes(x=data$sex, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

freq_tbl=table(data$`natice-country`)
head(freq_tbl)
ggplot(data) + aes(x=data$`native-country`, group=income, fill=income) + 
  geom_bar(color='black')  + 
  scale_colour_discrete(drop = FALSE)

list_of_numcols = sapply(data, is.numeric)
numcols = data[ , list_of_numcols]
melt_data = melt(numcols, id.vars=c("ID"))
ggplot(data = melt_data, mapping = aes(x = value)) + geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')
summary(data$age)
summary(data$`education-num`)
summary(data$`capital-gain`)
summary(`capital-gain`)
summary(`capital-loss`)
summary(`hrs-per-week`)
ggplot(data) + aes(x=as.numeric(data$age), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$age, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`education-num`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`education-num`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`capital-gain`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`capital-gain`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`capital-loss`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`capital-loss`, y=data$income)) + 
  geom_point()
ggplot(data) + aes(x=as.numeric(data$`hrs-per-week`), group=income, fill=income) + 
  geom_histogram(binwidth=1, color='black')
ggplot(data, aes(x=data$`hrs-per-week`, y=data$income)) + 
  geom_point()


#################################################################################
 ##########################  Feature Engineering   #############################
#################################################################################
data <- read.table("data.txt", sep =",", header = FALSE, dec =".")

colnames(data)[colnames(data)=="V1"] <- "age"
colnames(data)[colnames(data)=="V2"] <- "workclass"
colnames(data)[colnames(data)=="V3"] <- "fnlwgt"
colnames(data)[colnames(data)=="V4"] <- "education"
colnames(data)[colnames(data)=="V5"] <- "education-num"
colnames(data)[colnames(data)=="V6"] <- "marStat"
colnames(data)[colnames(data)=="V7"] <- "occupation"
colnames(data)[colnames(data)=="V8"] <- "relationship"
colnames(data)[colnames(data)=="V9"] <- "race"
colnames(data)[colnames(data)=="V10"] <- "sex"
colnames(data)[colnames(data)=="V11"] <- "capital-gain"
colnames(data)[colnames(data)=="V12"] <- "capital-loss"
colnames(data)[colnames(data)=="V13"] <- "hrs-per-week"
colnames(data)[colnames(data)=="V14"] <- "native-country"
colnames(data)[colnames(data)=="V15"] <- "income"


#search for missing data
is.na(data) = data=='?'
is.na(data) = data==' ?'
sum(is.na(data))
mean(is.na(data))
#erase those rows
data = na.omit(data)
#need to determine the order of this
#anyDuplicated(data)
#data <- distinct(data)
#we decided not to remove duplicate data

#outliers
boxplot(data, horizontal = T)
boxplot(data$`capital-gain`, horizontal = T)
boxplot(data$`capital-loss`, horizontal = T)

data$fnlwgt <- NULL
data$'capital-gain' <- NULL
data$'capital-loss' <- NULL
data$'native-country' <- NULL


#descision tree
s <- sample(30139,24111)
train <- data[s,]
test <- data[-s,]
dtmn <- rpart(income~., train, method="class")
plot(dtmn)
text(dtmn)
p <- predict(dtmn, test, type="class")
table(test[,11], p)
#results
#<=50K  >50K
#<=50K   4194   319
#>50K     708   807

#linear regression model
lmodel <- lm(income~., data=train)
p <- predict(lmodel, test)
p <- ifelse(p>1.3,'>50K','<=50K')
table(test[,11], p)

#logistic regression model
lrm <- glm(income~., data=train, family='binomial')
p <- predict(lrm, test, type = 'response')
p <- ifelse(p>0.25,'>50K','<=50K')
table(test[,11], p)

#svm model
library(e1071)
svmm <- svm(income ~., data=train, kernel = "linear")
p <- predict(svmm, test, type="class")
table(test[,11], p)

#naive bayes model
library(e1071)
nbm <- naiveBayes(income~., data=train)
p <- predict(nbm, test, type="class")
table(test[,11], p)