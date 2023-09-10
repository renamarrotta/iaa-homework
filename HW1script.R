# LIBRARY --------------------------
library(mgcv)
library(tidyverse)
library(car)
library(DescTools)
library(corrplot)
library(mosaic)
library(modelr)
library(plotly)
library(ggplot2)
library(Hmisc)
library(onehot)
library(jmuOutlier)
library(leaps)
library(glmnet)
library(nortest)
library(lmtest)
library(gmodels)
library(vcdExtra)
library(TSA)
library(carData)
library(epiDisplay)
library(gridExtra)
library(dplyr)

# SET & LOAD ------------------

setwd("/Users/renamarrotta/Desktop/IAA/Fall/Logistic Regression/Homework1_LR")
train <- read.csv("insurance_t.csv")


# MAKE VARIABLE TYPES --------------------------

for (i in 1:ncol(train)) {
  if (length(unique(train[,i])) <= 10) { 
    train[,i] <- as.factor(train[,i]) #factor any variables with <= 10 values
  }
}

train$BRANCH <- as.factor(train$BRANCH)


# CREATE NEW DATAFRAMES FOR LOOPS --------------------

target <- train %>% dplyr::select(INS) #df with only target 
#target <- data.frame(INS=train$INS)
predict <- train %>% dplyr::select(-INS) #df with all other variables 
#predict <- train[,-46]

# TESTS OF ASSOCIATION TABLES ----------------------------

table1 = data.frame(matrix(nrow = 0, ncol = 3))

#This for loop looks at each variable in the dataset and runs a test of association depending on the type of variable 
#Builds a new data frame table1 with the variable name, variable type, and pvalue 

for (i in 1:ncol(predict)) {
  if (class(predict[,i]) == "factor") { #categorical data 
    if (length(unique(predict[,i])) == 2) { #if the variable is binary 
      type <- "binary"
      variable <- names(predict[i]) 
      pvalue <- CMHtest(table(predict[,i], target[,1]))$table[1,3] #extract p-value from output
      newrow <- c(variable, type, pvalue)
      table1 <- rbind(table1, newrow)
    }
    else if ((names(predict[i]) == "BRANCH") | (names(predict[i]) == "RES")) { #These are the 2 nominal vars
      type <- "nominal" 
      variable <- names(predict[i])
      result <- chisq.test(table(predict[,i], target[,1])) #output to a list 
      pvalue <- result[[3]] #extract p-value from the list 
      newrow <- c(variable, type, pvalue)
      table1 <- rbind(table1, newrow)}
    else { #if not binary and not nominal, they are ordinal 
      type <-  "ordinal"
      variable <- names(predict[i])
      pvalue <- CMHtest(table(predict[,i], target[,1]))$table[1,3] #extract pvalue from output 
      newrow <- c(variable, type, pvalue)
      table1 <- rbind(table1, newrow) 
    }
  }
  else { #all other variables in the data set will be continuous 
    type <- "continuous"
    variable <- names(predict[i])
    df <- cbind(predict[i], target) #creates a df with only the target and one predictor variable 
    var_model <- glm(INS ~ ., data = df, family = binomial(link="logit")) #runs model with the one predictor variable using new df 
    pvalue <- summary(var_model)$coefficients[,4][2] #extracts pvalue from summary output 
    newrow <- c(variable, type, pvalue)
    table1 <- rbind(table1, newrow)
  }
}

columns= c("variable","type","pvalue") #define column names
colnames(table1) <- columns #add column names 
table1$pvalue <- as.numeric(table1$pvalue) #pvalue numeric 

#Filter p-values <= 0.0001 for the only significant. Sort descending by p-value 

table1_pvalue <- table1 %>% filter(pvalue <= 0.0001) %>% arrange(pvalue)
write_csv(table1_pvalue, "HW1_AssocationTests_OnlySignificant.csv")

#Use non-filtered table for the appendix table. Group_by and sort same way. 
table1_forappendix <- table1 %>% arrange((pvalue))
write_csv(table1_forappendix, "HW1_AssocationTests_Appendix.csv")


# ODDS RATIO TABLE ----------------------------------

table2 = data.frame(matrix(nrow = 0, ncol = 2)) #define empty dataframe
for (i in 1:ncol(predict)) {
  if (class(predict[,i]) == "factor") {#categorical data 
    if (length(unique(na.omit(predict[,i]))) == 2) { #if the variable is binary 
      variable <- names(predict[i]) 
      odds <-OddsRatio(table(predict[,i], target[,1])) #extract p-value from output
      newrow <- c(variable, odds)
      table2 <- rbind(table2, newrow)
    } 
  }
}
#create a table of odds ratio
columns= c("variable","oddsRatio")
colnames(table2) <- columns
table2$oddsRatio <- as.numeric(table2$oddsRatio)

#sort by the magnitude 
table2 <- table2 %>% arrange(desc(oddsRatio))
write_csv(table2, "HW1_OddsRatio.csv")


# LINEARITY ASSUMPTIONS -----------------

table3 = data.frame(matrix(nrow = 0, ncol = 2)) #define empty data frame
for (i in 1:ncol(predict)) {
  if (class(predict[,i]) != "factor") { #look at only continous variables 
     variable <- names(predict[i]) 
      fit.gam <- gam(INS ~ s(predict[,i]), #build GAM model for individual variable
                     data = train, family = binomial(link = 'logit'),
                     method = 'REML')
      logit.model <- glm(INS ~ predict[,i], #build logistic model 
                     data = train, family = binomial(link = "logit"))
    result <- as.list(anova(logit.model, fit.gam, test="Chisq")) #significance test to see if gam and glm are sig. different
    pvalue <- result[[5]][2] #extract p value
    newrow <- c(variable, pvalue) 
    table3 <- rbind(table3, newrow) #add to table 
}
}
columns= c("variable","pval_anova")
colnames(table3) <- columns
table3$pval_anova <- as.numeric(table3$pval_anova)
table3 <- table3 %>% mutate(Assumption = ifelse((pval_anova <= 0.0001) == TRUE, "Not Met", "Met"))


#write_csv(table3 , "HW1_LinearAssumptions.csv")

#table3_linearNOTmet <- table3 %>% filter(pval_anova < 0.0001) %>% arrange(pval_anova) #gam model fits better & assumption not met
#write_csv(table3_linearNOTmet , "HW1_LinearNotMet.csv")
#table3_linearmet <- table3 %>% filter(pval_anova > 0.0001) %>% arrange(pval_anova) #no difference between models & assumption met
#write_csv(table3_linearmet, "HW1_LinearMet.csv")

metvars <- table3 %>% filter(Assumption == "Met") %>% pull(variable)
metvars_sig <- dplyr::filter(table1_pvalue, variable %in% metvars)  %>% mutate(assumption = "Met") %>% dplyr::select(-type)
notmetvars <- table3 %>% filter(Assumption == "Not Met") %>% pull(variable) 
notmetvars_sig <- dplyr::filter(table1_pvalue, variable %in% notmetvars) %>% mutate(assumption = "Not Met") %>% dplyr::select(-type)
SigVars_Assumption <- rbind(metvars_sig, notmetvars_sig)
SigVars_Assumption <- SigVars_Assumption %>% dplyr::select(-pvalue)
write_csv(SigVars_Assumption, "HW1_SigVars_MetNotMet.csv")

# Univariate EDA -----------------------

univariate_analysis <- function(data) { #function from R exam to conduct EDA for data
  if (is.numeric(data)==TRUE){
    data_obs <- length(data)
    data_na <- sum(is.na(data))
    data_mean <- mean(data, na.rm=TRUE)
    data_sd <- sd(data, na.rm=TRUE)
    data_quart <- quantile(data, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
    histogram <- ggplot(data=data.frame(x=data), aes(x=x)) + geom_histogram()
    l <- list(data_obs, data_na, data_mean, data_sd, data_quart, histogram)
    names(l) <- c("obs", "na's", "mean", "stdev", "quartiles", "historgram")
  }
  if (is.factor(data) == TRUE) {
    data_obs <- length(data)
    data_na <- sum(is.na(data))
    unique_levels <- unique(data)
    bar_chart <- ggplot(data=data.frame(x=data), aes(x=x)) + geom_bar()
    l <- list(data_obs, data_na, unique_levels, bar_chart)
    names(l) <- c("obs", "na's", "unique_levels", "bar_chart")
  }
  return(l)
}
EDA_results <- list()
for (i in 1:ncol(train)) { #apply function to all variables and build a list to store all results 
  variable <- as.list(names(train[i])) 
  result <- univariate_analysis(train[,i])
  result <- append(variable, result)
  EDA_results[[length(EDA_results) + 1]] <- result
}
labels <- names(train)
names(EDA_results) <- labels 

#looking at significant variables 
# SAVBAL: right skewed, majority 0 values 
# MMBAL: right skewed 
# DEP: outliers that skew the data
# CDBAL: right skewed 
# CC: 1075 Nas
# DDABAL: right skewed
# INV: 1075 nas
# PHONE: majority in category 0
# CHECKS: right 
# IRABAL: right
# CCPURC: very little obs in 3 and 4 categories 
# ATMAMT: right 
# POS: right 




# NA table -----------

table4  = data.frame(matrix(nrow = 0, ncol = 2))
for (i in 1:ncol(train)) { #build table to look at variables with most NA's
  variable <- names(train[i])
  na <- sum(is.na(train[,i]))
  newrow <- c(variable, na)
  table4 <- rbind(table4, newrow)
}
columns= c("variable", "na")
colnames(table4) <- columns

table4$na <- as.numeric(table4$na)
table4 <- table4 %>% arrange(desc(na))
table_na <- table4 %>% filter(na > 0)
write_csv(table_na, "HW1_VarsNAs.csv")

# Bivariate EDA CONT VARS--------

##Continous variables 

#SAVBAL
# higher avg SAVBAL for INS = 1 
# those who have purchased INS have a higher savings acct balance on average
ggplot(train) + 
  geom_bar(aes(x=INS,y= SAVBAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
###

#MMBAL
# MMBAL avg higher for INS = 1
# Those with a higher MMBAL have purchased INS 
ggplot(train) + 
  geom_bar(aes(x=INS,y= MMBAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
###

#DEP
# DEP higher avg for INS = 0 
# INS = 1 have lower checking deposits avg 
ggplot(train) + 
  geom_bar(aes(x=INS,y= DEP), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#
wilcox.test(DEP ~ INS, data = train) #sig diff dist

#CDBAL 
# INS = 1 group has higher average CDBAL
# Those who have purchased the INS have a higher average CDBAL 
ggplot(train) + 
  geom_bar(aes(x=INS,y= CDBAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#right skewed so fails normality 
wilcox.test(CDBAL ~ INS, data = train) #distributions sig dif

#DDABAL 
# INS = 1 group has higher average DDBAL
# Those who have purchased the INS have a higher average checking acct balance 
ggplot(train) + 
  geom_bar(aes(x=INS,y= DDABAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#right skewed so fails normality 
wilcox.test(DDABAL ~ INS, data = train) #distributions sig dif

#PHONE
# higher phone average for INS = 0, lower avg for INS = 1
# those who have not purchased INS have higher averages of phone interactions with bank 
ggplot(train) + 
  geom_bar(aes(x=INS,y= PHONE), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
###

#HMVAL
# INS = 1 higher averag home value
ggplot(train) + 
  geom_bar(aes(x=INS,y= HMVAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#
ggplot(data = train, aes(sample = HMVAL)) + stat_qq() + stat_qq_line()
wilcox.test(HMVAL ~ INS, data = train) #looks small but dist are sig diff

#CHECKS
# INS = 1 lower number of checks written 
ggplot(train) + 
  geom_bar(aes(x=INS,y= CHECKS), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#
wilcox.test(CHECKS ~ INS, data = train) #sig diff

#IRABAL
# INS = 1 higher average IRA balance 
ggplot(train) + 
  geom_bar(aes(x=INS,y= IRABAL), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#
wilcox.test(IRABAL ~ INS, data = train) #sig diff

#ATMAMT
# total ATM withdrawal avg higher for INS = 1 group 
ggplot(train) + 
  geom_bar(aes(x=INS,y= ATMAMT), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#
wilcox.test(ATMAMT ~ INS, data = train) #distributions sig dif

#POS
# Avg # of point of sale interactions 
# INS = 1 have lower average point of sale interactions 
# test if this difference is sig
ggplot(train) + 
  geom_bar(aes(x=INS,y= POS), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 
#Test if this difference is different 
#POS is right skewed from EDA 
var.test(POS ~ INS, data = train)
#variance fails and normality fails: wilcoxon test left
wilcox.test(POS ~ INS, data = train) #distributions of group are sig. dif


#TELLER 
# not significant 
# those with INS = 1 have higher average # of teller visit interactions 
# would need to test if this difference is significant 
# weird that this is different than phone 
ggplot(train) + 
  geom_bar(aes(x=INS,y= TELLER), 
           position = "dodge", stat = "summary", fun = "mean") +                                      
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) 







# Bivariate EDA CATEGORICAL VARS ------

#CD 
# 
table(train$CD, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = CD))

table(train$DDA, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = DDA))

table(train$MM, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = MM))

table(train$SAV, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = SAV))

table(train$IRA, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = IRA))

table(train$ATM, train$INS)
ggplot(data = train) +
  geom_bar(mapping = aes(x = INS, fill = ATM))


