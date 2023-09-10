library(readr)
library(dplyr)
library(forecast)
library(tidyverse)
library(ggplot2)
library(car)

# Set & Load --------
setwd("/Users/renamarrotta/Desktop/IAA/Fall/Logistic Regression/Homework2_LR")
tbin <- read_csv("insurance_t_bin.csv")

#Variable Types & Missing -------
col_names <- names(tbin) #vector of col names
tbin <- tbin %>%
  mutate(across(all_of(col_names), as.character)) %>%  #convert character for next line
  replace(is.na(tbin), "M")  #replace any NA with "M"


# Check for Separation --------

#ad for loop here to identify which have separation 
#make a table for every variable with the target variable
# check: 0 %in% table if TRUE store variable name in a vector

table(tbin$INS, tbin$CASHBK)
tbin$CASHBK <- as.numeric(tbin$CASHBK)
tbin$CASHBK_c <- as.character(tbin$CASHBK)
tbin$CASHBK_c[which(tbin$CASHBK > 0)] <- "1+"
table(tbin$INS, tbin$CASHBK_c)

table(tbin$INS, tbin$MMCRED)
tbin$MMCRED <- as.numeric(tbin$MMCRED)
tbin$MMCRED_c <- as.character(tbin$MMCRED)
tbin$MMCRED_c[which(tbin$MMCRED > 2)] <- "3+"
table(tbin$INS, tbin$MMCRED_c)

col_names <- names(tbin)
tbin <- tbin %>%
  mutate(across(all_of(col_names), as.factor)) %>%
  select(-c(MMCRED, CASHBK))

#Remove indicator variables ?------
#did not run this 
tbin2 <- tbin %>% select(-c(CC, MTG, MM, ILS, INV, LOC, IRA, CD))

# Main effects model --------

full.model <- glm(INS ~ ., data = tbin, family = binomial(link = "logit"))

empty.model <- glm(INS ~ 1, data = tbin, family = binomial(link="logit"))

back.model <- step(full.model, scope = list(lower = empty.model, upper = full.model), 
                   direction = "backward", trace = FALSE, k = qchisq(0.0001, 1, lower.tail = FALSE)) 

summary(back.model)

# report and rank variables by pvalue (smallest to largest)

#Call:
#  glm(formula = INS ~ DDA + IRA + INV + ILS + MM + CC + DDABAL_Bin + 
#        CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + ATMAMT_Bin + CDBAL_Bin, 
#      family = binomial(link = "logit"), data = tbin)


# car::Anova(back.model, test = 'LR', type='III') #alias error bc Type III test
#alias(back.model) #perfect colinearity with CCM and INVM idk why
table(tbin$CC, tbin$INV) 

anova.bmodel <- car::Anova(back.model, test = 'LR') #Analysis of Deviance Table (Type II tests)

vars.bmodel <- rownames(subset(anova.model)) #extracts variable names from Anova output
df.bmodel <- anova.model[[2]] #extracts Df from Anova outpute
pvals.bmodel <- anova.model[[3]] #extracts Pvalues from Anova output
outputvals.bmodel <- data.frame(cbind(Variable = vars.bmodel, Df = df.bmodel, Pvalue = pvals.bmodel)) #creates a dataframe
outputvals.bmodel$Pvalue <- as.numeric(outputvals.bmodel$Pvalue) #make Pvalues numeric 
outputvals.bmodel <- arrange(outputvals.bmodel, Pvalue) #order smallest to largest by Pvalue


#Odds ratio interpretation of variables ------

exp(cbind(coef(back.model))) #extracts only coefficients of the back.model and exponentiate to get odds ratio

#Forward selection for interactions ------

int.fmodel <- glm(INS ~ (DDA + IRA + INV + ILS + MM + CC + DDABAL_Bin + CHECKS_Bin + TELLER_Bin + SAVBAL_Bin + 
                           ATMAMT_Bin + CDBAL_Bin)^2, data = tbin, family = binomial(link="logit"))

#warning: glm.fit: fitted probabilities numerically 0 or 1 occurred 

for.model <- step(back.model,
                    scope = list(lower=formula(back.model),
                                 upper=formula(int.fmodel)),
                    direction = "forward", trace = FALSE, k = qchisq(0.0001, 1, lower.tail = FALSE))
summary(for.model)
car::Anova(for.model)
