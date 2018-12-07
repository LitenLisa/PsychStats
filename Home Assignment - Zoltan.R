# Lund University - M.Sc. psychology -PSYP13                                     Course leader PSYP13: Zoltan Kekecs 
# Wintersemester 2018                                                            Student: Lisa Granath (li1636gr-s)
# 
# R version 3.5.1 (2018-07-02) -- "Feather Spray"
# Copyright (C) 2018 The R Foundation for Statistical Computing
# Platform: x86_64-w64-mingw32/x64 (64-bit)


########################################################################################################################
# Authors Note:  "R" is a complex program and in order to make it understandable for even a bloody beginner,           #
# I have decided to create this syntax as idiot proof as possible. I have chosen to do this                            #
# in order to learn how to create an easily understandable syntax and find a style which may serve me in the future.   #
# You may therefore choose to skip the first section which basically just gives an overview over the experiment, an    #
# introduction to the variables etc. Any other section which is not essential for the assignment will always be marked #
# and surrounded by a box of hashtags in order to make it easier and faster for you to read.                           #
########################################################################################################################


# ==================================Assignment 1 - Hierarchical regression model=======================================#



########################################################################################################################
###############################            SECTION 1           #########################################################
########################################################################################################################
#                                                                                                                      #  
# ########################  Simple experiment description:   ########################################################### 
# N=160 adults (surgical extraction of wisdom tooth)                                                                   #
# Participants were asked about sex, age, weight as well as anxiety, pain, catastrophizing, mindfulness,               #
# and blood serum +salivary cortisol levels taken 5 minutes before operation (which means that hightened               #
# cortisol levels should be found in (probably all) patients but especially patients prone to anxiety)                 #
#                                                                                                                      #
# #########################   Variables:   ############################################################################# 
# Apart from pain, sex, age and weight the following variables are in the sample:                                      #
#                                                                                                                      #
# "STAI_trait": STAI state trait anxiety inventory measures anxiery on a scale of 20-80                                #
# (higher scores mean higher anxiety). Correlates with the level of subjective pain experienced.                       #
#                                                                                                                      #
# "pain_cat": Pain catastrophizing Scale measures tendenty to magnify the threath value of a pain stimulus as well as  #
# feelings of helplessness when in pain and inabilit to prevent pain related thoughts before or during a               #
# painful event. Ranges from 0-52. Predictor of clinical pain.                                                         #
#                                                                                                                      #
# "mindfulness": The Mindful Attention Awareness Scale measures tendency to turn attention to present-moment           #
# experiences in an open, non-judgemental way. Ranges from 1-6 where higher scores represents higher                   #
# dispositional mindfulness. Protective factor                                                                         #
#                                                                                                                      #
# cortisol_serum: Cortisol as measured in blood serum. Cortisol is a stress hormone associated with acute and          #
# chronic stress. Cortisol levels are thought to be positively associated with pain experience.                        #
# Serum cortisol is often regarded in medical research as more reliably related to stress.                             #
#                                                                                                                      #
# cortisol_saliva: Cortisol as measured by saliva                                                                      #
#                                                                                                                      #      
###########################   Research question:   #####################################################################
# According to literature: Age is a negative predictor of pain and sex is a predictor moderated by the type of the     #
# procedure. Question: To determine the extent to which taking into account psychological and hormonal variables aside #
# from the already used demographic variables would improve our understanding of postoperative pain.                   #
#                                                                                                                      #  
###########################   What to do:   ############################################################################
# Create:                                                                                                              #
# Model 1: Model with age and sex as predictors of pain                                                                #
# Model 2: Build a new model with the predictors: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol      #
# measures.                                                                                                            #
#                                                                                                                      #
########################################################################################################################

#======================================================================================================================#
#################################################################################################################################################################
###################################################   Steps before we begin the analysis   ######################################################################
#################################################################################################################################################################
# 1: Set working directory

setwd("~/Semester 1/PSYP13")
getwd()

# 2: Read data set from github 

data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")

# 3: Load the packages needed for analysis 

library(dplyr)  
# information from ?dplyr and https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html : used when 
# working with data frames . Among others it helps us select cases based on their values but also to reorder the cases and 
# select variables based on their names.Also helps us add new variables which are functions of existing variables 
### "dplyr" might ask whether you want to install from source. if so, select 'n' for no.

library(psych)    
# information from ?psych: Contains functions most useful for personality and psychological research
# needed for the "describe" function

library(lm.beta)  
# from ?lm.beta: basic package used for linear modeling. Adds standardized regression coefficients to objects created by lm. 

library(car)      
# necessary to create residualPlots [see Navarro p. 485], vif, pairs.panels, ncvTest, scatter3d ect

library(ggplot2)  
# Creates data visualizations. Needed for ggplot

library(rgl)      
# needed for creating 3d visualizations in scatter3d

library(gsheet)   
# enable downloads of google sheets by using the URL

#################################################################################################################################################################
#############################################################   Variable Check:   ###############################################################################
#################################################################################################################################################################

describe (data_sample_1)                  # describe shows no missings.  
sum(is.na.data.frame(data_sample_1))      # confirms the same
summary(data_sample_1)

###################################################################################################################################################################
# To assure oneself of the no NA's, it is possible to check thee mean() functions which will only work if there are no NA                                         #
# If an NA is detected, this can be cleared by using: "mean(data_sample_1$Q2)help,na.rm = TRUE) code which will remove the NA before calculating the mean         #
# One can deal with missings by different methods or keeping the data set intact as it is. In R the easiest way is using the code:                                #
# data_set_1_example1 <- na.omit(data_sample_original).                                                                                                           #
# However, this should never be done in an arbitrary way as missing data also tells us something and can be analysed. If we have a high                           #
# dropout (mortality) rate on an intervention it will mean something. If we have a high dropout rate on a questionnaire, it will mean something too.              #
# If our data is missing at random, missing completely at random or if we have mode than 30%  missings for a participant or question this will all have           #
# implications, not only for the methods we use, but also for our analysis itself and the interpretation of our data                                              #
# The MCAR-test by Little is commonly used when concluding if the missing answers follow a particular pattern . If the datapool passes the test, the missing's    #
# could be replaced by using the expectation maximization-algorithm (EM-Algorithm). We could also use the multiple imputation method.                             #
# The preconditions for this imputative method are that the sample is large enough, the MCAR-test is passed and that not more than 30% of the values per          #
# items are missing (Tabernachnick & Fidell, 2007).                                                                                                               #
# As this data set however is complete, none of this is necessary and is only mentioned here for informatory purpose.                                              # 
###################################################################################################################################################################



#################################################################################################################################################################
##########################################################   Min. and Max. scores:   ############################################################################
#################################################################################################################################################################
# age = NOT OK
# shows the age ranging from Min: 27 to Max: 222 
# an obvious problem is the high maximum age. Lest we have set a new guinness world record in human age, there 
# is a flaw in the data. 
### Thus subject ID_28 will be omitted

# pain = OK
# "Min": 1(0=no pain) to "Max": 8.63 (10 =worst pain I can imagine)
# Values can either be rounded or kept as they are. Decision has been made to keep the values as they are (if there 
# had been more than 2 decimals, I would have rounded the values to the second decimal to fit APA standards)
# Values can always be rounded subsequently if this is necessary. 

# Sex = OK
# Male & Female coded with "1" and "2"
# Nothing out of the ordinary as min and max show only even numbers of one's and two's)

# pain_cat: OK
# Min:17(0=no pain catastrophizing) to Max:48(52=High catastrophizing)
# STAI_trait
# Min: Max:  

# mindfulness: Not OK  
# 0.12(1)-5.89(6)
# The values which are below zero are not ok 
# Thus ID_116 & ID_146 will be omitted 

# cortisol_serum: (probably***) OK 
# Min:2.72 to: 7.30

##########################################################################################################################
# ***                                                                                                                    #
# Scale is not described in the assignment. However there are no negative values, and the arithmetic mean of 5 would     #
# imply an elevated cortisol level which is to be expected before surgery. Comparing the two means of cortisol           #
# saliva(4.98) and serum(5) also indicate no problem since 4.98 and 5 are basically the same.                            #
#                                                                                                                        #
# windows()                                                                                                              #
# hist(data_sample_1$cortisol_serum)                                                                                     #
# windows()                                                                                                              #
# hist(data_sample_1$cortisol_saliva)                                                                                    #
#                                                                                                                        #
# A rough visual comparison of the two distributions also indicate that that is no obvious problem since we have two     #
# nice and uniform bell curves. This is also further confirmed when we look at the serum compared to saliva variance     #
# If saliva is an inferior measurement tool, we would expect a higher variance compared with the serum and this is       #
# also exactly what we find.                                                                                             #
##########################################################################################################################

###########################   Deeper look into cortisol:   ###############################################################
# from:https://www.urmc.rochester.edu/encyclopedia/content.aspx?contenttypeid=167&contentid=cortisol_serum               # 
# Knowing your data and what is measures is crucial to any analysis. Since the assignment mentions none of these facts   #
# concerning cortisol, here are a few points to keep in mind when dealing with cortisol measurements                     # 
# It is important to note that the cortisol levels fluctuate naturally during the day                                    #
# with higher values in the morning and sinking values in the evening. However, it is not strictly necessary at this     #
# point to take this into consideration. This only becomes important when evaluating the data as higher                  #
# cortisol levels not strictly imply elevated stress levels, it could be that the surgery is just                        #
# taking place really early in the morning. It should thus always be made clear that cortisol is measured and compared   #
# with the normal base line within the range of that time of the day                                                     #
##########################################################################################################################

# cortisol_saliva (probably) OK
# Min:2.40 to Max: 7.67
# see cortisol_serum 

# weight min/max = OK
## Ranges from 47.81.87-100.61 which gives no indication that something is amiss

#################################################################################################################################################################
##############################################################   cleaning the data   ############################################################################
#################################################################################################################################################################

# N=160 is a fairly large data set and excluding three participant can be done without any large consequence
data_sample_2<-data_sample_1[-c(28,112,146), ] # remove all rows with the errors identified above
rownames(data_sample_2) <- 1:nrow(data_sample_2)   # fix the row numbers after the previous deletion,otherwise later 
# functions or calculations will not work correctly and create an output with conflicting row numbers
 
################################################################################################################################################################# 
##########################################################  Control that New Data Set works #####################################################################
#################################################################################################################################################################

View(data_sample_2)      
describe(data_sample_2)
summary(data_sample_2)

# all scores look normal
# optional: remove the old data sample in the environment using: 
# rm(data_sample_1)

#################################################################################################################################################################
###########################################################  Identifying Extreme Univariate Outliers   ##########################################################
#################################################################################################################################################################

# pain:
describe(data_sample_2$pain)
summary(data_sample_2$pain)

# It is not necessary, but out of curiosity one can always make a normality check as well as a boxplot 

shapiro.test(data_sample_2$pain) # value is as expected not significant  
# This function returns a list object with a p.value. For pain, the p-value is clearly non-significant and higher than 0.05 
# (data:  data_sample_2$pain, p-value = 0.91)
# This shouldn't come as a surprise since the distribution is clearly uniform

hist(data_sample_2$pain, main="Pain Score", density= 20, angle=50, ylin=c(0,10), xlab="Pain Score", las=1, border="blue", col="blue", labels=TRUE)

boxplot(data_sample_2$pain)      # no outliers outside 1.5 IQR, nice and even distribution clearly visible

# sex:
summary(data_sample_2$sex)       # shows a sample of 85 females and 72 males which is a sign of an even and good sample
plot(data_sample_2$sex)       

# age:
summary(data_sample_2$age)
describe(data_sample_2$age)
hist(data_sample_2$age,main="Age",breaks=10,density=20,angle=50,ylim=c(0,62),xlim=c(25,60),xlab="Age - Years",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_2$age)  # normality assumption met (p=.16)
boxplot(data_sample_2$age)       # Only one univariate outlier outside of 1.5IQR. Discussion wherther to remove or keep this outlier can be justified either way. 
                                 # When one consideres whether one person over the age of 52 matters or not for the statistics,  there are at least two ways to 
                                 # argue. 
                                 # 1) the person should be removed because age is an important variable for the calculation and for that we will need to
                                 # have as nice and uniform age distribution as possible in the sample. 
                                 # 2) The sample is already very nice and uniform, one outlier will not matter too much to the statistical calculations. 
                                 # I have chosen to go with the second option. It seems to me to be important to keep this one older person especially because 
                                 # we are looking at age as a factor. A good rule of thumb is to always keep the data set as intact as possible.

# STAI_trait:
summary(data_sample_2$STAI_trait)
describe(data_sample_2$STAI_trait)
hist(data_sample_2$STAI_trait,main="State Trait Anxiety Inventory",density=20,angle=50,ylim=c(0,62),xlim=c(25,55),xlab="State Trait Anxiety Inventory Score",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_2$STAI_trait)  # Normality assumption not met: p-value = 0.03
boxplot(data_sample_2$STAI_trait)       # Several potential univariate outliers outside 1.5 IQR, however unclear if something should be done about all of them. 
                                        # Considering that none of the outliers are really abnormal (STAI measures trait anxiety on a scale of 20 to 80) there is a 
                                        # possibility to keep all of them. However, looking at the values of for example ID_90 we can see some inconcistencies.
                                        # ID_90 has a low pain_cat score and a low STAI score. However, the cortisol serum is unusually high (6.17) and so is the 
                                        # saliva cortisol (5.59). Thus, at least this subject can be removed

data_sample_3<-data_sample_2[-c(89), ]               # remove all rows with the errors identified above
rownames(data_sample_3) <- 1:nrow(data_sample_3)     # fix the row numbers

# view the data 
View(data_sample_3)      
describe(data_sample_3)
summary(data_sample_3)                                    

# Redo the normality test
summary(data_sample_3$STAI_trait)
describe(data_sample_3$STAI_trait)
hist(data_sample_3$STAI_trait,main="State Trait Anxiety Inventory",density=20,angle=50,ylim=c(0,62),xlim=c(25,55),xlab="State Trait Anxiety Inventory Score",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_3$STAI_trait)  
boxplot(data_sample_3$STAI_trait) 

# The shapiro-wilks test now show a p-value of 0.07 which is marginally above the required 0.5. 
# Since none of the other values are abnormal, we'll keep them as they are. 

# pain_cat:    
summary(data_sample_3$pain_cat)
describe(data_sample_3$pain_cat)
hist(data_sample_3$pain_cat,main="Pain Catastrophizing Scale",breaks=10,density=20,angle=50,ylim=c(0,30),xlim=c(15,45),xlab="Pain Catastrophizing Scale",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_3$pain_cat)  # normality assumption met p= 0.17
boxplot(data_sample_3$pain_cat)       # a single extreme outlier can be found outside of 1.5 IQR. In this case it might indicate an unrealistic score 
# This is due to the fact that the PCS manual http://sullivan-painresearch.mcgill.ca/pdf/pcs/PCSManual_English.pdf gives the following information 
# about what we can expect normal tests values to be:
#     Mean 20.90
#     Median 20.00
#     Std. Dev. 12.50
# This means that the values is off by roughly 10 points, meaning that this value would equal just 5 points in reality.
# Thus we can remove person ID_45

data_sample_4 <- data_sample_3[-c(44),]               # removing Id_45 with rownumber 44
rownames(data_sample_4) <- 1:nrow(data_sample_4)      # once again fixing the rownames
View(data_sample_4)                                   # viewing the new data sample
describe(data_sample_4)                               # describing the new data sample
summary(data_sample_4)                                # summarizing the new data sample
shapiro.test(data_sample_4$pain_cat)                  # redoing the Shapiro-Wilks test with a p=0.60 meaning that the normality assumption has been met

#  cortisol_serum
summary(data_sample_4$cortisol_serum)
describe(data_sample_4$cortisol_serum)
hist(data_sample_4$cortisol_serum,main="Cortisol Serum",density=20,angle=50,ylim=c(0,62),xlim=c(2,8),xlab="Cortisol Serum",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_4$cortisol_serum)  # normality assumption  has been met with a p=0.84 value  
boxplot(data_sample_4$cortisol_serum)       # no outliers

#  cortisol_saliva:
summary(data_sample_4$cortisol_saliva)
describe(data_sample_4$cortisol_saliva)
hist(data_sample_4$cortisol_saliva,main="Cortisol Saliva",breaks=10,density=20,angle=50,ylim=c(0,62),xlim=c(2,8),xlab="Cortisol Saliva",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_4$cortisol_saliva)  # normality assumption has been met with a p=0.76 value
boxplot(data_sample_4$cortisol_saliva)       # two outliers 
                                             # Since there is no information on how to interpret the values of the saliva cortisol measures 
                                             # (data found on internet is contradictory), it is difficult to assess if we should remove or keep the two outliers. 
                                             # However, as the normality assumption has been met, and also nothing else is amiss, I have chosen to keep the values. 

#  mindfulness
summary(data_sample_4$mindfulness)
describe(data_sample_4$mindfulness)
hist(data_sample_4$mindfulness,main="Mindfulness",density=20,angle=50,ylim=c(0,62),xlim=c(1,6),xlab="Mindfulness Score",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_4$mindfulness)  # Normality assumption has been met with a p=0.38 value 
boxplot(data_sample_4$mindfulness)       # Two outliers outside 1.5 IQR have been detected
                                         # Only one of the two outliers can be regarded as a bit extreme as it rates 5.9 on the MAAS scale which has 6 as its
                                         # maximum. Looking at the other scores gives the indication that we might be looking at a regular meditator as both 
                                         # cortisol levels are low and the other measures imply a rather normally anxious person. Based in this I have
                                         # chosen to keep data set as it is and not exclude the outlier.

#  Weight:
summary(data_sample_4$weight)
describe(data_sample_4$weight)
hist(data_sample_4$weight,main="Weight",density=20,angle=50,ylim=c(0,62),xlim=c(40,95),xlab="Weight in kg",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_4$weight)  # Normality assumption has been met p=0.47
boxplot(data_sample_4$weight)       # Only one outlier outside of 1.5 IQR towards the lower end
                                    # Value of 100 kilo seems plausible, no reason to exclude it.

#################################################################################################################################################################
################################################   Optional: Cleaning the Environment before Next Step ##########################################################
#################################################################################################################################################################
# It might be a good choice to clear upp the environment a bit before we move on to the regression model:                                                       #
# rm(data_sample_1)                         # removes data_sample_1-3 from the environment                                                                      #
# rm(data_sample_2)                                                                                                                                             #
# rm(data_sample_3)                                                                                                                                             #
#################################################################################################################################################################






#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#################################################################################################################################################################
######################################################  Research Question 1  ####################################################################################
#####################################   Regression Model 1 & 2 - Age and Sex as Predictors of Pain  #############################################################
#################################################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#


#################################################################################################################################################################
# According to the assignment, this part will deal with the following:                                                                                          #     
# 1: So a hierarchical regression, building a model containing age and sex as predictors of pain (model 1),                                                     # 
# 2: Create a new model with the predictors: age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures (model 2).                                # 
# 3) Do a model comparison to assess whether substantial new information was gained about pain in model 2 compared to model 1.                                  #
#                                                                                                                                                               #
# What to report:                                                                                                                                               #    
# 1)Run Data and Model Diagnostics.                                                                                                                             #
#         First, check the variables included in model 2 (age, sex, STAI, pain catastrophizing, mindfulness, and cortisol measures as predictors,               #
#         and pain as an outcome) for coding errors, and the model itself for influential outliers (for example using Cook's distance).                         #
#         Furthermore, check the final model to see if the assumptions of linear regression hold true, that is,                                                 #
#                  normality (of the residuals),                                                                                                                #
#                  linearity (of the relationship),                                                                                                             #
#                  homogeneity of variance (also called homoscedasticity) and                                                                                   #
#                  that there is no excess multicollinearity.                                                                                                   #
#                                                                                                                                                               #
# Reporting the results of model 1 and model 2:  Model test statistics R2, F, df, and p-value                                                                   #
#                                             :  Statistics describing the coefficients of the predictors in a table format                                     #
#                                                Unstandardized regression coefficients and 95% confidence intervals, standardized regression                   #
#                                                Coefficients b and beta-values, and p-values.                                                                  #
################################################################################################################################################################# 

################################################################################################################################################################# 
#################################################################  Creating regression model 1  #################################################################
################################################################################################################################################################# 
# Indicator or "dummy" variables are used in most cases where a variable only can take on two values. In this case, sex ought to be changed to an indicator 
# variable since it will make it more easy to calculate with while dealing with regressions. Remember that the first variable shown in every table will be male, 
# and the second one will concern females. In a regression, "male" will be the intercept b0, i.e. our b1 will be female. When our subject thus, is a female, the 
# regression will state the intercept as equal to 0, which means that it is dropped from the function and only the female remains.
# Please note that sex in its original state is a factor with two levels, changing it to a dummy variable will cause no large changes, this is only done
# in order to demonstrate all the cases where dummy variables are superior and ought to be used. 
# Source, (Steyer 2007)

data_sample_4$sex_I[data_sample_4$sex=="male"] <- 0        # Changes the value of the variable to male = 0
data_sample_4$sex_I[data_sample_4$sex=="female"] <- 1      # changes the value of the variable to female = 1

#
regression_m1 = lm(pain ~ sex_I + age, data=data_sample_4)

#The following functions are normally used when reporting on a regression

regression_m1             # Shows coefficients
summary(regression_m1)    # Also shows coefficients but offers more information than the above mentioned function  
AIC(regression_m1)        # Akaike's entropy-based Information Criterion: Is an estimator of the relative quality of statistical models for a given set of data. 
                          # AIC can estimate the quality of a model, relative to each of a set of other models. In this way it is a means for model selection
                          # Source: https://en.wikipedia.org/wiki/Akaike_information_criterion 
                          # Lower AIC values will sugggest a better model (Source:https://www.r-bloggers.com/how-do-i-interpret-the-aic/)
confint(regression_m1)    # Computes confidence intervals for one or more parameters in a fitted model. 
lm.beta(regression_m1)    # Adds standardized regression coefficients to objects created by the linear modeling function lm

################################################################################################################################################################# 
#######################################################  Identifying influential points via Cook's distance   ###################################################
################################################################################################################################################################# 

# Cook's distance cutoff value for identifying influential points is normally calculated by using the 4/N rule of thumb i.e. 4/154 
CD_cutoff<- 4/155

windows()
plot(x=regression_m1, which=4, las=1,main="Cook's Distance - Model 1")             # Calculates Cook's distance plot for model 1
abline(h = CD_cutoff, lty=2)                                                       # Function adds one or more straight lines through the current plot.
# Result: 9 points influence more or less greatly on the calculated coefficients in our regression model

# Every regression model will automatically contain statements about the residuals(R. Steyer - Wahrscheinlichkein & Regression, 2002). 
# Therefore, to check the normality of the residuals we can use a QQplot
windows()
plot(regression_m1, which=2, las=1)  # function shows some residuals in the negative end of the spectrum and a few residuals on the positive end
                                     # the linear appearance of the plot suggests that the data are normally distributed

# The histograms can give further informaton on the matter: 
describe(residuals(regression_m1))    # appearance is that of a normal distribution bell curve

# When checking for normality the unstandardized residuals are normally used
windows()
hist(residuals(regression_m1), breaks=20)
hist(residuals(regression_m1), breaks=20, main="Histogram of Residuals - Regression Model 1", density=20, angle=50, ylim=c(0,30), xlim=c(-4,4), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="blue", labels=TRUE)

# The histogram for standardized residuals will look slightly different
windows()
hist(rstandard(regression_m1), breaks=12, main="Histogram of Standardized Residuals - Regression Model 1", density=20, angle=50, ylim=c(0,40), xlim=c(-4,4), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="blue", labels=TRUE)

shapiro.test(residuals(regression_m1)) # No significant deviency from a normal distribution p=0.77

shapiro.test(rstandard(regression_m1)) # As a cautionary measure we can also look at shapiro test for the the standardized values of the residuals p=0.76

################################################################################################################################################################# 
############################################################ Linearity assumption check #########################################################################
#################################################################################################################################################################


#################################################################################################################################################################
########################################################### General information concerning GLM   ################################################################
#################################################################################################################################################################
# Source (steyer, 2002)                                                                                                                                         #
# Regression is all about predicting the value of a phenomenon using another predictor. It is thus about the relation between two variables. A multiple         #
# attempt the same, just that the number of regressors (predictors) can be as large as we want                                                                  #  
# Multiple regression can ( as well as any other linear regression) be parameterized using the so called general linear model (GLM) with the formula            #
# y=b0+b1X+b2X2+...+                                                                                                                                            #
# where                                                                                                                                                         # 
# y = a vector with the length of N                                                                                                                             #
# X = a design matrix consisting of nxn realixed fixed with a length of Nx(m+1), thus not a random variable                                                     #  
# B = a vector of not known constants which we have to guess using the model. This vector starts with the intercept of ??0 and is thus not a random variable    #
# e = a random variable consisting of the residuals                                                                                                             #
# we assume that the residuals follow a normal distribution and that their expected values are 0 as well as that the variance of the residuals show no          # 
# dependence                                                                                                                                                    #   
# Thus, if we using the assumptions above when we are asking for the expected value of the vector "y" we will get (E(y))=XB (this is because E(e)=0))           #
# We are thus stating that our observed numbers are non-dependent of each other (are not a linear combination of one another (multicolinearity)), that the      #
# residuals are non-dependant(have the same variance --> homoscedacity assumption) and therefore the y's will vary in the same way as the residuals around      #
# the XB's.                                                                                                                                                     #
# If these assumptions are not met, we have to take measures in our calculations, since the heteroscedacity causes our estimated standard error of the          #
# coefficients to become too large. (Source: http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis)           #                                                                                                           #
# The Huber-White test could then be an alternative (source: https://www.politikwissenschaften.ch/pdf.php?id=23)                                                #
#################################################################################################################################################################

# Predicted values compared with actual values
pred_m1 <- predict(object=regression_m1)                              
plot(x=pred_m1, y=data_sample_4$pain, xlab="Fitted Values", ylab="Observed Values", las=1)          # semi-linear appearance of the plot

# Predicted values compared with residuals
plot(regression_m1, which = 1)
# Residual plot for each predictor from the "car" package. This returnins the results of the linearity tests
residualPlots(regression_m1,las=1)    # linearity assumption met

################################################################################################################################################################# 
###########################################   Checking Assumption concerning the Variance Homogeneity    ########################################################
#################################################################################################################################################################

ncvTest(regression_m1)                    # p = 0.87 meaning that the homoscedasticity assumption has been met
plot(regression_m1, which=3, las=1)       # no pattern recognizable

#################################################################################################################################################################
###########################################################   Multicolinearity Check  ###########################################################################
#################################################################################################################################################################
# Source https://statisticalhorizons.com/multicollinearity. VIF= "variance inflation factor estimates how much the  variance of a coefficient is "inflated" 
# because of linear dependence with other predictors. Thus, a VIF of 1.8 tells us that the variance (the square of the standard error) of a particular coefficient 
# is 80% larger than it would be if that predictor was completely uncorrelated with all the other predictors"
# The lowe bound if 1, however scientists disagree on the upper bound of the VIF: 
# This paper recommends a threshold of 3 http://onlinelibrary.wiley.com/doi/10.1111/j.2041-210X.2009.00001.x/full
# while this blog states that a VIF with a level of 2.5 is enough to cause concern http://blog.minitab.com/blog/understanding-statistics/handling-multicollinearity-in-regression-analysis

# The VIF function 
vif(regression_m1)                        
pairs.panels(data_sample_4[,c("pain", "sex_I", "age")], col="blue", lm=TRUE)
# Result no multicolinearity
#           sex:         age: 
# VIF      1.01          1.01

#################################################################################################################################################################
################################################################   Scatter plot 3d  #############################################################################
#################################################################################################################################################################

# Scatter3d plotting  requires the rgl package
# scatter3d(pain ~ sex_I + age, data=data_sample_4)     # will not work if not the sex variable isn't transposed into an indicator variable first (sex_I)
# scatter3d(pain ~ sex_I + age, data = data_sample_4)
# scatter3d(pain ~ weight + age, data=data_sample_4)

# If not needed anymore, the sex_I variable can then be removed again using 
# data_sample_4<- data_sample_4[,-11]

#################################################################################################################################################################
###########################################################   Creating Regression Model 2  ######################################################################
#################################################################################################################################################################
#        REGRESSION MODEL 2 - [7 predictors] pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness

regression_m2 = lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + cortisol_saliva + mindfulness, data=data_sample_4)

summary(regression_m2)
AIC(regression_m2)
confint(regression_m2)
lm.beta(regression_m2)

################################################################################################################################################################# 
#######################################################  Identifying Influential Points via Cook's Distance   ###################################################
################################################################################################################################################################# 
windows()
plot(x=regression_m2, which=4, main="Cook's Distance - Model 2")   # Cook's distance plot for model 2
abline(h=CD_cutoff, lty=2)

# Shows 6 outliers with a comparatively high influence on the calculated coefficients in our regression model.
# The discussion of what to keep and what to exclude is as always difficult to answer. 
# on p. 480 lsr in the book by Navarro it says that a Cook's distance of 1 can be an alternative cut-off for extreme outliers. Other sources will 
# cite other criterions.
# A general rule of thumb is that removal should always be well considered and not be used in an arbitrary way. 
# Based on the fact that only one of the  outliers reach 1, but that the others seem believable and not false, 
# I have chosen to keep the data set otherwise intact and only remove ID_69

data_sample_5 <- data_sample_4[-c(67),]               # removing Id_69 with rownumber 67
rownames(data_sample_5) <- 1:nrow(data_sample_5)      # fix the row numbers after the previous deletion
View(data_sample_5)                                   # viewing the new data sample
describe(data_sample_5)                               # describing the new data sample
summary(data_sample_5)                                # summarizing the new data sample


################################################################################################################################################################# 
############################################################ Linearity Assumption Check #########################################################################
#################################################################################################################################################################

# QQplot
windows()
plot(regression_m2, which=2)                   # Some outliers on the lower end of the spectrum but no sign for concern 

# Checking residuals via describe() and histogram:
describe(residuals(regression_m2))             # Confirms the QQ-plot

# histograms of residuals 
hist(residuals(regression_m2), breaks=12, main="Histogram of Residuals - Regression Model 2", density=20, angle=50, ylim=c(0,40), xlim=c(-3,3), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="blue", labels=TRUE)
shapiro.test(residuals(regression_m2))         # normality assumption met p=.69

# Predicted values compared to actual values
pred_m2 <- predict(object=regression_m2)
plot(x=pred_m2, y=data_sample_5$pain, xlab="Fitted Values", ylab="Observed Values")

# Predicted values compared to residuals
plot(regression_m2, which=1)

# Residual plot for each predictor from the "car" package. This returnins the results of the linearity tests
residualPlots(regression_m2)    # Linearity assumption met

################################################################################################################################################################# 
###########################################   Checking Assumption concerning the Variance Homogeneity    ########################################################
#################################################################################################################################################################
ncvTest(regression_m2)          # p = 0.91 means that the homoscedasticty assumption is met
plot(regression_m2, which=3)    # shows a very similar appearance with the first model

#################################################################################################################################################################
###########################################################   Multicolinearity Check  ###########################################################################
#################################################################################################################################################################

vif(regression_m2)
# As expected, cortisol_serum and  cortisol_saliva have a VIF which is above 5. Since cortisol_serum has a lower VIF value and is according to the literature
# a more reliable measure, I have chosen to keep it and exclude cortisol_saliva

pairs.panels(data_sample_5[,c("pain", "sex_I", "age", "STAI_trait", "pain_cat", "cortisol_serum", "cortisol_saliva", "mindfulness")], col="blue", lm=TRUE)  

summary(regression_m2)
AIC(regression_m2)                              # A value of 479.76 for the first model, in comparison to 476.6 for the second model, could indicate
                                                # that the second model is the better one
confint(regression_m2)
lm.beta(regression_m2)

# The decision on which variable to keep, can be further justified by checking how much variance each of them explains
regression_m2.test1 = lm(pain ~ cortisol_saliva, data=data_sample_4)
regression_m2.test2 = lm(pain ~ cortisol_serum, data=data_sample_4)

summary(regression_m2.test1) # Multiple R-squared:  0.22,	Adjusted R-squared:  0.21  # the adjusted R^2 values takes the number of the predictors into account 
                                                                                     # and corrects the value accordingly
summary(regression_m2.test2) # Multiple R-squared:  0.23,	Adjusted R-squared:  0.23

rm(regression_m2.test1)       # removal from the global environment 
rm(regression_m2.test2)

#################################################################################################################################################################
#################################################   Creating Regression Model with 6 variables  #################################################################
#################################################################################################################################################################

# pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness
regression_m2.1 = lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=data_sample_5)
summary(regression_m2.1)
AIC(regression_m2.1)                              # AIC value of 476.77 indicates that this model is almost as good as the more complex second model with 7 variables
confint(regression_m2.1)
lm.beta(regression_m2.1)

################################################################################################################################################################# 
#######################################################  Identifying Influential Points via Cook's Distance   ###################################################
################################################################################################################################################################# 
windows()
plot(x=regression_m2.1, which=4, las=1, main="Cook's Distance - Model 2.1")   # Cook's distance plot for model 2.1
abline(h=CD_cutoff, lty=2)                                                    # 6 influential points detected, none of them worthy of an action of removal since 
                                                                              # none reach the >1 limit. 

################################################################################################################################################################# 
############################################################ Linearity Assumption Check #########################################################################
#################################################################################################################################################################

# QQplot
windows()
plot(regression_m2.1, which=2, las=1)                      # not completely unexpected, this QQ-plot suggest the same patterns as the previous ones

# Checking residuals via the describe() function and histogram:
describe(residuals(regression_m2.1))                       # As expected based on QQ plot once again hardly and skew/kurtosis at all

windows()
hist(residuals(regression_m2.1), breaks=12, main="Histogram of Residuals - Regression Model 2.1", density=20, angle=50, ylim=c(0,40), xlim=c(-3,3), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="blue", labels=TRUE)
shapiro.test(residuals(regression_m2.1))         # Normality assumption has been met p=0.79

# predicted values compared to actual values
pred_m2.1 <- predict(object=regression_m2.1)
plot(x=pred_m2.1, y=data_sample_5$pain, xlab="Fitted Values", ylab="Observed Values")

# predicted values compared to residuals
plot(regression_m2.1, which=1)

# Residual plot for each predictor from the "car" package. This returns the results of the linearity tests
windows()
residualPlots(regression_m2.1)

################################################################################################################################################################# 
###########################################   Checking Assumption Concerning the Variance Homogeneity    ########################################################
#################################################################################################################################################################
ncvTest(regression_m2.1)                    # normality assumption met p=0.81
plot(regression_m2.1, which=3, las=1)       # appearance similar to other plots


#################################################################################################################################################################
###########################################################   Multicolinearity Check  ###########################################################################
#################################################################################################################################################################
vif(regression_m2.1)                                # no cause for concern, no multicolinearity detected
pairs.panels(data_sample_5[,c("pain", "sex_I", "age", "STAI_trait", "pain_cat", "cortisol_saliva", "mindfulness")], col="blue", lm=TRUE)


#################################################################################################################################################################
#################################################################   Model Comparison  ###########################################################################
#################################################################################################################################################################

AIC(regression_m1,regression_m2.1)    

summary(regression_m1)

# Call:
#   lm(formula = pain ~ sex_I + age, data = data_sample_4)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -3.1911 -0.9226  0.0497  0.9208  3.2972 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)  9.11392    0.82358  11.066  < 2e-16 ***                      # When looking at the sig. test for the coefficients, the realized values are always
                                                                             # used, not the standardized coefficients
#  sex_I       -0.25246    0.21359  -1.182    0.239    
#  age         -0.10364    0.01997  -5.189 6.67e-07 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.321 on 152 degrees of freedom
# Multiple R-squared:  0.1533,	Adjusted R-squared:  0.1422 
# F-statistic: 13.76 on 2 and 152 DF,  p-value: 3.205e-06

summary(regression_m2.1)

# Call:
# lm(formula = pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data = data_sample_5)
 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -3.07412 -0.80697  0.02272  0.77872  2.73347 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     4.19787    1.39805   3.003  0.00315 ** 
# sex_I          -0.31301    0.18117  -1.728  0.08615 .  
# age            -0.06942    0.01927  -3.603  0.00043 ***
# STAI_trait     -0.01647    0.02673  -0.616  0.53880    
# pain_cat        0.05892    0.02533   2.326  0.02138 *  
# cortisol_serum  0.64959    0.12248   5.304 4.09e-07 ***
# mindfulness    -0.24209    0.11111  -2.179  0.03094 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.107 on 147 degrees of freedom
# Multiple R-squared:  0.4247,	Adjusted R-squared:  0.4012 
# F-statistic: 18.08 on 6 and 147 DF,  p-value: 1.188e-15

anova(regression_m1) 
# Analysis of Variance Table
# 
# Response: pain
#            Df  Sum Sq Mean Sq F value    Pr(>F)    
# sex_I       1   1.058   1.058  0.6064    0.4374    
# age         1  46.971  46.971 26.9228 6.674e-07 ***
# Residuals 152 265.186   1.745                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(regression_m2.1)

# Analysis of Variance Table
# Response: pain
#                 Df  Sum Sq Mean Sq F value    Pr(>F)    
# sex_I            1   1.042   1.042  0.8500 0.3580678    
# age              1  47.063  47.063 38.3945 5.516e-09 ***
# STAI_trait       1  25.004  25.004 20.3983 1.282e-05 ***
# pain_cat         1  14.174  14.174 11.5634 0.0008663 ***
# cortisol_serum   1  39.904  39.904 32.5537 6.174e-08 ***
# mindfulness      1   5.819   5.819  4.7473 0.0309376 *  
# Residuals      147 180.190   1.226                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# the anova comparing the two regression models in one go, cannot be performed in this form when comparing differently sized subsets of a data sample. 
# This is because of the fact that the variance will inevitably change with every different measure we delete or decide to keep. At the end we deal with two 
# different regressions. 
# This is the reason why we want to keep our dataset as clean from extreme outliers as possible, but also, not change too much and create the effects we want to
# see based on a "perfect" datasample, which inevidently does not exist in the messy real world. 
# After having spent quite some time on this problem and none of my attempts at solutions have worked, I must now, on the last evening forfeit and admit defeat 
# in this matter, at least for now. I have therefore showed the two anovas here, presented side by side. I would be really glad if you could give me a tip on how 
# to solve it. 
# I guess I have to change one of the models and turn it into a mixed multilevel model which allows for separate intercepts. I have had experience doing this, 
# but not in R, only in Lavaan using EffectLite.
# The entire assignment also seems a bit convoluted to me. Testing the linearity like this, as encouraged by the book, requires quite many steps as in comparison 
# to other methods. This particular assignment gives the feeling that we are basically assuming that the linear assumptions should all be met instead of actively 
# using the much more efficient statistical one-step methods which are out there to actually test it. 
# Going though it step by step might be nice as an assignment to get what we are actually doing, however, it is easy to drown in the 
# codes and forget the theory. Learning to properly use the tools, really requires a deep understanding of the basics, something I was fortunate enough to be given
# during 5 semesters at nightmare speed at my last university. However, here I find it failing, because the tool, in this case "R" falls short, or rather
# my knowledge of the endless amount of codes and techniques out there falls short. Its like being back in first grade and use your fingers to calculate
# instead of using the fancy calculator.
# Enough ranting.... 

# A less elegant way of going around it will be to change the regression formulas and base both on the same sample, i.e. 
# regression_m2.1 = lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=data_sample_5)
# regression_m1.1 = lm(formula = pain ~ sex_I + age, data = data_sample_5)
# doing this will yield the following answer:
# Analysis of Variance Table
#  
# Model 1: pain ~ sex_I + age
# Model 2: pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness
#   Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
# 1    151 265.09                                  
# 2    147 180.19  4    84.901 17.316 1.166e-11 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Please note that I am not saying that this is the solution to the problem, but it is at least a way to get around it and to get a result.

#===============================================================================================================================================================#
#===============================================================================================================================================================#
#################################################################################################################################################################
###############################################   Result Assignment 1   #########################################################################################
#################################################################################################################################################################

# Based on the calculations above. it is easy to see that just as expected, model 2.1 is a significant improvement over model 1
# with an F= 18.2(6,147), p<0.01, Adj R^2=0.40., AIC 476.7744.  
# Model 2.1 is able to explain 40.3% of the variance, compared to just 14.4 % for model 1 which has an F=13.84(2,151), p<0.01, Adj. R^2=0.14

# Regression equation model for model 2.1 
# pain= Ix=0(male)   + b1*Ix=1(female) + b2*age        +b3*Stai_trait  +b4*pain_cat    +b5*cortisol_serum     +b6*mindfulness
# Y =    4.00         + 0.32*X1         +(-0.08)X2      +(0.06)*X3      +0.06*X4        +0.62*x5               +(-0.25)*x6

# optional: remove unnecessary files in the environment
# rm(regression_m1)
# rm(regrsssion_m2)
# rm(data_sample_1)
# rm(data_sample_2)
# rm(data_sample_3)
# rm(data_sample_4)

#===============================================================================================================================================================#
#===============================================================================================================================================================#








#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#################################################################################################################################################################
######################################################  Research Question 2  ####################################################################################
#################################################   Backward Regression Model   #################################################################################
#################################################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

#IMPORTANT: see to that assignment 1 has been run before running this part. Do this as we are using some of the same data samples as well as the same 
# packages in this part of the assignment. 

# Goal: Compare the predicted values from model 2.1 with the actual pain ratings from the new data file home sample 2. 
# Which model was able to predict the actual pain ratings in data file 2 better?

# Full regression model in assignment 1 = "theory-based model"
# 1) Run a new regression model using the predictors that were retained in the end of the backward regression: modelname ="backward model"
# 2) Compare the backward model and the theory-based model based on AIC and using the anova() function.
# 3) Put the two models to the test on new data: home sample 2. N=160
# Data file 2: 'home_sample_2.csv', 
# https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv
# 4) On data file 2, make predictions on pain using the regression models or equations of the backward model and the theory-based model 
# IMPORTANT: do not fit the regression models on data file 2 (don't re-train your models), just use the regression equations that you derived based on data file 1. 
# These regression equations should be applied on the new data (data file 2), to predict pain.) 
# 
#################################################################################################################################################################
#######################################     Screening data_sample_4 for missing values, errors, outliers and irregularities #####################################
#################################################################################################################################################################
# No changes. All cases which were excluded remain excluded: N=154 see row 362 for more information
# codes for weight performed again. see assignment 1 for more discussion on these measures.

summary(data_sample_5$weight)
describe(data_sample_5$weight)
hist(data_sample_5$weight,main="Weight",density=20,angle=50,ylim=c(0,62),xlim=c(40,95),xlab="Weight in kg",las=1,border="blue",col="blue",labels=TRUE)
shapiro.test(data_sample_5$weight)  # Thw normality assumption has been met p=0.51
boxplot(data_sample_5$weight)       # No outliers except one, no measurements will be taken to remove the outlier. See discussion for assignment 1. 


#################################################################################################################################################################
#################################################### Creating Regression Model 3 - ##############################################################################
##########################  7 predictors] pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight   #################################
#################################################################################################################################################################

regression_m3 = lm(pain ~ sex_I+ age+ STAI_trait + pain_cat + cortisol_serum+ mindfulness+ weight, data = data_sample_5)

# regression_m3 = lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness + weight, data=data_sample_4)
summary(regression_m3)
AIC(regression_m3)                    #AIC=478.56
confint(regression_m3)
lm.beta(regression_m3)

#################################################################################################################################################################
#########################################  Identifying Influential Points via Cook's Distance ###################################################################
#################################################################################################################################################################
windows()
plot(x=regression_m3, which=4, las=1, main="Cook's Distance - Model 3")   # Cook's distance plot for model 3
abline(h=CD_cutoff, lty=2)                                                # 7 points were identified, however, based on the same principles as in assignment 1, 
                                                                          # no cases were removed

################################################################################################################################################################# 
############################################################ Linearity Assumption Check #########################################################################
#################################################################################################################################################################
windows()
plot(regression_m3, which=2, las=1)               # Lower end of the spectrum indicates some outliers but nothing too serious

# Checking residuals via the describe() function and the histogram:
describe(residuals(regression_m3))                # No obvious problems detected

windows()
hist(residuals(regression_m3), breaks=12, main="Histogram of Residuals - Regression Model 3", density=20, angle=50, ylim=c(0,40), xlim=c(-3,3), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="blue", labels=TRUE)
shapiro.test(residuals(regression_m3))            # As expected, no significant deviation from normal distribution of residuals 
                                                  # normality assumption met, p=0.52

# Predicted values compared to actual values
pred_m3 <- predict(object=regression_m3)
plot(x=pred_m3, y=data_sample_4$pain, xlab="Fitted Values", ylab="Observed Values")

# Predicted values compared to residuals
plot(regression_m3, which=1)

# Residual plot for each predictor from the "car" package. This returns the results of the linearity tests
windows()
residualPlots(regression_m3)    # linearity assumptions met for all variables 

################################################################################################################################################################# 
###########################################   Checking Assumption Concerning the Variance Homogeneity    ########################################################
#################################################################################################################################################################
ncvTest(regression_m3)          # homoscedacity assumption met p = 0.90
plot(regression_m3, which=3, las=1)


#################################################################################################################################################################
###########################################################   Multicolinearity Check  ###########################################################################
#################################################################################################################################################################
vif(regression_m3)
pairs.panels(data_sample_5[,c("pain", "sex_I", "age", "STAI_trait", "pain_cat", "cortisol_serum", "mindfulness", "weight")], col="blue", lm=TRUE)

# VIF indicates no excessive multicollinearity


#################################################################################################################################################################
############################################################# Creating Theoretical Model - ######################################################################
###############################################  Backward Regression Model (with stepwise elimination) ##########################################################
######################################   4 predictors: pain ~ age + pain_cat + cortisol_serum + mindfulness   ###################################################
#################################################################################################################################################################

# creating the theoretical model
theoretical_model <- lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + mindfulness, data=data_sample_5) 

# regression_m3 <- lm(pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_saliva + mindfulness + weight, data=data_sample_4)

step(object=regression_m3, direction="backward")

summary(regression_m3)    # STAI trait is just as it was in assignment 1, not significant, as is weight
AIC(regression_m3)        # AIC= 478.82
confint(regression_m3)    # 
lm.beta(regression_m3)

#################################################################################################################################################################
##############################################################   Result Stepwise Elimination:   #################################################################
#################################################################################################################################################################


backward_regression_m <- lm(pain ~ age + pain_cat + cortisol_serum + mindfulness, data=data_sample_5)

summary(backward_regression_m)
AIC(backward_regression_m)        
confint(backward_regression_m)
lm.beta(backward_regression_m)
Standardized Coefficients::
#   (Intercept)            age       pain_cat cortisol_serum    mindfulness 
# 0.0000000     -0.2673341      0.1824127      0.3664319     -0.1460366 


#################################################################################################################################################################
#########################################  Identifying Influential Points via Cook's Distance ###################################################################
#################################################################################################################################################################

CD_cutoff<- 4/154   #if not already available

windows()
plot(x=backward_regression_m, which=4, las=1,main="Cook's Distance Plot - Backward Model")
abline(h = CD_cutoff, lty=2)                                            

################################################################################################################################################################# 
############################################################ Linearity Assumption Check #########################################################################
#################################################################################################################################################################

# Checking normality of residuals
# QQplot to check visually for normality assumption
windows()
plot(backward_regression_m, which=2, las=1)

# Checking residuals via describe and histogram
describe(residuals(backward_regression_m))    
hist(residuals(backward_regression_m), breaks=20, main="Histogram Residuals - Backward Model", density=20, angle=50, ylim=c(0,30), xlim=c(-4,4), ylab="Frequency", xlab="Residuals", las=1, border="blue", col="red", labels=TRUE)
shapiro.test(residuals(backward_regression_m)) # Normality assumption met p=0.58

# Predicted values compared to actual values
pred_backward_regression_m <- predict(object=regression_m1)
plot(x=pred_backward_regression_m, y=data_sample_5$pain, xlab="Fitted Values", ylab="Observed Values", las=1)     

# Predicted values compared to residuals
plot(backward_regression_m, which = 1)

# Residual plot for each predictor from the "car" package. This returns the results of the linearity tests
windows()
residualPlots(backward_regression_m,las=1)

# linearity assumption met for all variables

################################################################################################################################################################# 
###########################################   Checking Assumption Concerning the Variance Homogeneity    ########################################################
#################################################################################################################################################################
ncvTest(backward_regression_m)          # homoscedacity assumption med p=0.75
plot(backward_regression_m, which=3, las=1)


#################################################################################################################################################################
#################################################################  Model Comparison  ############################################################################
#######################################################   Backward vs Regression Model 3 ########################################################################
#################################################################################################################################################################

AIC(backward_regression_m,regression_m3)    

# AIC(regression_m3)                # AIC = 476.82
# AIC(backward_regression_m)        # AIC = 478.82

summary(backward_regression_m)
# Call:
#   lm(formula = pain ~ age + pain_cat + cortisol_serum + mindfulness, 
#      data = data_sample_5)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.2022 -0.8325  0.0601  0.6792  2.6259 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     3.69462    1.37528   2.686  0.00804 ** 
#   age            -0.07140    0.01773  -4.027 8.96e-05 ***
#   pain_cat        0.05641    0.02326   2.425  0.01652 *  
#   cortisol_serum  0.60522    0.10942   5.531 1.39e-07 ***
#   mindfulness    -0.22448    0.11123  -2.018  0.04536 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.113 on 149 degrees of freedom
# Multiple R-squared:  0.4111,	Adjusted R-squared:  0.3953 
# F-statistic:    26 on 4 and 149 DF,  p-value: 2.339e-16


summary(regression_m3)

# Call:
#   lm(formula = pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + 
#        mindfulness + weight, data = data_sample_5)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3.13905 -0.80808  0.00825  0.77680  2.70651 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)     3.829386   1.522682   2.515  0.01299 *  
#   sex_I          -0.311103   0.181581  -1.713  0.08878 .  
#   age            -0.066283   0.019963  -3.320  0.00114 ** 
#   STAI_trait     -0.022470   0.028496  -0.789  0.43166    
#   pain_cat        0.059760   0.025419   2.351  0.02006 *  
#   cortisol_serum  0.663905   0.124905   5.315 3.91e-07 ***
#   mindfulness    -0.247520   0.111691  -2.216  0.02823 *  
#   weight          0.005713   0.009248   0.618  0.53767    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.109 on 146 degrees of freedom
# Multiple R-squared:  0.4262,	Adjusted R-squared:  0.3987 
# F-statistic: 15.49 on 7 and 146 DF,  p-value: 4.438e-15

anova(backward_regression_m)

# Analysis of Variance Table
# 
# Response: pain
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# age              1  45.765  45.765 36.9702 9.624e-09 ***
#   pain_cat         1  33.423  33.423 26.9994 6.589e-07 ***
#   cortisol_serum   1  44.519  44.519 35.9638 1.457e-08 ***
#   mindfulness      1   5.042   5.042  4.0734   0.04536 *  
#   Residuals      149 184.447   1.238                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(regression_m3)

# Analysis of Variance Table
# 
# Response: pain
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# sex_I            1   1.042   1.042  0.8464  0.359088    
# age              1  47.063  47.063 38.2330 5.968e-09 ***
#   STAI_trait       1  25.004  25.004 20.3125 1.339e-05 ***
#   pain_cat         1  14.174  14.174 11.5148  0.000889 ***
#   cortisol_serum   1  39.904  39.904 32.4168 6.606e-08 ***
#   mindfulness      1   5.819   5.819  4.7273  0.031298 *  
#   weight           1   0.470   0.470  0.3817  0.537666    
# Residuals      146 179.720   1.231                      
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(regression_m3, backward_regression_m)

# Analysis of Variance Table
 
# Model 1: pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + 
#   mindfulness + weight
# Model 2: pain ~ age + pain_cat + cortisol_serum + mindfulness
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    146 179.72                           
# 2    149 184.45 -3   -4.7263 1.2799 0.2836


AIC(backward_regression_m,regression_m3)    

# AIC(regression_m3)                # AIC = 478.82
# AIC(backward_regression_m)        # AIC = 476.82

#################################################################################################################################################################
###############################################   Result Model Comparison   #####################################################################################
#############################################   Backward vs Regression Model  3 #################################################################################
#################################################################################################################################################################

# Based on the calculations above, it becomes plain that the two models perform very similarly. 
# The backward regression model has an F= 26.79(4,149), p<0.01, Adj R^2=0.40., AIC 476.82. This model is thus able explain 39.53% compared to regression model 3
# which has a F=15.49(7,146), p<0.01, Adj. R^2=0.40 anc can explain 39.87 % of the total variance. 
# If in doubt which to choose, one should always go with the simplest model (Steyer, 2002), i.e. the backward regression model. 


#################################################################################################################################################################
#################################################################  Model Comparison  ############################################################################
#######################################################   Backward vs. Theoretical model #########################################################################
#################################################################################################################################################################
summary(theoretical_model)

# Call:
#   lm(formula = pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + 
#        mindfulness, data = data_sample_5)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -3.07412 -0.80697  0.02272  0.77872  2.73347 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     4.19787    1.39805   3.003  0.00315 ** 
#   sex_I          -0.31301    0.18117  -1.728  0.08615 .  
# age            -0.06942    0.01927  -3.603  0.00043 ***
#   STAI_trait     -0.01647    0.02673  -0.616  0.53880    
# pain_cat        0.05892    0.02533   2.326  0.02138 *  
#   cortisol_serum  0.64959    0.12248   5.304 4.09e-07 ***
#   mindfulness    -0.24209    0.11111  -2.179  0.03094 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.107 on 147 degrees of freedom
# Multiple R-squared:  0.4247,	Adjusted R-squared:  0.4012 
# F-statistic: 18.08 on 6 and 147 DF,  p-value: 1.188e-15

anova(backward_regression_m,theoretical_model)
# Analysis of Variance Table
# 
# Model 1: pain ~ age + pain_cat + cortisol_serum + mindfulness
# Model 2: pain ~ sex_I + age + STAI_trait + pain_cat + cortisol_serum + 
#   mindfulness
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1    149 184.45                           
# 2    147 180.19  2    4.2565 1.7362 0.1798

AIC(theoretical_model)            # AIC = 476.80
AIC(backward_regression_m)        # AIC = 477.30


#################################################################################################################################################################
###############################################   Result Model Comparison   #####################################################################################
#############################################   Backward vs Theoretical Model   #################################################################################
#################################################################################################################################################################

# Again, based on the calculations above. it becomes plain that the two models perform very similarly. 
# The backward regression model has an F= 26(4,149), p<0.01, Adj R^2=0.40., AIC 476.81. This model is thus able explain 39.53% compared to the theoretical 
# model which has a F=18.08(6,147), p<0.01, Adj. R^2=0.40 and is able to explain 40.12% of the variance. 
# Yet again, If in doubt which to choose, one should always go with the simplest model (steyer,2002), i.e. the backward regression model.

#################################################################################################################################################################
#################################################################  Model Comparison  ############################################################################
########################################################   Predictions: New Data Set vs Old Data Set  ###########################################################
#################################################################################################################################################################

# Get data file 2: 'home_sample_2.csv', 

data_sample_6 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")
View(data_sample_6)                                        # viewing the new data sample

data_sample_6$sex_I[data_sample_6$sex=="male"] <- 0        # Changes the value of the variable to male = 0
data_sample_6$sex_I[data_sample_6$sex=="female"] <- 1      # changes the value of the variable to female = 1


describe(data_sample_6)                 # Data on first view looks ok and complete
sum(is.na.data.frame(data_sample_6))    # No NA's detected 
summary(data_sample_6)                  # Summarizing the new data sample

windows()
boxplot(data_sample_6$pain)             # one outlier in the upper range ID_130 could be removed due to its value being comparatively high
boxplot(data_sample_6$age)              # no outliers
boxplot(data_sample_6$STAI_trait)       # two outliers (ID_25 and ID_38 in the lower range - they can be deleted
boxplot(data_sample_6$pain_cat)         # one outlier could be removed due to suspicously low value and inconsistencies for the other values , i.e. omit ID_89
boxplot(data_sample_6$cortisol_serum)   # no outliers
boxplot(data_sample_6$mindfulness)      # ID_113 and ID_123 can be omitted due to very low scores and also because their scores are somewhat contradictory seen
                                        # over all variabels
# To make the samples more comparable, I have chosen to remove the outliers based on the same arguments as used for the assignment 1 dataset. I have also
# chosen to continue to go with cortisol serum. This is due to the fact that it is mentioned in the assignment. However, if this was not the case, 
# cortisol serum, as stated in assignment 1 has a better track record with a higher reliability concerning its measures. When in doubt, always look at the theory, 
# as each effect in a sample is just a reflection of the sample itself 

# Data sample 5 thus has N=154 observations, as has data_sample 6

data_sample_7 <- data_sample_6[-c(25, 38, 89, 113, 123, 133), ]  
rownames(data_sample_7) <- 1:nrow(data_sample_7)
View(data_sample_7)                                   # viewing the new data sample
describe(data_sample_7)                               # describing the new data sample
summary(data_sample_7)                                # summarizing the new data sample

# creating the theoretical model
theoretical_model_1 <- lm(theoretical_model, data=data_sample_5)

##################################################################################################################################################################
#############################################  Comparing the Theoretical Trained Model with the Predicted Model  #################################################
##################################################################################################################################################################


?predict                       # "predict is a generic function for predictions from the results of various model fitting functions."

# Comparing sum of squared distances

pred_train_theoretical_model   <- predict(theoretical_model)
pred_train_theoretical_model_1 <- predict(theoretical_model_1)
RSS_train_theoretical_model    <- sum((data_sample_5["pain"] - pred_train_theoretical_model)^2)
RSS_train_theoretical_model_1  <- sum((data_sample_5["pain"] - pred_train_theoretical_model_1)^2)

RSS_train_theoretical_model                                                                        # RSS = 406.44
RSS_train_theoretical_model_1                                                                      # RSS = 180.19

RSS_train_theoretical_model - RSS_train_theoretical_model_1                                        # RSS = 226.25


anova(theoretical_model, theoretical_model_1)


# Comparing the trained backward model with the predicted model
backward_regression_m1 <- lm(backward_regression_m, data=data_sample_7)
summary(backward_regression_m)                                                                     # Adj. R^2 0.40
AIC(backward_regression_m)                                                                         # AIC= 476.82

# New data model
summary(backward_regression_m1)                                                                    # Adj.R^2 = 0.26
AIC(backward_regression_m1)                                                                        # AIC= 507.97


# Comparing the sum of squared distances

pred_train_backward_regression_m    <- predict(backward_regression_m)
pred_train_backward_regression_m1 <- predict(backward_regression_m1)
RSS_train_backward_regression_m     <- sum((data_sample_5["pain"] - pred_train_backward_regression_m)^2)
RSS_train_backward_regression_m1  <- sum((data_sample_5["pain"] - pred_train_backward_regression_m1)^2)
RSS_train_backward_regression_m                                                                    # RSS 417.8566
RSS_train_backward_regression_m1                                                                   # RSS 507.4408

RSS_train_backward_regression_m - RSS_train_backward_regression_m1                                 # RSS 258.10

# RSS_train_thoretical_m - RSS_train_theoretical_m_1
anova(backward_regression_m, backward_regression_m1)       
anova(theoretical_model_1, backward_regression_m1)



#===============================================================================================================================================================#
#===============================================================================================================================================================#



#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#################################################################################################################################################################
##########################################################   Result Model Comparison   ##########################################################################
##################################################   Theoretical Trained Model vs New Data Model ################################################################
#################################################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Based on the calculations above. it is easy to see that the backward regression model might not be that much better as judged by the AIC values, but it is 
# indeed better due to its simplicity with an F= 18.2(6,147), p<0.01, Adj R^2=0.40., AIC 476.7744 Model 2 is able to explain 40.3% of the variance, compared to just 14.4 % for model 1 
# F=13.84(2,151), p<0.01, Adj. R^2=0.14

# Regression equation model for new model 
# pain= Ix=0(male)   + b1*Ix=1(female) + b2*age        +b3*Stai_trait  +b4*pain_cat    +b5*cortisol_serum     +b6*mindfulness
# Y =    4.00         + 0.32*X1         +(-0.08)X2      +(0.06)*X3      +0.06*X4        +0.62*x5               +(-0.25)*x6

# optional: remove unnecessary files in the environment
# rm(regression_m1)
# rm(regrsssion_m2)
# rm(data_sample_1)
# rm(data_sample_2)
# rm(data_sample_3)

#===============================================================================================================================================================#
#===============================================================================================================================================================#







#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#################################################################################################################################################################
######################################################  Research Question 3  ####################################################################################
#################################################   Backward Regression Model   #################################################################################
#################################################################################################################################################################
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------------------------------#




# Data file 3 is called 'home_sample_3.csv': https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv
# Goal: Build a model that can accurately describe change over time in pain ratings, taking into account the demographic, psychological, and hormone variables 
# used in the theory-based model in assignment 2.
# Fit several linear mixed effect models, for example using the lme() or lmer() functions. 

# 1) Convert the dataframe from wide to long format. Once converted, fit a model including the fixed effects of age, sex, STAI, pain 
# catastrophizing, mindfulness, serum cortisol, time (day of pain report), and a random intercept for participant ID (variable called 'ID' in the dataset). 
# 2) Run a new model containing the same fixed effects, but also including a random slope over time for each participant (this model should also contain a 
# random intercept).
# 3) Plot the pain ratings (y axis) over time (x axis) for each participant separately in separate panels (or facets) in a single graph. This graph should 
# also contain the regression line (or predicted values connected with a line) for each participant, see Everit & Horthon on p. 239 
# (fig. 8.2). Use any plotting function : e.g. xyplot() or ggplot() could do the job). Do the same graph with the regression line of the random slope and the 
# random intercept model.
# 4) Compare the random intercept and the random slope models using conditional AIC (cAIC) to determine which one fits the data better. 
# Also contrast the fit of the two models visually using the two graphs made earlier. Based on these comparisons, choose the model which fits the data better.
# 5) Now build another model (either using random intercept or slope) containing the quadratic term of time. One way to do this would be to add this expression 
# to the list of fixed effects in the model: + I(time^2). (Use the name of your time variable instead of 'time' if named differently in the long format dataframe) 
# 6) Now plot the resulting regression line over the actual pain ratings of each participants again, just like with the previous two models.


#################################################################################################################################################################
###################################################   Steps before we begin the analysis   ######################################################################
#################################################################################################################################################################
# 1: Set working directory

setwd("~/Semester 1/PSYP13")
getwd()

# 2: Read data set from github 

data_sample_8 <- read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_3.csv")

# 3: Load the packages needed for analysis 


library(psych)                  # for the describe function
library(car)                    # as mentioned earlier, this function is for residualPlots, vif, pairs.panels, ncvTest
library(ggplot2)                # for plotting data in ggplot
library(cAIC4)                  # for cAIC
library(r2glmm)                 # for r2beta
library(influence.ME)           # for influence
library(lattice)                # for qqmath
library(reshape2)               # for melt function
library(lme4)                   # for linear mixed modeling lmer
library(lmerTest)               # used to get a singificance test in lmer

# 4: view the new data sample
View(data_sample_8)                                   # viewing the new data sample
describe(data_sample_8)                               # describing the new data sample
summary(data_sample_8)                                # summarizing the new data sample


# Function below extracts standardized beta coefficients out of the lmer models
# source: https://stackoverflow.com/questions/25142901/standardized-coefficients-for-lmer-model

stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

#################################################################################################################################################################
###################################################   Steps before we begin the analysis   ######################################################################
#################################################################################################################################################################


# assign ID as factor
data_sample_8$ID <- factor(data_sample_8$ID)

# variables
names(data_sample_8)

# designate the repeated pain varibales
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")


# Looking at the data a bit closer
describe(data_sample_8)
summary(data_sample_8)


# describe function shows no NA's
describe(data_sample_8$pain1)

#Histograms of the variables 
hist(data_sample_8$STAI_trait)       
hist(data_sample_8$pain_cat)        
hist(data_sample_8$cortisol_serum)  
hist(data_sample_8$mindfulness) 
hist(data_sample_8$pain1)
hist(data_sample_8$pain2)
hist(data_sample_8$pain3)
hist(data_sample_8$pain4)

#
boxplot(data_sample_8$age)
boxplot(data_sample_8$STAI_trait)       
boxplot(data_sample_8$pain_cat)         
boxplot(data_sample_8$cortisol_serum)  
boxplot(data_sample_8$mindfulness) 
boxplot(data_sample_8$pain1)                   # one outlier in the lower part of the plot
boxplot(data_sample_8$pain2)
boxplot(data_sample_8$pain3)
boxplot(data_sample_8$pain4)                   # one outlier, also in the lower part of the plot


# correlation of repeated variables
cor(data_sample_8[,repeated_variables])

# > cor(data_sample_8[,repeated_variables])
#           pain1     pain2     pain3     pain4
# pain1 1.0000000 0.8121490 0.8428394 0.5499557
# pain2 0.8121490 1.0000000 0.8981431 0.5356093
# pain3 0.8428394 0.8981431 1.0000000 0.7109364
# pain4 0.5499557 0.5356093 0.7109364 1.0000000


#################################################################################################################################################################
##############################################################   Starting the Analysis   ########################################################################
#################################################################################################################################################################

data_sample_8_l <- melt(data_sample_8, measure.vars=repeated_variables, variable.name = "day", value.name = "pain")

# changing day variable to a numerical vector
data_sample_8_l$day = as.numeric(data_sample_8_l$day)

# data is transformet into long format
data_sample_8_l

# from the assignment paper: 
# "fit a model including the fixed effects of age, sex, weight, STAI,pain catastrophizing, mindfulness, serum cortisol, time (day of pain report),and a random intercept for participant ID."

model_r_int = lmer(pain ~ day + age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|ID), data=data_sample_8_l)
model_r_slope = lmer(pain ~ day + age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + (day|ID), data=data_sample_8_l)
summary(model_r_slope)

#################################################################################################################################################################
##############################################################   Plot the Results  ##############################################################################
#################################################################################################################################################################

# Here we plot the regression line (prediction) and save both models predictions to variables
data_sample_8_l$pred_int = predict(model_r_int)
data_sample_8_l$pred_slope = predict(model_r_slope)


# random intercept model
windows()
ggplot(data_sample_8_l, aes(y = pain, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='blue', aes(y=pred_int, x=day))+
  facet_wrap( ~ ID, ncol = 5)

# random slope and intercept model
windows()
ggplot(data_sample_8_l, aes(y = pain, x = day, group = ID))+
  geom_point(size = 3)+
  geom_line(color='blue', aes(y=pred_slope, x=day))+
  facet_wrap( ~ ID, ncol = 5)

#################################################################################################################################################################
##############################################################   Model comparisons   ############################################################################
#################################################################################################################################################################

# compare models with cAIC
cAIC(model_r_int)$caic     # 211.64
cAIC(model_r_slope)$caic   # 211.64
# result gives identical values... something is rotten in Copenhagen...sadly no time to find a solution

# compare models with anova
anova(model_r_int, model_r_slope)


# adding a quadratic term of time to the intercept model 
model_r_int_quadterm <- lmer(pain ~ day + I(day^2) + age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + (1|ID), data=data_sample_8_l)

#################################################################################################################################################################
##############################################################   Plot the Results  ##############################################################################
#################################################################################################################################################################

data_sample_8_l$pred_int_quadterm = predict(model_r_int_quadterm)

# random intercept model
windows()
ggplot(data_sample_8_l, aes(y = pain, x = day, group = ID))+  geom_point(size = 3)+  geom_line(color='blue', aes(y=pred_int_quadterm, x=day))+  facet_wrap( ~ ID, ncol = 5)

#################################################################################################################################################################
##############################################################   Model comparisons   ############################################################################
#################################################################################################################################################################

# compare models with cAIC
cAIC(model_r_int)$caic                                  # 211.3971
cAIC(model_r_int_quadterm)$caic                         # 198.5904 >>> this time we get different values...

# compare models with anova
anova(model_r_int, model_r_int_quadterm)

# Data: data_sample_8_l
# Models:
#   model_r_int: pain ~ day + age + sex + weight + STAI_trait + pain_cat + mindfulness + 
#   model_r_int:     cortisol_serum + (1 | ID)
# model_r_int_quadterm: pain ~ day + I(day^2) + age + sex + weight + STAI_trait + pain_cat + 
#   model_r_int_quadterm:     mindfulness + cortisol_serum + (1 | ID)
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model_r_int          11 217.95 244.16 -97.977   195.95                             
# model_r_int_quadterm 12 207.93 236.52 -91.967   183.93 12.019      1  0.0005266 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


#################################################################################################################################################################
#################################################################################################################################################################
#############################################################   Ending the Session   ############################################################################
#################################################################################################################################################################
#################################################################################################################################################################
#                                                                                                                                                               #
#clear workspace                                                                                                                                                #
#rm(list=ls(all=TRUE))                                                                                                                                          #
#                                                                                                                                                               #
#clear graphics:                                                                                                                                                #
#graphics.off()                                                                                                                                                 #
#                                                                                                                                                               #
#to quit your R session, type                                                                                                                                   #
#quit()                                                                                                                                                         #
#################################################################################################################################################################