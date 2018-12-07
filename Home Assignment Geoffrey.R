# Lund University - M.Sc. psychology -PSYP13                                     Course leader PSYP13: Geoffrey Patching 
# Wintersemester 2018                                                            Student: Lisa Granath (li1636gr-s)
# 
########################################################################################################################
# Authors Note:  "R" is a complex program and in order to make it understandable for even a bloody beginner,           #
# I have decided to create this syntax as idiot proof as possible. I have chosen to do this                            #
# in order to learn how to create an easily understandable syntax and find a style which may serve me in the future.   #
# You may therefore choose to skip the first section which basically just gives an overview over the experiment, an    #
# introduction to the variables etc.                                                                                   #
# Any other section which is not essential for the assignment will always be marked                                    #
# and surrounded by a box of hashtags in order to make it easier and faster for you to read.                           #
########################################################################################################################

########################################################################################################################
# The first part (Part 1) concerns students' anxiety about PSYP13.                                                     #
#                                                                                                                      #
# The second part (Part 2) is about how a group of 1960's students rated differences between 12 nations.               # 
#                                                                                                                      #
# The third part (Part 3) is about planning the number of participants required to obtain a statistically              #
# significant effect in a one way independent groups design given that the alternate hypothesis is true.               #
########################################################################################################################


#==================================================================================================================================================================#

####################################################################################################################################################################
####################################################################################################################################################################
##############################################################   Assignment 1 ######################################################################################
#####################################################  Exploratory Factor Analysis - PCA ###########################################################################
####################################################################################################################################################################
####################################################################################################################################################################


#################################################################################################################################################################
########################################################  Simple experiment description:   ###################################################################### 
#################################################################################################################################################################
# N=300 adults taken a questionnaire on anxiety PAQ                                                                                                             #
# Participants were asked about their attitudes concerning doing statistics with R  with answers given on a 5-point                                             #
# Likert-Scale going from strongly agree, agree, neither agree or disagree, disagree, to strongly disagree                                                      #                                                
#                                                                                                                                                               #
# #########################   Variables:   ###################################################################################################################### 
# Anxiety as measured by the PAQ questionnaire                                                                                                                  #
#                                                                                                                                                               #	 
# ID           # participant number N=300                                                                                                                       #
# sex          # participant gender coded with 0,1 and 2                                                                                                        #
# age          # articipant age, given in numbers                                                                                                               #
# Q1_cry       # Item 1 - " Statistics make me cry"                                                                                                             #
# Q2_help      # Item 2 - " The help files in RStudio are not helpful at all"                                                                                   #
# Q3_breathe   # Item 3 - " I cannot breathe when I see an equation"                                                                                            #
# Q4_freeze    # Item 4 - " My brain freezes at the mere mention of matrix operations"                                                                          #
# Q5_alien     # Item 5 - " R was designed by aliens to humiliate me"                                                                                           #
# Q6_inferior  # Item 6 - " My friends are better at statistics than me"                                                                                        #
# Q7_weep      # Item 7 - " R coding makes me weep openly"                                                                                                      #
# Q8_Support   # Item 8 - " There is little or no support for learning R"                                                                                       #
# Q9Nerd       # Item 9 - " If I'm good at statistics people will think I'm a nerd"                                                                             #
#                                                                                                                                                               #
###############################################################   Assignment Goal  ##############################################################################
# Following the procedures discussed by Everitt and Hothorn (2011), use principle components analysis (PCA) to assess                                           #
# whether anxiety about PSYP13 can be broken down into various forms of anxiety about different aspects of Sub-course 1                                         #                                                                                                                     #  
#                                                                                                                                                               #
#################################################################################################################################################################


#################################################################################################################################################################
###################################################   Steps before we begin the analysis   ######################################################################
#################################################################################################################################################################
# 1: Set working directory

setwd("~/Semester 1/PSYP13")
getwd()


# 2: Read data set 
PAQ_Granath = read.https("https://raw.githubusercontent.com/LitenLisa/PsychStats/master/PAQ_Granath.txt")

# 3: save as new dataset
data_sample_1<-PAQ_Granath
View(data_sample_1)

# 4: Load the packages needed for analysis 
library(psych)    # Information from ?psych: Contains functions most useful for personality and psychological research, needed for the "describe" function
library(MVA)      # multivariate analysis function


#################################################################################################################################################################
###################################################   Pre-Analysis and Dealing with Missing's   #################################################################
#################################################################################################################################################################
# Summarizing the data  
#  Among other things, the summary() function shows where missing values are as well as how many they are(denoted by "NA" for "not available")
summary(data_sample_1)
# No NA's, no oddities with min-max values

# The describe() function in the "psych" package confirms no missing values
describe(data_sample_1)

sum(is.na.data.frame(data_sample_1))      # confirms the no NA's 

###################################################################################################################################################################
# To assure oneself of the no NA's, it is possible to check thee mean() functions which will only work if there are no NA                                         #
# If an NA is detected, this can be cleared by using: "mean(data_sample_1$Q2)help,na.rm = TRUE) code which will remove the NA before calculating the mean         #
# One can deal with missings by different methods or keeping the data set intact as it is. In R the easiest way is using the code:                                #
# data_set_1_example1 <- na.omit(data_sample_original).                                                                                                           #
# However, this should never be done in an arbitrary way as missing data also tells us something and can be analysed. If we have a high                           #
# dropout (mortality) rate on an intervention it will mean something. If we have a high dropout rate on a questionnaire, or an item, it will mean something too.  #
# If our data is missing at random, missing completely at random or if we have mode than 30%  missings for a participant or question this will all have           #
# implications, not only for the methods we use, but also for our analysis itself and the interpretation of our data                                              #
# The MCAR-test by Little is commonly used when concluding if the missing answers follow a particular pattern . If the datapool passes the test, the missing's    #
# could be replaced by using the expectation maximization-algorithm (EM-Algorithm). We could also use the multiple imputation method.                             #
# The preconditions for this imputative method are that the sample is large enough, the MCAR-test is passed and that not more than 30% of the values per          #
# items are missing (Tabernachnick & Fidell, 2007).                                                                                                               #
# As this data set however is complete, none of this is necessary and is only mentioned here for informatory purpose.                                             # 
###################################################################################################################################################################

mean(data_sample_1$sex)
mean(data_sample_1$age)                
mean(data_sample_1$Q1_cry)
mean(data_sample_1$Q2_help)
mean(data_sample_1$Q3_breathe)
mean(data_sample_1$Q4_freeze)
mean(data_sample_1$Q5_alien)
mean(data_sample_1$Q6_inferior)
mean(data_sample_1$Q7_weep)
mean(data_sample_1$Q8_Support)
mean(data_sample_1$Q9_Nerd)

# the performance of the means operations confirm the no NA's
# If a missing is detected and deleted, the list again needs to be fixed using the: 
# rownames(data_sample_example2) <- 1:nrow(data_sample_example1). 
# Otherwise later functions or calculations like the lev/mahad will not function correctly and output conflicting row numbers

###################################################################################################################################################################
# Alternatively once can replace the missing value with the mean                                                                                                  #
# Q1_crymean=mean(data_sample_example3$Q1_cry,na.rm=TRUE)                      # creates an object: Q1_crymean which then forces a calculation by                 #
#                                                                              # removing the NA's                                                                #
# print(Q1_crymean)                                                            # prints the mean number                                                           #
# data_sample_example3$Q1_cry[is.na(data_sample_example3$Q1_cry)]=Q1_crymean   # replacing NA with the mean calculated above                                      #
# summary(psyk)                                                                # function can be used to check if the NA was removed                              #
# print(data_sample_example3$Q1_cry[111])                                      # shows the replaced value                                                         #
# print(data_sample_example_originalsample$Q1_crymean[111])                    # Show line 111 in the psyk_original data set                                      #
###################################################################################################################################################################


###################################################################################################################################################################
##################################################################   Checking for Outliers   ######################################################################
###################################################################################################################################################################

# histograms, boxplots, normality check     
# color codes for R found in http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# age: 
hist(data_sample_1$age, main="Age",breaks=5,
     density=20,angle=50,ylim=c(0,151),xlim=c(22,28),
     xlab="Age in Years", ylab="Number of Participants", 
     las=1,border="red",col="red",labels=TRUE) 
describe(data_sample_1$age)
summary(data_sample_1$age)
shapiro.test(data_sample_1$age)                                  # Data sample is clearly not normally distributed, this is however not relevant for the PCA

# Gender:
describe(data_sample_1$sex)                                      # The data set gives no information of what 0, 1 and two refer to, I have chosen to interpret 0
                                                                 # as "male", 1 as "female" and 2, "do not want to disclose". I do not really think that it matters
                                                                 # to the PCA 
summary(data_sample_1$sex)

#Q1:
windows()
hist(data_sample_1$Q1_cry, main="Item 1",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree" , ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q1_cry)                                     # The boxplots are not very relevant in this case, however, they can offer valuable insights
                                                                  # when analyzing Items in real circumstances as they reveal a first hint of item difficulty
describe(data_sample_1$age)
summary(data_sample_1$age)
shapiro.test(data_sample_1$age)                                   # Data sample is clearly not normally distributed, this is however not relevant for the PCA

#Q2:
windows()
hist(data_sample_1$Q2_help, main="Item 2",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q2_help)

describe(data_sample_1$Q2_help)
summary(data_sample_1$Q2_help)
shapiro.test(data_sample_1$Q2_help)                               # Data sample is clearly not normally distributed, this is however not relevant for the PCA


#Q3:
windows()
hist(data_sample_1$Q3_breathe, main="Item 3",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q3_breathe)

describe(data_sample_1$Q3_breathe)
summary(data_sample_1$Q3_breathe)
shapiro.test(data_sample_1$Q3_breathe)                            # Data sample is clearly not normally distributed, this is however not relevant for the PCA


#Q4
windows()
hist(data_sample_1$Q4_freeze, main="Item 4",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q4_freeze)

describe(data_sample_1$Q4_freeze)
summary(data_sample_1$Q4_freeze)
shapiro.test(data_sample_1$Q4_freeze)                             # Data sample is clearly not normally distributed, this is however not relevant for the PCA

#Q5
windows()
hist(data_sample_1$Q5_alien, main="Item 5",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q5_alien)

describe(data_sample_1$Q5_alien)
summary(data_sample_1$Q5_alien)
shapiro.test(data_sample_1$Q5_alien)                              # Data sample is clearly not normally distributed, this is however not relevant for the PCA

#Q6
windows()
hist(data_sample_1$Q6_inferior, main="Item 6",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q6_inferior)

describe(data_sample_1$Q6_inferior)
summary(data_sample_1$Q6_inferior)
shapiro.test(data_sample_1$Q6_inferior)                           # Data sample is clearly not normally distributed, this is however not relevant for the PCA

# Q7
windows()
hist(data_sample_1$Q7_weep, main="Item 7",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q7_weep)

describe(data_sample_1$Q7_weep)
summary(data_sample_1$Q7_weep)
shapiro.test(data_sample_1$Q7_weep)                               # Data sample is clearly not normally distributed, this is however not relevant for the PCA

# Q8
windows()
hist(data_sample_1$Q8_Support, main="Item 8",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q8_Support)

describe(data_sample_1$Q8_Support)
summary(data_sample_1$Q8_Support)
shapiro.test(data_sample_1$Q8_Support)                            # Data sample is clearly not normally distributed, this is however not relevant for the PCA

# Q9
windows()
hist(data_sample_1$Q9_Nerd, main="Item 9",breaks=5,density=100,angle=50,ylim=c(0,200),xlim=c(1,5),xlab="1=Strongly Agree, 2=Agree, 3=Neither Disagree or Agree, 4=Disagree, 5=Strongly Disagree", ylab="Number of Participants", las=1,border="hotpink3",col="lightpink3",labels=TRUE) 

boxplot(data_sample_1$Q9_Nerd)

describe(data_sample_1$Q9_Nerd)
summary(data_sample_1$Qp_Nerd)
shapiro.test(data_sample_1$Q9_Nerd)                               # Data sample is clearly not normally distributed, this is however not relevant for the PCA

#################################################################################################################################################################
# However, if you have a non-normal distribution and want to be able to calculate with measures which require a normal distribution,                            #
# the log10 to can be used:                                                                                                                                     #
#                                                                                                                                                               #
# data_sample_example4 <- data_sample_originaldataset                                                                                                           #
# data_sample_example4$log10.Q1_cru=log2(data_sample_example4$Q1_cry+1)  # Logarithmic transformation                                                           #
# hist(data_sample_example4$log10.Q1_cry,breaks=5)                                                                                                              #
# describe(data_sample_example4$Q1_cry)                                                                                                                         #
#################################################################################################################################################################

#################################################################################################################################################################
# Concerning the respondence effects: A respondent will sometimes answer by picking the first acceptable option or only choose extreme values, or just randomly #
# choose values throughout the test. here are a number of ways in which a questionnaire can help to avoid this. One of them is by reversing the item polarity,  #
# that is to say, provide one question with an answer order which ranges from a positive to a negative and another which ranges in the reverse direction        #
# (Krosnick & Presser, 2010),  Before the data can be analyzed, the data, of course, needs to be reversed again to make the results between items more          #
# comparable. This will thus make the interpretation of a later PCA more correct and also easier ("An Introduction to Applied Multivariate Analysis with R" )   #                                                                                   #
#                                                                                                                                                               #
# data_sample_example5 <- data_sample_originaldataset                                                                                                           # 
# data_sample_example5$Q1_cry <- with(data_sample_originaldataset, max(Q1_cry)-Q1_cry)                                                                          #
# hist(data_sample_originaldataset$Q1_cry)                                                                                                                      #
# hist(data_sample_example5$Q1_cry)                                                                                                                             #
#################################################################################################################################################################

#################################################################################################################################################################
################################################################## PCA Analysis - First Steps   #################################################################
#################################################################################################################################################################

#################################################################################################################################################################
# Short explanation on what we are trying to achieve - (Source: Bortz, J. & Schuster, C. 2010 "Statistik fuer Human und Sozialwissenschaftler, 7th ed.)         #
# Factor analysis is a broad expression used to describe methods by which one can reduce existing data in a dataset into a smaller more understandable structure#
# PCA can be seen as a kind of factor analysis. It is done in order to generate a hypothesis, not to test it.                                                   #
# The goal of any PCA is to summarize the variance of all the variables into a smaller number of main components                                                # 
# This is done through a rotation transformation method where the variance of the principal components is maximized e.g. through Varimax rotation, Oblimin etc. #
# In the first step, a regression line is found which explains the first principal component. This is followed by the next step where the orthogonal line is    #
# found which can best explain the rest of the variance. This goes on until all the variance has been properly explained                                        #
# In this way, the variance is separated into different groups - principal components, sometimes refered to as Fjk in the linear regression expression          #
# Xij=aj1*Fj1+aj2*Fj2+...+eij = Sum ajk * Fjk + eij.                                                                                                            #
# aj1 etc can thus be seen as a kind of factor weight, similar to multiple regression where the coefficients play this part                                     #
# Xij is the realized value of the variable X from person i, on item j                                                                                          #
# Fj1 is the factor score of person i,on the principal component j                                                                                              #
# How important a single component is, is decided by the eigenvalue (eigenwert in German). It is basically the squared sum of the weights of the component      #
# the last score, is the communality score (kommunalitaet in German) which describes the explained squared variance as described by all the components          #
# in the final model                                                                                                                                            #
# The problem is always to decide which and how many components are the most important. This can be done by either establishing an a priori rule, as in         #
# we are looking for two factors. This can be done if one is lucky enough to have a very strong theoretical fundament.                                          #
# The Kaiser-Guttman criteria states that we should keep all the principal components with an eigenvalue of >1. If we have a lot of variables, this             #
# criteria will overestimate  the number of components                                                                                                          #                   
# The Screeplot is a diagram which can give an indication on how many important components we have by dropping suddenly. The "knick" will be the criteria,      #
# i. where the line in the plot sharply makes a drop indicates the number of components of importance.                                                          #
# Oblimin/oblique rotation will allow correlations between the components                                                                                       #
#################################################################################################################################################################  
# a PCA normally has 5 steps                                                                                                                                    #
# 1) Create a correlation matrix of the manifest variables                                                                                                      #
# 2) Orthogonal rotation transformation                                                                                                                         #
# 3) Deciding on the numbers of principal components                                                                                                            #
# 4) Renewed rotation in order to reach a "simple structure" (Thurstone, 1947)                                                                                  #
# 5) Interpretation of the principal components as judged by the factor weights (ajk)                                                                           #
#################################################################################################################################################################

# Before we start with the correlation matrix and the rest of the steps in the PCA it is important that we try to identifying the multivariate outliers 
# This can be done by first creating a regression, looking at leverage and then the mahad values:

# To detect the outliers this way, we first need to run and save a linear regression (lm). Here ID is used  as a dummy variable in order to perform the calculation
regression_m1<-(lm(formula=ID~ Q1_cry+ Q2_help 
                   + Q3_breathe + Q4_freeze 
                   + Q5_alien + Q6_inferior 
                   + Q7_weep + Q8_Support 
                   + Q9_Nerd, data=data_sample_1))

lev <-hat(model.matrix(regression_m1))
data_sample_1[lev>.09]
plot(lev)


N <- nrow(data_sample_1)                   # N is the number of rows (pps) in our data set
mahad <- (N-1)*(lev-1/N)                   # mahad = (df)*(leverage model-1)/number of pps
tail(sort(mahad),5)                        # Show the five highest mahad scores
order(mahad,decreasing=T)[c(5,4,3,2,1)]    # Gives the rows with with the highest mahad scores
                                           # The order function function is important so the position of the rows correlates with the m. distances in the line 
                                           # before 
# 9 predictor variables = mahad distances in a Chi2 table in the line for df=9 & a=< 0.001 which is a little above 27.877
# This means that we have no multivariate outliers in the dataset

################################################################################## Step 1&2 #######################################################################

# The next step is to exclude the colums which we do not need in our matrix
subset1_PCA<-within(data_sample_1, rm(ID,age,sex))

# we then start our PCA for real by saving our matrix as a correlation matrix
subset1_PCA<-cor(subset1_PCA)                           # this is our starting point for the PCA analysis

PCA1<-princomp(covmat=subset1_PCA)                  #d_cor is the code for the orthogonal transformation
summary(PCA1,loadings=TRUE)

# Importance of components:
#                           Comp.1    Comp.2    Comp.3     Comp.4     Comp.5     Comp.6    Comp.7     Comp.8     Comp.9
# Standard deviation     2.1630861 1.0931272 1.0133604 0.74872671 0.68370513 0.63746313 0.5433661 0.45727292 0.40060336      # the standard deviation 
                                                                                                                             # = the squared eigenvalues
# Proportion of Variance 0.5198824 0.1327697 0.1140999 0.06228796 0.05193919 0.04515103 0.0328052 0.02323317 0.01783145      # This is our principal components "F"
# Cumulative Proportion  0.5198824 0.6526521 0.7667520 0.82903997 0.88097916 0.92613018 0.9589354 0.98216855 1.00000000
 

# Loadings:                                                                    = our earlier mentioned "aji" factor loadings
# The interpretation of these loadings is dependent on the sample size where everything below N=200 makes the interpretation of loadings >0.35 
# very unsure. In our sample size of n=300, we can accept a boundry of interpretation to be >0.25
# High positive loadings can be interpreted as positive correlations, while negative values indicate a negative correlation

#              Comp.1 Comp.2 Comp.3 Comp.4 Comp.5 Comp.6 Comp.7 Comp.8 Comp.9
# Q1_cry       0.378  0.412                       0.184  0.102         0.789
# Q2_help      0.345 -0.357  0.257 -0.256 -0.186        -0.752         0.141
# Q3_breathe   0.383  0.348  0.138                0.187         0.671 -0.464
# Q4_freeze    0.371  0.398               -0.222  0.107        -0.707 -0.358
# Q5_alien     0.320 -0.403  0.204 -0.523         0.267  0.589              
# Q6_inferior  0.287 -0.168 -0.570         0.671  0.240 -0.156 -0.150       
# Q7_weep      0.312 -0.205  0.432  0.365  0.406 -0.592  0.145              
# Q8_Support   0.305  0.140 -0.508 -0.365 -0.248 -0.648         0.104       
# Q9_Nerd      0.280 -0.413 -0.301  0.617 -0.481  0.144  0.135              
#              # the commonality score (kommunalitaet) is the sum of the squared loadings on the components. 


#################################################################################################################################################################
# An alternative to reach the same numbers we just reviewed can be done by using the code                                                                       #
# subset1_pca <- princomp(subset1, cor = TRUE)                                                                                                                  #
# print(subset1_pca)                                                                                                                                            #
# summary(subset1_pca,loadings=TRUE)                                                                                                                            #
# Source: p. 81 Multivariate Analysis for the code                                                                                                              #                                                                                                                        #
#################################################################################################################################################################

################################################################################## Step 3 ##########################################################################


# In the book multivariate data analysis, it is once again discussed on how to choose and settle on the number of most important components

# 1) values betw. 70 and 90% are suggested, less might be ok if the number of participants or the variables themselves increase
summary(subset1_PCA,loadings=TRUE)
# Cumulative proportion indicates our values for this criteria. 
# With this criteria we would include the first three components. As N increases of (our sample is large, N=300) the error rate grows thus, 
# we would be safe keeping the first 3 components

# 2) exclude the components with eigenvalues less than average
summary(subset1_PCA)
pca_sd <- c( 2.1630861, 1.0931272, 1.0133604, 0.74872671, 0.68370513, 0.63746313, 0.5433661, 0.45727292, 0.40060336)
print(pca_sd)
sum(pca_sd)/9  # dividing by 9 to get the average
               # Result as judged by this criteria is that we keep the first four components

# 3) exclude pcs with eigenvalues less than 0.7 (or 1 Kaiser-Guttmann criteria)
print(pca_sd)
# The result of this criteria is that we ought to keep the first three components and adding the fourth if we allow a 0.7 boundry

# 4) Read the log eigenvalue diagram
windows()
plot(log(subset1_PCA$sdev^2),     # on the log eigenvalue diagram the suggested cut-off is at 0.0
     xlab= "Component number", 
     ylab="Component variance",
     type = "l", 
     main="log(Eigenvector) diagram",     
     xaxt = 'n'
)
axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c("1","2","3","4","5","6","7","8","9"),
     abline(h=0))
#The result once again implies that we should keep the first three components as the rest drops below the cut-off at 0.0

# before we also talked about the screeplot

# 5)screeplot
windows()
plot(subset1_PCA$sdev^2,          # in this plot the y-axis = the explained variance (SD^2) and the x-axis is the component number with a cut-off value = 1.0
     xlab= "Component number",  
     ylab="Component variance",
     type = "l", 
     main="Scree diagram",
     xaxt = 'n')                  
axis(side=1,at=c(1,2,3,4,5,6,7,8,9),labels=c("1","2","3","4","5","6","7","8","9"),
     abline(h=1))       
#The result once again implies that we should keep the first three components as the rest drops below the cut-off at 1.0

#ROverall Result: ============= as most analyses imply that we should keep the first three components the choice becomes rather simple.  

################################################################################## Step 4 ########################################################################
# 4) PCA continues by performing a new rotation


library(GPArotation)


# visualization of the components in 3d
library(pca3d)       
pca3d(subset1_pca)    
# https://cran.r-project.org/web/packages/pca3d/vignettes/pca3d.pdf


# Varimax rotation 
# "Varimax rotation is an orthogonal rotation of the factor axes to maximize the variance of the squared loadings of a factor (column) on all the variables (rows) 
# in a factor matrix, which has the effect of differentiating the original variables by extracted factor. Each factor will tend to have either large or small 
# loadings of any particular variable. A varimax solution yields results which make it as easy as possible to identify each variable with a single factor. 
# This is the most common rotation option." Source wikipedia https://en.wikipedia.org/wiki/Factor_analysis


# Since we most likely cannot assume that our factors are independent, the used method for this specifiq tast will be done using the oblimin rotation presented 
# down below. 
# Here is just an example of how a varimax rotation would be done

# PCA_varimax <- principal(d_cor, nfactors=3, rotate = "varimax")
# print(PCA_varimax)
# 
# PCA_varimax$loadings
# PCA_varimax$values
# PCA_varimax$rotation
# PCA_varimax$communality
# PCA_varimax$r.scores


# Oblique/oblimin rotation

# "However, the orthogonality (i.e., independence) of factors is often an unrealistic assumption. Oblique rotations are 
# inclusive of orthogonal rotation, and for that reason, oblique rotations are a preferred method. Allowing for factors that are correlated with one another is 
# especially applicable in psychometric research, since attitudes, opinions, and intellectual abilities tend to be correlated, and since it would be unrealistic
# in many situations to assume otherwise" Source: https://en.wikipedia.org/wiki/Factor_analysis

# In this part we test whether the postulated hypothesis that 3 components offers us our best model as assumed by the tests we did before
PCA_oblimin <-principal(d_cor, nfactors=3, rotate = "oblimin")
print(PCA_oblimin)
# Principal Components Analysis
# Call: principal(r = d_cor, nfactors = 3, rotate = "oblimin")
# Standardized loadings (pattern matrix) based upon correlation matrix
#               TC1   TC2   TC3   h2   u2 com
# Q1_cry       0.91  0.02  0.05 0.88 0.12 1.0
# Q2_help      0.09  0.81  0.07 0.78 0.22 1.0
# Q3_breathe   0.86  0.13  0.00 0.85 0.15 1.0
# Q4_freeze    0.89  0.04  0.02 0.84 0.16 1.0
# Q5_alien    -0.01  0.80  0.12 0.71 0.29 1.0
# Q6_inferior  0.05  0.06  0.82 0.75 0.25 1.0
# Q7_weep      0.24  0.75 -0.18 0.70 0.30 1.3
# Q8_Support   0.41 -0.16  0.68 0.72 0.28 1.8
# Q9_Nerd     -0.17  0.45  0.62 0.66 0.34 2.0
# 
# TC1  TC2  TC3
# SS loadings           2.84 2.32 1.74
# Proportion Var        0.32 0.26 0.19
# Cumulative Var        0.32 0.57 0.77
# Proportion Explained  0.41 0.34 0.25
# Cumulative Proportion 0.41 0.75 1.00
# 
# With component correlations of 
#      TC1  TC2  TC3
# TC1 1.00 0.43 0.36
# TC2 0.43 1.00 0.33
# TC3 0.36 0.33 1.00
# 
# Mean item complexity =  1.3
# Test of the hypothesis that 3 components are sufficient.
# 
# The root mean square of the residuals (RMSR) is  0.07                      # The RMSR = is a measure of the mean absolute value of the covariance residuals
                                                                             # Ought to be below .05

# 
# Fit based upon off diagonal values = 0.98


PCA_oblimin_4_factors <-principal(d_cor, nfactors=4, rotate = "oblimin")
print(PCA_oblimin_4_factors)
# 
# Principal Components Analysis
# Call: principal(r = d_cor, nfactors = 4, rotate = "oblimin")
# Standardized loadings (pattern matrix) based upon correlation matrix
#               TC1   TC2   TC4   TC3   h2   u2 com
# Q1_cry       0.95 -0.05  0.00  0.04 0.88 0.12 1.0
# Q2_help      0.07  0.82  0.08 -0.08 0.81 0.19 1.1
# Q3_breathe   0.91  0.05 -0.02 -0.01 0.85 0.15 1.0
# Q4_freeze    0.94 -0.02 -0.02  0.02 0.85 0.15 1.0
# Q5_alien    -0.07  0.99 -0.06  0.07 0.87 0.13 1.0
# Q6_inferior  0.05  0.10  0.55  0.51 0.75 0.25 2.1
# Q7_weep      0.36  0.34  0.31 -0.48 0.77 0.23 3.5
# Q8_Support   0.34  0.15  0.11  0.67 0.80 0.20 1.7
# Q9_Nerd     -0.04 -0.02  0.96 -0.02 0.88 0.12 1.0
# 
#                        TC1  TC2  TC4  TC3
# SS loadings           3.01 1.97 1.49 0.99
# Proportion Var        0.33 0.22 0.17 0.11
# Cumulative Var        0.33 0.55 0.72 0.83
# Proportion Explained  0.40 0.26 0.20 0.13
# Cumulative Proportion 0.40 0.67 0.87 1.00
# 
# With component correlations of 
# TC1  TC2  TC4  TC3
# TC1 1.00 0.51 0.40 0.16
# TC2 0.51 1.00 0.51 0.04
# TC4 0.40 0.51 1.00 0.15
# TC3 0.16 0.04 0.15 1.00
# 
# Mean item complexity =  1.5
# Test of the hypothesis that 4 components are sufficient.
# 
# The root mean square of the residuals (RMSR) is  0.06                            # This could indicate the the final model is better off with 4 factors
# 
# Fit based upon off diagonal values = 0.98

################################################################################## Step 5 #########################################################################

# 5) interpreting the Principal components

# reminder of the Items: 
# Q1_cry       # Item 1 - " Statistics make me cry"                                                                                                             #
# Q2_help      # Item 2 - " The help files in RStudio are not helpful at all"                                                                                   #
# Q3_breathe   # Item 3 - " I cannot breathe when I see an equation"                                                                                            #
# Q4_freeze    # Item 4 - " My brain freezes at the mere mention of matrix operations"                                                                          #
# Q5_alien     # Item 5 - " R was designed by aliens to humiliate me"                                                                                           #
# Q6_inferior  # Item 6 - " My friends are better at statistics than me"                                                                                        #
# Q7_weep      # Item 7 - " R coding makes me weep openly"                                                                                                      #
# Q8_Support   # Item 8 - " There is little or no support for learning R"                                                                                       #
# Q9Nerd       # Item 9 - " If I'm good at statistics people will think I'm a nerd"                                                                             #


# The interpretation of the principal components and the final result is that the first factor has high loadings on item 1 Q1_cry(.91), Q3_breathe, Q4_freeze(.89) 
# and a moderate loading of item 8 Q8_Support(.41). It can thus be summarized as measuring a construct related to "feeling of fear of stats". 

# The second component comes with high loading on item 2 Q2_help(.81), Q5_alien(.80) and Q7_weep(.75). This component could then be hypothesized to measure a 
# construct related to feelings of humiliation and helplessness.  

# The third component comes with high loadings on the remaining Q6_inferior(.82) and Q9_Nerd (.62). However, it also shows a moderately high loading on Q8_support 
# (.68). It is difficult to summarize this but it is related to fear of what other people might think. 

# As always, it is difficult if not even impossible to come to any final conclusion. This is after all exploratory and performing a confirmatory factor analysis
# will be the only way to shed more light on the matter. We seem however as based on the standardized loadings to be able to reach the conclusion that three 
# constructs is optimal. When analyzing the four factors standardized loadings table we see that it confirms the above mentioned constructs rather than pointing
# us in another direction
library(ggplot)
library(ggfortify)


##################################################################################################################################################################
##################################################################################################################################################################
#############################################################   Ending the Session   #############################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#                                                                                                                                                                #
#clear workspace                                                                                                                                                 #
#rm(list=ls(all=TRUE))                                                                                                                                           #
#                                                                                                                                                                #
#clear graphics:                                                                                                                                                 #
#graphics.off()                                                                                                                                                  #
#                                                                                                                                                                #
#to quit your R session, type                                                                                                                                    #
#quit()                                                                                                                                                          #
##################################################################################################################################################################



#===============================================================================================================================================================#
#===============================================================================================================================================================#
#===============================================================================================================================================================#
#===============================================================================================================================================================#
#===============================================================================================================================================================#




#################################################################################################################################################################
#################################################################################################################################################################
##############################################################   Assignment 2  ##################################################################################
##########################################################  Multidimensional Scaling  ###########################################################################
#################################################################################################################################################################
#################################################################################################################################################################

#################################################################################################################################################################
###################################################   Steps before we begin the analysis   ######################################################################
#################################################################################################################################################################
# 1: Set working directory

setwd("~/Semester 1/PSYP13")
getwd()

# 2 Rename and download the dataset nations downloaded from LUVIT:
nations1 <- read.delim("~/Semester 1/PSYP13/Datafiles/nations.txt")
View(nations1)                                                   # The table in the text file "Nations.txt" shows the mean similarity ratings.

# 3 Creating an original backup copy:
nations_original <- nations1  

# 3: Load the packages needed for analysis 
library(psych)    # for describe function
require(smacof)   # needed for the sim2diss function
require(MASS)     # for isoMDS function i.e. Kruskal's Non-metric Multidimensional Scaling


# Wish et al (1970) asked 18 students to rate the global similarity of different pairs of nations
# such as 'France and China' on a 9-point rating scale ranging from `1=very different' to `9=very
# similar'. The table in the text file "Nations.txt" shows the mean similarity ratings.

# Goal: Following the procedures discussed by Everitt and Hothorn (2011), use multidimensional
# scaling to examine the students' perceived dissimilarities between the nations.


#################################################################################################################################################################
###################################################   Pre-Analysis and Dealing with Missing's   #################################################################
#################################################################################################################################################################

# Summarizing the data - Among other things, the summary() function shows where missing values are as well as how many they are(denoted by "NA" for "not available")
summary(nations1)
# No NA's, no appearent oddities with the values on the first glance

# The describe() function in the "psych" package confirms no missing values
describe(nations1)

sum(is.na.data.frame(nations1))      # also confirms the no NA's 


describe(nations1)
summary(nations1)   # No NA's
# MIN = 0, no values over 9 (theoretical maximum, because similarity scores betw. 1-9 were given)

#################################################################################################################################################################
########################################   Converting Similarity Ratings to Dissimilarity Ratings ###############################################################
#################################################################################################################################################################

# Right now the means we see were obtained by ratings between 1 (very dissimilar) to 9 (very similar)
# Because MDS uses distances (i.e. higher distances = higher numbers) to reduce dimensions and arrange data points in a 2d plot the similarity ratings need to be 
# converted into dissimilarity ratings using the sim2diss function below

?sim2diss                                               # gives an explanation

nations_dissilimilar <- sim2diss(nations1, method=9)    # For the method argument the highest value is chosen, i.e. similarity values were rated 1-9


print(nations1)                                         # look at the original ratings
print(nations_dissilimilar)                             # look at the new ratings


#################################################################################################################################################################
#####################################################   Multidimensional Scaling ################################################################################
#################################################################################################################################################################

# "Multidimensional scaling (MDS) is a means of visualizing the level of similarity of individual cases of a dataset. 
# It refers to a set of related ordination techniques used in information visualization, in particular to display the information contained in a distance matrix. 
# It is a form of non-linear dimensionality reduction. An MDS algorithm aims to place each object in N-dimensional space such that the between-object distances 
# are preserved as well as possible. Each object is then assigned coordinates in each of the N dimensions." https://en.wikipedia.org/wiki/Multidimensional_scaling


?isoMDS
# MDS <- cmdscale(data_diss_example)  # "Metric MDS is used for continuous variables (objective measures e.g. distance in mm)"
# MDS <- isoMDS(data_diss_example)    # "Non-Metric MDS is used for ordinal data (subjective measures, e.g. likert scales)"

nations1<-as.matrix(nations1)                                                      
# turning the data set into a matrix. This is necessary in order to progress to the next steps

nations_dist<-dist(nations1, method = "euclidean", diag=FALSE, upper=FALSE, p=2)                  
# The dist function computes and returns the distance matrix computed. This is done by using the specified distance measure, in this case the euclidian 
# to compute the distances between the rows of a data. This methods uses the most direct path between two points as its distance measuring
# method. Others methods exist, such as manhattan metric etc. 

nations.mds<-isoMDS(nations_dist, maxit = 20) # ISO MDS is a for of non-metric multdimensional scaling. Here we are giving the maximum iterations of 20

print(nations.mds)   # our final stress value = 20.01411 where 10 = fine, < 10 = great and > 10 = not so great

# A possible interpretation is that we are dealing with a methods which tries to force our data to be reduced wo just two dimensions. This results in a 
# fit far from perfect. 


#################################################################################################################################################################
#####################################################   Multidimensional Scaling  Plot  #########################################################################
#################################################################################################################################################################

print(nations.mds) # the $points show us where in the 2D graph the nation points are to be plotted..
# ..we extract them into two seperate vectors

windows()
x <- nations.mds$points[,1] # 1 refers to 1st coordinate
y <- nations.mds$points[,2] # 2 refers to 2nd coordinate

print(x)
print(y)

plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
     font.axis=2, font.lab=2, cex.lab=1.2,                   # font.axis=2 = makes axis labels bold: cex.lab=1.2 = increases size of labels text
     xlim=c(-6,6), ylim=c(-6,6),                             # xlim extends or decreases x-axis
     type="p", pch=19, las=1)                                # plot points
text(x, y, labels=colnames(nations1), col="black", cex=1.3)  # las=1rotates the labels on the y axis and cex = text size
abline(h=0, v=0, col = "hotpink3", lty = 2)                  # vertical and horizontal lines
abline(a=0, b=1, col = "burlywood4", lty = 3)                # 1st diagonal line
abline(a=0, b=-1, col = "chartreuse4", lty = 3)              #lty = linetype


##################################################################################################################################################################
##################################################################################################################################################################
#############################################################   Ending the Session   #############################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#                                                                                                                                                                #
#clear workspace                                                                                                                                                 #
#rm(list=ls(all=TRUE))                                                                                                                                           #
#                                                                                                                                                                #
#clear graphics:                                                                                                                                                 #
#graphics.off()                                                                                                                                                  #
#                                                                                                                                                                #
#to quit your R session, type                                                                                                                                    #
#quit()                                                                                                                                                          #
##################################################################################################################################################################







#================================================================================================================================================================#
#================================================================================================================================================================#
#================================================================================================================================================================#
#================================================================================================================================================================#
#================================================================================================================================================================#
#================================================================================================================================================================#







##################################################################################################################################################################
##################################################################################################################################################################
##############################################################   Assignment 3   ##################################################################################
##############################################################  Power Analysis   #################################################################################
##################################################################################################################################################################
##################################################################################################################################################################

# Due to a lack of time, I was not able to finish this part

#===============================================================================================================================================================#

##################################################################################################################################################################
##################################################################################################################################################################
#############################################################   Ending the Session   #############################################################################
##################################################################################################################################################################
##################################################################################################################################################################
#                                                                                                                                                                #
#clear workspace                                                                                                                                                 #
#rm(list=ls(all=TRUE))                                                                                                                                           #
#                                                                                                                                                                #
#clear graphics:                                                                                                                                                 #
#graphics.off()                                                                                                                                                  #
#                                                                                                                                                                #
#to quit your R session, type                                                                                                                                    #
#quit()                                                                                                                                                          #
##################################################################################################################################################################



#===================================================================================================================================================================#