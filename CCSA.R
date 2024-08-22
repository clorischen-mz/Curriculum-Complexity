# Curriculum Complexity and Study Abroad Study
# Analyzing Study Abroad and Enrollment Data

#Loading necessary packages
library(psych)
library(ggplot2)
library(dplyr)
library(plyr)
library(gmodels)
library(pastecs)
library(car)
library(lavaan)
library(qgraph)
library(semPlot)
library(effsize)
library(reshape2)
library(ez)
library(QuantPsyc)
library(sjstats)
library(psych)

# Load Analytic Dataset####
CCSA <- read.csv(file = "C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/UPDATED Analytic dataset_CCSA_Finalcsv.csv")
summary(CCSA$CurrComp)

#CORRELATION####

# Create Numeric Data Set
colnames(CCSA) #get column names
CCSA_a<-dplyr::select(CCSA,Pc_Male:Pc_UScz,Pc_SA_08.09:Pc_Total_SA)

# Create Pearson Correlation Matrix #use Pearson because all vars are continuous
corCCSA_a <- corr.test(x=CCSA_a, y=NULL, use="pairwise", method= "pearson",adjust="holm")
write.csv(corCCSA_a$r, "C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/CCSA_Pear_corrmatrix.csv")
write.csv(corCCSA_a$p, "C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/CCSA_Pear_matrix_pvalues.csv")

#Descriptive statistics for CCSA_a data set
CCSAdesc <- describe(CCSA_a[])
write.csv(CCSAdesc,"C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/Descriptive Statistics_CCSA.csv")

#Kruskal–Wallis Test####
#selected due to small sample size and non-normal distribution instead of ANOVA

#1. Load in the dataset
#KW_CC - compare curricular complexity across colleges
#KW_SA - compare % study abroad participation across colleges 
KW_SA <- read.csv(file = "C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/Results/CCSA_KW_SAcsv.csv")
KW_CC <- read.csv(file = "C:/Users/mengz/OneDrive/Desktop/ENE Research/#Curriculum Complexity/Results/CCSA_KW_CCcsv.csv")


#Note: The group levels must be ordered alphabetically (in our case college names)
#2. Compute summary statistics by groups:

#For KW_SA
library(dplyr)
group_by(KW_SA,Group) %>%
  summarise(
    count = n(),
    mean = mean(Pc_Total_SA, na.rm = TRUE),
    sd = sd(Pc_Total_SA, na.rm = TRUE),
    median = median(Pc_Total_SA, na.rm = TRUE),
    IQR = IQR(Pc_Total_SA, na.rm = TRUE)
  )

#For KW_CC
group_by(KW_CC, Group) %>%
  summarise(
    count = n(),
    mean = mean(CurrComp, na.rm = TRUE),
    sd = sd(CurrComp, na.rm = TRUE),
    median = median(CurrComp, na.rm = TRUE),
    IQR = IQR(CurrComp, na.rm = TRUE)
  )

#Visualize the data
#Note: if need color for each box, need extra package seems like
install.packages("ggpubr") #Install and load ggpubr - Publication Ready Plots

# Boxplots
#For KW_SA
library("ggpubr")
ggboxplot(KW_SA, x = "Group", y = "Pc_Total_SA",  #note that x and y here must match the name exactly with the column name in data frame!
          order = c("Ag", "Engr", "HHS","LA","PI","Sci"),
          ylab = "%Total Study Abroad", xlab = "College Affiliation")

#For KW_CC
library("ggpubr")
ggboxplot(KW_CC, x = "Group", y = "CurrComp", 
          order = c("Ag", "Engr", "HHS","LA","PI","Sci"),
          ylab = "CurrComp", xlab = "College Affiliation")

# Mean plots
#For KW_SA
library("ggpubr")
ggline(KW_SA, x = "Group", y = "Pc_Total_SA", 
       add = c("mean_se", "jitter"), 
       order = c("Ag", "Engr", "HHS","LA","PI","Sci"),
       ylab = "%Total Study Abroad", xlab = "College Affiliation")

#For KW_CC
library("ggpubr")
ggline(KW_CC, x = "Group", y = "CurrComp", 
       add = c("mean_se", "jitter"), 
       order = c("Ag", "Engr", "HHS","LA","PI","Sci"),
       ylab = "CurrComp", xlab = "College Affiliation")

#Compute Kruskal-Wallis test
kruskal.test(CurrComp ~ Group, data = KW_CC) #p-value significant at 5%
kruskal.test(Pc_Total_SA ~ Group, data = KW_SA) #p-value significant at 5%

#Wilcoxon Rank Sum Test####
#Find out which pairs of groups are different - do Multiple pairwise-comparison between groups
Wilcox_CC <- pairwise.wilcox.test(KW_CC$CurrComp,KW_CC$Group,p.adjust.method = "BH")
Wilcox_SA <- pairwise.wilcox.test(KW_SA$Pc_Total_SA,KW_SA$Group,p.adjust.method = "BH") 
print(Wilcox_CC)
print(Wilcox_SA)


wilcox.test(KW_CC$CurrComp,KW_SA$Pc_Total_SA)
#Wilcoxon rank sum test with continuity correction
#W = 1764, p-value = 3.118e-15

#alternative hypothesis: true location is not equal to 0

--------------------------------------------
#LINEAR REGRESSION####
CCSA_MLR<- dplyr::select(CCSA,Pc_Male:Sci,Total_Enrol:Pc_Total_SA) #Vet,Pharm,Edu removed due to singularity (n=1)
CCSA_Reg<-lm(Pc_Total_SA~Pc_Male+Pc_UScz+Ag+HHS+KSM+Engr+LA+PI+Sci+Total_Enrol+CurrComp,data=CCSA_MLR)
summary(CCSA_Reg) #Ag - significant; other predictors - insig
#CCSA_Reg<-lm(Pc_Total_SA~Pc_Male+Pc_UScz+Total_Enrol+CurrComp,data=CCSA_MLR)
--------------------------------------------------------------
#Check assumptions for doing MLR####

#1.Check for multicollinearity - PASSED
car::vif(CCSA_MLR)#vif>1.5 indicates multicollinearity

#Results of VIF
#Pc_Male     Pc_UScz Total_Enrol    CurrComp 
#1.366865    1.479187    1.182372    1.061611 

-------------------------------------------------
#Levene's test#
#Create dataset so that pharm,vet,edu are left out (due to n=1)
CCSA_lev<-subset(CCSA,!CCSA$College %in% c("Pharm","Vet","Edu"))

# Levene's test for homogeneity of variance/Homoscedasticity
#If Levene's test is significant, this means that the two groups did not show homogeneity of variance on the dependent or outcome variable.
#p< 0.05->significant->violates assumption for homogeneity
#Field book pg 373
#center=median because provides good robustness against many types of non-normal data while retaining good power.
# this test is not appropriate with quantitative explanatory variables!
leveneTest(CCSA_lev$Pc_Total_SA,CCSA_lev$College.Name, center=median)  #insignificant
leveneTest(CCSA_lev$Pc_Total_SA,CCSA_lev$Pc_Male, center=median)  # insignificant
leveneTest(CCSA_lev$Pc_Total_SA,CCSA_lev$Pc_UScz, center=median)  # significant
leveneTest(CCSA_lev$Pc_Total_SA,CCSA_lev$Total_Enrol, center=median)  # significant
leveneTest(CCSA_lev$Pc_Total_SA,CCSA_lev$CurrComp, center=median)  # significant
leveneTest(lm(Pc_Total_SA~Pc_Male*Pc_UScz*Ag*HHS*KSM*Engr*LA*PI*Sci*Total_Enrol*CurrComp,data=CCSA_lev))

#Create scatterplots of residuals against the quantitative explanatory variables
#produce a residual vs fitted plot for visulaizting heteroscedasticity
#should look like a random array of dots evenly dispersed around zero
#If this graph funnels out, then the chances are that there is heteroscedasticity in the data.
#From results of lm(): CCSA_Reg
res <- resid(CCSA_Reg) #get list of residuals 
plot(fitted(CCSA_Reg), res) #produce residual vs. fitted plot
abline(0,0) #add a horizontal line at 0 


# fit a regression model
standard_res <- rstandard(CCSA_MLR) #calculate the standardized residuals
round(standard_res,4) #view the standardized residuals
hist(standard_res)


#Normality - DIDN'T PASS BUT DOESN'T MATTER
#Cite "Norman - 2010 - Likert scales, levels of measurement and the laws"
#Shapiro-Wilk normality test - whether a distribution is normal
#Field book pg 182-3
#if P value < 0.05 (significant) -> non-normal
#if the test is significant (p < .05) then the distribution is significantly different
#from a normal distribution (i.e., it is non-normal).
shapiro.test(CCSA$Pc_Male) #non-normal
shapiro.test(CCSA$Pc_UScz) #non-normal
shapiro.test(CCSA$Total_Enrol) #non-normal
shapiro.test(CCSA$CurrComp) #non-normal
shapiro.test(CCSA$Pc_Total_SA) #non-normal
#Plot histograms to check for normality
hist(CCSA$Pc_Male) #multimodal
hist(CCSA$Pc_UScz) #left-skewed
hist(CCSA$CurrComp) #right-skewed
hist(CCSA$Pc_Total_SA) #right-skewed
---------------------------------------------
#Scatterplot
plot(CCSA$CurrComp,CCSA$AvgSA,main="Scatterplot",xlab="Curriculum Complexity",ylab="Average Study Abroad Experience")


#Scatter plots
plot(CCSA$CurrComp,CCSA$AvgSA,main="Scatterplot",xlab="Curriculum Complexity",ylab="Average Percent of Study Abroad")
plot(CCSA$AvgEnroll,CCSA$AvgSA,main="Scatterplot",xlab="Average Enrollment Size",ylab="Average Percent of Study Abroad")