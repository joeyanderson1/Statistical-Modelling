# stat 10060 final project code

#opening data
data<- read.csv("~/Final project/STAT10060_20493252.csv")
View(data)
#________________
#Exploratory Data
#________________

#1 - 176 in placebo and 157 in treatment
table(data$Group)
#puts the number of people in each group into a table

counts <- table(data$Group)
barplot(counts, 
        main="Number of patients in each group",
        horiz=TRUE, col="violet",
        names.arg=c("Placebo", "Treatment"),
        cex.names=0.8,las=1)
#gives us a horizontal bar chart of the number of people
#in each group

#2 - 
hist(data$Age,col="blue",xlab="Age",
     main="Histogram of age distribution",breaks=10)

#calculating the number of people in the modal interval

cond1<-data$Age >45
#gives us the amount of people older than 45

cond2<-data$Age <=50
#gives us the amount of people younger or equal to 50

data$Age[cond1 & cond2]
#applies both conditions on the data

length(data$Age[cond1 & cond2])
#gives us the number of people aged between 46 and 50 
#including people who are 50

length(data$Age[data$Age<=20])
# number of people less than or equal to 20 

length(data$Age[data$Age>=70])
#number of people older than or equal to 70

#3  
weight<-data$Weight
height_cm<-data$Height
height_metres<-height_cm/100
#converts our height into centimetres

BMI<-weight/height_metres^2
#applies the given formula
BMI
hist(BMI,col="red",
     xlab=expression(paste("Body Mass Index kg/m"^"2","")),
     main="Histogram of BMI distribution",breaks=10)

cond1<-BMI >=20
#gives us the amount of people with a BMI equal to 
#or above 20

cond2<-BMI <=25
#gives us the amount of people with a BMI equal to 
#or below 25

length(BMI[cond1 & cond2])
#applies our condition to the list BMI and calculates 
#the amount that meet the conditions

# 114 is the frequency in the modal interval
length(BMI[BMI>40])
# there is only 8 people with a BMI of over 40

bmimale<-mean(BMI[data$Sex=="Male"])
bmimale

bmifemale<-mean(BMI[data$Sex=="Female"])
bmifemale
#Male bmi average =24.84059
#Female bmi average =27.76996

#4 - off the table the hieghest is Monaghan with avg =77.95455
# the lowest is Wicklow 61.26364
aggregate(data$Weight~County,data=data,FUN="mean")
# gives us the mean weight in each county

#5 
# Percentage of males with blood group A=23.52941%

cond1<-data$Sex=="Male"
#gives us all the males in the data

cond2<-data$BloodGroup=="B"
#gives us the people in blood group B

males.A<-length(data$Sex[cond1&cond2])
#the number of men with blood group B

no.males<-length(data$Sex[data$Sex=="Male"])
#number of men in the data

males.A/no.males
#divides the number of men with blood group B by the 
#total number of men to give us our percentage of men

table(data$BloodGroup)
#puts the amount of men of each blood group into a table

#6 mean difference in each subjects chol level from 
# start to finish = 0.2107463
# on average they decrease from the start to finish
# by 0.2107463 mol/l
choldiff<-data$Cholesterol1 - data$Cholesterol2
mean(choldiff)

#____________________
# Hypothesis Testing
#____________________

#1 x^2 = 3.995 p-value=0.1357
tab<-table(data$CardioRisk,data$Sex)
# gives us a table with the amount of males and 
# females in each risk level of cardiovascular disease

tab

addmargins(tab)
#adds the margins 

chisq.test(tab)
#conducts a chi-squared test on our table above

#2- I did a z test due to how large the 
#   populations we are testing
#   I used the BSDA package to allow me to do this
install.packages("BSDA")
library(BSDA)
weight_treatment <- data$Weight[data$Group=="Treatment"]
#the weight of people in the treatment group

weight_placebo <- data$Weight[data$Group=="Placebo"]
#the weight of people in the placebo group

sd_treat<-sd(weight_treatment)
#standard deviation of weight in the treatment group

sd_plac<-sd(weight_placebo)
#standard deviation of weight in the placebo group

z.test(weight_treatment,weight_placebo,
       alternative="two.sided",mu=0,
       sigma.x=sd_treat,sigma.y=sd_plac,
       conf.level=0.95)
#conducts the x test on our means giving us a z stat of
#1.962 and a p value of 0.04977

#boxplot
boxplot(data$Weight ~ data$Group, ylab="Weight (kg)", 
        xlab="Group",col="green",
        main="Boxplot comparing
        weights of groups",las=1)
#gives us an informal analysis of our difference in means

#3 
# 1 -2 is giving a plus value for the reduction 
# so we are testing if the reduction in cholesterol says 
# that treatment > reduction in placebo

choldiff_treatment<-data$Cholesterol1[data$Group=="Treatment"]-data$Cholesterol2[data$Group=="Treatment"]
#the difference between cholesterol before the study
#and cholesterol after the study of each patient in 
#the treatment group

choldiff_placebo<-data$Cholesterol1[data$Group=="Placebo"]-data$Cholesterol2[data$Group=="Placebo"]
#the difference between cholesterol before the study
#and cholesterol after the study of each patient in 
#the placebo group

sd_treatdiff<-sd(choldiff_treatment)
sd_placdiff<-sd(choldiff_placebo)
#gives us the standard deviation of both difference in 
#cholesterol for each group

z.test(choldiff_treatment,choldiff_placebo,
       alternative="greater",mu=0,
       sigma.x=sd_treatdiff,sigma.y=sd_placdiff,
       conf.level=0.99)
#conducts our z test giving us a z score of 1.47 and
#a p-value of 0.07078

z.test(choldiff_treatment,choldiff_placebo,
       alternative="two.sided",mu=0,
       sigma.x=sd_treatdiff,sigma.y=sd_placdiff,
       conf.level=0.99)
#gives us our 99% confidence interval for the difference
#which is (-0.1949007,0.7130643)

boxplot(choldiff ~ data$Group, ylab="Cholesterol (mol/l)", 
        xlab="Group",col="magenta",
        main="Boxplot comparing
        cholesterol of groups",las=1)
#gives us an informal analysis of our difference in 
#cholesterol reduction


#______________
# Model Fitting
#______________

#1 r = 0.02301364

#caclulates the BMI for each individual in the study based
#off there height and weight
r=cor(BMI,data$Cholesterol1,)
r

#2 y intercept =4.112817391 
# slope = 0.004634462
reg<-lm(data$Cholesterol1~BMI,data=data)
reg
#gives us our regression line

plot(BMI,data$Cholesterol1, 
     xlab=expression(paste("BMI kg/(m"^"2",")")),
     ylab="Cholesterol (mol/l)", 
     main = "Scatterplot and regression line", 
     pch=20,col="black")
#draws our scatter plot of BMI against cholesterol level

abline(reg,col="blue",lwd=3,lty=1)
#plots our regression line onto our scatter plot
coefficients(reg)
#tells us our y-intercept and our slope of our line

summary(reg)



#3 coeff of determiantion = 0.0005296276
coefficient_of_determination<-r^2
#calculates teh coefficient of determination

coefficient_of_determination

#4
sig<-sigma(reg)
#calculates the standard deviation about the 
#regression line

e<-residuals(reg)
#finds our residual plot and puts our intervals 
#on the plot

plot(x = BMI, y = e, pch = 20,
     xlab = expression(paste("BMI kg/(m"^"2",")")), 
     ylab = "residuals", 
     main = "Residual plot")
#makes our residual plot

abline(h = c(-sig, +sig), col = "red")
#this gives us our first confidence band

abline(h=0, col="blue")
#this plots our 0 residual line

summary(reg)
confint(reg,level=0.95)

#______________
# Secondary Analysis
#______________

#1 average height for each blood group
# A   165.6779
# AB   165.7023
# B   164.5973
# O   164.0488
aggregate(Height~BloodGroup,data=data,FUN="mean")
#gives us the mean height base one each blood type


#2
# the next line will calculate the sample variance for each
# blood group
aggregate(Height~BloodGroup,data=data,FUN="var")

# Next we carry out ANOVA
ANOVA<-aov(Height~BloodGroup,data=data)
summary(ANOVA)

TukeyHSD(ANOVA)
#Uses the Tukey-Kramer methos to cheack whether
#the means differ significantly

#3
# degrees of freedom = (3,331)
# F=1.057
# p=0.368
# Thus we fail to reject null based on our significance
# level of 0.05

