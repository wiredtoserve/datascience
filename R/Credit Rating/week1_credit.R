#####################################################
#Reading in the Credit data
#####################################################
data = read.csv("Credit.csv", header = TRUE, sep=",")

#####################################################
#Model #1: All numerical variables

#####################################################
fit1<-lm(Balance~Income+Limit+Rating+Cards+Age+Education,data=data)
#look at the summary of the linear model fit
summary(fit1)

# creating a dummy variable for Gender
gender_dummy <- ifelse(data$Gender == 'Female', 1, 0)

# creating a dummy variable for Married
married_dummy <- ifelse(data$Married == 'Yes', 1, 0)

# creating a dummy variable for Student
student_dummy <- ifelse(data$Student == 'Yes', 1, 0)

# adding the dummy variables to the dataframe
data[,13] = gender_dummy #V13
data[,14] = married_dummy #V14
data[,15] = student_dummy #V15

#####################################################
#Model #2: With dummy variables

#####################################################
fit2<-lm(Balance~Income+Limit+Rating+Cards+Age+Education+V13+V14+V15,data=data)
#look at the summary of the linear model fit
summary(fit2)

# creating a dummy variable for Asian Ethinicity
asian_dummy <- ifelse(data$Ethnicity == 'Asian', 1, 0)

# creating a dummy variable for African Ethinicity
african_dummy <- ifelse(data$Ethnicity == 'African American', 1, 0)

data[,16] = asian_dummy #V16
data[,17] = african_dummy #V17

#####################################################
#Model #3: With dummy variables (ethinicity)

#####################################################
fit3<-lm(Balance~Income+Limit+Rating+Cards+Age+Education+V13+V14+V15+V16+V17,data=data)
#look at the summary of the linear model fit
summary(fit3)

#plot scatter of residuals vs Income
plot(fit3$residuals~Income,data=data)
abline(h=0)

#plot scatter of residuals vs Limit
plot(fit3$residuals~Limit,data=data)
abline(h=0)

#plot scatter of residuals vs Rating
plot(fit3$residuals~Rating,data=data)
abline(h=0)

#plot scatter of residuals vs Cards
plot(fit3$residuals~Cards,data=data)
abline(h=0)

#plot scatter of residuals vs V15
plot(fit3$residuals~V15,data=data)
abline(h=0)

#plot scatter of residuals vs Age
plot(fit3$residuals~Age,data=data)
abline(h=0)

#####################################################
#Defining a function for the wald test - require MASS library
#H0: RB=c vs Ha: RB!=c
#R=the R matrix
#B=the estimated coefficients
#S=the covariance matrix of B
#c=the vector of constants
#####################################################
library(MASS)
wald<-function(R,B,S,c){
  stats<-matrix(0,1,2)
  dif=(R%*%B-c)
  VV=R%*%(S)%*%t(R)
  W=t(dif)%*%ginv(VV)%*%dif
  stats[1]=W
  stats[2]=pchisq(W,nrow(c),lower.tail=FALSE)
  colnames(stats)<-c("Wald stat","p-value")
  return(stats)
}

#define inputs to the wald test - joint significance of the three slope parameters
RR=cbind(rbind(0,0,0,0,0,0,0,0,0,0,0),diag(11))
cc=rbind(0,0,0,0,0,0,0,0,0,0,0)
bhat=(fit3$coefficients)
Shat=vcov(fit3)
wald1=wald(RR,bhat,Shat,cc)
wald1

###########################################################
#constructing quadratic terms for age, cards, limit
###########################################################
data[,18]=data$Age^2
names(data)[names(data)=="V18"] <- "age2"

data[,19]=data$Cards^2
names(data)[names(data)=="V19"] <- "cards2"

data[,20]=data$Limit^2
names(data)[names(data)=="V20"] <- "limit2"

#####################################################
#Model #4: Updated with transformations and multicolineariy

#####################################################
fit4<-lm(Balance~Income+Limit+Cards+Age+Education+V13+V14+V15+age2+cards2+limit2,data=data)
#look at the summary of the linear model fit
summary(fit4)


#####################################################
#Analysis : Factors affecting credit balance
 # Income | Limit | Cards | Age | Student | Limit^2
#####################################################
#Modelling interactions - context!

#####################################################
data[,21]=data$Limit*data$Cards
names(data)[names(data)=="V21"] <- "LimitxCards"
data[,22]=data$limit2*data$Cards
names(data)[names(data)=="V22"] <- "Limit2xCards"

# Step 1
fit_step1<-lm(Balance~Income+Limit+Cards+Age+V15+limit2+LimitxCards+Limit2xCards,data=data)
#look at the summary of the linear model fit
summary(fit_step1)

# Step 2
fit_step2<-lm(Balance~Income+Limit+Age+V15+limit2+LimitxCards+Limit2xCards,data=data)
#look at the summary of the linear model fit
summary(fit_step2)

# Step 3
fit_step3<-lm(Balance~Income+Limit+Age+V15+limit2+LimitxCards,data=data)
#look at the summary of the linear model fit
summary(fit_step3)

# Step 4
fit_step4<-lm(Balance~Income+Limit+V15+limit2+LimitxCards,data=data)
#look at the summary of the linear model fit
summary(fit_step4)

step(fit_step4)
