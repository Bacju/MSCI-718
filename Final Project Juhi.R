library(MASS)
library(fitdistrplus)
#### Just for reference given O for blue fish and 1 for Gold fish
b <- 0 
g <- 1
Lake <- c (b, g)


Blue1 <- 60
Gold1 <- 40
Catch1 <- Blue1 + Gold1  # 1st catch where there were 60 blue & 40 gold fish

#Catch2 <- sample(Catch1,size=20,replace=TRUE)
#Catch2

y <- rmultinom(100, size = 20, prob = c(0.6,0.4))
#generating random matrix where 20 fishes are picked with probability 0.6 and 0.4
z <- t(y)  # converting row to column for ease
z 

likelihood <- c() #creating likelihood function empty list

#creating likelihood with previous random outputs generated
for(i in 1:nrow(z)){
  likelihood[i] <- -dmultinom(z[i,], prob=c(0.6,0.4),log =T)
}
#combining z matrix and likelihood in one data
f <- cbind(z,likelihood)  
colnames(f) <- c("Nb","Ng","Likelihood")  # giving column names
f
max(likelihood)    #searching for max likelihood value
ml <- f[which(likelihood == max(likelihood)), ]
ml

s<- function(Nb,Ng){
  Ng1<- Ng/20
  print(Ng)
  Nb1<- Nb/20
  print(Nb)
}
s(280,220)
s(290,210)
### here it seems the 2st hypothesis is more likely to happen as it shows chance of dividing probability in 0.6 and 0.4 value.

#############################################################################
#######################################################################################
###################################################################################
#######Section 2: Data Analysis Questions --- Q-1)

library(foreign)
#Loading the 2 datasets
Dat1 = read.dta("Q1Data1.dta")
Dat2 = read.csv("Q1Data2.csv", header =T, stringsAsFactors = F)

names(Dat1)    #to view all column headings name

#----- a-1) Creating Subset by Removing  the states named "Hawaii", "Alaska", and "Washington D.C"
x<-0
x <- subset(Dat1, state!= c("hawaii"))      #Removed hawaii
x <- subset(x, state!= c("alaska"))         #Removed alaska
x <- subset(x, state!= c("washington dc"))  #washington dc
x
# only four columns "state","marital", "heat2", and "heat4"
Dat1_mod <- x[,c("state","marital", "heat2", "heat4")]

#----- a-2) If heat 2 and heat4 ==NA , then remove that row
Dat1_mod_no_na<-subset(Dat1_mod,!(heat2 %in% NA & heat4 %in% NA))
#Removing all NA values from marital column
Dat1_mod_no_naa<-subset(Dat1_mod_no_na,!(marital %in% NA))

#----- a-3) Heat2 has only 2 values after subsetting "dem/lean dem" & "rep/lean rep"
Dat1_mod_no_naa_h <- subset(Dat1_mod_no_naa, heat2 == c("dem/lean dem", "rep/lean rep"))
Out <- Dat1_mod_no_naa_h

#-----a-4) changing all values of marital column to "others" except "married" 
Out$marital <- ifelse(Out$marital != "married", Out$marital<-"others",Out$marital<-"married")
Out
View(Out)


#----b-1)


#for (i in 1:length(levels(Out$state))) {
group_by(Out, state) %>% mutate(ratio = heat2 / sum(heat2))
m<- Out$heat2


library(dplyr)
summarise_at(group_by(Out,state),funs(mean(.,na.rm=TRUE)))

#---- b-2)

for(state in 1:length(marital)){
  marital <- Out %>%select(state, marital)
  sum(Out$married)/sum(Out$marital)
}
#---- b-3)
group_by(Out, state) %>% mutate(ratio = marital$married / marital)


#---- c-1) creating subset by removing three states,"Hawaii", "Alaska", and "District of Columbia"
#for Dat2
x1 <- subset(Dat2, state != c("Hawaii", "Alaska" , "District of Columbia"))
#----c-2) Reducing columns to only two columns "state," and "vote_Obama_pct"

Dat2_mod <- x1[,c(1,3)]
head(Dat2_mod)
head
######
#---- d) logistic regression

mm_outcome <-Out$marital   # y for model
mm_predict <- model.matrix(marital~state, data = Out) #x predict vlue
library(glmnet)
#applied glmnet using binomial regression
f <- glmnet(x = mm_predict,y = mm_outcome, family = "binomial" ) 

#Assumption 1) No pooling (means lambda = 0)
no_pooling <- glmnet(x = mm_predict, y = mm_outcome, family = "binomial",alpha = 0, lambda = 0)
coef(no_pooling)
#Assumption 2) Complete pooling (means lambda = high)
complete_pooling <- glmnet(x = mm_predict, y = mm_outcome,family = "binomial",alpha = 0, lambda = 10^5)
coef(complete_pooling)
#Assumption 3) Partial Pooling(Used lasso)
### will find value of lambda 1st using cross validation(k=10) and the will find glm output after applying best lambda
cv_model_lasso <- cv.glmnet(x = mm_predict, y = mm_outcome,family = "binomial", alpha = 1)
best_lambda_lasso <- cv_model_lasso$lambda.min
best_lambda_lasso 
## best_lambda_lasso =0.005330175
model_lasso <- glmnet(x = mm_predict, y = mm_outcome,family = "binomial", alpha = 1, lambda =best_lambda_lasso)
coef(model_lasso )

## prediction using mm_predict as data now
y_predicted_lasso <- predict(model_lasso, s = best_lambda_lasso, newx = mm_predict)
y_predicted_lasso

y_predicted_no_pooling <- predict(no_pooling, s = 0, newx = mm_predict)
y_predicted_complete_pooling <- predict(complete_pooling, s = 0, newx = mm_predict)

#####e) use ass -3

library(glmnet)
#fit = glmnet(as.matrix(y_predicted_lasso[1],model_lasso[-1]))
#plot(fit, xvar='lambda')
plot(Out$heat2)
plot(model_lasso, type = "b")
plot(y_predicted_lasso)

plot(no_pooling, type = "b")
plot(y_predicted_no_pooling)
plot(complete_pooling, type = "b")
plot(y_predicted_complete_pooling)

