#####################################################################################
#######Q-1
###Q-1(A)

set.seed(4)                         #To store the value we use set.seed
x <-  rnorm(50, mean = 0,sd = 2)    # Generate 50 random numbers using mean = 0 and SD = 2 then, stored it in x 
y <- rnorm(50, mean = 2 + 1.5 * x,sd = 10)   # Generate 50 random numbers using mean = 2 + 1.5 * x and SD =10 then, stored it in y

#head(x)
#boxplot(y)        #### to check for outliers ( no tiny circles in box plot that means no outliers)

xy <- data.frame(x=x, y=y)    #created dataframe
model_r <- lm(y~x , data = xy) # to fit the model we use lm function

plot(x,y, main =  "scatterplot") # scatterplot to view relationship between two variable x & y (plot shows linear regression with x, y also increases)
abline(model_r)      # to generate line 


###Q-1(b)
summary(model_r)     # summary of the fitted model

summary(model_r)$coefficients[,"Pr(>|t|)"][[2]] #extracting the b1 pvalue from the summary
# the p-value for B1 is 0.00560216. If the significance level is 0.05(alpha = 0.05) then our p-value is less than significance level, so we would reject the null hypothesis.


###Q-1(c)
set.seed(200)      # repeating (a) & (b) using different random numbers and storing it in different set.seed
x <-  rnorm(50, mean = 0,sd = 2) 
y <- rnorm(50, mean = 2 + 1.5 * x,sd = 10) 
xy <- data.frame(x=x, y=y)
model_r <- lm(y~x , data = xy)         #fit the model

plot(x,y, main =  "scatterplot")      # scatterplot
abline(model_r)


summary(model_r)
summary(model_r)$coefficients[,"Pr(>|t|)"][[2]]   #finding pvalue of b1

# the p-value for B1 is 0.2846968. If the significance level is 0.05(alpha = 0.05) then our p-value is "NOT" less than significance level, so we would "NOT" reject the null hypothesis.




############################################################################################
#####Q-2) 


dat1<- read.csv("voting_data.csv", header = T, stringsAsFactors = F)    #read the csv file
View(dat1)                  # to check if the file if loaded properly, using view function to view the data
#attach(dat1)

single_fit_m <- lm(dem_vote ~ marital_id, data = dat1)    #Fit model only considering x1(marital_id)
single_fit_s <- lm(dem_vote ~ state_id, data = dat1)      #Fit model only considering x2(state_id)
model <- lm(dem_vote ~ marital_id + state_id, data = dat1)  #Fit model using both x1 & x2(marital_id & state_id)
summary(model)      # Summary of model

table(dat1$dem_vote,dat1$marital_id)     # printing table of dem_vote & marital_id
bn_fit_m <- glm(dem_vote ~ marital_id, data = dat1, family = binomial(link = "logit"))
#as our y variable id binary and discrete so i am using glm for binomal logistic regression model
summary(bn_fit_m)
#exp(cbind(coef(bn_fit_m),confint(bn_fit_m)))

bn_fit_s <- glm(dem_vote ~ state_id, data = dat1, family = binomial(link = "logit"))
summary(bn_fit_s)
#exp(cbind(coef(bn_fit_s),confint(bn_fit_s)))

bn_fit_ms <- glm(dem_vote ~ marital_id + state_id, data = dat1, family = binomial(link = "logit"))
summary(bn_fit_ms)
#Summary of logistic regression considering both x1 & x2
#exp(cbind(coef(bn_fit_ms),confint(bn_fit_ms)))

#plot(marital_id,dem_vote)
#c2 <- cbind(marital_id, state_id~ marital_id)

#we are asked for the summary of the regression results and interpret the coefficient related to x1
# and it is shown by summary of(bn_fit_m)


###############################################################################
#####Q_3 (a) & (b)
set.seed(1)
N<-20     #Number of balls in bagA   

bagA <- c(rep('white',N/2), rep('black',N/2))   #bagA has some white and some black balls
kw <- 10   # white balls in bag b
kb <- 10   #black balls in bag b
BagB <- c(rep('white',kw), rep('black',kb))
R <- 5    # balls taken out from bag a 

size <- 10^4
# c(bag1, size, replace = FALSE, ordered = FALSE)
sample(bagA,replace=FALSE)     #shuffling bagA
Bag <-sample(bagA,R, replace = FALSE) # selecting R balls from A
BagB <- c(BagB,Bag)      # adding selected balls from A to B
t <- 10       #Number of balls selected from bag B
seq <- sample(BagB, t, replace = FALSE)    
seq           #selected sequence



target_seq <- c('white','black')
p <- 0.5
sample_size <- t
sp_1 <- c()
for(i in 1:10^4){
  a <- sample(seq, size = sample_size, replace = F, prob = c(seq(p,20)))
  h_obs <- which(a == "white")     #searching white balls from the sequence
  sp_1[i] <- h_obs/sample_size      
}
a
h_obs            

for(i in 1:length(h_obs)){
  
  find_black <- sum((h_obs[i]+1) == target_seq[2])  #searching for black ball, one after white ball 
  
}
find_black

success_func <- function(x){    
  target_seq <- c('black','white')
  success <- c()
  
  find_white <- which(a == target_seq[1]) 
  find_white
  
  for(i in 1:length(find_white)){
    find_white <- x[find_white[i] + 1] == target_seq[2]     #alternate method to find black methods
    
  }
  total_sucess <- sum(success == 2, na.rm=T)
  
  return(total_sucess)
}
