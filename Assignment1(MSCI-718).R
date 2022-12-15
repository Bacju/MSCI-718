#################################################################################
###Q-2
urn <- c(rep('black',2), rep('white',3), rep('red',3))
trail <- 10000

replacement <- matrix(urn, nrow = trail, ncol = length(urn))
no_replacement <- matrix(urn, nrow = trail, ncol = length(urn))

for(i in 1:trail){        
  replacement[i,] <- sample(urn, length(urn), replace = TRUE)   #sample function is used to have random sequence
  no_replacement[i,] <- sample(urn, length(urn), replace = FALSE) # replace = True when replacement is needed  and is False when replacement is not needed
}

fun <- function(x){
  urn <- c('black','white','red')   #took # different colors
  success <- c()                    #created a empty vector
  
  black <- which(x==urn[1])         #tried to find first black ball
  for (i in 1:length(black)) {       #for every black ball present, tried to find white ball by skipping 7 balls
    white <- x[black[i]+7] == urn[2]  #so there are 8 ball, the only possiblity in this case is first ball being black and the last being white
    success[i]<-sum(white,0)         #sum of white ball found
    
  }
  total_success <- sum(success ==1, na.rm = T) 
  return(total_success)
}

fun(c('black','white','blue','red','white','blue','black','white'))  #the function where we are checking the first and last ball

replacement_result <- apply(replacement, 1, fun) #so according to order of balls we have we will check it with replacement and without replacement
no_replacement_result <- apply(no_replacement, 1, fun)

(length(replacement_result) - sum(replacement_result == 0))/length(replacement_result)   # checking the prob with replacement and without replacement

(length(no_replacement_result) - sum(no_replacement_result == 0))/length(no_replacement_result)



##################################################################################################
#################################################################################################

###Q-3

set.seed(99)
int_n <- c(rep('black',20),rep('white',10), rep('red',7),rep('green',7))
lambda=-2
n_trial=20
polya_run <- function(int_n, lambda, n_trial){           #called function
  
  result <- matrix(1, nrow = n_trial, ncol = length(int_n)) #matrix
  result[1,] <- int_n  
  
  print(result[1,])     #print 1st row of all columns
  
  for(i in 2:n_trial){
    
    interval <- cumsum(result[i-1,])    # took cumulative sum
    
    draw <- runif(1)                  #picked a random number
    draw                              #print random number
    
    check <- c(interval > draw)*1   # check whether cumsum is greater than draw and if it isthat number is picked 
    
    check_select <- c()             #null list
    for(j in 1:length(check)){
      if(check[j] == 1){
        check_select <- j                 
        break
      } 
    }
    
    result[i,] <- result[i-1,]         
    result[i, check_select] <- result[i, check_select] + lambda
    
  }
}  

###Q-3(b)
set.seed(100)
polya_run

set.seed(101)
polya_run

set.seed(103)
polya_run()

###Q-3(c)

results <- polya_run(c(5,1,1,1), 2, 1000)
print(head(results))

pr1 <- set.seed(99)
pr2 <- set.seed(100)
pr3 <- set.seed(101)
pr4 <- set.seed(103)

list_polya <- list(pr1, pr2, pr3, pr4)
colorslist <- c('#030ffc','#0af50a','#fcb603','#e0091f')

for(i in 1:length(list_polya)){
  y_min <- min(list_polya[[i]])
  y_max <- max(list_polya[[i]])
  ?min
  plot(1, type="n", ylim= c(y_min, y_max), xlim=c(1,20), xlab='Time', ylab='Number of occurences')
  
  for(j in 1:4){
    lines(1:1000, list_polya[[i]][,j], col=colorslist[j], lwd=1.5)
  }
}



##############################################################################################
###############################################################################################################




###Q-4(a)

set.seed(200)   #set the value so that it gives same values every time code is run
rn_norm <- rnorm(50,mean=0, sd=2)
print(rn_norm)
#generates 50 random numbers with 0 mean and 2 standard deviation

###Q-4(b)

m <- mean(rn_norm, na.rm = FALSE)
print(m)      #checked mu

s<- sd(rn_norm)
print(s)        #checked sigma

vec<- c(0,1)
#initial values for mu and sigma

likeFunc <- function(vec){
  log_like_sum <- -sum(dnorm(rn_norm, mean = vec[1], sd = vec[2],
                             log = T))
  return(log_like_sum)
}         #called the likelihoodfunction

optim(vec,likeFunc, method='L-BFGS-B', lower = 0, upper = 50)$par
#the actual parameters are not exactly close to the original ones

###Q-4(c)
set.seed(200)
x = rnorm(5000, mean = 0, sd = 2)      #generating 5000 numbers with 0 mean and 2 standard deviation
print(x)
mean(x)
sd(x)
vec<- c(0,1)
likelihoodNLFunc <- function(vec){
  log_like_sum <- -sum(dnorm(x, mean = vec[1], sd = vec[2],
                             log = T))
  return(log_like_sum)
}             # repeated the above step for 5000 random numbers this time instead of 50

optim(vec,likelihoodNLFunc, method='L-BFGS-B', lower = 0, upper = 50)$par
#when more random numbers were generated (5000), the estimate parameters were more accurate than the other one where less random numbers were generated(50)


#################################################################################################################
########################################################################################################


###Bonus Q-2
### bonus Q-2(a)

library(fitdistrplus)

set.seed(200)        #used to get the same numbers when this code is executed
data<- rnorm(100, mean = 10, sd = 2)  #Generate 100 random number with 10 mu and 2 sigma
data

k_fold <- 10              # Asked to do 10 fold Cross-validation
data_s <- sample(data)    #to randomize the data

folds <- cut(seq(1, length(data_s)), breaks = k_fold, labels=FALSE) # assign index

testID <- which(folds==1)  #random numbers which are used to test the model(in this case its 10)
testD <- data_s[testID]
testD

trainD <- data_s[-testID]  #random numbers used to train the model
trainD

trained <- fitdist(trainD, "norm", method="mle")     #uses mle to know the parameters of train signal
trained$estimate    #uses normal distribution
test_result_n <- sum(dnorm(testD, trained$estimate, log = T))  
test_result_n 

trainedg<- fitdist(trainD, "gamma", method="mle")  #same step done for gamma distribution
trainedg$estimate[1]; trainedg$estimate[2]

test_result_gamma <- sum(dgamma(testD, shape = trainedg$estimate[1], rate = trainedg$estimate[2], log = T))  
test_result_gamma 

########### Used CV_fun from the pdf of topic 2 provided for exp and gamma here by changing exp to normal to do the c step
CV_fun <- function(n_fold, n_rep, uni_data, distribution){
  
  test_list <- list()      #Null list
  test_list_all <- list()   
  for(k in 1:n_rep){        
    
    uni_data_s <- sample(uni_data)
    # Split in n_fold subsets
    folds <- cut(seq(1, length(uni_data_s)), breaks = k_fold, labels=FALSE)
    
    for(i in 1:n_fold){ 
      testID <- which(folds==i) 
      testD <- uni_data_s[testID]
      trainD <- uni_data_s[-testID]
      
      if (distribution == "Normal") {           #checks if distribution is normal or not
        trained <- fitdist(trainD, "norm", method="mle")
        test_result <- sum(dnorm(testD, trained$estimate, log = T)) 
      }else if (distribution == "Gamma"){       #checks if the distribution is gamma or not
        trained <- fitdist(trainD, "gamma", method="mle")
        test_result <- sum(dgamma(testD, shape = trained$estimate[1], rate = trained$estimate[2], log = T))  
      }else{          #if the distrubution is not gamma or normal then quit
        print(paste("Unrecognized distribution requested", distribution, sep=" "))
        quit(status=1)
      }
      test_list[[i]] <- test_result
    }
    test_list_all[[k]] <- sum(unlist(test_list)) # the sum of all n-fold likelihood
  }
  return(test_list_all)
}





CV_norm <- CV_fun(n_fold = 10, n_rep = 20, uni_data = data, distribution = "Normal")
CV_gamma <- CV_fun(n_fold = 10, n_rep = 20, uni_data = data, distribution = "Gamma")

unlist(CV_norm)

unlist(CV_gamma)

mean(unlist(CV_norm))
mean(unlist(CV_gamma))




###Bonus Q-2(c)

set.seed(200)
rand_num <- rgamma(100,2,0.5)     #Asked to generate 100 random numbers from gamma distribution using shape(r)=2 & rate(lambda) = 0.5
rand_num

CV_norm <- CV_fun(n_fold = 10, n_rep = 20, uni_data = rand_num, distribution = "Normal")
CV_gamma <- CV_fun(n_fold = 10, n_rep = 20, uni_data = rand_num, distribution = "Gamma")

unlist(CV_norm)
unlist(CV_gamma)

mean(unlist(CV_norm))            #finds the mean
mean(unlist(CV_gamma))




