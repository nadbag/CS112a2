sesame <- read.csv("https://tinyurl.com/wlgl63b")
library(stats)
library(ggplot2)

#question 1 replication (1a)
treat_sesame <- sesame[-c(22:42),]
control_sesame <- sesame[-c(1:21),]

treat_lm <- lm(post.test ~ pre.test, data = treat_sesame)

control_lm <- lm(post.test ~ pre.test, data = control_sesame)
summary(treat_lm)

plot(sesame$pre.test,sesame$post.test, xlab = "Pre Test Results", ylab = "Post Test Results", main="Treatment effect in grade 4")
points(control_sesame$pre.test,control_sesame$post.test, col = "red")
points(treat_sesame$pre.test, treat_sesame$post.test, col = "blue")
abline(treat_lm, col = "blue")
abline(control_lm, col = "red")

#1b
#the following loop finds the smallest change for each data point that will create a negative treatment effect, 
#as long as the change is less than 100
for (i in 1:21) {
  new_sesame <- sesame[-c(22:42),]
  for (j in 1:100){
    new_sesame[i,1] <- new_sesame[i,1]-j
    new_lm <- lm(post.test ~ pre.test, data = new_sesame)
    if ((new_lm$coef[1]+new_lm$coef[2]*70 < control_lm$coef[1]+control_lm$coef[2]*70)&(new_lm$coef[1]+new_lm$coef[2]*130 < control_lm$coef[1]+control_lm$coef[2]*130)){
      cat("row number ", i ," is good for " , j)
      break
    }  
  }
}

neg_sesame <- sesame
neg_sesame[11,1] <- neg_sesame[11,1] - 8
neg_lm <- lm(post.test ~ pre.test, data = neg_sesame[-c(22:42)])

#ploting the new data
plot(neg_sesame$pre.test,neg_sesame$post.test, xlab = "Pre Test Results", ylab = "Post Test Results", main="treatment effect in grade 4")
points(control_sesame$pre.test,control_sesame$post.test, col = "red")
points(neg_sesame$pre.test, neg_sesame$post.test, col = "blue")
points(neg_sesame[11,1],neg_sesame[11,2], col = "green") #this is the point that was changed, it is presented in green
abline(neg_lm, col = "blue")
abline(control_lm, col = "red")


#1c
error_lm <- lm(post.test ~ treatment + pre.test + treatment:pre.test, data=sesame)

error_lm.sim <- sim(error_lm)
plot (0, 0, xlim=range (sesame$pre.test), ylim=c(-5,10),
      xlab="Pre-test", ylab="Treatment Effect",
      main="Treatment effect in grade 4")
abline (0, 0, lwd=.5, lty=2)
for (i in 1:20){
  curve (error_lm.sim@coef[i,2] + error_lm.sim@coef[i,4]*x, lwd=.5, col="gray",
         add=TRUE)}
curve (coef(error_lm)[2] + coef(error_lm)[4]*x, lwd=.5, add=TRUE)

#question 2

tinting = read.csv(url("https://tinyurl.com/v4bq99k"))
library("Matching")
library("arm")
library(dplyr)

lm_tint <- lm(csoa ~ age+sex+target+I(tint != "no") + I(as.numeric(tint!= "no")*age), data = tinting)
summary(lm_tint)

sim_tint <- sim(lm_tint, n.sims = 1000)

ages <- c(20,30,40,50,60,70,80)

treat_means <- c()
treat_confidence <- data.frame(ages,low,high)

#2a interval and mean for treated females by age
for (j in 1:7){
  predictions <- c()
  for (i in 1:1000) {
    predictions[i] <- ages[j]*sim_tint@coef[i,2]+sim_tint@coef[i,5]+ages[j]*sim_tint@coef[i,6]+sim_tint@coef[i,1]+rnorm(1,0,sim_tint@sigma[i])
  } #both female and highcon are the are control values
  treat_means[j] <- mean(predictions)
  treat_confidence[j,2] <- quantile(predictions,0.025)
  treat_confidence[j,3] <- quantile(predictions,0.975)
  #age_percentiles <- rbind(age_percentiles,quantile(predictions,c(0.025,0.975)))
}
treat_confidence$means <- treat_means

no_treat_means <- c()
no_treat_confidence <- data.frame(ages,low,high)

#2b interval and mean for non-treated females by age

for (j in 1:7){
  predictions <- c()
  for (i in 1:1000) {
    predictions[i] <- ages[j]*sim_tint@coef[i,2]+ages[j]*sim_tint@coef[i,6]+rnorm(1,0,sim_tint@sigma[i])
  }
  no_treat_means[j] <- mean(predictions)
  no_treat_confidence[j,2] <- quantile(predictions,0.025)
  no_treat_confidence[j,3] <- quantile(predictions,0.975)
}
no_treat_confidence$means <- no_treat_means
no_treat_confidence$means

data.frame(ages,no_treat_confidence$means,treat_confidence$means)
treat_confidence
no_treat_confidence
#the results for non-treated females

plot(ages,treat_confidence$means, col = "green", xlab = "Age", ylab = "Mean for Treated Females", main = "Mean for Treated Females by Age")
plot(treat_confidence$ages,treat_confidence$high, col= "red",xlab = "Age", ylab = "Upper Limit Confidence Interval for Treated Females", main = "Coefficients for Treated Females by Age")
plot(treat_confidence$age, treat_confidence$low, col = "blue", xlab = "Age", ylab = "Lower Limit Confidence Interval for Treated Females", main = "Coefficients for Treated Females by Age")
plot(ages,no_treat_confidence$means, col = "green", xlab = "Age", ylab = "Mean for Treated Females", main = "Mean for Non-Treated Females by Age")
plot(no_treat_confidence$ages,no_treat_confidence$high, col= "red",xlab = "Age", ylab = "Upper Limit Confidence Interval for Non-Treated Females", main = "Coefficients for Treated Females by Age")
plot(no_treat_confidence$age, no_treat_confidence$low, col = "blue", xlab = "Age", ylab = "Lower Limit Confidence Interval for Non-Treated Females", main = "Coefficients for Treated Females by Age")

#question 3

r_sqrd <- function(real_y, y_hat) {
  TSS_vec <- c()
  for (i in 1:length(lalonde$re78)) {
    TSS_vec[i] <- (c(real_y)[i] - mean(real_y))^2
  }
  RSS_vec <- c()
  for (j in 1:length(real_y)) {
    RSS_vec[j] <- (c(real_y)[j] - c(y_hat)[j])^2
  }
  1-(sum(RSS_vec)/sum(TSS_vec))
  return(1-(sum(RSS_vec)/sum(TSS_vec)))
}

lalonde <- data(lalonde)
lm_lalonde <- lm(re78 ~., data = lalonde)
lalonde_prediction <- predict(lm_lalonde)

summary(lm_lalonde)
r_sqrd(lalonde$re78, lalonde_prediction)

#question 4
library("foreign")
setwd("~/CS112")
mazedata <- read.dta("mazedata1.dta")

treat_vec <- c()

for (i in 1:length(mazedata$treatment)) {
  if (mazedata[i,6] == "Caste Revealed"){
    treat_vec[i] <- 1
  }
  else {
    treat_vec[i] <- 0
  }
}

mazedata$treat_binari <- treat_vec

bs_samples <- c()

for (i in 1:10000) {
  bs_data <- sample_n(mazedata,length(mazedata$treat_binari)*2/3, replace = TRUE)
  bs_samples[i] <- mean(bs_data$treat_binari)
}

bs_coefs_low <- quantile(bs_samples,0.025)
bs_coefs_high <- quantile(bs_samples,0.975)

maze_coefs <- confint(lm(round1 ~ treat_binari, data = mazedata))

data.frame(bs_coefs_low, bs_coefs_high, maze_coefs[2,1],maze_coefs[2,2])

hist(bs_samples, xlab = "Treatment Effect Mean", ylab = "Frequency", main = "Treatment Effect Mean by Frequency")

