#Anastasios Papazafeiropoulos 03118079

#1
ARD <- 100*abs(data$sensors-data$real)/data$real
MARD <- 1/5041 * sum(ARD)
print(paste0("Mean Absolute Relative Difference = ", MARD))

over <- sum((data$sensors-mean(data$sensors))*(data$real-mean(data$real)))
under <- sqrt(sum((data$sensors-mean(data$sensors))^2)*sum((data$real-mean(data$real))^2))
r <- over/under
print(paste0("Correlation Coefficient = ",r))



#2
tp <- tn <- fp <- fn <- 0

for(i in 1:nrow(data)){
  if(data[i,2]>70){
    if(data[i,3] >70){
      tn <- tn+1
    }
    else fp <- fp+1
  }
  else {
    if(data[i,3] < 70){
      tp <- tp+1
    }
    else fn <- fn+1
  }
}

print(paste0("False Positive: ",fp))
print(paste0("True Negative: ",tn))
print(paste0("True Positive: ",tp))
print(paste0("False Negative: ",fn))

sens <- tp /(tp+fn)
print(paste0("Sensitivity: ",sens))

spec <- tn/(tn+fp)
print(paste0("Specificity = ",spec))

posPredValue <- tp/(tp+fp)
print(paste0("Positive Predictive Value: ", posPredValue))

negPredValue <- tn/(tn+fn)
print(paste0("Negative Predictive Value: ", negPredValue))

acc <- (tp+tn)/(tp+tn+fn+fp)
print(paste0("Accuracy: ", acc))


#3
tp1 <- tn1 <- fp1 <- fn1 <-0

for (i in 1:nrow(data)) {
  if(data[i,2]>180){
    if(data[i,3]>180){
      tp1 <- tp1 + 1
    }
    else fn1 <- fn1 + 1
  }
  else {
    if(data[i,3]>180){
      fp1 <- fp1 + 1
    }
    else tn1 <- tn1 + 1
  }
}
print(paste0("True Positive: ",tp1))
print(paste0("True Negative ",tn1))
print(paste0("False Positive: ",fp1))
print(paste0("False Negaive: ",fn1))

sens1 <-tp1/(tp1+fn1)
print(paste0("Sensitivity: ",sens1))

spec1 <- tn1/(tn1+fp1)
print(paste0("Specificity: ",spec1))

posPredValue1 <- tp1/(tp1+fp1)
print(paste0("Positive Predictive Value: ", posPredValue1))

negPredValue1 <- tn1/(tn1+fn1)
print(paste0("Negative Predictive Value: ", negPredValue1))

acc1 <- (tp1+tn1)/(tp1+tn1+fn1+fp1)
print(paste0("Accuracy: ", acc1))
