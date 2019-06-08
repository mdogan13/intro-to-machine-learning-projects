#COMP421 HW04
#Murat Doğan
#27624
###########################################################################
# read data into memory
training_data<-read.csv("hw04_data_set.csv",skip=0,nrows = 100)
test_data<-read.csv("hw04_data_set.csv",skip=100,nrows = 33)


# get x and y values
x_train <- training_data$x
y_train <- training_data$y
x_test <- test_data[,1]
y_test <- test_data[,2]

# get number of classes and number of samples
K <- max(y_train)
N <- length(y_train)


minimum_value <- 0
maximum_value <- +60
bin_width <- 3
data_interval <- seq(from = minimum_value, to = maximum_value, by = 0.01)
left_borders <- seq(from = minimum_value, to = maximum_value - bin_width, by = bin_width)
right_borders <- seq(from = minimum_value + bin_width, to = maximum_value, by = bin_width)
##############################################################################################
#Histogram estimator
p_hat<-c()

for(i in 1:length(left_borders)){
  binelems<-c()
  for(j in 1:length(x_train)){
    if(left_borders[i] < x_train[j] & x_train[j] <= right_borders[i]){
      binelems<-c(binelems,y_train[j])
    }
    if(!is.null(binelems)){
      p_hat[i]<-mean(binelems)
    }
  
  }
  
}

plot(x_train,y_train,col="blue",pch=19,xlim=c(0,60),ylim=c(min(y_train),max(y_train)),ylab = "", xlab = "")
par(new=TRUE)
plot(x_test,y_test,col="red",pch=19,xlim=c(0,60), ylim=c(min(y_train),max(y_train)),ylab = "y", xlab = "x",main = sprintf("h = %g, Histogram Estimator", bin_width))

legend(45,80, legend=c("training", "test"),
       col=c("blue", "red"),lwd=1,lty=c(2,4))

for (b in 1:length(left_borders)) {
  lines(c(left_borders[b], right_borders[b]), c(p_hat[b], p_hat[b]), lwd = 2, col = "black")
  if (b < length(left_borders)) {
    lines(c(right_borders[b], right_borders[b]), c(p_hat[b], p_hat[b + 1]), lwd = 2, col = "black") 
  }
}

#Calculate RMSE
  sum<-0
  ctr<-1
  binctr<-bin_width
  for(i in 1:length(left_borders)){
    for(j in 1:length(x_test)){
      if(left_borders[i] < x_test[j] & x_test[j] <= right_borders[i]){
        sum<-sum+(y_test[j]-p_hat[i])^2
        if(ctr>=binctr){
          break
        }
      }
     
    }
    binctr<-binctr+bin_width
  }
  RMSE_histogram <-sqrt(sum/33)
  
  
#################################################################################################
#Running mean smoother
  g_hat<-c()
 
  for(i in 1:length(data_interval)){
    binelems<-c()
    for(j in 1:length(x_train)){
      if((data_interval[i] - 0.5 * bin_width) < x_train[j] & x_train[j] <= (data_interval[i] + 0.5 * bin_width)){
        binelems<-c(binelems,y_train[j])
      }
      g_hat[i]<-mean(binelems)
    }
  }
  
  plot(x_train,y_train,col="blue",pch=19,xlim=c(0,60),ylim=c(min(y_train),max(y_train)),ylab = "", xlab = "")
  par(new=TRUE)
  plot(x_test,y_test,col="red",pch=19,xlim=c(0,60), ylim=c(min(y_train),max(y_train)),ylab = "y", xlab = "x",main = sprintf("h = %g, Running Mean Smoother", bin_width))
  
  legend(45,80, legend=c("training", "test"),
         col=c("blue", "red"),lwd=1,lty=c(2,4))
  lines(data_interval, g_hat, type = "l", lwd = 2, col = "black")
  
  #RMSE for running mean
  sum<-0
  ctr<-1
  binctr<-bin_width
  for(i in 1:length(data_interval)){
    for(j in 1:length(x_test)){
      if((data_interval[i] - 0.5 * bin_width) < x_train[j] & x_train[j] <= (data_interval[i] + 0.5 * bin_width)){
        sum<-sum+(y_test[j]-g_hat[i])^2
        if(ctr>=binctr){
          break
        }
      }
      
    }
    binctr<-binctr+bin_width
  }
  RMSE_running_mean <-sqrt(sum/8000)
  
  
#################################################################################################
#Kernel smoother  
  
  k_hat<-c()
  bin_width<-1
  k_hat_num<-sapply(data_interval, function(x) {
   kfunc<- (1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2))
    sum(kfunc*y_train)
    }) 
  k_hat_denum<-sapply(data_interval, function(x) {
    sum((1 / sqrt(2 * pi) * exp(-0.5 * (x - x_train)^2 / bin_width^2)))
    
  }) 
  k_hat<-k_hat_num/k_hat_denum

  plot(x_train,y_train,col="blue",pch=19,xlim=c(0,60),ylim=c(min(y_train),max(y_train)),ylab = "", xlab = "")
  par(new=TRUE)
  plot(x_test,y_test,col="red",pch=19,xlim=c(0,60), ylim=c(min(y_train),max(y_train)),ylab = "y", xlab = "x",main = sprintf("h = %g, Kernel Smoother", bin_width))
  
  legend(45,80, legend=c("training", "test"),
         col=c("blue", "red"),lwd=1,lty=c(2,4))
  lines(data_interval, k_hat, type = "l", lwd = 2, col = "black")
  
  #Calculate RMSE
  sum<-0
  ctr<-1
  binctr<-bin_width
  
  for(i in 1:length(left_borders)){
    for(j in 1:length(x_test)){
      if(left_borders[i] < x_test[j] & x_test[j] <= right_borders[i]){
        sum<-sum+(y_test[j]-k_hat[i])^2
        
        if(ctr>=binctr){
          break
        }
      }
      
    }
    binctr<-binctr+bin_width
  }
  RMSE_kernel <-sqrt(sum/33)
 
  
  
sprintf("Regressogram => RMSE is %f when h is %d", RMSE_histogram, 3)
sprintf("Running Mean Smoother => RMSE is %f when h is %d", RMSE_running_mean, 3)
sprintf("Kernel Smoother => RMSE is %f when h is %d", RMSE_kernel, 1)
