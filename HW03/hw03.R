#COMP421 HW03
#Murat Doğan
#27624
####################################################################################################
#Function definitions
safelog <- function(x) {
  return (log(x + 1e-100))
}
readData <- function(filename,skipsize,rowsize) {
  data<-read.csv(filename,skip=skipsize,nrows = rowsize,header=FALSE)
  return (data)
  
}
sigmoid <- function(X, W, w0) {
  return (1 / (1 + exp(-(X %*% W + w0))))
}
sigmoid <- function(a) {
  return (1 / (1 + exp(-a)))
}
gradient_W <- function(X, Y_truth, Y_predicted) {
  
  return (sapply(X = 1:ncol(Y_truth), function(c) colSums(matrix((Y_truth[,c] - Y_predicted[,c]), nrow = nrow(X), ncol = ncol(X), byrow = FALSE) * X)))
}

gradient_w0 <- function(Y_truth, Y_predicted) {
  return (colSums(Y_truth - Y_predicted))
}



####################################################################################################
#Get training data and labels

training_a <- readData("hw03_data_set_images.csv",0,25)
training_b <- readData("hw03_data_set_images.csv",39,25)
training_c <- readData("hw03_data_set_images.csv",78,25)
training_d <- readData("hw03_data_set_images.csv",117,25)
training_e <- readData("hw03_data_set_images.csv",156,25)

trlabel_a <- readData("hw03_data_set_labels.csv",0,25)
trlabel_b <- readData("hw03_data_set_labels.csv",39,25)
trlabel_c <- readData("hw03_data_set_labels.csv",78,25)
trlabel_d <- readData("hw03_data_set_labels.csv",117,25)
trlabel_e <- readData("hw03_data_set_labels.csv",156,25)

names(training_a)[1]=c("training")
names(training_b)<-names(training_a) 
names(training_c)<-names(training_a) 
names(training_d)<-names(training_a) 
names(training_e)<-names(training_a) 

#Get testing data and labels

test_a <- readData("hw03_data_set_images.csv",25,14)
test_b <- readData("hw03_data_set_images.csv",64,14)
test_c <- readData("hw03_data_set_images.csv",103,14)
test_d <- readData("hw03_data_set_images.csv",141,14)
test_e <- readData("hw03_data_set_images.csv",181,14)

tlabel_a <- readData("hw03_data_set_labels.csv",25,14)
tlabel_b <- readData("hw03_data_set_labels.csv",64,14)
tlabel_c <- readData("hw03_data_set_labels.csv",103,14)
tlabel_d <- readData("hw03_data_set_labels.csv",141,14)
tlabel_e <- readData("hw03_data_set_labels.csv",181,14)

names(test_a)[1]=c("test")
names(test_b)<-names(test_a) 
names(test_c)<-names(test_a) 
names(test_d)<-names(test_a) 
names(test_e)<-names(test_a) 

####################################################################################################
#Create training, test data and their labels.
training_data<-rbind(training_a,training_b,training_c,training_d,training_e)
test_data<-rbind(test_a,test_b,test_c,test_d,test_e)
training_labels<-rbind(trlabel_a,trlabel_b,trlabel_c,trlabel_d,trlabel_e)
test_labels<-rbind(tlabel_a,tlabel_b,tlabel_c,tlabel_d,tlabel_e)

#Renaming the label columns as y.
colnames(training_labels) <- "y"
colnames(test_labels) <- "y"

#Change labels A,B,C,D,E to 1,2,3,4,5
training_labels$y <- as.character(training_labels$y)
training_labels[training_labels== "A"] = 1
training_labels[training_labels== "B"] = 2
training_labels[training_labels== "C"] = 3
training_labels[training_labels== "D"] = 4
training_labels[training_labels== "E"] = 5

test_labels$y <- as.character(test_labels$y)
test_labels[test_labels== "A"] = 1
test_labels[test_labels== "B"] = 2
test_labels[test_labels== "C"] = 3
test_labels[test_labels== "D"] = 4
test_labels[test_labels== "E"] = 5
##############################################
eta <- 0.005
epsilon <- 1e-3
H <- 20
max_iteration <- 200

y_truth <-as.numeric(unlist(training_labels))
X<- as.matrix(training_data)
N <- length(y_truth)
D <- ncol(X)

# one-of-K-encoding
Y_truth <- matrix(0, N, 5)
Y_truth[cbind(1:N, y_truth)] <- 1

W <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01), D + 1, H)
v <- matrix(runif((D + 1) * H, min = -0.01, max = 0.01),H, 5)

Z <- sigmoid(cbind(1, X) %*% W)
y_predicted <- sigmoid(Z %*% v)


objective_values <- -sum(Y_truth * safelog(y_predicted))

# learn W and v using gradient descent and online learning
iteration <- 1
while (1) {
  for (i in sample(N)) {
    # calculate hidden nodes
    Z[i,] <- sigmoid(c(1, X[i,]) %*% W)
    # calculate output node
    y_predicted[i] <- sigmoid( Z[i,] %*% v)
    
    v <- v + eta * (y_truth[i] - y_predicted[i]) *  Z[i,]
    for (h in 1:H) {
      W[,h] <- W[,h] + eta * (Y_truth[i] - y_predicted[i]) * v[h + 1] * Z[i, h] * (1 - Z[i, h]) * c(1, X[i,])
    }
  }
  
  Z <- sigmoid(cbind(1, X) %*% W)
  y_predicted <- sigmoid(Z %*% v)
  objective_values <- c(objective_values, -sum(Y_truth * safelog(y_predicted) ))
  
  if (abs(objective_values[iteration + 1] - objective_values[iteration]) < epsilon | iteration >= max_iteration) {
    break
  }
  
  iteration <- iteration + 1
}

plot(1:(iteration + 1), objective_values,
     type = "l", lwd = 2, las = 1,
     xlab = "Iteration", ylab = "Error")

y_predicted <- 1 * (y_predicted > 0.5)
confusion_matrix <- table(y_predicted, Y_truth)
print(confusion_matrix)
