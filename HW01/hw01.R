#COMP421 HW01
#Murat Doğan
#27624
####################################################################################################
#Function definitions

safelog <- function(x){
  return (log(x+1e-100))
}

readData <- function(filename,skipsize,rowsize) {
  data<-read.csv(filename,skip=skipsize,nrows = rowsize,header=FALSE)
  return (data)
  
}

sample_mean <- function(training_x) {
  p <- c()
  for(j in 1:320) {
    p[j] <- mean(training_x[,j])
  }
  return(p)
}

calculate_scores <- function(p,c, datasize, data_matrix) {
  scores <- c()
  letterscore <- 0
  for(i in 1:datasize) {
    for (d in 1:320) {
      letterscore<-letterscore+(data_matrix[i,d]*safelog(p[d]) + (1-data_matrix[i,d]) * safelog(1-p[d])) 
    }
    scores[i]<-letterscore
    letterscore <- 0
  }
  scores<-scores+safelog(class_priors[c]) 
  return(scores)
}

####################################################################################################
#Get training data and labels

training_a <- readData("hw01_data_set_images.csv",0,25)
training_b <- readData("hw01_data_set_images.csv",39,25)
training_c <- readData("hw01_data_set_images.csv",78,25)
training_d <- readData("hw01_data_set_images.csv",117,25)
training_e <- readData("hw01_data_set_images.csv",156,25)

trlabel_a <- readData("hw01_data_set_labels.csv",0,25)
trlabel_b <- readData("hw01_data_set_labels.csv",39,25)
trlabel_c <- readData("hw01_data_set_labels.csv",78,25)
trlabel_d <- readData("hw01_data_set_labels.csv",117,25)
trlabel_e <- readData("hw01_data_set_labels.csv",156,25)

names(training_a)[1]=c("training")
names(training_b)<-names(training_a) 
names(training_c)<-names(training_a) 
names(training_d)<-names(training_a) 
names(training_e)<-names(training_a) 

####################################################################################################
#Get testing data and labels

test_a <- readData("hw01_data_set_images.csv",25,14)
test_b <- readData("hw01_data_set_images.csv",64,14)
test_c <- readData("hw01_data_set_images.csv",103,14)
test_d <- readData("hw01_data_set_images.csv",141,14)
test_e <- readData("hw01_data_set_images.csv",181,14)

tlabel_a <- readData("hw01_data_set_labels.csv",26,14)
tlabel_b <- readData("hw01_data_set_labels.csv",64,14)
tlabel_c <- readData("hw01_data_set_labels.csv",103,14)
tlabel_d <- readData("hw01_data_set_labels.csv",141,14)
tlabel_e <- readData("hw01_data_set_labels.csv",181,14)

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

#Calculate P(y=A),P(y=B),P(y=C),P(y=D),P(y=E)
class_priors <- sapply(X = 1:5, FUN = function(c) {mean(training_labels == c)})

#Calculate p1,p2,p3,p4,p5
p1 <- sample_mean(training_a)
p2 <- sample_mean(training_b)
p3 <- sample_mean(training_c)
p4 <- sample_mean(training_d)
p5 <- sample_mean(training_e)

####################################################################################################
#Calculating scores for training data
training_size <- 5 * 25
a_scores <-  calculate_scores(p1,1, training_size, training_data)
b_scores <-  calculate_scores(p2,2, training_size, training_data)
c_scores <-  calculate_scores(p3,3, training_size, training_data)
d_scores <-  calculate_scores(p4,4, training_size, training_data)
e_scores <-  calculate_scores(p5,5, training_size, training_data)

#Creating a score matrix for the training data
training_score_mtrx<-cbind(a_scores,b_scores,c_scores,d_scores,e_scores)
#Calculating the class assignments using the score matrix
class_assignments <- apply(X = training_score_mtrx, MARGIN = 1, FUN = which.max)
y_truth<-t(training_labels)
#Creating the confusion matrix for the test data
confusion_matrix <- table(class_assignments, y_truth)

####################################################################################################
#Calculating scores for the test data
test_size <- 5 * 14
a_test_scores <- calculate_scores(p1, 1, test_size, test_data)
b_test_scores <- calculate_scores(p2, 2, test_size, test_data)
c_test_scores <- calculate_scores(p3, 3, test_size, test_data)
d_test_scores <- calculate_scores(p4, 4, test_size, test_data)
e_test_scores <- calculate_scores(p5, 5, test_size, test_data)

#Creating a score matrix for the test data
test_score_mtrx<-cbind(a_test_scores,b_test_scores,c_test_scores,d_test_scores,e_test_scores)
#Calculating the class assignments using the score matrix
test_class_assignments <- apply(X = test_score_mtrx, MARGIN = 1, FUN = which.max)
y_truth<-t(test_labels)
#Creating the confusion matrix for the test data
test_confusion_matrix <- table(test_class_assignments, y_truth)

#Print the results
confusion_matrix
test_confusion_matrix

