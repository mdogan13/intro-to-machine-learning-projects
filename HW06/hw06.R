#COMP421 HW06
#Murat Doğan
#27624
library(mixtools)
library(MASS)
# Generate data
mu1 = c(2.5,2.5)
mu2 =  c(-2.5,2.5)
mu3 = c(-2.5,-2.5)
mu4 = c(2.5,-2.5)
mu5 =c(0,0)
sigma1 = matrix(c(0.8,-0.6,-0.6,0.8), 2, 2)
sigma2 = matrix(data = c(0.8,0.6,0.6,0.8), nrow = 2, ncol = 2)
sigma3 = matrix(data = c(1.6,0,0,1.6), nrow = 2, ncol = 2)
density1 = mvrnorm(n = 50, mu1, sigma1, tol = 1e-06)
density2 = mvrnorm(n = 50, mu2, sigma2, tol = 1e-06)
density3 = mvrnorm(n = 50, mu3, sigma1, tol = 1e-06)
density4 = mvrnorm(n = 50, mu4, sigma2, tol = 1e-06)
density5 = mvrnorm(n = 100, mu5, sigma3, tol = 1e-06)

data = rbind(density1,density2,density3,density4,density5)

# Set K and N
K = 5
N = 300
color=rep("red",300)
# Initialize centroids randomly first
centroids <- data[sample(1:N, K),]
# Run k means
for (k in 1:10){
  D <- as.matrix(dist(rbind(centroids, data), method = "euclidean"))
  D <- D[1:nrow(centroids), (nrow(centroids) + 1):(nrow(centroids) + nrow(data))]
  assignments <<- sapply(1:ncol(D), function(i) {which.min(D[,i])})
  # Add assignments to the data, seperate into clusters, calculate new centroids
  data_with_assignments=cbind(data,assignments)
  data_with_assignments=cbind(data_with_assignments,color)
  data_with_assignments.df = as.data.frame(data_with_assignments,stringsAsFactors=FALSE)
  cluster1 = data_with_assignments.df[data_with_assignments.df$assignments==1,]
  cluster2 = data_with_assignments.df[data_with_assignments.df$assignments==2,]
  cluster3 = data_with_assignments.df[data_with_assignments.df$assignments==3,]
  cluster4 = data_with_assignments.df[data_with_assignments.df$assignments==4,]
  cluster5 = data_with_assignments.df[data_with_assignments.df$assignments==5,]
  cluster1[1:2] <- lapply(cluster1[1:2], as.numeric)
  cluster2[1:2] <- lapply(cluster2[1:2], as.numeric)
  cluster3[1:2] <- lapply(cluster3[1:2], as.numeric)
  cluster4[1:2] <- lapply(cluster4[1:2], as.numeric)
  cluster5[1:2] <- lapply(cluster5[1:2], as.numeric)
  c1new_centroid = c(mean(cluster1$V1),mean(cluster1$V2))
  c2new_centroid = c(mean(cluster2$V1),mean(cluster2$V2))
  c3new_centroid = c(mean(cluster3$V1),mean(cluster3$V2))
  c4new_centroid = c(mean(cluster4$V1),mean(cluster4$V2))
  c5new_centroid = c(mean(cluster5$V1),mean(cluster5$V2))
  centroids = rbind(c1new_centroid,c2new_centroid,c3new_centroid,c4new_centroid,c5new_centroid)
  
}

# Add colors for plotting
data_with_assignments.df$assignments <- as.numeric(as.character( data_with_assignments.df$assignments ))
data_with_assignments.df$color[data_with_assignments.df$assignments==1]="blue"
data_with_assignments.df$color[data_with_assignments.df$assignments==2]="red"
data_with_assignments.df$color[data_with_assignments.df$assignments==3]="green"
data_with_assignments.df$color[data_with_assignments.df$assignments==4]="darkorange"
data_with_assignments.df$color[data_with_assignments.df$assignments==5]="purple"


plot(data_with_assignments.df[,1],data_with_assignments.df[,2], type = "p", pch=19,col=data_with_assignments.df$color, xlim = c(-6, 6), ylim = c(-6, 6), xlab="x1", ylab="x2")


mixtools::ellipse(mu1, sigma1, npoints = 50, newplot = FALSE)
mixtools::ellipse(mu2, sigma2, npoints = 50, newplot = FALSE)
mixtools::ellipse(mu3, sigma1, npoints = 50, newplot = FALSE)
mixtools::ellipse(mu4, sigma2, npoints = 50, newplot = FALSE)
mixtools::ellipse(mu5, sigma3, npoints = 50, newplot = FALSE)
###############################################################
#Expectation-Maximization

means = rbind(c1new_centroid,c2new_centroid,c3new_centroid,c4new_centroid,c5new_centroid)
cov1 = sigma1
cov2 = sigma2
cov3 = sigma1
cov4 = sigma2
cov5 = sigma3
assignment_counts = table(assignments)
mixture_weights = c(assignment_counts[names(assignment_counts)==1]/50,assignment_counts[names(assignment_counts)==2]/50,assignment_counts[names(assignment_counts)==3]/50,assignment_counts[names(assignment_counts)==4]/50,assignment_counts[names(assignment_counts)==5]/100)

mvpdf <- function(x, mu, sigma) {
  if (det(sigma) == 0) {
    warning("Determinant is equal to 0.")
  }
  apply(x, 1, function(x) exp(-(1/2) * (t(x) - mu) %*% MASS::ginv(sigma) %*% 
                                +                                 t(t(x) - mu))/sqrt(det(2 * pi * sigma)))
}
for(i in 1:3){
  # E step
  
  # Calculating pdf for each
  z <- cbind(mvpdf(x = data, mu = means[1, ], sigma = cov1), 
             mvpdf(x = data , mu = means[2, ], sigma = cov2), 
             mvpdf(x = data, mu = means[3, ], sigma = cov3),
             mvpdf(x = data, mu = means[4, ], sigma = cov4),
             mvpdf(x = data, mu = means[5, ], sigma = cov5))
  
  r <- cbind((mixture_weights[1] * z[, 1])/rowSums(t((z * mixture_weights))), 
             (mixture_weights[2] * z[, 2])/rowSums(t((z * mixture_weights))), 
             (mixture_weights[3] * z[, 3])/rowSums(t((z * mixture_weights))),
             (mixture_weights[4] * z[, 4])/rowSums(t((z * mixture_weights))),
             (mixture_weights[5] * z[, 5])/rowSums(t((z * mixture_weights))))
  
  # M step
  
  mc <- colSums(r)
  
  # Update mixing components and centroids(means).
  mixture_weights <- mc/NROW(data)
  
  means <- rbind(colSums(data * r[, 1]) * 1/mc[1], 
                 colSums(data * r[, 2]) * 1/mc[2], 
                 colSums(data * r[, 3]) * 1/mc[3],
                 colSums(data * r[, 4]) * 1/mc[4],
                 colSums(data * r[, 5]) * 1/mc[5])
  
  # Update Covariance matrix.
  cov1 <- t(r[, 1] * t(apply(data, 1, function(x) x - means[1, ]))) %*% 
    (r[, 1] * t(apply(data, 1, function(x) x - means[1, ]))) * 1/mc[1]
  
  cov2 <- t(r[, 2] * t(apply(data, 1, function(x) x - means[2, ]))) %*% 
    (r[, 2] * t(apply(data, 1, function(x) x - means[2, ]))) * 1/mc[2]
  
  cov3 <- t(r[, 3] * t(apply(data, 1, function(x) x - means[3, ]))) %*% 
    (r[, 3] * t(apply(data, 1, function(x) x - means[3, ]))) * 1/mc[3]
  
  cov4 <- t(r[, 4] * t(apply(data, 1, function(x) x - means[4, ]))) %*% 
    (r[, 4] * t(apply(data, 1, function(x) x - means[4, ]))) * 1/mc[4]
  
  cov5 <- t(r[, 5] * t(apply(data, 1, function(x) x - means[5, ]))) %*% 
    (r[, 5] * t(apply(data, 1, function(x) x - means[5, ]))) * 1/mc[5]
}


print("Centroids after EM:")
print(means)
mixtools::ellipse(means[1,], sigma3, npoints = 50, newplot = FALSE)
mixtools::ellipse(means[2,], sigma2, npoints = 50, newplot = FALSE)
mixtools::ellipse(means[3,], sigma1, npoints = 50, newplot = FALSE)
mixtools::ellipse(means[4,], sigma2, npoints = 50, newplot = FALSE)
mixtools::ellipse(means[5,], sigma1, npoints = 50, newplot = FALSE)