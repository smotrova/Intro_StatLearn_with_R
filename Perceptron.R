# Perceptron
library(ggplot2)
library(ggthemes)
library(colorRamps)
library(ggsci)
library(e1071)

#===============================================================================
perceptron <- function(data, plot) {
  
  # initial guess - perceptron coefficients are 0
  beta = rep(0, 2)
  beta0 = 0
  
  # Max number of interation
  max_iter = 1000
  
  iter = 0
  D_unclass = data
  
  iter = 0
  while (nrow(D_unclass) != 0 & iter <= max_iter) {
    # Choose arbitrary point deom data set
    i = sample.int(nrow(D_unclass),1)
    
    # Correct the separator line with respect to this point 
    if( (beta0 + beta[1]*D_unclass[i, 1]+beta[2]*D_unclass[i, 2])*D_unclass[i, 3] <= 0) {
      beta = beta + c(D_unclass[i, 1], D_unclass[i, 2])*D_unclass[i, 3]
      beta0 = beta0 + D_unclass[i, 3]
    }
    
    # Check the status of other points with respect to updated separator
    # Save all incorrectly clissified points in new data set, the set of unclassified points
    
    D_unclass_new = data.frame()
    
    for (i in (1:nrow(data))) {
      if ( (beta0 + beta[1]*data[i, 1]+beta[2]*data[i, 2])*data[i, 3] <= 0) {
        D_unclass_new = rbind(D_unclass_new, D[i,])
      }
    }
    D_unclass = D_unclass_new
    iter = iter + 1
  }
  
  if (iter > max_iter) {
    print(paste("Perceptron algorithm: did not converge within the specified number of iterations",
                as.character(max_iter)))
    
  } else {
    g = plot + geom_abline(slope = -beta[1]/beta[2], intercept = -beta0/beta[2], lty = 1, color = "darkgrey", size = 1)
    return (g)
  }
  
}

#======================================================================================
# Prepearing a data set on plane
# Number of points in the plane
N = 20

# Generate a set of points
x = runif(N, -1, 1)
y = runif(N, -1, 1)

# choose a random line in the plane 
x1 = sample(runif(N, -1, 1), 1)
x2 = sample(runif(N, -1, 1), 1)

y1 = sample(runif(N, -1, 1), 1)
y2 = sample(runif(N, -1, 1), 1)

a = (y1 - y2)/(x1-x2)
b = y1 - a*x1

# label the point acording to the random line
label = (ifelse(y > a*x + b, 1, -1))

D = cbind.data.frame(x,y,label)

# Data set separeted by line and colored according to the class
g = ggplot(data = D, aes(x,y)) + 
  geom_point(aes(col = as.factor(label)), size = 2) +
#  geom_abline(slope = a, intercept = b, lty = 2) +
    labs(color = "Class")+
      scale_color_aaas()

perceptron(D, g)


# support vector classifier
# max margins classifier => cost is large
# soft margine classifier => cost is low

svmfit = svm(formula = as.factor(label) ~ ., data = D, kernel='linear', cost = 1.5, scale = FALSE) 
summary(svmfit)

ggplot(data = D, aes(x,y)) + 
  geom_point(aes(col = as.factor(label)), size = 2) +
    geom_abline(slope = -sum(svmfit$coefs*svmfit$SV[,1])/sum(svmfit$coefs*svmfit$SV[,2]), 
                intercept = svmfit$rho/sum(svmfit$coefs*svmfit$SV[,2]), lty = 1) +
        geom_abline(slope = -sum(svmfit$coefs*svmfit$SV[,1])/sum(svmfit$coefs*svmfit$SV[,2]), 
                  intercept = svmfit$rho/sum(svmfit$coefs*svmfit$SV[,2]) + 1.0/sum(svmfit$coefs*svmfit$SV[,2]), lty = 2) +
           geom_abline(slope = -sum(svmfit$coefs*svmfit$SV[,1])/sum(svmfit$coefs*svmfit$SV[,2]), 
                    intercept = svmfit$rho/sum(svmfit$coefs*svmfit$SV[,2]) - 1.0/sum(svmfit$coefs*svmfit$SV[,2]), lty = 2) +
               labs(color = "Class")+
                      scale_color_aaas()

