library(ggplot2)
library(ggthemes)
library(gridExtra)
library(ggsci)
library(e1071)


# Prepearing a new data set on plane that is not linear-separable
# Number of pints in the plane
#==============================================================================
set.seed(2018)
N = 40

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

# label the point according to the class
label = (ifelse((y > a*x + b)&(y < a*x + b + 1), 1, -1))
D = cbind.data.frame(x,y,label)


svmfit.linear = svm(formula = as.factor(label) ~ ., data = D, kernel='linear', cost = 1e5, scale = FALSE) 
summary(svmfit.linear)

#==============================================================================
svmfit.polynomial = svm(formula = as.factor(label) ~ ., data = D, kernel='polynomial', 
                        degree = 2, cost = 1e5, scale = FALSE) 

summary(svmfit.polynomial)
#====================================================================================
svmfit.radial = svm(formula = as.factor(label) ~ ., data = D, kernel='radial', 
                    cost = 1e5, scale = FALSE) 
summary(svmfit.radial)

#====================================================================================
boundaries_plot <- function(model) {
  
  u = expand.grid(x = seq(-1, 1, .01), y = seq(-1, 1, .01))
  
  model.pred = predict(model, u, decision.values = TRUE)
  f = attributes(model.pred)$decision.values
  Z = cbind(u, f)
  
  # Data set colored according to the class
  g = ggplot(data = D, aes(x,y)) + 
    geom_point(aes(col = as.factor(label)), size = 3) +
    labs(color = "Class")+
    scale_color_aaas()
  
  g + stat_contour(data = Z, aes(z=Z[,3]), breaks = 0, color = 'black', size = 1.2)+
    stat_contour(data = Z, aes(z=Z[,3]+1), breaks = 0, color = 'darkgrey', lty=2, size = 1) +
    stat_contour(data = Z, aes(z=Z[,3]-1), breaks = 0, color = 'darkgrey', lty=2, size = 1)
} 


grid.arrange(boundaries_plot(svmfit.linear), 
             boundaries_plot(svmfit.polynomial),
             boundaries_plot(svmfit.radial),
             ncol = 3)

# =========================================================================================
# Cross validation to choose SVM parameters

set.seed(2018)
svm.cv = tune(svm, as.factor(label) ~ ., data = D, kernel='radial',
                ranges = list(cost = c(1, 10, 100, 1000),
                              gamma = c(0.1, 0.5, 1, 2, 10)) )
svm.cv$best.parameters


svmfit.cv = svm(formula = as.factor(label) ~ ., data = D, kernel='radial', 
                cost = 10, 
                gamma = 0.5, scale = FALSE) 


summary(svmfit.cv)

boundaries_plot(svmfit.cv)
