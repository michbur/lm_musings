x <- 1L:10
y <- 1.5*x + 0.5 + rnorm(10)

#Add ones to x 
x_mat <- cbind(1,x)

# initalize theta vector
theta<- c(0,0)

# Number of the observations
n <- nrow(x_mat)

#Calculate cost
cost <- sum(((x_mat%*%theta)- y)^2)/(2*n)

# Set learning parameter
alpha <- 0.001

#Number of iterations
iterations <- 10000

# updating thetas using gradient update
for(i in 1:iterations) {
  theta[1] <- theta[1] - alpha * (1/n) * sum(((x_mat%*%theta)- y))
  theta[2] <- theta[2] - alpha * (1/n) * sum(((x_mat%*%theta)- y)*x_mat[,2])
}
