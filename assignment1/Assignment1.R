#question 1
p1 <- (0.72)^(1/6)
d1 <- 0.01
m1 <- 20-3
clv1 <- (m1*(1+d1))/(1+d1-p1)
#question 2
p <- (0.67)^(1/6)
d <- 0.01
m <- 23-3
clv <- (m*(1+d))/(1+d-p)

#question 3
m3 <- 26.8
d3 <- 0.0153
S1 <- 67/100
S2 <- 50/100              
S3 <- 41/100
S4 <- 34/100
S5 <- 31/100
S6 <- 28/100

r1 <- S1
r2 <- S2/S1
r3 <- S3/S2
r4 <- S4/S3
r5 <- S5/S4
r6 <- S6/S5


pTotal <- (r1+r2+r3+r4+r5+r6)/6
clv3 <- (m3*(1+d3))/(1+d3-pTotal)

###question 3 
p33 <- 28/100
clv33 <- (m3*(1+d3))/(1+d3-p33)

#Question 4
active_cust <- c(100,67,50,41,34,31,28)
lost <- -diff(active_cust[1:5])
active <- active_cust[2:5]

loop.lik<-function(params) {
  a<-params[1]
  b<-params[2]
  ll<-0
  for (i in 1:length(lost)) {
    ll<-ll+lost[i]*log(beta(a+1,b+i-1)/beta(a,b))
  }
  ll<-ll+active[i]*log(beta(a,b+i)/beta(a,b))
  return(-ll)    #return the negative of the function to maximize likelihood
} 

#find parameters for a and b, par=c(1,1) are the starting values
sBG<-optim(par=c(1,1),loop.lik)

a<-sBG$par[1]
b<-sBG$par[2]

#calculate retention using model parameters
t<-1:length(active)
r_pred<-r_sBG(a,b,t)

#question 5
# Required library
# Defining the data
time <- 0:4
customers <- c(100, 67, 50, 41, 34)

# Initial values for a and b
a <- 1
b <- 1

# Function to compute s(t) for given a, b, and t
s <- function(t, a, b) {
  (b + t - 1) / (a + b + t - 1)
}

# Fitting the Beta-Geometric model
fit <- optim(par = c(a, b), fn = function(par) {
  sum((log(s(time, par[1], par[2])) + log(1 - s(time + 1, par[1], par[2]))) * customers)
}, method = "L-BFGS-B", lower = c(0, 0))

# Extracting the estimated parameters
estimated_a <- fit$par[1]
estimated_b <- fit$par[2]

# Printing the estimated parameters
print(paste("Estimated parameters: a =", estimated_a, "b =", estimated_b))

#Question 6 
t=6
ret6 <- (b+t-1)/(a+b+t-1)
