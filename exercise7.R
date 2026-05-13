set.seed(123)
m <- 10000   #monte carlo draws

#integrand
f <- function(x) exp(-x) / (1 + x^2)
#true value
theta_true <- integrate(f, lower = 0, upper = 1)$value

#
#phi0(x) = 1  on (0,1) -> Uniform(0,1)
x0 <- runif(m, 0, 1)               #sample from Uniform(0,1)
phi0_x <- dunif(x0, 0, 1)          #phi0
w0 <- f(x0) / phi0_x               
theta0_hat <- mean(w0)             #IS estimate


#phi1(x) = exp(-x)  on (0,\infty)

x1 <- rexp(m, rate = 1)            #sample from Exp(1)
phi1_x <- dexp(x1, rate = 1)       #phi1

# only values with 0 < x <= 1 contribute to the integral
w1 <- ifelse(x1 <= 1, f(x1) / phi1_x, 0)

theta1_hat <- mean(w1)


#phi2(x) = exp(-x) / (1 - exp(-1))  on (0,1)

u <- runif(m)                             #Uniform(0,1)
x2 <- -log(1 - u * (1 - exp(-1)))         #X = F2^{-1}(U)
phi2_x <- exp(-x2)/(1 - exp(-1))        #phi2

w2 <- f(x2)/phi2_x

theta2_hat <- mean(w2)



#Compare
results <- data.frame(
  envelope = c("phi0:",
               "phi1:",
               "phi2:"),
  estimate = c(theta0_hat, theta1_hat, theta2_hat)
)


print(results)
cat("True value (numerical):", theta_true, "\n")
