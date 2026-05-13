set.seed(123)

#parameters
n <- 1000
alpha <- 3
beta  <- 2

#Beta(alpha, beta) on [0, 1]
# f(x) = dbeta(x, alpha, beta)

#Uniform(0, 1)
# g(x) = dunif(x) = 1

#Find c so that f(x) <= c * g(x)  -> c is max value of beta density
f <- function(x) dbeta(x, alpha, beta)
c_max <- optimize(f, interval = c(0, 1), maximum = TRUE)$objective

#accept–reject algo
x <- numeric(n)   #store accepted samples
k <- 0            #accept-counter
t <- 0            #total proposals

while (k < n) {
  y <- runif(1)   #propose y ~ Uniform(0,1)
  u <- runif(1)   #draw u ~ Uniform(0,1)
  
  #accept probability
  alpha_acc <- dbeta(y, alpha, beta) / (c_max * dunif(y))
  
  #accept or reject
  if (u < alpha_acc) {
    k <- k + 1
    x[k] <- y
  }
  t <- t + 1
}

cat("Proposals used:", t, "for", n, "accepted")

#Plot histogram and true Beta density
hist(x, breaks = 30, freq = FALSE,
     main = "Accept–Reject sampling from Beta(3,2)",
     xlab = "x", col = "gray", border = "white")

curve(dbeta(x, alpha, beta), from = 0, to = 1,
      add = TRUE, col = "red", lwd = 2)
legend("topright", legend = c("Sample histogram", "True Beta(3,2) density"),
       col = c("gray", "red"), lwd = c(10, 2), bty = "n")

#Compare empirical/theoretical quantiles
probs <- c(0.025, 0.25, 0.5, 0.75, 0.975)
emp_q  <- quantile(x, probs)
theo_q <- qbeta(probs, alpha, beta)

cat("\nEmpirical vs. theoretical quantiles:\n")
print(round(cbind(prob = probs, empirical = emp_q, theoretical = theo_q), 4))
