set.seed(42)

n <- 200
d <- 3

# Covariancematrix 
Sigma <- matrix(c( 1.0, -0.5,  0.5,
                   -0.5,  1.0, -0.5,
                   0.5, -0.5,  1.0),
                nrow = d, byrow = TRUE)

# Meanvector
mu <- matrix(c(0, 1, 2), ncol = 1)

#Z:nxd Matrix,  iid N(0,1)
Z <- matrix(rnorm(n * d), nrow = n, ncol = d)

#spectral decomposition
eig  <- eigen(Sigma)
Lambda <- diag(eig$values)
P      <- eig$vectors

#Q = \Sigma^{1/2} = P \Lambda^{1/2} P^T
Q <- P %*% sqrt(Lambda) %*% t(P)

#J: n x 1 Vector with 1's
J <- matrix(1, nrow = n, ncol = 1)

#Multivariate Normal-Pulls: X ~ N_d(\mu, \Sigma)
X <- Z %*% Q + J %*% t(mu)

# Test:
colMeans(X)  # Should be about (0, 1, 2)
cov(X)       # Should be about \Sigma

