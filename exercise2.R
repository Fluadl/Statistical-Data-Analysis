library(MASS)
data("Boston")

# Nur die relevanten Variablen ansehen
summary(Boston[, c("medv", "lstat", "age")])

# Ein paar einfache Plots
par(mfrow = c(1, 2))
plot(Boston$lstat, Boston$medv,
     xlab = "lstat (% lower status)", ylab = "medv (Hauspreis)",
     main = "medv vs. lstat")
plot(Boston$age, Boston$medv,
     xlab = "age (% vor 1940)", ylab = "medv",
     main = "medv vs. age")
par(mfrow = c(1, 1))

cor(Boston[, c("medv", "lstat", "age")])


#med = B_0 + B_1*lstat + B_2*age + eps
ols_mod <- lm(medv ~ lstat + age, data = Boston)
summary(ols_mod)


library(MCMCpack)

set.seed(123)

b0_vague <- c(0, 0, 0)        # prior means for (intercept, lstat, age)
B0_vague <- diag(1e-6, 3)     # small precision -> vague prior
c0 <- 0.01                    # vague prior on sigma^2
d0 <- 0.01

bayes_vague <- MCMCregress(
  medv ~ lstat + age,
  data = Boston,
  b0 = b0_vague, B0 = B0_vague,
  c0 = c0, d0 = d0,
  mcmc = 10000, burnin = 2000, thin = 5
)

summary(bayes_vague)

b0_strong <- c(0, -1, 0)      # prior means (intercept=0, lstat=-1, age=0)
prec_intercept <- 1e-6
prec_lstat     <- 1/0.01      # variance 0.01 => strong prior
prec_age       <- 1e-4
B0_strong <- diag(c(prec_intercept, prec_lstat, prec_age))

set.seed(123)
bayes_strong <- MCMCregress(
  medv ~ lstat + age,
  data = Boston,
  b0 = b0_strong, B0 = B0_strong,
  c0 = c0, d0 = d0,
  mcmc = 10000, burnin = 2000, thin = 5
)

summary(bayes_strong)

