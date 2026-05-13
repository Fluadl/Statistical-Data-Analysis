# Daten
IQ  <- c(99, 105, 106, 115, 92, 108, 84, 105, 104, 118)
n   <- length(IQ)
ybar <- mean(IQ)
sigma2 <- 100

cat("Sample mean =", ybar, "\n\n")

# Weak prior: little prior knowledge
m0_weak <- 100
M0_weak <- 10000  # large variance

Mn_weak <- 1 / (1/M0_weak + n/sigma2)
mn_weak <- Mn_weak * (m0_weak/M0_weak + n*ybar/sigma2)
sd_weak <- sqrt(Mn_weak)
CI_weak <- mn_weak + c(-1.96, 1.96) * sd_weak

cat("Weak prior (little prior knowledge): N(100, 10000)\n")
cat("Posterior mean =", round(mn_weak, 3), "\n")
cat("Posterior SD   =", round(sd_weak, 3), "\n")
cat("95% CI =", paste0("(", round(CI_weak[1], 2), ", ", round(CI_weak[2], 2), ")"), "\n")


# 2) Strong prior around 100: N(100, 25)
m0_1 <- 100
M0_1 <- 25
Mn_1 <- 1 / (1/M0_1 + n/sigma2)
mn_1 <- Mn_1 * (m0_1/M0_1 + n*ybar/sigma2)
sd_1 <- sqrt(Mn_1)
CI_1 <- mn_1 + c(-1.96, 1.96) * sd_1

cat("2) Strong prior around 100: N(100, 25)\n")
cat("Posterior mean =", round(mn_1, 3), "\n")
cat("Posterior SD   =", round(sd_1, 3), "\n")
cat("95% Credible Interval =", paste0("(", round(CI_1[1], 2), ", ", round(CI_1[2], 2), ")"), "\n\n")

# 3) Strong prior around 70: N(70, 25)
m0_2 <- 70
M0_2 <- 25
Mn_2 <- 1 / (1/M0_2 + n/sigma2)
mn_2 <- Mn_2 * (m0_2/M0_2 + n*ybar/sigma2)
sd_2 <- sqrt(Mn_2)
CI_2 <- mn_2 + c(-1.96, 1.96) * sd_2

cat("3) Strong prior around 70: N(70, 25)\n")
cat("Posterior mean =", round(mn_2, 3), "\n")
cat("Posterior SD   =", round(sd_2, 3), "\n")
cat("95% Credible Interval =", paste0("(", round(CI_2[1], 2), ", ", round(CI_2[2], 2), ")"), "\n")
