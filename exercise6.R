mc_integral <- function(g, a, b, m_max = 10000, exact = NULL, main = "") {
  # 1) Pull numbers uniformly
  x <- runif(m_max, min = a, max = b)
  
  # 2) Values of g(X)
  gx <- g(x)
  
  # 3) Running means of g(X)
  running_means <- cumsum(gx) / (1:m_max)
  
  # 4) MC-Estimate of the Integral: (b-a)*E[g(X)]
  theta_hat <- (b - a) * running_means
  
  # 5) Plot the convergence
  plot(1:m_max, theta_hat, type = "l",
       xlab = "m", ylab = "theta_hat",
       main = main)
  if (!is.null(exact)) {
    abline(h = exact, col = "red", lwd = 2)
  }
  
  # 6) Print final results in console
  approx_value <- theta_hat[m_max]
  cat("Approximate value:", round(approx_value, 6),
      "| Exact value:", ifelse(is.null(exact), "N/A", round(exact, 6)), "\n")
  
  invisible(theta_hat)
}


set.seed(50)

# 1) \int_0^1 sin(x) dx
exact1 <- 1 - cos(1)
mc_integral(g = sin, a = 0, b = 1,
            main = "MC estimate of int_0^1 sin(x) dx",
            exact = exact1)

# 2) \int_0^{π/3} sin(x) dx
exact2 <- 1 - cos(pi/3)  # = 0.5
mc_integral(g = sin, a = 0, b = pi/3,
            main = "MC estimate of int_0^{π/3} sin(x) dx",
            exact = exact2)

# 3) \int_0^{1/2} exp(-x) dx
exact3 <- 1 - exp(-0.5)
mc_integral(g = function(x) exp(-x), a = 0, b = 0.5,
            main = "MC estimate of int_0^{1/2} exp(-x) dx",
            exact = exact3)

# 4)\int∫_0^{1} exp(-x^2) dx
exact4 <- integrate(function(x) exp(-x^2), 0, 1)$value
mc_integral(g = function(x) exp(-x^2), a = 0, b = 1,
            main = "MC estimate of int_0^{1} exp(-x^2) dx",
            exact = exact4)
