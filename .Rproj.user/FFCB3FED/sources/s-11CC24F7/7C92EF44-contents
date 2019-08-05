
# This function runs an 8-schools type model and plots the results
# Data should be a data matrix with means an variances from country level analyses
# (first row frequentist meta result, then countries in rows 2 to 7)

library(rstan)

bayes_result <- function(my_data, main = "Effects of Bad News:", stancode = '3_ma_bayes.stan',
                         countrynames = (c("Overall", "Benin", "Brazil", "Burkina Faso", "Mexico", "Uganda 1", "Uganda 2")), rnd = 2){
  
  n <- nrow(my_data)-1
  
  stan_dat <- list(J = n, y = my_data[-1,1], sigma = my_data[-1,2])
  
  fit <- stan(file = stancode, data = stan_dat, iter = 2000, chains = 4)
  
  my_posterior <- extract(fit, permuted = TRUE)
  means <- apply(my_posterior$theta, 2, mean)
  cred  <- apply(my_posterior$theta, 2, quantile, probs = c(.025, .975))
  meta_mean <- mean(my_posterior$mu)
  meta_sd   <- mean(my_posterior$tau)
  rounded_m <- round(meta_mean, rnd)
  rounded_sd <- round(meta_sd, rnd)
  #main <- paste0(main, ":\n mu = ", round(meta_mean, rnd), ", tau = ", round(meta_sd, rnd))  

    plot(means, n:1, xlim = c(-.15, +.15), 
       xlab = "Posterior on Effect Sizes (95% credibility)", pch = 19, ylim = c(.5,7), axes = FALSE, 
       ylab = "", main = main)
  mtext(bquote( mu == .(rounded_m ) ~  ~ tau == .(rounded_sd )))
  axis(1)
  axis(2, at = (n+1):1, 
       labels = countrynames, las = 1)
  segments(cred[1,], n:1, cred[2,], n:1)
  abline(v=0, col = "red")
  points(my_data[,1], (n+1):1) # Frequentist values
  box()
  points(meta_mean, n+1, pch = 19, col = "red")
  segments(quantile(my_posterior$mu, probs = .025), n+1, 
           quantile(my_posterior$mu, probs = .975), n+1, col = "red")
}
