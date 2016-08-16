

library(LearnBayes)


#~~~~~~~~~~~~~~~~~~~~~~~~
#### National Priors ####
#~~~~~~~~~~~~~~~~~~~~~~~~

# Nationally the chance that a driver is texting is:
#   > P = 0.5, at x = 0.1
#   > P = 0.75 at x = 0.3
  
## Compute the Beta prior, and report the coefficents
beta.prior <- beta.select(list(p=0.5, x=0.1), list(p=0.75, x=.3))
beta.prior



#~~~~~~~~~~~~~~~~~~~~~~~~
#### Update beliefs  ####
#~~~~~~~~~~~~~~~~~~~~~~~~

## Plot the prior, likelihood and posterior three times as you update
## your belief based on collecting more data

# a. 2 texting out of 20 drivers
# b. 4 texting out of 20 drivers
# c. 1 texting out of 20 drivers

par(mfrow = c(3,1))

beta.prior + c(2, 18)
triplot(beta.prior, c(2, 18))

beta.prior + c(2 + 4, 18 + 16)
triplot(beta.prior, c(2 + 4, 18 + 16))

beta.prior + c(2 + 4 + 1, 18 + 16 + 19)
triplot(beta.prior, c(2 + 4 + 1, 18 + 16 + 19))

par(mfrow = c(1,1))



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Simulate final posterior  ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# – Simulate the final posterior distribution and do the following:
#   > Plot the posterior with the 90% HDI shown

beta.post <- beta.prior + c(2 + 4 + 1, 18 + 16 + 19)
post.sample <- rbeta(10000, beta.post[1], beta.post[2])

par(mfrow = c(1,2))
quants = quantile(post.sample, c(0.05, 0.95))
breaks = seq(min(post.sample), max(post.sample), length.out = 41)
hist(post.sample, breaks = breaks, 
     main = 'Distribution of samples \n with 90% HDI',
     xlab = 'Sample value',
     ylab = 'Density')
abline(v = quants[1], lty = 3, col = 'red', lwd = 3)
abline(v = quants[2], lty = 3, col = 'red', lwd = 3)
qqnorm(post.sample)
par(mfrow = c(1,1))



# > Report the upper and lower limits of the 90% HDI
quants



# > Of the next hundred drivers what are the number of texting drivers
# in the 90% HDI?

f_prob_dist <- function(n = 100, beta, title) {
  s <- 0:n
  pred.probs <- pbetap(beta, n, s)
  discrete.distribution <- discint(cbind(s, pred.probs), 0.90)
  
  plot(s, pred.probs, type="h", 
       main = title,
       xlab = 'Texters')
  abline(v = max(discrete.distribution$set), lty = 3, col = 'red', lwd = 3)
  abline(v = min(discrete.distribution$set), lty = 3, col = 'red', lwd = 3)
}

f_prob_dist(beta = beta.post, title = "Posterior distribution of texters")



# > Are the drivers in this area better or worse that the national figures
# indicate?
par(mfrow = c(2,1))
f_prob_dist(beta = beta.prior, title = "Prior distribution of texters")
f_prob_dist(beta = beta.post, title = "Posterior distribution of texters")
par(mfrow = c(1,1))

