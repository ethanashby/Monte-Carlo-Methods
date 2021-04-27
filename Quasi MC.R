library(tidyverse)
library(qrng)

integrate(function(x) return(sqrt(x)), lower=0, upper=1)

set.seed(4747)
w<-runif(100)
tibble(
  v=sqrt(w),
) %>% summarize(est_mean=mean(v), est_se=sd(v)/sqrt(length(v)))


set.seed(4747)
n <- 100
w <- seq(0, 1 - 1 / n, by = 1 / n) 
w <- w + runif(1) / n
tibble(
  v=sqrt(w),
) %>% summarize(est_mean=mean(v), est_se=sd(v)/sqrt(length(v)))

true=integrate(function(x) return(sqrt(x)), lower=0, upper=1)
true
#additive error
mean(sqrt(w))-true$value


##korobov nets
qmc <- function(n) {
  w <- korobov(n, d = 2, c(5, 7), randomize = "shift") 
  return(mean(1 / (w[, 1] + w[, 2]^2)))
}

set.seed(4747)
w<-korobov(100, d = 2, c(5, 7), randomize = "shift")
mean(1/(w[,1]+w[,2]^2))

#sobol 

w<-sobol(100, d = 2, randomize = TRUE)

set.seed(4747)
w<-sobol(100, d = 2, randomize = TRUE)
mean(1/(w[,1]+w[,2]^2))

qmc_sobol<-function(n){
  w<-sobol(n, d = 2, randomize = TRUE)
  return(mean(1/(w[,1]+w[,2]^2)))
}

qmc_sobol(n=10^3)

#qmc finance
stock_price <- function(times, s_0, mu, sigma, u) {
  w <- cumsum( sqrt( diff( c(0, times ) ) ) * qnorm(u) ) 
  return(s_0 * exp( (mu - sigma^2 / 2) * times + sigma * w) )
}
qmc_asian<-function(n){
  u_qmc <- sobol(n, 5, randomize = TRUE)
  stock_prices_qmc <- apply(u_qmc, 1, function(row) return(stock_price(1:5, 100, 0, 0.1, row)))   
  asian_prices_qmc <- apply(stock_prices_qmc, 2, function(col) return(max(mean(col) - 105, 0)))   
  return(mean(asian_prices_qmc))
}
qmc_asian(10^5)
