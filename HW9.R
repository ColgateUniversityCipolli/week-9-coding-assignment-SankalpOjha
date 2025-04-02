library(tidyverse)

precip.data <- read_csv("agacis.csv")
precip.long <- precip.data |>
  pivot_longer(cols = c("Jan","Feb","Mar","Apr","May","Jun",
                        "Jul","Aug","Sep","Oct", "Nov","Dec"), 
               names_to = "Month", 
               values_to = "Precipitation") |>
  select(-Annual) |>
  mutate(Precipitation = if_else(Precipitation == "M", 
                                 NA_character_, Precipitation)) |>
  mutate(Precipitation = as.numeric(Precipitation))

#Problem 1

# 1a
llgamma <- function(par, data, neg = F){
  alpha <- exp(par[1])
  beta <- exp(par[2])
  lgamma <- sum(log(dgamma(x=data, shape=alpha, scale=beta)),na.rm=T)
  
  return(ifelse(neg, -lgamma, lgamma))
}

MLEs.gamma <- optim(fn = llgamma,
                    par = c(1,1),
                    data = precip.long$Precipitation,
                    neg = T)

alpha.gamma <- exp(MLEs.gamma$par[1])
beta.gamma <- exp(MLEs.gamma$par[2])


lllognorm <- function(par, data, neg = F){
  mu <- exp(par[1])
  sigma <- exp(par[2])
  llognorm <- sum(log(dlnorm(x=data, meanlog = mu, sdlog = sigma)),na.rm=T)
  
  return(ifelse(neg, -llognorm, llognorm))
}

MLEs.lnorm <- optim(fn = lllognorm,
                    par = c(1,1),
                    data = precip.long$Precipitation,
                    neg = T)

mu.lnorm <- exp(MLEs.lnorm$par[1])
sigma.lnorm <- exp(MLEs.lnorm$par[2])

#Gather LLs
weibull.ll <- -2166.496

gamma.ll <- llgamma(par = log(c(alpha.gamma, beta.gamma)), 
                    data = precip.long$Precipitation, neg = F)

lognorm.ll <- lllognorm(par = log(c(mu.lnorm, sigma.lnorm)),
                        data = precip.long$Precipitation, neg = F)


wei.gamma.ratio <- exp(weibull.ll - gamma.ll)

wei.lnorm.ratio <- exp(weibull.ll - lognorm.ll)

gamma.lnorm.ratio <- exp(gamma.ll - lognorm.ll)

pdfs <- tibble(x = seq(0,13,length.out=1000)) |>
  mutate(gamma.pdf = dgamma(x=x, shape = alpha.gamma, scale = beta.gamma),
         lognorm.pdf = dlnorm(x=x, meanlog = mu.lnorm, sdlog = sigma.lnorm),
         weibull.pdf = dweibull(x=x, shape = 2.1871, scale = 3.9683))

plots <- ggplot() +
  geom_histogram(data=precip.long,
                 aes(x=Precipitation, y=after_stat(density)),
                 breaks = seq(0,13,1)) +
  geom_line(data = pdfs, aes(x=x, y= gamma.pdf, color = "gamma"))+
  geom_line(data = pdfs, aes(x=x, y=lognorm.pdf, color = "lognorm"))+
  geom_line(data = pdfs, aes(x=x, y=weibull.pdf, color = "weibull"))+
  theme_bw()+
  geom_hline(yintercept = 0)+
  xlab("Precipitation (Inches)")+
  ylab("Density")


(plots)




