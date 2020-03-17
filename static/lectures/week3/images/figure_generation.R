
library(tidyverse)
library(mvtnorm)

s <- matrix(c(4,2,2,3), ncol = 2)

m1 <- c(0, 0)
m2 <- c(-3, 4)

n <- 250
set.seed(42)
x1 <- rmvnorm(n = n, mean = m1, sigma = s)
x2 <- rmvnorm(n = n, mean = m2, sigma = s)

d <- data.frame(rbind(x1,x2))
d$class <- as.factor(rep(c("1", "2"), each = 250))

ggplot(d, aes(x = X1, y = X2, group = class, color =class)) +
  geom_point(alpha = .5, size = 4) +
  geom_density_2d() + 
  scale_color_brewer(palette = "Dark2") + 
  theme_minimal()  +
  theme(text = element_text(size=20),
        legend.position = "none") 

  
  
fit <-  glm(data=d, 
            class ~ ., #<<
            family=binomial(link="logit")) 
slope <- coef(fit)[2]/(-coef(fit)[3])
intercept <- coef(fit)[1]/(-coef(fit)[3])

ggplot(d, aes(x=X1, y=X2, color=factor(class)))+ 
  geom_point(alpha = 0.5, size = 4) + 
  geom_abline(slope=slope, intercept=intercept, linetype ="dashed", size = 1.5) +
  scale_color_brewer(palette= "Dark2") +
  theme_minimal()+
  theme(text = element_text(size=20),
        legend.position = "none") 
