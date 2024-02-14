
source("../setup.R")



library(tidyverse)
library(gapminder)
library(gridExtra)
flea <- read_csv("http://www.ggobi.org/book/data/flea.csv")
p1 <- ggplot(flea, aes(x=tars1, y=aede1, colour = species)) + 
  geom_point() + 
  scale_colour_brewer(palette = "Dark2") +
  xlab("Var 1") + ylab("Var 2") +
  ggtitle("Classification") +
  theme(legend.position="None")
p2 <- ggplot(flea, aes(x=tars1, y=aede1)) + 
  geom_point() + xlab("Var 1") + ylab("Var 2") +  
  ggtitle("Clustering") 
grid.arrange(p1, p2, ncol=2)



library(mvtnorm)
vc <- matrix(c(1, 0.5, 0.2, 
               0.5, 1, -0.3, 
               0.2, -0.3, 1), 
             ncol=3, byrow=TRUE)
set.seed(449)
x <- rmvnorm(5, 
             mean = c(-0.2, 0, 0.3), 
             sigma = vc)
x



library(palmerpenguins)
p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) 
p_tidy |> slice_head(n=10)



x[2,]



p_tidy |> slice_sample()



x[,1]



p_tidy |> pull(fl)



set.seed(424)
p_tidy |> slice_sample(n=10)



set.seed(424)
model.matrix(~ 0 + species, data = p_tidy) |>
  as_tibble() |>
  slice_sample(n=10)



x
t(x)



x
proj <- matrix(c(1/sqrt(2), 1/sqrt(2), 0, 
                 0, 0, 1), ncol=2, byrow=FALSE)
proj
x %*% proj



## t(x) %*% proj



diag(1, 8, 8)



vc

vc_i <- solve(vc)
vc_i

vc %*% vc_i



proj
sum(proj[,1]^2)
sum(proj[,2]^2)
sum(proj[,1]*proj[,2])



d_bal <- tibble(y=c(rep("A", 6), rep("B", 6)),
                x=c(runif(12)))
d_bal$y
set.seed(130)
d_bal_split <- initial_split(d_bal, prop = 0.70)
training(d_bal_split)$y
testing(d_bal_split)$y



d_unb <- tibble(y=c(rep("A", 2), rep("B", 10)),
                x=c(runif(12)))
d_unb$y
set.seed(132)
d_unb_split <- initial_split(d_unb, prop = 0.70)
training(d_unb_split)$y
testing(d_unb_split)$y



d_unb_strata <- initial_split(d_unb, prop = 0.70, strata=y)
training(d_unb_strata)$y
testing(d_unb_strata)$y


#| echo: false
a2 <- tibble(y = c(rep("bilby", 12),
                      rep("quokka", 15)),
             pred = c(rep("bilby", 9),
                      rep("quokka", 13), 
                      rep("bilby", 5)),
             bilby = c(0.9, 0.8, 0.9, 
                       0.7, 0.6, 0.8, 
                       0.9, 0.7, 0.6,# true
                       0.4, 0.3, 0.4,# error
                       0.2, 0.4, 0.1,# true
                       0.4, 0.1, 0.3, 
                       0.2, 0.4, 0.3,# true 
                       0.4, 
                       0.6, 0.7,# error 
                       0.6, 0.9, 0.7)) |>
  mutate(quokka = 1-bilby)

a3 <- a2 |>
  bind_rows(tibble(
    y = rep("numbat", 8), 
    pred = c(rep("numbat", 6),
                     rep("quokka", 2))))

a2 <- a2 |> 
  mutate(y = factor(y),
         pred = factor(pred))
a3 <- a3 |> 
  mutate(y = factor(y),
         pred = factor(pred))


#| eval: false
#| echo: false
## # tidymodels has it transposed
## cm <- conf_mat(a2, y, pred)
## autoplot(cm)
## # Make it show in right direction
## conf_mat(a2, pred, y, dnn=c("Truth", "Pred"))



# Write out the confusion matrix in standard form
cm <- a2 %>% count(y, pred) |>
  group_by(y) |>
  mutate(cl_err = n[pred==y]/sum(n)) 
cm |>
  pivot_wider(names_from = pred, 
              values_from = n) |>
  select(y, bilby, quokka, cl_err)



accuracy(a2, y, pred) |> pull(.estimate)
bal_accuracy(a2, y, pred) |> pull(.estimate)
sens(a2, y, pred) |> pull(.estimate)
specificity(a2, y, pred) |> pull(.estimate)



# Write out the confusion matrix in standard form
cm3 <- a3 %>% count(y, pred) |>
  group_by(y) |>
  mutate(cl_err = n[pred==y]/sum(n)) 
cm3 |>
  pivot_wider(names_from = pred, 
              values_from = n, values_fill=0) |>
  select(y, bilby, quokka, numbat, cl_err)



accuracy(a3, y, pred) |> pull(.estimate)
bal_accuracy(a3, y, pred) |> pull(.estimate)



a2 |> slice_head(n=3)
roc_curve(a2, y, bilby) |>
  autoplot()


#| echo: false
#| eval: false
## # Generate the sine-curve data
## set.seed(1259)
## x1 <- runif(340)
## x2 <- runif(340)
## y <- 3*x1+sin(x1*15)
## y <- (y-min(y))/(max(y)-min(y))
## d <- tibble(x1, x2, y)
## d$cl <- ifelse(x2 > y, "A", "B")
## d$cl[sample(1:340, 25)] <- sample(c("A", "B"), 25, replace=TRUE)
## write_csv(d, file="data/sine-curve.csv")
## # Test set
## x1 <- runif(212)
## x2 <- runif(212)
## y <- 3*x1+sin(x1*15)
## y <- (y-min(y))/(max(y)-min(y))
## d <- tibble(x1, x2, y)
## d$cl <- ifelse(x2 > y, "A", "B")
## d$cl[sample(1:212, 18)] <- sample(c("A", "B"), 18, replace=TRUE)
## write_csv(d, file="data/sine-curve-test.csv")


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#w <- read_csv(here::here("data/wiggly.csv")) |>
#  rename(x1=x, x2=y) |>
#  mutate(class = factor(class))
w <- read_csv(here::here("data/sine-curve.csv")) |>
  mutate(cl = factor(cl))
ggplot(w, aes(x=x1, y=x2, colour = cl)) +
  geom_point(size=2.5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Observed data") + 
  theme(legend.position = "none")


#| echo: false
#| fig-width: 4
#| fig-height: 8
library(geozoo)
w_grid <- cube.solid.random(p=2)$points
colnames(w_grid) <- c("x1", "x2")
w_grid <- w_grid |> as_tibble() |>
  mutate(x1 = x1*(max(w$x1)-min(w$x1)+min(w$x1)),
         x2 = x2*(max(w$x2)-min(w$x2)+min(w$x2)))
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w_grid)$class
w_rf <- randomForest(cl~x1+x2, data=w, ntree=2)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(plda = w_lda_pred,
         prf = w_rf_pred)
p1 <- ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(alpha=0.5, size=2) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Parametric") +
  theme(legend.position = "none",
        axis.text = element_blank())
p2 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(alpha=0.5, size=2) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Non-parametric") +
  theme(legend.position = "none",
        axis.text = element_blank())
p1 + p2 + plot_layout(ncol=1)


#| echo: false
#| fig-width: 5
#| fig-height: 5
w <- w |>
  mutate(plda = predict(w_lda, w)$class,
         prf = predict(w_rf, w)) |>
  mutate(lda_err = ifelse(cl != plda, "1", "0"),
         rf_err = ifelse(cl != prf, "1", "0"))

ggplot(w, aes(x=x1, y=x2, 
                   colour = cl, 
                   shape=lda_err)) +
  geom_point(size=5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), 
            inherit.aes = FALSE, 
            colour="black") +
  scale_color_discrete_qualitative() +
  scale_shape_manual(values=c(1, 19)) +
  ggtitle("Parametric errors") + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


#| echo: false
#| fig-width: 5
#| fig-height: 5
ggplot(w, aes(x=x1, y=x2, 
                   colour = cl, 
                   shape=rf_err)) +
  geom_point(size=5, alpha=0.8) +
  geom_line(aes(x=x1, y=y), 
            inherit.aes = FALSE,
            colour="black") +
  scale_color_discrete_qualitative() +
  scale_shape_manual(values=c(1, 19)) +
  ggtitle("Non-parametric errors") + 
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())


#| echo: false
#| fig-width: 9
#| fig-height: 3
p1 <- ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Less flexible") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())
w_rf <- randomForest(cl~x1+x2, data=w, ntree=4)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(prf = w_rf_pred)
p2 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("<--------------->") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())
w_rf <- randomForest(cl~x1+x2, data=w, ntree=100)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(prf = w_rf_pred)
p3 <- ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(size=2, alpha=0.5) +
  geom_line(data=w, aes(x=x1, y=y), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("More flexible") +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank())

p1 + p2 + p3 + plot_layout(ncol=3)


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p1 + ggtitle("Large bias")


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p3 + ggtitle("Small bias")


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p1 + ggtitle("Small variance")


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p3 + ggtitle("Large variance")


#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 60%
ai <- tibble(flexibility=c(5, 1, 4, 2, 3), 
             interpretability=c(1, 5, 4, 5, 3), 
             name=c("neural network", "LDA", "random forests", "trees", "SVM"))
library(ggrepel)
ggplot(ai, aes(x=flexibility, 
               y=interpretability, 
               label=name, colour=name)) +
  geom_text(hjust = 1, vjust=0) +
  scale_x_continuous("flexibility/accuracy", 
                     limits=c(0, 5), 
                     breaks = c(1, 5), 
                     labels=c("low", "high")) +
  scale_y_continuous(
                     limits=c(1, 5), 
                     breaks = c(1, 5), 
                     labels=c("low", "high")) +
  theme(legend.position="none", panel.grid = element_blank())


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
w_test <- read_csv(here::here("data/sine-curve-test.csv"))
ggplot() +
  geom_point(data=w_grid, aes(x=x1, y=x2, 
                              colour = prf), 
             alpha=0.05, size=4) +
  geom_point(data=w, aes(x=x1, y=x2, colour = cl), 
             shape=3, size=3) +
  geom_point(data=w_test, aes(x=x1, y=x2, colour = cl), 
             shape=19, size=3) +
  scale_color_discrete_qualitative() +
  theme(legend.position = "none",
        axis.text = element_blank())

