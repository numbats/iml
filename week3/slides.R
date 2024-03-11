
source("../setup.R")


#| label: balanced-data1
d_bal <- tibble(y=c(rep("A", 6), rep("B", 6)),
                x=c(runif(12)))
d_bal$y
set.seed(130)
d_bal_split <- initial_split(d_bal, prop = 0.70)
training(d_bal_split)$y
testing(d_bal_split)$y


#| label: balanced-data2
d_bal$y
set.seed(1225)
d_bal_split <- initial_split(d_bal, 
                             prop = 0.70, 
                             strata=y)
training(d_bal_split)$y
testing(d_bal_split)$y


#| label: unbalanced-data
d_unb <- tibble(y=c(rep("A", 2), rep("B", 10)),
                x=c(runif(12)))
d_unb$y
set.seed(132)
d_unb_split <- initial_split(d_unb, prop = 0.70)
training(d_unb_split)$y
testing(d_unb_split)$y


#| label: unbalanced-split
d_unb_strata <- initial_split(d_unb, 
                              prop = 0.70, 
                              strata=y)
training(d_unb_strata)$y
testing(d_unb_strata)$y


#| echo: false
#| label: penguins-good-split1
#| fig-width: 3
#| fig-height: 6
#| out-width: 70%
library(palmerpenguins)
p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) |>
  filter(!is.na(bl)) |>
  arrange(species)
set.seed(224)
p_split <- initial_split(p_tidy, 
                         prop = 0.7,
                         strata = species)
p_tr <- training(p_split) |> 
  mutate(set = "train")
p_ts <- testing(p_split) |> 
  mutate(set = "test")
p_both <- bind_rows(p_tr, p_ts) |>
  mutate(set = factor(set, levels=c("train", "test")))
ggplot(p_both, aes(x=species, fill=species)) +
  geom_bar() +
  facet_wrap(~set, ncol=1, scales="free_x") +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  xlab("") +
  theme(legend.position="none") + coord_flip()


#| echo: false
#| label: penguins-bad-split1
#| fig-width: 3
#| fig-height: 6
#| out-width: 70%
p_bad <- p_tidy |>
  mutate(set = factor(ifelse(species == "Adelie", "train", "test"),
                      levels=c("train", "test")))
ggplot(p_bad, aes(x=species, fill=species)) +
  geom_bar() +
  facet_wrap(~set, ncol=1) +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  xlab("") +
  theme(legend.position="none") + coord_flip()


#| echo: false
#| label: penguins-good-split2
#| eval: false
## render_gif(p_both[,2:5],
##            grand_tour(),
##            display_xy(col=p_both$species,
##              pch=p_both$set, shapeset=c(4, 16), rescale=TRUE),
##            rescale=TRUE,
##            gif_file = "../gifs/p_split_good.gif",
##            apf = 1/30,
##            frames = 500,
##            width = 400,
##            height = 400)


#| echo: false
#| label: penguins-bad-split2
#| fig-width: 6
#| fig-height: 3
#| out-width: 80%
p_bad2 <- p_tidy |>
  mutate(set = factor(ifelse((species == "Adelie" & bd > 17.5 & fl > 183) | (species == "Chinstrap" & bd > 17.8 & fl > 187) |
        (species == "Gentoo" & bd > 14.2 & fl > 211), "train", "test"),
                      levels=c("train", "test")))
ggplot(p_bad2, aes(x=species, fill=species)) +
  geom_bar() +
  facet_wrap(~set, ncol=2, scales="free") +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  xlab("") +
  theme(legend.position="none") + coord_flip()


#| echo: false
#| eval: false
## render_gif(p_bad2[,2:5],
##            grand_tour(),
##            display_xy(col=p_bad2$species,
##              pch=p_bad2$set, shapeset=c(4, 16), rescale=TRUE),
##            rescale=TRUE,
##            gif_file = "../gifs/p_split_bad.gif",
##            apf = 1/30,
##            frames = 500,
##            width = 400,
##            height = 400)


#| label: penguins-subset
#| echo: false
set.seed(1140)
p_sub_split <- initial_split(p_tidy, 
                             prop=0.25, 
                             strata=species)
p_sub <- training(p_sub_split)


#| label: penguins-CV
p_folds <- vfold_cv(p_sub, 5, strata=species)
c(1:nrow(p_sub))[-p_folds$splits[[1]]$in_id]
c(1:nrow(p_sub))[-p_folds$splits[[2]]$in_id]
c(1:nrow(p_sub))[-p_folds$splits[[3]]$in_id]
c(1:nrow(p_sub))[-p_folds$splits[[4]]$in_id]
c(1:nrow(p_sub))[-p_folds$splits[[5]]$in_id]


#| label: penguins-CV-fit
#| echo: false
p_rp1 <- rpart(species~., data=p_folds$splits[[1]])
p_cv1 <- p_sub[c(1:nrow(p_sub))[-p_folds$splits[[1]]$in_id],]
p_cv1 <- p_cv1 |> 
  mutate(pspecies = predict(p_rp1, p_cv1, type="class"))
accuracy(p_cv1, species, pspecies)


#| label: penguins-CV-fitk
#| echo: false
acc <- NULL
for (i in 1:5) {
  p_rp1 <- rpart(species~., data=p_folds$splits[[i]])
  p_cv1 <- p_sub[c(1:nrow(p_sub))[-p_folds$splits[[i]]$in_id],]
  p_cv1 <- p_cv1 |> 
    mutate(pspecies = predict(p_rp1, p_cv1, type="class"))
  acc <- c(acc, accuracy(p_cv1, species, pspecies)$.estimate)
}
acc



set.seed(115)
df <- tibble(id = 1:26, 
             cl = c(rep("A", 12), rep("B", 14)))
df_b <- bootstraps(df, times = 100, strata = cl)
t(df_b$splits[[1]]$data[df_b$splits[[1]]$in_id,])



track <- read_csv(here::here("data/womens_track.csv"))
track_pca <- prcomp(track[,1:7], center=TRUE, scale=TRUE)
track_pca


#| label: bootstrap-PC
#| code-fold: true
library(boot)
compute_PC2 <- function(data, index) {
  pc2 <- prcomp(data[index,], center=TRUE, scale=TRUE)$rotation[,2]
  # Coordinate signs: make m100 always positive
  if (sign(pc2[1]) < 0) 
    pc2 <- -pc2 
  return(pc2)
}
# Make sure sign of first PC element is positive
set.seed(201)
PC2_boot <- boot(data=track[,1:7], compute_PC2, R=1000)
colnames(PC2_boot$t) <- colnames(track[,1:7])
PC2_boot_ci <- as_tibble(PC2_boot$t) %>%
  gather(var, coef) %>% 
  mutate(var = factor(var, levels=c("m100", "m200", "m400", "m800", "m1500", "m3000", "marathon"))) %>%
  group_by(var) %>%
  summarise(q2.5 = quantile(coef, 0.025), 
            q5 = median(coef),
            q97.5 = quantile(coef, 0.975)) %>%
  mutate(t0 = PC2_boot$t0) 
pb <- ggplot(PC2_boot_ci, aes(x=var, y=t0)) + 
  geom_hline(yintercept=0, linetype=2, colour="red") +
  geom_point() +
  geom_errorbar(aes(ymin=q2.5, ymax=q97.5), width=0.1) +
  xlab("") + ylab("coefficient") 


#| out-width: 90%
#| fig-width: 6
#| fig-height: 6
pb



set.seed(238)
dp <- tibble(x=c(rexp(5), rexp(5,0.3)+0.5), cl=c(rep("A", 5), rep("B", 5)))
dp



set.seed(246)
dpp <- dp |> mutate(cl = sample(cl))
dpp



set.seed(238)
n <- 50
dp <- tibble(x=c(rexp(n), rexp(n,0.3)+0.5), 
             cl=c(rep("A", n), rep("B", n))) |>
    mutate(cl <- factor(cl)) 
dpp <- dp |> mutate(cl = sample(cl))
dp_plot <- ggplot(dp, aes(x=cl, y=x)) +
  geom_point(alpha=0.6, size=3) +
  ggtitle("DATA")
dpp_plot <- ggplot(dpp, aes(x=cl, y=x)) +
  geom_point(alpha=0.6, size=3) +
  ggtitle("PERMUTED")
grid.arrange(dp_plot, dpp_plot, ncol=2)
  



dp_meds <- dp |>
  group_by(cl) |>
  summarise(med = median(x))
dp_plot +
  geom_point(data=dp_meds, aes(x=cl, y=med), colour="red", size=5)



dp_perm <- dp |>
  specify(x ~ cl) |>
  hypothesize(null = "independence") |>
  generate(reps = 1000, type = "permute") |>
  calculate("diff in medians", order = c("A", "B")) |>
  mutate(stat = abs(stat)) # Not interested in direction
dp_true <- abs(dp_meds$med[1]-dp_meds$med[2])
ggplot(dp_perm, aes(x=stat)) +
  geom_histogram(binwidth = 0.1, colour="white") +
  geom_vline(xintercept=dp_true, colour="red")


#| label: track-scree
#| fig-width: 6
#| fig-height: 6
#| out-width: 80%
ggscree(track_pca, q=7) +
  ggtitle("Scree plot of womens track data")



set.seed(357)
my_sparse_data <- tibble(cl = c(rep("A", 12), 
                                rep("B", 9)),
                         x1 = rnorm(21),
                         x2 = rnorm(21), 
                         x3 = rnorm(21),
                         x4 = rnorm(21),
                         x5 = rnorm(21), 
                         x6 = rnorm(21), 
                         x7 = rnorm(21), 
                         x8 = rnorm(21), 
                         x9 = rnorm(21), 
                         x10 = rnorm(21), 
                         x11 = rnorm(21), 
                         x12 = rnorm(21), 
                         x13 = rnorm(21), 
                         x14 = rnorm(21), 
                         x15 = rnorm(21)) |>
  mutate(cl = factor(cl)) |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))



## render_gif(my_sparse_data[,-1],
##            guided_tour(lda_pp(my_sparse_data$cl), sphere=TRUE),
##            display_xy(col=my_sparse_data$cl, axes="bottomleft"),
##            rescale=TRUE,
##            gif_file = "../gifs/my_sparse_data.gif",
##            apf = 1/30,
##            loop = FALSE,
##            frames = 1000,
##            width = 400,
##            height = 400)

