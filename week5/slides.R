## ----include = FALSE------------------------------------
source("../setup.R")
library(ggpubr)
library(kableExtra)


## -------------------------------------------------------
#| echo: false
set.seed(65)
df <- tibble(x = sort(sample(10:90, 7)), 
             cl = factor(c("A", "A", "B", "A", 
                           "A", "B", "B")))
df %>% kable() %>% 
  kable_styling(bootstrap_options = 
                  c("striped", full_width = F))


## ----fig.width=5, fig.height=3, out.width="100%"--------
#| echo: false
possible_splits <- df$x[-7] + 
  diff(df$x, lag = 1, differences = 1)/2
ggplot(df, aes(x=x, y=cl, colour=cl)) + 
  geom_point() +
  geom_vline(xintercept = possible_splits, 
             colour = "#6F7C4D",
             linetype = 2) +
  annotate("text", x = possible_splits, y=2.25, 
           label = 1:6, 
           colour = "#6F7C4D", 
           hjust=0.05) +
  scale_colour_discrete_divergingx(
    palette = "Zissou 1") +
  ylab("") +
  theme(legend.position="none", aspect.ratio = 0.6)


## -------------------------------------------------------
#| echo: false
df %>%
  slice(1:5) %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", full_width = F))


## -------------------------------------------------------
#| echo: false
df %>%
  slice(6:7) %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", full_width = F))


## ----fig.width=5, fig.height=3, out.width="100%"--------
#| echo: false
df_c <- tibble(x=c("emu", "emu", "roo", "roo", "koala", "koala"),
               cl=c("WA", "Vic", "WA", "Vic", "WA", "Vic"),
               count=c(5,3,7,1,2,6))
ggplot(df_c, aes(x=x, y=count, fill=cl)) + 
  geom_col() +
  xlab("") + ylab("") +
  scale_fill_discrete_divergingx() +
  theme(axis.text.y = element_blank())


## -------------------------------------------------------
#| echo: false
set.seed(87)
df_m <- tibble(x1 = sample(10:20, 8), 
               x2 = sample(-10:0, 8),
               x3 = sample(21:37, 8),
               x4 = sample(-35:(-23), 8),
               y=c("A", "A", "B", "A", "A", "B", "B", "B")) 
df_m$x1[2] <- NA
df_m$x2[3] <- NA
df_m$x3[5] <- NA
df_m$x4[6] <- NA
df_m %>% kable() %>% 
  kable_styling(bootstrap_options = c("striped", full_width = F))


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 6
#| out-width: 80%
p_sub <- p_tidy |>
  filter(species != "Gentoo") |>
  mutate(species = factor(species)) |>
  select(species, bl, bm)

p_sub |>
  ggplot(aes(x=bl, y=bm, colour=species)) +
  geom_point() +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  theme(legend.position="bottom")


## -------------------------------------------------------
set.seed(1156)
p_split <- initial_split(p_sub, 2/3, strata=species)
p_tr <- training(p_split)
p_ts <- testing(p_split)

tree_spec <- decision_tree() |>
  set_mode("classification") |>
  set_engine("rpart")

p_fit_tree <- tree_spec |>
  fit(species~., data=p_tr)

p_fit_tree


## -------------------------------------------------------
tree_spec <- decision_tree() |>
  set_mode("classification") |>
  set_engine("rpart",
             control = rpart.control(minsplit = 10), 
             model=TRUE)

p_fit_tree <- tree_spec |>
  fit(species~., data=p_tr)

p_fit_tree


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 6
#| out-width: 80%
p_tr |>
  ggplot(aes(x=bl, y=bm, colour=species)) +
  geom_point(shape=1) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  annotate("segment", x=43, xend=43, 
           y=3388, yend=3825, linetype=2) +
  annotate("segment", x=41, xend=41, 
           y=2700, yend=3388, linetype=2) +
  annotate("segment", x=41, xend=43, 
           y=3388, yend=3388, linetype=2) +
  annotate("segment", x=46, xend=46, 
           y=3825, yend=4800, linetype=2) +
  annotate("segment", x=43, xend=46, 
           y=3825, yend=3825, linetype=2) +  
  theme(legend.position="bottom")


## -------------------------------------------------------

p_fit_tree |>
  extract_fit_engine() |>
  rpart.plot(type=3, extra=1)


## -------------------------------------------------------
#| echo: false
p_ts_pred <- p_ts |>
  mutate(pspecies = predict(p_fit_tree$fit, 
                            p_ts, 
                            type="class"))
accuracy(p_ts_pred, species, pspecies)
p_ts_pred |>
  count(species, pspecies) |>
  group_by(species) |>
  mutate(Accuracy = n[species==pspecies]/sum(n)) |>
  pivot_wider(names_from = "pspecies", 
              values_from = n) |>
  select(species, Adelie, Chinstrap, Accuracy)
bal_accuracy(p_ts_pred, species, pspecies)


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 6
#| out-width: 80%
p_all <- p_sub |>
  mutate(set = ifelse(c(1:nrow(p_sub)) %in% p_split$in_id, "train", "test")) |>
  mutate(set = factor(set, levels=c("train", "test")))
p_all |>
  ggplot(aes(x=bl, y=bm, colour=species, 
             shape=set)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  annotate("segment", x=43, xend=43, 
           y=3388, yend=3825, linetype=2) +
  annotate("segment", x=41, xend=41, 
           y=2700, yend=3388, linetype=2) +
  annotate("segment", x=41, xend=43, 
           y=3388, yend=3388, linetype=2) +
  annotate("segment", x=46, xend=46, 
           y=3825, yend=4800, linetype=2) +
  annotate("segment", x=43, xend=46, 
           y=3825, yend=3825, linetype=2) +  theme(legend.position="bottom",
        legend.title = element_blank())


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 70%
p_explore <- explore(p_fit_tree$fit, p_all) |>
  mutate(set = factor(c(rep(NA, 10000), p_all$set)))

ggplot() +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "simulated",], 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "actual",], 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none")


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 70%
lda_spec <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS")
p_lda_fit <- lda_spec |> 
  fit(species ~ ., data = p_tr)

p_explore <- explore(p_lda_fit$fit, p_all) |>
  mutate(set = factor(c(rep(NA, 10000), p_all$set)))

ggplot() +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "simulated",], 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "actual",], 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none")


## -------------------------------------------------------
#| echo: false
#| fig-width: 15
#| fig-height: 7
#| out-width: 100%
d <- as.data.frame(matrix(1, nrow=32, ncol=7))

set.seed(529)
d1 <- d 
d1[sample(1:32, 11), ] <- 0
d1[, sample(1:7, 3)] <- 0

p1 <- d1 |> 
  mutate(id = 1:32) |>
  pivot_longer(V1:V7, names_to = "V", 
               values_to = "value") |>
  mutate(value = factor(value, 
                        labels=c("out", "in"))) |>
  ggplot(aes(x=V, y=id, fill=value)) +
    geom_tile(colour="white") +
    scale_fill_discrete_divergingx(palette="Cividis",
                                   rev=1) +
    ggtitle("Sample 1") +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) 

d1 <- d 
d1[sample(1:32, 11), ] <- 0
d1[, sample(1:7, 3)] <- 0

p2 <- d1 |> 
  mutate(id = 1:32) |>
  pivot_longer(V1:V7, names_to = "V", 
               values_to = "value") |>
  mutate(value = factor(value, 
                        labels=c("out", "in"))) |>
  ggplot(aes(x=V, y=id, fill=value)) +
    geom_tile(colour="white") +
    scale_fill_discrete_divergingx(palette="Cividis",
                                   rev=1) +
    ggtitle("Sample 2") +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) 
  
d1 <- d 
d1[sample(1:32, 11), ] <- 0
d1[, sample(1:7, 3)] <- 0

p3 <- d1 |> 
  mutate(id = 1:32) |>
  pivot_longer(V1:V7, names_to = "V", 
               values_to = "value") |>
  mutate(value = factor(value, 
                        labels=c("out", "in"))) |>
  ggplot(aes(x=V, y=id, fill=value)) +
    geom_tile(colour="white") +
    scale_fill_discrete_divergingx(palette="Cividis",
                                   rev=1) +
    ggtitle("Sample 3") +
    theme(axis.title = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank()) 

p1 + p2 + p3 + plot_layout(ncol=3, guides = "collect")


## -------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
p_explore <- explore(p_fit_tree$fit, p_all) |>
  mutate(set = factor(c(rep(NA, 10000), p_all$set)))

ggplot() +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "simulated",], 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "actual",], 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none")


## -------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
set.seed(632)
rf_spec <- rand_forest(mtry=2, trees=1000) |>
  set_mode("classification") |>
  set_engine("randomForest")
p_fit_rf <- rf_spec |> 
  fit(species ~ ., data = p_tr)

# explore() throws an error
p_explore <- tibble(bl = runif(10000, 32, 58),
                    bm = runif(10000, 2700, 4800)) 
p_explore$species <- predict(p_fit_rf$fit, 
                             p_explore,
                           type="response")
p_explore <- p_explore |>
  select(species, bl, bm)

ggplot() +
  geom_point(data=p_explore, 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=p_all, 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none")


## -------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
lda_spec <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS")
p_lda_fit <- lda_spec |> 
  fit(species ~ ., data = p_tr)

p_explore <- explore(p_lda_fit$fit, p_all) |>
  mutate(set = factor(c(rep(NA, 10000), p_all$set)))

ggplot() +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "simulated",], 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=
               p_explore[p_explore$.TYPE == 
                           "actual",], 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none")


## -------------------------------------------------------
#| eval: false
## rf_spec <- rand_forest(mtry=2, trees=1000) |>
##   set_mode("classification") |>
##   set_engine("randomForest")
## p_fit_rf <- rf_spec |>
##   fit(species ~ ., data = p_tr)


## -------------------------------------------------------
#| echo: false
p_fit_rf


## -------------------------------------------------------
#| echo: false
p_ts_pred <- p_ts |>
  mutate(pspecies = predict(p_fit_rf$fit, 
                            p_ts, 
                            type="response"))
accuracy(p_ts_pred, species, pspecies)
p_ts_pred |>
  count(species, pspecies) |>
  group_by(species) |>
  mutate(Accuracy = n[species==pspecies]/sum(n)) |>
  pivot_wider(names_from = "pspecies", 
              values_from = n, 
              values_fill = 0) |>
  select(species, Adelie, Chinstrap, Accuracy)
bal_accuracy(p_ts_pred, species, pspecies)


## -------------------------------------------------------
p_fit_rf$fit$votes


## -------------------------------------------------------
#| echo: false
p_fit_rf


## -------------------------------------------------------
#| echo: false
# Need to run again bc LDA overwrote
p_explore <- tibble(bl = runif(10000, 32, 58),
                    bm = runif(10000, 2700, 4800)) 
p_explore$species <- predict(p_fit_rf$fit, 
                             p_explore,
                           type="response")
p_explore <- p_explore |>
  select(species, bl, bm)



## -------------------------------------------------------
#| echo: false
p_tr_pred <- p_tr |>
  mutate(pspecies = p_fit_rf$fit$predicted) |>
  bind_cols(p_fit_rf$fit$votes)

p_tr_pred |>
  filter(species != pspecies)


## -------------------------------------------------------
#| echo: false
p_tr_wrong <- p_tr_pred |>
  filter(species != pspecies)
ggplot() +
  geom_point(data=p_explore, 
             aes(x=bl, y=bm, colour=species), 
             shape=16, alpha=0.1) +
  geom_point(data=p_all, 
             aes(x=bl, y=bm, colour=species, 
                 shape=set), 
             inherit.aes = FALSE) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  scale_shape_manual(values=c(1, 19)) +
  theme(legend.position="none") +
  geom_point(data=p_tr_wrong, 
             aes(x=bl, y=bm, colour=species), 
             shape=5, size=4,
             inherit.aes = FALSE)


## -------------------------------------------------------
p_fit_rf$fit$importance


## -------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 5
#| out-width: 70%
ggplot(p_tr, aes(x=bl, y=bm, colour=species)) +
  geom_point()  +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  ggtitle("Training data") +
  theme(legend.position="none")


## -------------------------------------------------------
#| fig-width: 5
#| fig-height: 5
#| out-width: 70%
p_tr_perm <- p_tr |>
  mutate(bl = sample(bl))
ggplot(p_tr_perm, aes(x=bl, y=bm, colour=species)) +
  geom_point()  +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  ggtitle("Permuted bl") +
  theme(legend.position="none")


## -------------------------------------------------------
#| echo: false
set.seed(923)
p_split2 <- initial_split(p_tidy, 2/3, strata=species)
p_tr2 <- training(p_split2)
p_ts2 <- testing(p_split2)

rf_spec <- rand_forest(mtry=2, trees=1000) |>
  set_mode("classification") |>
  set_engine("randomForest")
p_fit_rf2 <- rf_spec |> 
  fit(species ~ ., data = p_tr2)


## -------------------------------------------------------
#| echo: false
#| eval: false
## # Votes
## animate_xy(p_fit_rf2$fit$votes, col=p_tr2$species)
## 
## # Save an animated gif
## render_gif(p_fit_rf2$fit$votes,
##            grand_tour(),
##            display_xy(v_rel=0.02,
##              col=p_tr2$species,
##              axes="bottomleft"),
##            gif_file="../gifs/penguins_rf_votes.gif",
##            frames=500,
##            width=300,
##            height=300,
##            loop=FALSE
## )
## 


## -------------------------------------------------------
#| echo: false
# Votes reduce dim
proj <- t(geozoo::f_helmert(3)[-1,])
p_rf_v_p <- as.matrix(p_fit_rf2$fit$votes) %*% proj
colnames(p_rf_v_p) <- c("x1", "x2")
p_rf_v_p <- p_rf_v_p %>%
  as.data.frame() %>%
  mutate(species = p_tr2$species)

# Add simplex
simp <- simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(p_tr2$species))
p_ternary <- ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4)) +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.06, 0.07, 0),
            nudge_y=c(0.05, 0.05, -0.05)) +
  geom_point(data=p_rf_v_p, aes(x=x1, y=x2, 
                                colour=species), 
             size=2, alpha=0.5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme_map() +
  theme(aspect.ratio=1, legend.position="none")


## -------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
p_ternary


## -------------------------------------------------------
#| echo: true
#| eval: false
## library(detourr)
## library(crosstalk)
## library(plotly)
## library(viridis)
## p_tr2_std <- p_tr2 |>
##   mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
## p_tr2_v <- bind_cols(p_tr2_std, p_rf_v_p[,1:2])
## p_tr2_v_shared <- SharedData$new(p_tr2_v)
## 
## detour_plot <- detour(p_tr2_v_shared, tour_aes(
##   projection = bl:bm,
##   colour = species)) |>
##     tour_path(grand_tour(2),
##                     max_bases=50, fps = 60) |>
##        show_scatter(alpha = 0.9, axes = FALSE,
##                     width = "100%",
##                     height = "450px",
##                     palette = hcl.colors(3,
##                       palette="Zissou 1"))
## 
## vot_mat <- plot_ly(p_tr2_v_shared,
##                     x = ~x1,
##                     y = ~x2,
##                     color = ~species,
##                     colors = hcl.colors(3,
##                         palette="Zissou 1"),
##                     height = 450) |>
##   highlight(on = "plotly_selected",
##               off = "plotly_doubleclick") %>%
##     add_trace(type = "scatter",
##               mode = "markers")
## 
## bscols(
##      detour_plot, vot_mat,
##      widths = c(5, 6)
##  )


## -------------------------------------------------------
p_fit_rf2$fit$importance


## -------------------------------------------------------
#| echo: false
ggplot(p_tr2, aes(x=bl, y=fl, colour=species)) +
  geom_point() + 
  scale_colour_discrete_divergingx(palette = "Zissou 1")


## -------------------------------------------------------
set.seed(1110)
bt_spec <- boost_tree() |>
  set_mode("classification") |>
  set_engine("xgboost")
p_fit_bt <- bt_spec |> 
  fit(species ~ ., data = p_tr2)


## -------------------------------------------------------
#| echo: false
p_ts2_pred <- p_ts2 |>
  mutate(pspecies = predict(p_fit_bt, 
                            p_ts2)$.pred_class)
accuracy(p_ts2_pred, species, pspecies)
p_ts2_pred |>
  count(species, pspecies) |>
  group_by(species) |>
  mutate(Accuracy = n[species==pspecies]/sum(n)) |>
  pivot_wider(names_from = "pspecies", 
              values_from = n, values_fill = 0) |>
  select(species, Adelie, Chinstrap, Accuracy)

