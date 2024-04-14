## ----include = FALSE-------------------------------------
source("../setup.R")


## --------------------------------------------------------
#| echo: false
#| out-width: 80%
lda_spec <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS", prior = c(1/3, 1/3, 1/3))
lda_fit <- lda_spec |> 
  fit(species ~ ., data = p_std)

lda_fit



## --------------------------------------------------------
#| echo: false
#| out-width: 80%
ggplot(p_std, aes(x=bd, y=bl, colour=species)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1")



## --------------------------------------------------------
#| echo: false
lda_fit$fit$scaling


## --------------------------------------------------------
#| echo: false
#| out-width: 50%
#| fig-width: 4
#| fig-height: 4
ggscatmat(p_std, columns=2:5, color="species", alpha=0.5) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.position="none", 
        axis.text = element_blank())


## --------------------------------------------------------
#| echo: false
set.seed(855)
p_std_cor <- p_std |>
  mutate(bl2 = bl + runif(nrow(p_std), -0.1, 0.1),
         bd2 = bd + runif(nrow(p_std), -0.1, 0.1)) |>
  select(species, bl, bl2, bd2, bd, fl, bm)

lda_spec <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS", prior = c(1/3, 1/3, 1/3))
lda_fit2 <- lda_spec |> 
  fit(species ~ ., data = p_std_cor)

lda_fit2$fit$scaling



## --------------------------------------------------------
#| echo: false
#| out-width: 50%
#| fig-width: 4
#| fig-height: 4
ggscatmat(p_std_cor, columns=2:5, color="species", alpha=0.5) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.position="none", 
        axis.text = element_blank())


## --------------------------------------------------------
# Using DALEX with tidymodels
# https://www.tmwr.org/explain
# https://ema.drwhy.ai/featureImportance.html
vip_features <- colnames(p_std)[2:5]

vip_train <- 
  p_std |>
  select(all_of(vip_features))

explainer_lda <- 
  explain_tidymodels(
    lda_fit, 
    data = vip_train, 
    y = p_std$species,
    verbose = FALSE
  )
vip_lda <- model_parts(explainer_lda,
                       B=100)


## --------------------------------------------------------
#| echo: false
#| out-width: 100%
vip_lda |>
  filter(variable %in% 
           c("bl", "bd", "fl", "bm")) |>
  mutate(variable = 
           fct_reorder(variable, 
                       dropout_loss)) |>
  ggplot(aes(x=variable, y=dropout_loss)) +
    geom_quasirandom(alpha=0.5, size=1) +
    stat_summary(fun = "median", 
                 colour = "#D93F00",
                 geom="point", size=2) +
    xlab("") + ylab("importance") 


## --------------------------------------------------------
#| echo: false
#| out-width: 100%
vip_features2 <- colnames(p_std_cor)[2:7]
vip_train2 <- 
  p_std_cor |>
  select(all_of(vip_features2))

explainer_lda2 <- 
  explain_tidymodels(
    lda_fit2, 
    data = vip_train2, 
    y = p_std_cor$species,
    verbose = FALSE
  )
vip_lda2 <- model_parts(explainer_lda2,
                       B=100)

vip_lda2 |>
  filter(variable %in% 
           c("bl", "bl2", "bd2", "bd", "fl", "bm")) |>
  mutate(variable = 
           fct_reorder(variable, 
                       dropout_loss)) |>
  ggplot(aes(x=variable, y=dropout_loss)) +
    geom_quasirandom(alpha=0.5, size=1) +
    stat_summary(fun = "median", 
                 colour = "#D93F00",
                 geom="point", size=2) +
    xlab("") + ylab("importance")


## --------------------------------------------------------
# With DALEX
pdp_lda <- model_profile(
            explainer_lda,
            N=100)


## --------------------------------------------------------
#| label: PDP-lda
#| echo: false
#| out-width: 80%
#| fig-width: 5
#| fig-height: 5.5
pdp_lda$agr_profiles |>
  rename(vname = `_vname_`,
         x = `_x_`,
         pred = `_yhat_`,
         species = `_label_`) |>
  mutate(species = str_remove(species,
           "model_fit.")) |>
  ggplot(aes(x=x, y=pred, 
             colour=species)) +
    geom_line() +
    facet_wrap(~vname, ncol=2) + 
    xlab("") + ylab("predicted") +
    scale_color_discrete_divergingx(
      palette = "Zissou 1") +
    theme(legend.position = "bottom",
          legend.title = element_blank())


## --------------------------------------------------------
#| label: PDP-lda
#| echo: false
#| out-width: 75%
#| fig-width: 5
#| fig-height: 5.5
pdp_lda$agr_profiles |>
  rename(vname = `_vname_`,
         x = `_x_`,
         pred = `_yhat_`,
         species = `_label_`) |>
  mutate(species = str_remove(species,
           "model_fit.")) |>
  ggplot(aes(x=x, y=pred, 
             colour=species)) +
    geom_line() +
    facet_wrap(~vname, ncol=2) + 
    xlab("") + ylab("predicted") +
    scale_color_discrete_divergingx(
      palette = "Zissou 1") +
    theme(legend.position = "bottom",
          legend.title = element_blank())


## --------------------------------------------------------
#| echo: false
#| out-width: 75%
#| fig-width: 6
#| fig-height: 7
p_std |>
  pivot_longer(bl:bm, 
               names_to="vname",
               values_to="x") |>
  ggplot(aes(x=x, 
             colour=species,
             fill=species)) +
    geom_density(alpha=0.5) +
    facet_wrap(~vname, ncol=2) + 
    xlab("") + ylab("") +
    scale_color_discrete_divergingx(
      palette = "Zissou 1") +
    scale_fill_discrete_divergingx(
      palette = "Zissou 1") +
    theme(legend.position = "bottom",
          legend.title = element_blank())


## --------------------------------------------------------
#| echo: false
#| eval: false
#| label: sine-curve-data
## # Generate the more wiggly data
## set.seed(1231)
## x <- runif(3000, -1.2, 1.2)
## y <- sin(2*pi*x)
## d <- tibble(x, y)
## ggplot(d, aes(x, y)) +
##     geom_point(alpha=0.5)
## rx <- cos(pi/4)*x - sin(pi/4)*y
## ry <- sin(pi/4)*x + cos(pi/4)*y
## rd <- tibble(rx, ry)
## ggplot(rd, aes(rx, ry)) +
##     geom_point(alpha=0.5)
## set.seed(139)
## x1 <- runif(3450, -2, 2)
## x2 <- runif(3450, -2, 2)
## cl <- ifelse(x2 > sin(2*pi*x1)/2, "A", "B")
## rx1 <- cos(pi/4)*x1 - sin(pi/4)*x2
## rx2 <- sin(pi/4)*x1 + cos(pi/4)*x2
## dx <- tibble(rx1, rx2, cl)
## ggplot(dx, aes(rx1, rx2, colour=cl)) +
##     geom_point(alpha=0.5) +
##   scale_color_discrete_divergingx(
##     palette = "Zissou 1")
## dx2 <- dx |>
##   filter(rx1 > -1, rx1 < 1, rx2 > -1, rx2 < 1)
## ggplot(dx2, aes(rx1, rx2, colour=cl)) +
##     geom_point(alpha=0.5) +
##   scale_color_discrete_divergingx(
##     palette = "Zissou 1") +
##   geom_point(data=rd, aes(x=rx, y=ry), colour="black")
## dx2 <- dx2 |>
##   rename(x1 = rx1, x2 = rx2)
## write_csv(dx2, file="data/sine-rotated.csv")


## --------------------------------------------------------
#| fig-width: 4
#| fig-height: 4
#| echo: false
w <- read_csv("../data/sine-rotated.csv") |>
  mutate(cl = factor(cl))
ggplot(w, aes(x1, x2, colour=cl)) +
    geom_point(alpha=0.5) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| label: sine-selected
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
w_new <- data.frame(
  x1=c(-0.5, 0, 0.2, -0.8, 0.8, 0.8), 
  x2=c(-0.25, 0, -0.5, 0.8, -0.8, 0.5),
  cl=factor(c("A", "B", "B", "A", "B", "A"),
                      levels=c("A", "B")))
ggplot() +
    geom_point(data=w, aes(x1, x2, colour=cl), 
               alpha=0.6, shape=1) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  geom_text(data=w_new, aes(x1, x2, colour=cl, 
                            label=1:6),
            size=4, fontface="bold") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| results: hide
library(DALEXtra)
library(lime)
w_rf <- randomForest(cl~., data=w)
w_rf_exp <- DALEX::explain(model = w_rf,  
                        data = w[, 1:2],
                        y = w$cl == "A")
model_type.dalex_explainer <-
  DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <-
  DALEXtra::predict_model.dalex_explainer
w_lime <- predict_surrogate(
  explainer = w_rf_exp, 
              new_observation = w_new, 
              n_features = 2, 
              n_permutations = 100,
              type = "lime")



## --------------------------------------------------------
#| label: sine-selected
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 65%
w_new <- data.frame(
  x1=c(-0.5, 0, 0.2, -0.8, 0.8, 0.8), 
  x2=c(-0.25, 0, -0.5, 0.8, -0.8, 0.5),
  cl=factor(c("A", "B", "B", "A", "B", "A"),
                      levels=c("A", "B")))
ggplot() +
    geom_point(data=w, aes(x1, x2, colour=cl), 
               alpha=0.6, shape=1) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  geom_text(data=w_new, aes(x1, x2, colour=cl, 
                            label=1:6),
            size=4, fontface="bold") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
w_lime_coef <- w_lime |>
  select(case, model_intercept, feature_weight) |>
  as_tibble() |>
  rename(x1 = feature_weight) |>
  mutate(x2 = 0)
w_lime_coef$x2[seq(1, 11, 2)] <- w_lime_coef$x1[seq(2, 12, 2)]
w_lime_coef <- w_lime_coef[seq(1, 11, 2),]
w_lime_coef


## --------------------------------------------------------
library(iml)
# devtools::install_github("dandls/counterfactuals")
library(counterfactuals)
predictor_rf = iml::Predictor$new(w_rf, 
                                  type = "prob")
# predictor_rf$predict(w_new[1,])
w_classif <- counterfactuals::NICEClassif$new(
  predictor_rf)

w_new_cf <- w_new
w_new_cf$cl <- ifelse(w_new[,3]=="A", 
                           "B", "A")
for (i in 1:nrow(w_new)) {
  w_cf = w_classif$find_counterfactuals(
    x_interest = w_new[i,], 
    desired_class = w_new_cf[i,3],
                 desired_prob = c(0.5, 1)
  )
  w_new_cf[i, 1] <- w_cf$data$x1
  w_new_cf[i, 2] <- w_cf$data$x2
}


## --------------------------------------------------------
#| echo: false
w_new_cf_all <- w_new |>
  rename(x1o = x1, x2o = x2, clo = cl) |>
  bind_cols(w_new_cf)

w_new_cf_all


## --------------------------------------------------------
#| label: cf
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
ggplot() +
    geom_point(data=w, aes(x1, x2, colour=cl), 
               alpha=0.6, shape=1) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  geom_segment(data=w_new_cf_all, 
               aes(x=x1, y=x2, 
                   xend=x1o, yend=x2o,
                   colour=cl),
            linewidth=3, alpha=0.5) +
  geom_text(data=w_new, aes(x1, x2, colour=cl, 
                            label=1:6),
            size=4, fontface="bold") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 80%
w_anchors <- data.frame(
  xmin=c(-0.6, -0.05, 0.18, -1, -0.65, 0.1),
  xmax=c(-0.3, 1.0, 1.0, -0.2, 1.0, 0.8),
  ymin=c(-0.25, 0, -1, -0.2, -1, 0.4),
  ymax=c(1, 0.15, 0.15, 1, -0.5, 0.6),
  cl=c("A", "B", "B", "A", "B", "A"))
ggplot() +
    geom_point(data=w, aes(x1, x2, colour=cl), 
               alpha=0.6, shape=1) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  geom_rect(data=w_anchors, 
               aes(xmin=xmin, ymin=ymin, 
                   xmax=xmax, ymax=ymax,
                   colour=cl),
            linewidth=0.5, alpha=0.5,
            fill=NA) +
  geom_text(data=w_new, aes(x1, x2, colour=cl, 
                            label=1:6),
            size=4, fontface="bold") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| eval: false
## library(kernelshap)
## library(shapviz)
## w_explain <- kernelshap(
##     w_rf,
##     w_new[,1:2],
##     w[,1:2],
##     verbose = FALSE
##   )


## --------------------------------------------------------
#| eval: false
#| echo: false
## save(w_explain, file="data/w_explain.rda")


## --------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 7
#| out-width: 70%
load("../data/w_explain.rda")
w_shap <- w_new |>
  mutate(shapAx1 = w_explain$S$A[,1],
         shapAx2 = w_explain$S$A[,2])


## --------------------------------------------------------
#| label: sine-selected
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 50%
w_new <- data.frame(
  x1=c(-0.5, 0, 0.2, -0.8, 0.8, 0.8), 
  x2=c(-0.25, 0, -0.5, 0.8, -0.8, 0.5),
  cl=factor(c("A", "B", "B", "A", "B", "A"),
                      levels=c("A", "B")))
ggplot() +
    geom_point(data=w, aes(x1, x2, colour=cl), 
               alpha=0.6, shape=1) +
  scale_color_discrete_divergingx(
    palette = "Zissou 1") +
  geom_text(data=w_new, aes(x1, x2, colour=cl, 
                            label=1:6),
            size=4, fontface="bold") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
w_shap


## ----echo=FALSE------------------------------------------
#| message: false
set.seed(821)
p_split <- p_std %>% 
  initial_split(prop = 2/3, 
                strata=species)
p_train <- training(p_split)
p_test <- testing(p_split)

p_train_x <- p_train %>%
  select(bl:bm) %>%
  as.matrix()
p_train_y <- p_train %>% pull(species) %>% as.numeric() 
p_train_y <- p_train_y-1 # Needs to be 0, 1, 2
p_test_x <- p_test %>%
  select(bl:bm) %>%
  as.matrix()
p_test_y <- p_test %>% pull(species) %>% as.numeric() 
p_test_y <- p_test_y-1 # Needs to be 0, 1, 2



## --------------------------------------------------------
#| message: false
#| eval: false
## library(keras)
## p_nn_model <- load_model_tf("../data/penguins_cnn")
## p_nn_model
## 
## # Explanations
## # https://www.r-bloggers.com/2022/08/kernel-shap/
## library(kernelshap)
## library(shapviz)
## p_explain <- kernelshap(
##     p_nn_model,
##     p_train_x,
##     bg_X = p_train_x,
##     verbose = FALSE
##   )
## p_exp_sv <- shapviz(p_explain)
## save(p_exp_sv, file="../data/p_exp_sv.rda")


## --------------------------------------------------------
#| echo: false
library(keras)
p_nn_model <- load_model_tf("../data/penguins_cnn")

p_train_pred <- p_nn_model %>% 
  predict(p_train_x, verbose = 0)
p_train_pred_cat <- levels(p_train$species)[
  apply(p_train_pred, 1,
        which.max)]
p_train_pred_cat <- factor(
  p_train_pred_cat,
  levels=levels(p_train$species))



## --------------------------------------------------------
#| echo: false
load("../data/p_exp_sv.rda")
p_exp_gentoo <- p_exp_sv$Class_3$S
p_exp_gentoo <- p_exp_gentoo %>%
  as_tibble() %>%
  mutate(species = p_train$species,
         pspecies = p_train_pred_cat,
  ) %>%
  mutate(error = ifelse(species == pspecies, 0, 1))
library(ggpcp)
p_exp_gentoo %>%
  filter(species == "Gentoo") %>%
  pcp_select(1:4) %>%
  ggplot(aes_pcp()) +
    geom_pcp_axes() + 
    geom_pcp_boxes(fill="grey80") + 
    geom_pcp(aes(colour = factor(error)), 
             linewidth = 2, alpha=0.3) +
  scale_colour_discrete_divergingx(palette="Geyser") +
  xlab("") + ylab("SHAP") +
  theme_minimal() + 
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
p_nn_wgts <- keras::get_weights(p_nn_model, trainable=TRUE)
p_nn_wgts_on <- tourr::orthonormalise(p_nn_wgts[[1]])
p_nn_wgts_on


## --------------------------------------------------------
#| echo: false
table(p_train$species, p_train_pred_cat)


## --------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 5
#| out-width: 70%
# Check position on bm
shap_proj <- p_exp_gentoo %>%
  filter(species == "Gentoo", error == 1) %>%
  select(bl:bm)
shap_proj <- as.matrix(shap_proj/sqrt(sum(shap_proj^2)))
p_exp_gentoo_proj <- p_exp_gentoo %>%
  rename(shap_bl = bl, 
         shap_bd = bd,
         shap_fl = fl, 
         shap_bm = bm) %>%
  bind_cols(as_tibble(p_train_x)) %>%
  mutate(shap1 = shap_proj[1]*bl+
           shap_proj[2]*bd+
           shap_proj[3]*fl+
           shap_proj[4]*bm)
sp1 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=bl, 
             colour=species, 
             shape=factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(19, 1)) +
  theme_minimal() + 
  theme(aspect.ratio=1, legend.position="bottom")
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=shap1, 
             colour=species, 
             shape=factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(19, 1)) +
  ylab("SHAP") +
  theme_minimal() + 
  theme(aspect.ratio=1, legend.position="bottom")
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=shap1, 
             fill=species, colour=species)) +
  geom_density(alpha=0.5) +
  geom_vline(xintercept = p_exp_gentoo_proj$shap1[
    p_exp_gentoo_proj$species=="Gentoo" &
    p_exp_gentoo_proj$error==1], colour="black") +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  theme_minimal() + 
  theme(aspect.ratio=1, legend.position="bottom")
sp2 <- ggplot(p_exp_gentoo_proj, aes(x=bm, y=bd, 
             colour=species, 
             shape=factor(1-error))) +
    geom_point(alpha=0.8) +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual("error", values=c(19, 1)) +
  theme_minimal() + 
  theme(aspect.ratio=1, legend.position="bottom")
sp1 + sp2 + plot_layout(ncol=1, guides = "collect") &
  theme(#legend.position="bottom",
        legend.direction="vertical")

