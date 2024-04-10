## ----include = FALSE------------------------------------
source("../setup.R")
library(ggpubr)
library(kableExtra)


## -------------------------------------------------------
#| echo: false
#| out-width: 70%
#| fig-width: 4
#| fig-height: 4
library(tidyverse)
x <- seq(-2, 2, 0.1)
y <- exp(1+3*x)/(1+exp(1+3*x))
df2 <- tibble(x, y)
ggplot(df2, aes(x=x, y=y)) + 
  geom_line() +
  geom_hline(yintercept=0.5, colour="orange") +
  annotate("text", x=0.84, y=0.55, label="Activation threshold ??", colour="orange") +
  geom_hline(yintercept=c(0,1), linetype=2)


## -------------------------------------------------------
#| echo: false
#| out-width: 70%
#| fig-width: 4
#| fig-height: 5
w <- read_csv(here::here("data/wiggly.csv"))
ggplot(w, aes(x=x, y=y, colour=class, shape=class)) + 
  geom_point() +
  scale_color_brewer("", palette="Dark2") +
  scale_shape("") +
  theme(legend.position = "bottom") 


## -------------------------------------------------------
#| echo: false
#| out-width: 70%
#| fig-width: 4
#| fig-height: 5
load(here::here("data/nnet_many.rda"))
load(here::here("data/nnet_best.rda"))

ggplot(subset(best$output,  node == 1), aes(x, y)) +
  geom_raster(aes(fill = pred)) +
  geom_point(aes(shape = class), data = w) +
  scale_fill_gradient2("", low="#1B9E77", 
                       high="#D95F02", 
                       mid = "white", 
                       midpoint = 0.5,
                       guide = "colourbar",
                       limits = c(0,1)) +
  scale_shape("") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=6)) 


## -------------------------------------------------------
#| echo: false
#| out-width: 90%
#| fig-width: 5
#| fig-height: 4
ggplot(data=best$hidden, aes(x, y)) +
  geom_tile(aes(fill=pred)) + 
  geom_point(data=w, aes(shape=class)) +
  facet_wrap(~node, ncol=2) + 
  scale_fill_gradient2("", low="#AF8DC3",
                                    mid="#F7F7F7",
                                    high="#7FBF7B",
                                    midpoint=0.5,
                                    limits=c(0,1)) +
  scale_shape("") +
  theme(axis.text = element_blank(), 
        axis.title = element_blank(),
        legend.text = element_text(size=6))


## -------------------------------------------------------
#| echo: false
#| out-width: 70%
#| fig-width: 4
#| fig-height: 4
ggplot(data=best$hidden, aes(x, y)) + 
  geom_contour(aes(z=pred, group=node), 
               colour="grey50", 
               size=2, 
               breaks = 0.5) +
  geom_point(data=w, aes(colour=class, 
                 shape=class)) + 
  scale_color_brewer("", palette="Dark2") +
  scale_shape("") +
  theme(legend.position = "bottom",
        legend.text = element_text(size=6)) 




## -------------------------------------------------------
#| echo: false
#| out-width: 100%
#| fig-width: 12
#| fig-height: 4
qual <- unique(many[, c("value", "accuracy", "nodes", "id")])
ggplot(qual, aes(x=accuracy, y=value)) +
  geom_point(alpha=0.7, size=3) +
  xlab("Accuracy") +
  ylab("Value of fitting criterion") +
  facet_wrap(~nodes)



## ----echo=FALSE-----------------------------------------
#| message: false
#| eval: false
## animate_xy(p_std[,2:5], col=factor(p_tidy$species))
## render_gif(p_std[,2:5],
##            grand_tour(),
##            display_xy(half_range=4.2, axes="off",
##                       col=p_std$species),
##            gif_file="gifs/penguins_lbl.gif",
##            frames=500,
##            width = 400,
##            height = 400,
##            loop=FALSE)


## ----eval=FALSE-----------------------------------------
#| echo: true
## library(keras)
## tensorflow::set_random_seed(211)
## 
## # Define model
## p_nn_model <- keras_model_sequential()
## p_nn_model %>%
##   layer_dense(units = 2, activation = 'relu',
##               input_shape = 4) %>%
##   layer_dense(units = 3, activation = 'softmax')
## p_nn_model %>% summary
## 
## loss_fn <- loss_sparse_categorical_crossentropy(
##   from_logits = TRUE)
## 
## p_nn_model %>% compile(
##   optimizer = "adam",
##   loss      = loss_fn,
##   metrics   = c('accuracy')
## )


## ----echo=FALSE-----------------------------------------
set.seed(821)
p_split <- p_std %>% 
  initial_split(prop = 2/3, 
                strata=species)
p_train <- training(p_split)
p_test <- testing(p_split)

# Check training and test split
p_split_check <- bind_rows(
  bind_cols(p_train, type = "train"), 
  bind_cols(p_test, type = "test")) %>%
  mutate(type = factor(type))


## ----echo=FALSE, eval=FALSE-----------------------------
## render_gif(p_split_check[,2:5],
##            guided_tour(lda_pp(p_split_check$species)),
##            display_xy(
##              col=p_split_check$species,
##              pch=p_split_check$type,
##              shapeset=c(16,1),
##              cex=1.5,
##              axes="bottomleft"),
##            gif_file="gifs/p_split_guided.gif",
##            frames=500,
##            loop=FALSE
## )


## ----echo=TRUE------------------------------------------
# Data needs to be matrix, and response needs to be numeric
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


## ----echo=TRUE, eval=FALSE------------------------------
#| message: false
## # Fit model
## p_nn_fit <- p_nn_model %>%
##   keras::fit(
##     x = p_train_x,
##     y = p_train_y,
##     epochs = 200,
##     verbose = 0
##   )


## ----echo=FALSE-----------------------------------------
#| message: false
p_nn_model <- load_model_tf("../data/penguins_cnn")
p_nn_model


## ----echo=TRUE------------------------------------------
p_nn_model %>% 
  evaluate(p_test_x, p_test_y, verbose = 0)


## ----echo=FALSE-----------------------------------------
# Predict training and test set
p_train_pred <- p_nn_model %>% 
  predict(p_train_x, verbose = 0)
p_train_pred_cat <- levels(p_train$species)[
  apply(p_train_pred, 1,
        which.max)]
p_train_pred_cat <- factor(
  p_train_pred_cat,
  levels=levels(p_train$species))
table(p_train$species, p_train_pred_cat)

p_test_pred <- p_nn_model %>% 
  predict(p_test_x, verbose = 0)
p_test_pred_cat <- levels(p_test$species)[
  apply(p_test_pred, 1, 
        which.max)]
p_test_pred_cat <- factor(
  p_test_pred_cat,
  levels=levels(p_test$species))
table(p_test$species, p_test_pred_cat)


## -------------------------------------------------------
# Extract hidden layer model weights
p_nn_wgts <- keras::get_weights(p_nn_model, trainable=TRUE)
p_nn_wgts 


## ----echo=FALSE-----------------------------------------
# Orthonormalise the weights to make 2D projection
p_nn_wgts_on <- tourr::orthonormalise(p_nn_wgts[[1]])

# Hidden layer
p_train_m <- p_train %>%
  mutate(nn1 = as.matrix(p_train[,2:5]) %*%
           as.matrix(p_nn_wgts_on[,1], ncol=1),
         nn2 = as.matrix(p_train[,2:5]) %*%
           matrix(p_nn_wgts_on[,2], ncol=1))

# Now add the test points on.
p_test_m <- p_test %>%
  mutate(nn1 = as.matrix(p_test[,2:5]) %*%
           as.matrix(p_nn_wgts_on[,1], ncol=1),
         nn2 = as.matrix(p_test[,2:5]) %*%
           matrix(p_nn_wgts_on[,2], ncol=1))
p_train_m <- p_train_m %>%
  mutate(set = "train")
p_test_m <- p_test_m %>%
  mutate(set = "test")
p_all_m <- bind_rows(p_train_m, p_test_m)
ggplot(p_all_m, aes(x=nn1, y=nn2, 
                     colour=species, shape=set)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(16, 1)) +
  theme_minimal() +
  theme(aspect.ratio=1)


## ----echo=FALSE-----------------------------------------
# Set up the data to make the ternary diagram
# Join data sets
colnames(p_train_pred) <- c("Adelie", "Chinstrap", "Gentoo")
colnames(p_test_pred) <- c("Adelie", "Chinstrap", "Gentoo")
p_train_pred <- as_tibble(p_train_pred)
p_train_m <- p_train_m %>%
  mutate(pspecies = p_train_pred_cat) %>%
  bind_cols(p_train_pred) %>%
  mutate(set = "train")
p_test_pred <- as_tibble(p_test_pred)
p_test_m <- p_test_m %>%
  mutate(pspecies = p_test_pred_cat) %>%
  bind_cols(p_test_pred) %>%
  mutate(set = "test")
p_all_m <- bind_rows(p_train_m, p_test_m)

# Add simplex to make ternary
library(geozoo)
proj <- t(geozoo::f_helmert(3)[-1,])
p_nn_v_p <- as.matrix(p_all_m[,c("Adelie", "Chinstrap", "Gentoo")]) %*% proj
colnames(p_nn_v_p) <- c("x1", "x2")
p_nn_v_p <- p_nn_v_p %>%
  as.data.frame() %>%
  mutate(species = p_all_m$species,
         set = p_all_m$set)

simp <- geozoo::simplex(p=2)
sp <- data.frame(cbind(simp$points), simp$points[c(2,3,1),])
colnames(sp) <- c("x1", "x2", "x3", "x4")
sp$species = sort(unique(p_std$species))
ggplot() +
  geom_segment(data=sp, aes(x=x1, y=x2, xend=x3, yend=x4)) +
  geom_text(data=sp, aes(x=x1, y=x2, label=species),
            nudge_x=c(-0.1, 0.15, 0),
            nudge_y=c(0.05, 0.05, -0.05)) +
  geom_point(data=p_nn_v_p, aes(x=x1, y=x2, 
                                colour=species,
                                shape=set), 
             size=2, alpha=0.5) +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_shape_manual(values=c(19, 1)) +
  theme_map() +
  theme(aspect.ratio=1, legend.position = "right")

