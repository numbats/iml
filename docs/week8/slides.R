## ----include = FALSE-------------------------------------
source("../setup.R")


## --------------------------------------------------------
#| echo: false
#| eval: false
## set.seed(427)
## n1 <- 102
## n2a <- 25
## n2b <- 37
## n2c <- 32
## vc1 <- matrix(c(4, -1, -1, 4), ncol=2, byrow=TRUE)
## vc2 <- matrix(c(0.25, -0.2, -0.2, 0.5), ncol=2, byrow=TRUE)
## d1 <- rmvnorm(n1, c(5, 5), vc1)
## d2a <- rmvnorm(n2a, c(-1.2, 0), vc2)
## d2b <- rmvnorm(n2b, c(-1, -1), vc2)
## d2c <- rmvnorm(n2c, c(-0.5, -3), vc2)
## pebbles <- tibble(x1 = c(d1[,1], d2a[,1],
##                          d2b[,1], d2c[,1]),
##                   x2 = c(d1[,2], d2a[,2],
##                          d2b[,2], d2c[,2]),
##                   cl = factor(c(rep("A", n1),
##                               rep("B", n2a+n2b+n2c))))
## pebbles <- pebbles |>
##   mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
## ggplot(pebbles, aes(x=x1, y=x2,
##                     colour=cl)) +
##   geom_point() +
##   scale_color_discrete_divergingx(palette = "Zissou 1")
## write_csv(pebbles, file="data/pebbles.csv")


## --------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
pebbles <- read_csv(here::here("data/pebbles.csv")) |>
  mutate(cl = factor(cl))
ggplot(pebbles, aes(x=x1, y=x2, 
                    colour=cl)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
lda_spec <- discrim_linear() |>
  set_mode("classification") |>
  set_engine("MASS", prior = c(0.5, 0.5))
lda_fit <- lda_spec |> 
  fit(cl ~ ., data = pebbles)

pebbles_grid <- tibble(
  x1 = runif(10000, -1.5, 2.5),
  x2 = runif(10000, -1.75, 2.25))
pebbles_grid <- pebbles_grid |>
  mutate(cl_lda = factor(predict(lda_fit$fit,
                             pebbles_grid)$class))
ggplot(pebbles_grid, aes(x=x1, y=x2, 
                    colour=cl_lda)) +
  geom_point(alpha=0.05, size=2) +
  geom_point(data=pebbles, aes(colour=cl), 
             shape=16, size=2) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.position = "none")



## --------------------------------------------------------
#| message: false
#| results: hide
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
svm_spec <- svm_linear() |>
  set_mode("classification") |>
  set_engine("kernlab")
svm_fit <- svm_spec |> 
  fit(cl ~ ., data = pebbles)

pebbles_grid <- pebbles_grid |>
  mutate(cl_svm = factor(predict(svm_fit$fit,
                             pebbles_grid)))
ggplot(pebbles_grid, aes(x=x1, y=x2, 
                    colour=cl_svm)) +
  geom_point(alpha=0.05, size=2) +
  geom_point(data=pebbles, aes(colour=cl),
             shape=16, size=2) +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  theme(legend.position = "none")



## --------------------------------------------------------
lda_fit$fit$scaling


## --------------------------------------------------------
#| echo: false
#| eval: false
## p_sub <- p_std |>
##   filter(species != "Chinstrap") |>
##   mutate(species = factor(species)) |>
##   select(species, bl:bm)
## 
## lda_fit2 <- lda_spec |>
##   fit(species ~ ., data = p_sub)
## 
## p_explore <- classifly::explore(lda_fit2$fit, p_sub)
## p_lda_bnd <- p_explore |>
##   as_tibble() |>
##   #filter(.TYPE == "simulated") |>
##   filter(!.BOUNDARY)
## 
## animate_xy(p_lda_bnd[,1:4],
##            col=p_lda_bnd$species,
##            pch=p_lda_bnd$.TYPE, shapeset = c(19, 4))
## 
## render_gif(p_lda_bnd[,1:4],
##            grand_tour(),
##            display_xy(col=p_lda_bnd$species,
##            pch=p_lda_bnd$.TYPE,
##            shapeset = c(19, 4)),
##            gif_file = "gifs/p_svm.gif",
##            frames=500,
##            width=400,
##            height=400)


## --------------------------------------------------------
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
#| results: hide
svm_spec2 <- svm_linear(cost=1.5) |>
  set_mode("classification") |>
  set_engine("kernlab")
svm_fit2 <- svm_spec2 |> 
  fit(cl ~ ., data = pebbles)
svm_fit2$fit@SVindex
svm_fit2$fit@alpha

pebbles_sv <- pebbles |>
  mutate(id = row_number()) |>
  mutate(sv = ifelse(id %in%
                       svm_fit2$fit@SVindex, 
                     "yes", "no")) 
ggplot(pebbles_sv, aes(x=x1, y=x2, 
             colour=cl)) +
  geom_point() +
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=filter(pebbles_sv, 
                         sv == "yes"), shape=1, size=3) +
  theme(legend.position = "none")


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
#| out-width: 100%
p_tiny <- p_std[c(1:10, 152:161, 275:284),]
obs1 <- 28  
p_tiny_dst <- dist(p_tiny[,2:3])
# sort(as.matrix(p_tiny_dst)[obs1,])
kn3 <- c(22, 23, 25)
kn3_lines <- tibble(x=rep(p_tiny$bl[obs1], 3), 
                    xend=p_tiny$bl[kn3], 
                    y=rep(p_tiny$bd[obs1], 3), 
                    yend=p_tiny$bd[kn3], 
                    species=p_tiny$species[kn3])
p1 <- ggplot(p_tiny, aes(x=bl, y=bd, colour=species)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=p_tiny[obs1,], shape=1, size=4) +
  geom_segment(data=kn3_lines, aes(x=x, xend=xend,
                                   y=y, yend=yend)) +
  ggtitle("k=3") + xlim(c(-2,2)) + ylim(c(-2,2))
obs2 <- 12  
# sort(as.matrix(p_tiny_dst)[obs2,])
kn5 <- c(14,  18,  20,  15,  21) 
kn5_lines <- tibble(x=rep(p_tiny$bl[obs2], 5), 
                    xend=p_tiny$bl[kn5], 
                    y=rep(p_tiny$bd[obs2], 5), 
                    yend=p_tiny$bd[kn5], 
                    species=p_tiny$species[kn5])

p2 <- ggplot(p_tiny, aes(x=bl, y=bd, colour=species)) +
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  geom_point(data=p_tiny[obs2,], shape=1, size=4) +
  geom_segment(data=kn5_lines, aes(x=x, xend=xend,
                                   y=y, yend=yend)) +
  ggtitle("k=5") + xlim(c(-2,2)) + ylim(c(-2,2))
p1 + p2 + plot_layout(ncol=2)


## --------------------------------------------------------
#| echo: false
#| out-width: 100%
#| fig-width: 8
#| fig-height: 3
set.seed(839)
tr <- matrix(rnorm(20*100),ncol=100)
colnames(tr) <- paste0("x", 1:100)
tr[1:10,1] <- tr[1:10,1]+5
tr <- apply(tr, 2, function(x) (x-mean(x))/sd(x))
tr <- as_tibble(tr) %>% mutate(cl=c(rep("A",10), rep("B",10)))
p1 <- ggplot(data=tr, aes(x=x1, y=x2, colour=cl, shape=cl)) + 
  geom_point(size=3) + 
  scale_color_brewer(palette="Dark2") +
  theme_bw() + 
  theme(legend.position="none", aspect.ratio=1) +
  ggtitle("Gap in x1")
p2 <- ggplot(data=tr, aes(x=x2, y=x3, colour=cl, shape=cl)) + 
  geom_point(size=3) + 
  scale_color_brewer(palette="Dark2") +
  theme_bw() + 
  theme(legend.position="none", aspect.ratio=1) +
  ggtitle("Noise")
grid.arrange(p1, p2, ncol=2)

# Generate test data
ts <- matrix(rnorm(10*100),ncol=100)
colnames(ts) <- paste0("x", 1:100)
ts[1:5,1] <- ts[1:5,1]+5
ts <- apply(ts, 2, function(x) (x-mean(x))/sd(x))
ts <- as_tibble(ts) %>% mutate(cl=c(rep("A",5), rep("B",5)))


## --------------------------------------------------------
#| echo: false
library(MASS)
tr_lda <- lda(cl~., data=tr[,c(1:2,101)], prior=c(0.5,0.5))
tr_lda


## --------------------------------------------------------
#| echo: false
tr_p <- predict(tr_lda, tr)
table(tr_p$class, tr$cl)

ts_p <- predict(tr_lda, ts)
table(ts_p$class, ts$cl)


## --------------------------------------------------------
#| echo: false
#| out-width: "80%"
#| fig-width: 4
#| fig-height: 2
ggplot(data=data.frame(
  LD1=tr_p$x, cl=tr$cl), 
  aes(x=LD1, y=cl)) +
         geom_point(size=5, alpha=0.5) +
         ylab("Class") + xlim(c(-10,10)) +
  geom_point(data=data.frame(LD1=ts_p$x, cl=ts$cl), 
             shape=2, size=5, colour="red")


## ----animation.hook='gifski', out.width="100%", fig.width=9, fig.height=4.5, echo=FALSE----
for (i in 2:16) {
  tr_lda <- lda(cl~., data=tr[,c(1:i,101)], prior=c(0.5,0.5))
  tr_p <- predict(tr_lda, tr)
  ts_p <- predict(tr_lda, ts)
  t1 <- table(tr$cl, tr_p$class)
  t2 <- table(ts$cl, ts_p$class)
  tr_err <- (t1[1,2]+t1[2,1])/sum(t1)
  ts_err <- (t2[1,2]+t2[2,1])/sum(t2)

  print(
    ggplot(data=data.frame(LD1=tr_p$x, cl=tr$cl), aes(x=LD1, y=cl)) +
         geom_point(size=5, alpha=0.5) +
         ylab("Class") + xlim(c(-20,20)) +
      geom_point(data=data.frame(LD1=ts_p$x, cl=ts$cl), 
             shape=2, size=5, colour="red") +
      ggtitle(paste0("p = ", i, " train = ", tr_err, " test = ", ts_err))
  )
}


## ----animation.hook='gifski', out.width="100%", fig.width=10, fig.height=4, echo=FALSE----
for (i in 2:20) {
  tr_lda <- lda(cl~., data=tr[,c(1:i,101)], prior=c(0.5,0.5))
  coef <- tibble(var=colnames(tr)[1:20], coef=c(1,rep(0,19)))
  coef$var <- factor(coef$var, levels=c(paste0("x",1:20)))
  coef$coef[1:i] <- abs(tr_lda$scaling)/sqrt(sum(tr_lda$scaling^2))
  print(
    ggplot(data=coef, aes(x=var, y=coef)) +
    geom_col() + ylim(c(0,1)) + xlab("Variable") + 
    ylab("Coefficient") +
    ggtitle(paste0("p = ", i)) +
      theme(aspect.ratio=0.3)
  )
}


## --------------------------------------------------------
w <- matrix(runif(48*40), ncol=40) |>
  as.data.frame() |>
  mutate(cl = factor(rep(c("A", "B", "C", "D"), rep(12, 4))))
w_lda <- lda(cl~., data=w)
w_pred <- predict(w_lda, w, dimen=2)$x
w_p <- w |>
  mutate(LD1 = w_pred[,1],
         LD2 = w_pred[,2])


## --------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 4
#| out-width: 80%
ggplot(w_p, aes(x=LD1, y=LD2, colour=cl)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) 


## --------------------------------------------------------
#| eval: false
#| echo: false
## animate_xy(w[,1:40], guided_tour(lda_pp(w$cl)),
##            col=w$cl,
##            sphere=TRUE,
##            axes="off",
##            half_range=4)


## --------------------------------------------------------
#| echo: true
set.seed(951)
ws <- w |>
  mutate(cl = sample(cl))


## --------------------------------------------------------
#| echo: false
#| fig-width: 5
#| fig-height: 4
#| out-width: 80%
ws_lda <- lda(cl~., data=ws)
ws_pred <- predict(ws_lda, ws, dimen=2)$x
ws_p <- ws |>
  mutate(LD1 = ws_pred[,1],
         LD2 = ws_pred[,2])
ggplot(ws_p, aes(x=LD1, y=LD2, colour=cl)) + 
  geom_point() +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) 

