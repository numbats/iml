
source("../setup.R")


#| label: data-in-model-space1
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
w <- read_csv(here::here("data/sine-curve.csv")) |>
  mutate(cl = factor(cl))
w_rf <- randomForest(cl~x1+x2, data=w, ntree=200)
w_rf_pred <- predict(w_rf, w, type="prob") |>
  as_tibble(.name_repair="unique")
w_rf_pred |> 
  bind_cols(w) |>
  select(`A`, cl) |>
  ggplot(aes(x=`A`, colour=cl, fill=cl)) + 
    geom_density(alpha=0.7) +
    xlab("Prob class A") +
    ggtitle("Random forest") + 
    theme(legend.position="none")
  


#| label: data-in-model-space2
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w, method="predictive")$posterior |>
  as_tibble(.name_repair="unique")
w_lda_pred |> 
  bind_cols(w) |>
  select(`A`, cl) |>
  ggplot(aes(x=`A`, colour=cl, fill=cl)) + 
    geom_density(alpha=0.7) +
    xlab("Prob class A") +
    ggtitle("Linear DA") + 
    theme(legend.position="none")


#| label: model-in-the-data-space1
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
library(geozoo)
w_grid <- cube.solid.random(p=2)$points
colnames(w_grid) <- c("x1", "x2")
w_grid <- w_grid |> as_tibble() |>
  mutate(x1 = x1*(max(w$x1)-min(w$x1)+min(w$x1)),
         x2 = x2*(max(w$x2)-min(w$x2)+min(w$x2)))
w_lda <- lda(cl~x1+x2, data=w)
w_lda_pred <- predict(w_lda, w_grid)$class
w_rf <- randomForest(cl~x1+x2, data=w, ntree=200)
w_rf_pred <- predict(w_rf, w_grid)
w_grid <- w_grid |>
  mutate(plda = w_lda_pred,
         prf = w_rf_pred)
ggplot(w_grid, aes(x=x1, y=x2, colour = plda)) +
  geom_point(alpha=0.5, size=2) +
  geom_text(data=w, aes(x=x1, y=x2, label=cl), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Linear DA") +
  theme(legend.position = "none",
        axis.text = element_blank())


#| label: model-in-the-data-space2
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 60%
ggplot(w_grid, aes(x=x1, y=x2, colour = prf)) +
  geom_point(alpha=0.5, size=2) +
  geom_text(data=w, aes(x=x1, y=x2, label=cl), colour="black") +
  scale_color_discrete_qualitative() +
  ggtitle("Random forest") +
  theme(legend.position = "none",
        axis.text = element_blank())


#| label: load-penguins
#| fig-width: 4
#| fig-height: 4
#| echo: false
library(palmerpenguins)
p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) 
ggpairs(p_tidy, columns=2:5) +
  theme(axis.text = element_blank())


#| code-fold: true
#| echo: false
#| label: hiding
set.seed(946)
d <- tibble(x1=runif(200, -1, 1), 
            x2=runif(200, -1, 1), 
            x3=runif(200, -1, 1))
d <- d %>%
  mutate(x4 = x3 + runif(200, -0.1, 0.1))
d <- bind_rows(d, c(x1=0, x2=0, x3=-0.5, x4=0.5))

d_r <- d %>%
  mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
         x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
         x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
         x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)



#| label: visible
#| fig-width: 4
#| fig-height: 4
#| echo: false
ggpairs(d) +
  theme(axis.text = element_blank())


#| label: invisible
#| fig-width: 4
#| fig-height: 4
#| echo: false
ggpairs(d_r) +
  theme(axis.text = element_blank())


#| eval: false
#| echo: false
## # Code to make the plots
## ggscatmat(d)
## animate_xy(d)
## render_gif(d,
##            grand_tour(),
##            display_xy(
##              axes="bottomleft", cex=2.5),
##            gif_file = "gifs/anomaly1.gif",
##            start = basis_random(4, 2),
##            apf = 1/60,
##            frames = 1500,
##            width = 500,
##            height = 400)
## ggscatmat(d_r)
## animate_xy(d_r)
## render_gif(d_r,
##            grand_tour(),
##            display_xy(
##              axes="bottomleft", cex=2.5),
##            gif_file = "gifs/anomaly2.gif",
##            start = basis_random(4, 2),
##            apf = 1/60,
##            frames = 1500,
##            width = 500,
##            height = 400)
## 
## dsq <- tibble(x1=runif(200, -1, 1),
##             x2=runif(200, -1, 1),
##             x3=runif(200, -1, 1))
## dsq <- dsq %>%
##   mutate(x4 = x3^2 + runif(200, -0.1, 0.1))
## dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=0, x4=1.1))
## dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=0.1, x4=1.05))
## dsq <- bind_rows(dsq, c(x1=0, x2=0, x3=-0.1, x4=1.0))
## ggscatmat(dsq)
## animate_xy(dsq, axes="bottomleft")
## dsq_r <- dsq %>%
##   mutate(x1 = cos(pi/6)*x1 + sin(pi/6)*x3,
##          x3 = -sin(pi/6)*x1 + cos(pi/6)*x3,
##          x2 = cos(pi/6)*x2 + sin(pi/6)*x4,
##          x4 = -sin(pi/6)*x2 + cos(pi/6)*x4)
## ggscatmat(dsq_r)
## animate_xy(dsq_r, axes="bottomleft")


#| label: aspectratio
#| fig-width: 8
#| fig-height: 4
#| out.width: 80%
#| echo: false
p <- ggplot(p_tidy, aes(x=fl, y=bm)) + geom_point() + 
  ggtitle("Yes") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5)) 
pp <- p + ggtitle("Nope")+ 
  theme(aspect.ratio=0.6, 
        axis.title = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5)) 
# Note aspect.ratio=1 is default plot setting for slides
grid.arrange(pp, p, ncol=2)


#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggparcoord(p_tidy, columns = 2:5, alphaLines = 0.5) + 
  xlab("") + ylab("") + 
  theme(aspect.ratio=0.8)


#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggparcoord(p_tidy, columns = 2:5, alphaLines = 0.5,
           scale="globalminmax") + 
  xlab("") + ylab("") + 
  theme(aspect.ratio=0.8)


#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggparcoord(p_tidy, columns = 2:5, alphaLines = 0.5,
           scale="uniminmax") + 
  xlab("") + ylab("") + 
  theme(aspect.ratio=0.8)


#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggparcoord(p_tidy, columns = 2:5, alphaLines = 0.5,
           groupColumn = 1) + 
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  xlab("") + ylab("") +
  theme(legend.position="none", aspect.ratio=0.8)


#| fig-width: 4
#| fig-height: 4
#| out-width: 100%
ggparcoord(p_tidy, columns = 2:5, alphaLines = 0.5,
           groupColumn = 1, order=c(4, 2, 5, 3)) + 
  scale_color_discrete_divergingx(palette = "Zissou 1") +
  xlab("") + ylab("") +
  theme(legend.position="none", aspect.ratio=0.8)


#| label: interactive-scatmat1
library(plotly)
g <- ggpairs(p_tidy, columns=2:5) +
  theme(axis.text = element_blank()) 


#| label: interactive-scatmat2
ggplotly(g, width=600, height=600)


#| label: interactive-pcp1
p_pcp <- p_tidy |>
  na.omit() |>
  plot_ly(type = 'parcoords',
          line = list(),
          dimensions = list(
            list(range = c(172, 231),
                 label = 'fl', values = ~fl),
            list(range = c(32, 60),
                 label = 'bl', values = ~bl),
           list(range = c(2700, 6300),
                 label = 'bm', values = ~bm),
            list(range = c(13, 22),
                 label = 'bd', values = ~bd)
             )
          )



#| label: interactive-pcp2
p_pcp


#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
#| fig-alt: "Scatterplot showing the 2D data having two clusters."
data("simple_clusters")

ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point(size=2, alpha=0.8, colour="#EC5C00") 


#| echo: false
#| eval: false
## library(tourr)
## library(geozoo)
## set.seed(1351)
## d <- torus(3, n=4304)$points
## d <- apply(d, 2, function(x) (x-mean(x))/sd(x))
## colnames(d) <- paste0("x", 1:3)
## d <- data.frame(d)
## animate_xy(d, axes="bottomleft")
## animate_slice(d, axes="bottomleft")
## set.seed(606)
## path_t2 <- save_history(d, little_tour(), 4)
## render_gif(d,
##            planned_tour(path_t2),
##            display_xy(col="#EC5C00",
##              half_range=3,
##              axes="bottomleft"),
##            gif_file = "gifs/torus.gif",
##            apf = 1/75,
##            frames = 1000,
##            width = 400,
##            height = 300)
## render_gif(d,
##            planned_tour(path_t2),
##            display_slice(col="#EC5C00",
##              half_range=3,
##              axes="bottomleft"),
##            gif_file = "gifs/torus_slice.gif",
##            apf = 1/75,
##            frames = 1000,
##            width = 400,
##            height = 300)


#| echo: false
penguins <- penguins %>%
  na.omit() # 11 observations out of 344 removed
# use only vars of interest, and standardise
# them for easier interpretation
penguins_sub <- penguins %>% 
  select(bill_length_mm,
         bill_depth_mm,
         flipper_length_mm,
         body_mass_g,
         species, 
         sex) %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)


#| eval: false
#| echo: false
## set.seed(645)
## render_gif(penguins_sub[,1:4],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##              half_range=3.8,
##              axes="bottomleft", cex=2.5),
##            gif_file = "gifs/penguins1.gif",
##            apf = 1/60,
##            frames = 1500,
##            width = 500,
##            height = 400)


#| label: invisible
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
#| echo: false
ggpairs(d_r) +
  theme(axis.text = element_blank())



## library(tourr)
## animate_xy(flea[, 1:6])



flea |> slice_head(n=3)



## animate_xy(flea[, 1:6],
##            col = flea$species)



## animate_xy(flea[, 1:6],
##            tour_path = guided_tour(holes()),
##            col = flea$species,
##            sphere = TRUE)



## set.seed(915)
## animate_xy(flea[, 1:6],
##            radial_tour(basis_random(6, 2),
##                        mvar = 1),
##            rescale = TRUE,
##            col = flea$species)


#| eval: false
## set.seed(645)
## render_gif(penguins_sub[,1:4],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##              half_range=3.8,
##              axes="bottomleft", cex=2.5),
##            gif_file = "../gifs/penguins1.gif",
##            apf = 1/60,
##            frames = 1500,
##            width = 500,
##            height = 400)


#| echo: false
library(mvtnorm)
vc <- matrix(c(1, -0.9,  
               -0.9, 1), 
             ncol=2, byrow=TRUE)
set.seed(937)
eg1 <- rmvnorm(107, 
             mean = c(0, 0), 
             sigma = vc)
eg1 <- eg1 |>
  as_tibble()
ggplot(eg1, aes(x=V1, y=V2)) + geom_point()


#| echo: false
vc <- matrix(c(1, 0,  
               0, 1), 
             ncol=2, byrow=TRUE)
set.seed(942)
x1 <- rmvnorm(86, 
             mean = c(0, 0), 
             sigma = vc) |>
  as_tibble()
x2 <- rmvnorm(98, 
             mean = c(7, 0), 
             sigma = vc) |>
  as_tibble()
stdd <- function(x) (x-mean(x))/sd(x)
eg2 <- bind_rows(x1, x2) |>
  mutate_at(c("V1", "V2"), stdd)
ggplot(eg2 , aes(x=V1, y=V2)) + geom_point()


#| echo: false
## library(tidyverse)
## f.norm.vec<-function(x) {
##   x<-x/f.norm(x)
##   x
## }
## f.norm<-function(x) { sqrt(sum(x^2)) }
## f.gen.sphere<-function(n=100,p=5) {
##   x<-matrix(rnorm(n*p),ncol=p)
##   xnew<-t(apply(x,1,f.norm.vec))
##   xnew
## }
## f.vc.ellipse <- function(vc, xm, n=500) {
##   p<-ncol(vc)
##   x<-f.gen.sphere(n,p)
## 
##   evc<-eigen(vc)
##   vc2<-(evc$vectors)%*%diag(sqrt(evc$values))%*%t(evc$vectors)
##   x<-x%*%vc2
## 
##   x + matrix(rep(xm, each=n),ncol=p)
## }
## df <- f.vc.ellipse(vc=matrix(c(1,0.8,0.8,2), ncol=2), xm=c(0,0), n=1000)
## df <- as_tibble(df)
## ev <- tibble(e1=c(0.4847685, -0.8746425), e2=c(0.8746425, 0.4847685),
##                 group=c("pc1","pc2"))


#| echo: false
## library(gganimate)
## ggplot(df, aes(x=V1, y=V2)) + geom_point() +
##   xlim(c(-1.5, 1.5)) + ylim(c(-1.5, 1.5)) +
##   geom_abline(data=ev, aes(intercept=0, slope=e2/e1, colour=group), size=2) +
##   scale_colour_brewer("", palette="Dark2") +
##   theme(legend.position="none") +
##   transition_states(group, 1, 1) + enter_grow() + exit_shrink() +
##   labs(title = "{closest_state}")


#| echo: false
## df_pca <- prcomp(df, retx=T, center=T, scale=TRUE)
## df_lines <- tibble(V1=c(seq(-1,1, 0.5), seq(-1,1, 0.5), c(-1.5, 1.5), c(0, 0)),
##                    V2=c(seq(-1,1, 0.5)*ev$e2[1]/ev$e1[1],
##                         seq(-1,1, 0.5)*ev$e2[2]/ev$e1[2], c(0, 0), c(-1.5, 1.5)),
##                    pc=c(rep("pc1", 5), rep("pc2", 5), "pc1", "pc1", "pc2", "pc2"),
##                    group=c(rep("raw data", 10), rep("principal components", 4)))
## df_pc <- as_tibble(df_pca$x) %>%
##   rename(V1=PC1, V2=PC2) %>%
##   mutate(V1 = V1/df_pca$sdev[1], V2 = V2/df_pca$sdev[2])
## ggplot(df_pc, aes(x=V1, y=V2)) + geom_point() + theme(aspect.ratio=1)
## all <- bind_rows(df, df_pc) %>%
##   mutate(group=c(rep("raw data", 1000), rep("principal components", 1000))) %>%
##   mutate(group = factor(group, levels=c("raw data", "principal components")))
## p <- ggplot(all, aes(x=V1, y=V2)) + geom_point(size=1) +
##   geom_line(data=df_lines, aes(x=V1, y=V2, group=interaction(group, pc), colour=pc)) +
##   scale_colour_brewer("", palette="Dark2") +
##   xlim(c(-2, 2)) + ylim(c(-2, 2)) +
##   xlab("") + ylab("") +
##   theme(aspect.ratio=1) +
##   transition_states(group, 1, 1) + enter_grow() + exit_shrink() +
##   labs(title = "{closest_state}")
## anim_save(filename = "images/pc-demo.gif", animation = p,
##           start_pause = 15, width = 480, height = 480, res = 150)


#| echo: false
df <- tibble(npcs=1:10, evl=c(3.5,2.7,2.2,0.5,0.3,0.3,0.2,0.1,0.1,0.1))
p <- ggplot(df, aes(x=npcs, y=evl)) + geom_line() + 
  xlab("Number of PCs") + ylab("Eigenvalue/Variance") +
  scale_x_continuous(breaks=seq(0,10,1)) 
p


#| echo: false
p + geom_vline(xintercept=4, colour="orange", size=3, alpha=0.7) +
  annotate("text", x=4.5, y=3, label="Choose k=4", colour="orange", hjust = 0, size=8)



track <- read_csv(here::here("data/womens_track.csv"))
glimpse(track)


#| echo: false
#| out.width: "100%"
#| fig.width: 8
#| fig.height: 8
ggscatmat(track[,1:7])


#| echo: false
## render_gif(track[,1:7],
##            grand_tour(),
##            display_xy(col="#EC5C00",
##              cex=2),
##            rescale=TRUE,
##            gif_file = "gifs/track.gif",
##            apf = 1/30,
##            frames = 1500,
##            width = 400,
##            height = 400)



options(digits=2)



track_pca <- prcomp(track[,1:7], center=TRUE, scale=TRUE)
track_pca


#| echo: false
library(kableExtra)
library(knitr)
track_pca_smry <- tibble(evl=track_pca$sdev^2) %>%
  mutate(p = evl/sum(evl), cum_p = cumsum(evl/sum(evl))) %>% t() 
colnames(track_pca_smry) <- colnames(track_pca$rotation)
rownames(track_pca_smry) <- c("Variance", "Proportion", "Cum. prop")
kable(track_pca_smry, digits=2, align="r") %>% 
  kable_styling(full_width = T) %>%
  row_spec(0, color="white", background = "#7570b3") %>%
  column_spec(1, width = "2.5em", color="white", background = "#7570b3") %>%
  column_spec(1:8, width = "2.5em") %>%
  row_spec(3, color="white", background = "#CA6627")


#| echo: false
track_pca_var <- tibble(n=1:length(track_pca$sdev), evl=track_pca$sdev^2)
ggplot(track_pca_var, aes(x=n, y=evl)) + geom_line() +
  xlab("Number of PCs") + ylab("Eigenvalue") +
  theme_minimal(base_size = 18)


#| echo: false
#| out-width: 100%
#| fig-width: 5
#| fig-height: 5
track_pca_pcs <- as_tibble(track_pca$x[,1:2]) %>%
  mutate(cnt=track$country)
track_pca_evc <- as_tibble(track_pca$rotation[,1:2]) %>% 
  mutate(origin=rep(0, 7), variable=colnames(track)[1:7],
         varname=rownames(track_pca$rotation)) %>%
  mutate(PC1s = PC1*(track_pca_var$evl[1]*2.5), 
         PC2s = PC2*(track_pca_var$evl[2]*2.5))
pca_p <- ggplot() + 
  geom_segment(data=track_pca_evc, 
               aes(x=origin, xend=PC1s, 
                   y=origin, yend=PC2s), colour="orange") +
  geom_text(data=track_pca_evc, aes(x=PC1s, y=PC2s,
                                    label=variable),
            colour="orange", nudge_x=0.7) +
  geom_point(data=track_pca_pcs, aes(x=PC1, y=PC2)) +
  geom_text(data=filter(track_pca_pcs, abs(PC2)>1.3),
            aes(x=PC1, y=PC2, label=cnt), 
            nudge_y=0.15, nudge_x=-0.5) +
  xlab("PC1") + ylab("PC2") 
pca_p



## track_std <- track |>
##   mutate_if(is.numeric, function(x) (x-
##       mean(x, na.rm=TRUE))/
##       sd(x, na.rm=TRUE))
## track_std_pca <- prcomp(track_std[,1:7],
##                scale = FALSE,
##                retx=TRUE)
## track_model <- pca_model(track_std_pca, d=2, s=2)
## track_all <- rbind(track_model$points, track_std[,1:7])
## animate_xy(track_all, edges=track_model$edges,
##            edges.col="#E7950F",
##            edges.width=3,
##            axes="off")
## render_gif(track_all,
##            grand_tour(),
##            display_xy(
##                       edges=track_model$edges,
##                       edges.col="#E7950F",
##                       edges.width=3,
##                       axes="off"),
##            gif_file="gifs/track_model.gif",
##            frames=500,
##            width=400,
##            height=400,
##            loop=FALSE)


#| label: penguins-umap
#| message: false
#| echo: false
library(uwot)
p_tidy_std <- p_tidy |> 
  na.omit() |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))

set.seed(253)
p_tidy_umap <- umap(p_tidy_std[,2:5], init = "spca")
p_tidy_umap_df <- p_tidy_umap |>
  as_tibble() |>
  rename(UMAP1 = V1, UMAP2 = V2) 
ggplot(p_tidy_umap_df, aes(x = UMAP1, 
                           y = UMAP2)) +
  geom_point(colour = "#EC5C00") 



## library(uwot)
## set.seed(253)
## p_tidy_umap <- umap(p_tidy_std[,2:5], init = "spca")


#| label: track-umap
#| message: false
#| echo: false
track_std <- track |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))
  
set.seed(305)
track_umap <- umap(track_std[,1:7], init = "spca")
track_umap_df <- track_umap |>
  as_tibble() |>
  rename(UMAP1 = V1, UMAP2 = V2) |>
  mutate(country = track$country)
p_track <- ggplot(track_umap_df, 
                  aes(x = UMAP1, 
                      y = UMAP2, 
                      label = country)) +
  geom_point(colour = "#EC5C00") 
ggplotly(p_track, width=500, height=500)

