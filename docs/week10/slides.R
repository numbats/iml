## ----include = FALSE-------------------------------
source("../setup.R")


## --------------------------------------------------
#| echo: false
#| out-width: 80%
set.seed(20190512)
df <- data.frame(x1=scale(c(rnorm(50, -4), rnorm(50, 4))),
                   x2=scale(c(rnorm(100))))
ggplot(data=df, aes(x1, x2)) + geom_point() +
    theme_bw() + theme(aspect.ratio=1)


## --------------------------------------------------
#| echo: true
df_mc <- Mclust(df, G = 2)
summary(df_mc)


## --------------------------------------------------
#| echo: true
#| out-width: 80%
#| fig-width: 4
plot(df_mc, what = "density")


## --------------------------------------------------
#| echo: true
#| out-width: 80%
#| fig-width: 4
plot(df_mc, what = "uncertainty")


## --------------------------------------------------
#| echo: true
options(digits=2)
df_mc$parameters$mean


## --------------------------------------------------
#| echo: true
df_mc$parameters$variance$sigma


## --------------------------------------------------
#| echo: false
#| out-width: 80%
set.seed(20190514)
x <- (runif(20)-0.5)*4
y <- x
df <- data.frame(x1=scale(c(rnorm(50, -3), rnorm(50, 3), x)),
                   x2=scale(c(rnorm(50, -3), rnorm(50, 3), y)))
ggplot(data=df, aes(x1, x2)) + geom_point() +
    theme_bw() + theme(aspect.ratio=1)


## --------------------------------------------------
#| echo: true
df_mc <- Mclust(df, G = 2)
summary(df_mc)


## --------------------------------------------------
#| echo: true
#| fig-width: 4
#| out-width: 80%
plot(df_mc, what = "density")


## --------------------------------------------------
#| echo: true
#| fig-width: 4
#| out-width: 80%
plot(df_mc, what = "uncertainty")


## --------------------------------------------------
#| echo: true
df_mc$parameters$mean


## --------------------------------------------------
#| echo: true
df_mc$parameters$variance$sigma


## ----echo=TRUE-------------------------------------
#| echo: true
p_mc <- mclustBIC(p_std[,2:5], G=2:8)
summary(p_mc)


## --------------------------------------------------
#| out-width: 80%
pmc1 <- ggmcbic(p_mc) +
  scale_x_continuous(breaks=1:7, labels=2:8)
ggplotly(pmc1)


## --------------------------------------------------
#| echo: false
#| out-width: 80%
pmc2 <- ggmcbic(p_mc, top=6) +
  scale_x_continuous(breaks=1:7, labels=2:8)
ggplotly(pmc2)



## --------------------------------------------------
#| code-fold: true
penguins_mc <- Mclust(p_std[,2:5], 
                      G=4, 
                      modelNames = "VEE")
penguins_mce <- mc_ellipse(penguins_mc)
penguins_cl <- p_std
penguins_cl$cl <- factor(penguins_mc$classification)

penguins_mc_data <- penguins_cl |>
  select(bl:bm, cl) |>
  mutate(type = "data") |>
  bind_rows(bind_cols(penguins_mce$ell,
                      type=rep("ellipse",
                               nrow(penguins_mce$ell)))) |>
  mutate(type = factor(type))


## --------------------------------------------------
#| eval: false
#| echo: false
## animate_xy(penguins_mc_data[,1:4],
##            col=penguins_mc_data$cl,
##            pch=c(4, 20 )[as.numeric(penguins_mc_data$type)],
##            axes="off")


## --------------------------------------------------
set.seed(947)
p_grid <- kohonen::somgrid(xdim = 5, ydim = 5,
                           topo = 'rectangular')
p_init <- somInit(as.matrix(p_std[,2:5]), 5, 5)
p_som <- som(as.matrix(p_std[,2:5]), 
             rlen=2000,
             grid = p_grid,
             init = p_init)


## --------------------------------------------------
plot(p_som, type="changes")


## --------------------------------------------------
#| echo: false
p_som_df_net <- som_model(p_som)
p_som_data <- p_som_df_net$data |> 
  mutate(species = p_std$species)
ggplot() +
  geom_segment(data=p_som_df_net$edges_s, 
               aes(x=x, xend=xend, y=y, 
                   yend=yend)) +
  geom_point(data=p_som_data, 
             aes(x=map1, y=map2, 
                 colour=species), 
             size=3, alpha=0.5) +
  xlab("map 1") + ylab("map 2") +
  scale_color_discrete_divergingx(
    palette="Zissou 1") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_blank())

# Set up data
p_som_map <- p_som_df_net$net |>
  mutate(species = "0", type="net")
p_som_data <- p_som_data |> 
  select(bl:bm, species) |>
  mutate(type="data", 
         species = as.character(species)) 
p_som_map_data <- bind_rows(p_som_map, p_som_data)
p_som_map_data$type <- factor(p_som_map_data$type,
  levels=c("net", "data"))
p_som_map_data$species <- factor(p_som_map_data$species,
  levels=c("0","Adelie","Chinstrap","Gentoo"))
p_pch <- c(46, 16)[as.numeric(p_som_map_data$type)]
p_col <- c("black", hcl.colors(3, "Zissou 1"))[as.numeric(p_som_map_data$species)]


## --------------------------------------------------
#| eval: false
#| echo: false
## animate_xy(p_som_map_data[,1:4],
##            col=p_col,
##            pch=p_pch,
##            edges=as.matrix(p_som_df_net$edges),
##            edges.col = "black",
##            axes="bottomleft")


## --------------------------------------------------
#| out-width: 80%
c3_som <- som(as.matrix(c3), 
  grid = kohonen::somgrid(5, 5, "hexagonal"))


## --------------------------------------------------
plot(c3_som, type="changes")


## --------------------------------------------------
#| echo: false
#| out-width: 80%
c3_som_df_net <- som_model(c3_som)
c3_som_data <- c3_som_df_net$data 
ggplot() +
  geom_segment(data=c3_som_df_net$edges_s, 
               aes(x=x, xend=xend, y=y, 
                   yend=yend)) +
  geom_point(data=c3_som_data, 
             aes(x=map1, y=map2), 
             size=3, alpha=0.5) +
  xlab("map 1") + ylab("map 2") +
  theme(axis.text = element_blank())


## --------------------------------------------------
#| echo: false
# Set up data
c3_som_map <- c3_som_df_net$net |>
  mutate(type="net")
c3_som_data <- c3_som_data |> 
  select(x1:x10) |>
  mutate(type="data") 
c3_som_map_data <- bind_rows(c3_som_map, c3_som_data)
c3_som_map_data$type <- factor(c3_som_map_data$type,
  levels=c("net", "data"))
c3_pch <- c(19, 16)[as.numeric(c3_som_map_data$type)]


## --------------------------------------------------
#| eval: false
#| echo: false
## animate_xy(c3_som_map_data[,1:3],
##            pch=c3_pch,
##            col=c3_som_map_data$type,
##            edges=as.matrix(c3_som_df_net$edges),
##            edges.col = "#3B99B1",
##            axes="bottomleft")
## render_gif(c3_som_map_data[,1:3],
##            grand_tour(),
##            display_xy(pch=c3_pch,
##            col=c3_som_map_data$type,
##            edges=as.matrix(c3_som_df_net$edges),
##            edges.col = "#3B99B1",
##            axes="bottomleft"),
##            width=400,
##            height=400,
##            frames=300,
##            gif_file = "../gifs/c3_som.gif")

