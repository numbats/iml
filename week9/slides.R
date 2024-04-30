## ----include = FALSE-------------------------------------
source("../setup.R")


## --------------------------------------------------------
#| message: false
#| echo: false
#| out-width: 100%
#| fig-width: 6
#| fig-height: 6

stdd <- function(x) (x-mean(x))/sd(x)
set.seed(20230513)
d_sep_cl <- matrix(rnorm(99*2), ncol=2)
d_sep_cl[1:33,1] <-
  d_sep_cl[1:33,1]+8
d_sep_cl[34:66,2] <-
  d_sep_cl[34:66,2]+8
d_sep_cl[34:66,1] <-
  d_sep_cl[34:66,1]+4
d_sep_cl <- data.frame(x1=stdd(d_sep_cl[,1]), 
                       x2=stdd(d_sep_cl[,2]))

x <- (runif(20)-0.5)*4
y <- x
d_nuis_pts <- data.frame(x1 = stdd(c(rnorm(50, -3), 
                            rnorm(50, 3), x)),
                 x2 = stdd(c(rnorm(50, -3), 
                            rnorm(50, 3), y)))

d_nuis_vars <- matrix(rnorm(99*2), ncol=2)
d_nuis_vars[1:49,1] <- d_nuis_vars[1:49,1]+8
d_nuis_vars <-
  data.frame(x1=stdd(d_nuis_vars[,1]),
             x2=stdd(d_nuis_vars[,2]))

d_odd_shapes <- matrix(rnorm(99*2),ncol=2)
d_odd_shapes[1:66,2] <- (d_odd_shapes[1:66,1])^2-5 + rnorm(66)*0.6
d_odd_shapes[1:66,1] <- d_odd_shapes[1:66,1]*3
d_odd_shapes <-
  data.frame(x1=stdd(d_odd_shapes[,1]),
             x2=stdd(d_odd_shapes[,2]))

p1 <- ggplot(d_sep_cl, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="a") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
p2 <- ggplot(d_nuis_pts, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="b") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
p3 <- ggplot(d_nuis_vars, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="c") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
p4 <- ggplot(d_odd_shapes, aes(x=x1, y=x2)) + 
  geom_point(colour="#3B99B1", alpha=0.7) + 
  annotate("text", -2.5, 2.5, label="d") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
    theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
print(p1 + p2 + p3 + p4 + plot_layout(ncol=2))



## --------------------------------------------------------
#| message: false
#| echo: false
#| out-width: 70%
#| fig-width: 6
#| fig-height: 6
set.seed(515)
km1 <- kmeans(d_sep_cl, 3)
d_sep_cl <- d_sep_cl |> 
  mutate(cl = factor(km1$cluster))

km2 <- kmeans(d_nuis_pts, 2)
d_nuis_pts <- d_nuis_pts |> 
  mutate(cl = factor(km2$cluster))

km3 <- kmeans(d_nuis_vars, 2)
d_nuis_vars <- d_nuis_vars |> 
  mutate(cl = factor(km3$cluster))

km4 <- kmeans(d_odd_shapes, 2)
d_odd_shapes <- d_odd_shapes |> 
  mutate(cl = factor(km4$cluster))

kp1 <- ggplot(d_sep_cl, aes(x=x1, y=x2, colour=cl)) + 
  geom_point(alpha=0.7) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  annotate("text", -2.5, 2.5, label="a") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
kp2 <- ggplot(d_nuis_pts, aes(x=x1, y=x2, colour=cl)) + 
  geom_point(alpha=0.7) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  annotate("text", -2.5, 2.5, label="b") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
kp3 <- ggplot(d_nuis_vars, aes(x=x1, y=x2, colour=cl)) + 
  geom_point(alpha=0.7) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  annotate("text", -2.5, 2.5, label="c") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
kp4 <- ggplot(d_odd_shapes, aes(x=x1, y=x2, colour=cl)) + 
  geom_point(alpha=0.7) + 
  scale_color_discrete_divergingx(palette="Zissou 1") +
  annotate("text", -2.5, 2.5, label="d") +
  xlim(-2.8, 2.8) + ylim(-2.8, 2.8) +
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
print(kp1 + kp2 + kp3 + kp4 + plot_layout(ncol=2))


## --------------------------------------------------------
#| echo: true
#| eval: false
## library(detourr)
## set.seed(645)
## detour(p_std[,1:4],
##        tour_aes(projection = bl:bm)) |>
##        tour_path(grand_tour(2), fps = 60,
##                  max_bases=40) |>
##        show_scatter(alpha = 0.7,
##                     axes = FALSE)


## --------------------------------------------------------
#| message: FALSE
#| warning: false
#| echo: false
#| fig-width: 4
#| fig-height: 4
#| out-width: 70%
x <- data.frame(V1 = c(7.3, 7.4, 4.1),
                    V2 = c(7.6, 7.2, 4.6),
                    V3 = c(7.7, 7.3, 4.6),
                    V4 = c(8.0, 7.2, 4.8),
                    point = factor(c("a1", "a2", "a3")))
x
pscat <- ggpairs(x, columns=1:4,
                 upper=list(continuous="points"),
                 diag=list(continuous="blankDiag"),
                 axisLabels="internal",
                 ggplot2::aes(colour=point)) +
    scale_colour_discrete_divergingx(
      palette = "Zissou 1", nmax=4) +
    xlim(3.7, 8.5) + ylim(3.7, 8.5) + 
    theme_minimal() +
    theme(aspect.ratio=1)
pscat


## --------------------------------------------------------
#| echo: false
set.seed(8) #6
df <- tibble(lbl=letters[1:12], 
             x1=sample(1:10, 12, replace=TRUE),
             x2=sample(1:10, 12, replace=TRUE))
df[1:4,2] <- df[1:4,2] + 12
df[5:8,3] <- df[5:8,3] + 12
kable(df) %>%
  kable_styling("striped", position = "center", 
                row_label_position = "c", 
                font_size=24) %>%
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:3, border_right=TRUE, width="2cm") 


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
ggplot(data=df, aes(x1, x2)) + geom_text(aes(label=lbl)) + 
  xlab("") + ylab("") + 
  xlim(c(1,22)) + ylim(c(1,22)) 


## --------------------------------------------------------
#| echo: false
# Initial means
xb <- data.frame(cl = factor(c(1, 2)), x1 = c(10,11), x2 = c(11, 9))


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
mn <- data.frame(cl=xb$cl, lbl=c("m1", "m2"), 
                 x1=xb$x1, x2=xb$x2)
ggplot(data=df, aes(x1, x2)) + geom_text(aes(label=lbl)) + 
  xlab("") + ylab("") + theme_bw() + 
  xlim(c(1,22)) + ylim(c(1,22)) +
  geom_point(data=mn, aes(x=x1, y=x2, color=cl), 
             shape=3, size=3) + 
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1, legend.position="none") 


## --------------------------------------------------------
#| echo: false
# Compute distances between each observation and each mean

d1 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[1,2:3])^2)),1))
d2 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[2,2:3])^2)),1))
df.km <- cbind(df, d1, d2)
df.km %>%
  kable("html", escape=F) %>%
  kable_styling("striped", position = "center", 
                font_size=24) %>% 
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:5, border_right=TRUE, width="1cm") %>%
  column_spec(4:5, color="#7570B3") 


## --------------------------------------------------------
#| echo: false
df.km$cl <- ifelse(d1 < d2, 1, 2)
df.km$cl <- factor(df.km$cl)
kable(df.km) %>%
  kable_styling("striped", position = "center", 
                font_size=24) %>% 
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:6, border_right=TRUE, width="1cm") %>%
  column_spec(4:6, color="#7570B3")



## --------------------------------------------------------
#| echo: false
xb <- df.km %>%
  group_by(cl) %>%
  summarise(x1=mean(x1), x2=mean(x2))
xb1 <- data.frame(x1=xb$x1[1], x2=xb$x2[1])
xb2 <- data.frame(x1=xb$x1[2], x2=xb$x2[2])


## --------------------------------------------------------
#| echo: false
d1 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[1,2:3])^2)),1))
d2 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[2,2:3])^2)),1))
df.km$d1 <- round(d1, 1)
df.km$d2 <- round(d2, 1)

df.km$cl <- ifelse(d1 < d2, 1, 2)
df.km$cl <- factor(df.km$cl)
kable(df.km) %>%
  kable_styling("striped", position = "center", 
                font_size=24) %>% 
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:6, border_right=TRUE, width="1cm") %>%
  column_spec(4:6, color="#7570B3")



## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
mn <- data.frame(cl=xb$cl, lbl=c("m1", "m2"), 
                 x1=xb$x1, x2=xb$x2)
ggplot() + 
  geom_text(data=df.km, aes(x=x1, y=x2, label=lbl, color=cl)) + 
  geom_point(data=mn, aes(x=x1, y=x2, color=cl), 
             shape=3, size=3) + 
  scale_colour_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() +
  xlim(c(1,22)) + ylim(c(1,22)) +
  theme(aspect.ratio=1, legend.position="None") 


## --------------------------------------------------------
#| echo: false
xb <- df.km %>%
  group_by(cl) %>%
  summarise(x1=mean(x1), x2=mean(x2))
xb1 <- data.frame(x1=xb$x1[1], x2=xb$x2[1])
xb2 <- data.frame(x1=xb$x1[2], x2=xb$x2[2])


## --------------------------------------------------------
#| echo: false
d1 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[1,2:3])^2)),1))
d2 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[2,2:3])^2)),1))
df.km$d1 <- round(d1, 1)
df.km$d2 <- round(d2, 1)

df.km$cl <- ifelse(d1 < d2, 1, 2)
df.km$cl <- factor(df.km$cl)
kable(df.km) %>%
  kable_styling("striped", position = "center", 
                font_size=24) %>% 
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:6, border_right=TRUE, width="1cm") %>%
  column_spec(4:6, color="#7570B3")



## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
mn <- data.frame(cl=xb$cl, lbl=c("m1", "m2"), 
                 x1=xb$x1, x2=xb$x2)
ggplot() + 
  geom_text(data=df.km, aes(x=x1, y=x2, label=lbl, color=cl)) + 
  geom_point(data=mn, aes(x=x1, y=x2, color=cl), 
             shape=3, size=3) + 
  scale_colour_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() +
  xlim(c(1,22)) + ylim(c(1,22)) +
  theme(aspect.ratio=1, legend.position="None") 


## --------------------------------------------------------
#| echo: false
xb <- df.km %>%
  group_by(cl) %>%
  summarise(x1=mean(x1), x2=mean(x2))
xb1 <- data.frame(x1=xb$x1[1], x2=xb$x2[1])
xb2 <- data.frame(x1=xb$x1[2], x2=xb$x2[2])


## --------------------------------------------------------
#| echo: false
d1 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[1,2:3])^2)),1))
d2 <- apply(df[,2:3], 1, function(x) round(sqrt(sum((x-xb[2,2:3])^2)),1))
df.km$d1 <- round(d1, 1)
df.km$d2 <- round(d2, 1)

df.km$cl <- ifelse(d1 < d2, 1, 2)
df.km$cl <- factor(df.km$cl)
kable(df.km) %>%
  kable_styling("striped", position = "center", 
                font_size=24) %>% 
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1:6, border_right=TRUE, width="1cm") %>%
  column_spec(4:6, color="#7570B3")



## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
mn <- data.frame(cl=xb$cl, lbl=c("m1", "m2"), 
                 x1=xb$x1, x2=xb$x2)
ggplot() + 
  geom_text(data=df.km, aes(x=x1, y=x2, label=lbl, color=cl)) + 
  geom_point(data=mn, aes(x=x1, y=x2, color=cl), 
             shape=3, size=3) + 
  scale_colour_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() +
  xlim(c(1,22)) + ylim(c(1,22)) +
  theme(aspect.ratio=1, legend.position="None") 


## --------------------------------------------------------
set.seed(712)
p_km3 <- kmeans(p_std[,2:5], 3)
p_std_km <- p_std |>
  mutate(cl = factor(p_km3$cluster))


## --------------------------------------------------------
#| echo: false
#| eval: false
## animate_xy(p_std_km[,2:5], col=p_std_km$cl)
## render_gif(p_std_km[,2:5],
##            grand_tour(),
##            display_xy(col=p_std_km$cl),
##            width=400,
##            height=400,
##            frames=100,
##            gif_file = "../gifs/p_km.gif")


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 90%
set.seed(31)
f.km <- NULL; f.km.stats <- NULL
for (i in 2:10) {
  cl <- kmeans(p_std[,2:5], i, nstart=5)$cluster
  x <- cluster.stats(dist(p_std[,2:5]), cl)
  f.km <- cbind(f.km, cl)
  f.km.stats <- rbind(f.km.stats, 
                      c(x$within.cluster.ss, 
                        x$wb.ratio, x$ch,
                        x$pearsongamma, x$dunn, 
                        x$dunn2))
}
colnames(f.km.stats) <- c("within.cluster.ss",
                          "wb.ratio", "ch",
                          "pearsongamma", "dunn",
                          "dunn2")
f.km.stats <- data.frame(f.km.stats)
f.km.stats$cl <- 2:10
f.km.stats.m <- f.km.stats %>% 
  pivot_longer(`within.cluster.ss`:dunn2,
               names_to="stat", 
               values_to="value") |>
  mutate(stat = factor(stat,
                       levels=colnames(f.km.stats)))
ggplot(data=f.km.stats.m) + 
  geom_line(aes(x=cl, y=value)) + xlab("# clusters") + ylab("") +
  facet_wrap(~stat, ncol=3, scales = "free_y") + 
  theme_bw()


## --------------------------------------------------------
#| echo: false
d <- as.matrix(round(dist(df[,2:3], diag=TRUE, upper=TRUE),1))
colnames(d) <- df$lbl
rownames(d) <- df$lbl
kable(d) %>%
  kable_styling("striped", position = "center", 
                row_label_position = "c", 
                font_size=18) %>%
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1, color = "white", background = "#3F9F7A") %>%
  column_spec(1:12, border_right=TRUE, width="1cm") 


## ----out.width="60%", fig.width=4, fig.height=4----------
#| echo: false
ggplot(data=df, aes(x1, x2)) + geom_text(aes(label=lbl)) + 
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1) 


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
ggplot(data=df, aes(x1, x2)) + geom_text(aes(label=lbl)) + 
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1) 


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
df_hc <- hclust(as.dist(d), method="average")
ggdendrogram(df_hc, rotate = TRUE, size = 4)


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
df$cl11 <- factor(c(1,2,1,2,1,1,1,1,1,1,1,1))
ggplot(data=df, aes(x=x1, y=x2, colour=cl11)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") 


## ----out.width="100%", fig.width=4, fig.height=4---------
#| echo: false
ggdendrogram(df_hc, rotate = TRUE, size = 4)


## --------------------------------------------------------
#| echo: false
kable(d) %>%
  kable_styling("striped", position = "center", 
                row_label_position = "c", 
                font_size=20) %>%
  row_spec(0, color = "white", background = "#3F9F7A") %>%
  column_spec(1, color = "white", background = "#3F9F7A") %>%
  column_spec(1:12, border_right=TRUE, width="1cm") %>%
  column_spec(c(3,5), color="#D95F02") %>%
  row_spec(c(2,4), color="#D95F02") %>%
  column_spec(c(2,4), color="#7570B3") %>%
  row_spec(c(1,3), color="#7570B3")


## ----out.width="50%", fig.width=3, fig.height=3----------
#| echo: false
df$cl11 <- factor(c(3,2,3,2,1,1,1,1,1,1,1,1))
ggplot(data=df, aes(x=x1, y=x2, colour=cl11)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") 


## ----out.width="80%", fig.width=10, fig.height=6---------
#| echo: false
df_hc1 <- hclust(as.dist(d), method="single")
p1 <- ggdendrogram(df_hc1, rotate = TRUE, size = 4) + ggtitle("single")
df_hc2 <- hclust(as.dist(d), method="complete")
p2 <- ggdendrogram(df_hc2, rotate = TRUE, size = 4) + ggtitle("complete")
df_hc3 <- hclust(as.dist(d), method="average")
p3 <- ggdendrogram(df_hc3, rotate = TRUE, size = 4) + ggtitle("average")
df_hc4 <- hclust(as.dist(d), method="centroid")
p4 <- ggdendrogram(df_hc4, rotate = TRUE, size = 4) + ggtitle("centroid")
df_hc5 <- hclust(as.dist(d), method="ward.D2")
p5 <- ggdendrogram(df_hc5, rotate = TRUE, size = 4) + ggtitle("wards")
p6 <- ggplot(data=df, aes(x=x1, y=x2)) +
  geom_text(aes(label=lbl)) + 
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") 
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3)


## ----out.width="90%", fig.width=10, fig.height=6---------
#| echo: false
df$cl1 <- factor(cutree(df_hc1, 3))
df$cl2 <- factor(cutree(df_hc2, 3))
df$cl3 <- factor(cutree(df_hc3, 3))
df$cl4 <- factor(cutree(df_hc4, 3))
df$cl5 <- factor(cutree(df_hc5, 3))
p1 <- ggplot(data=df, aes(x=x1, y=x2, colour=cl1)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") + ggtitle("single")
p2 <- ggplot(data=df, aes(x=x1, y=x2, colour=cl2)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") + 
  ggtitle("complete")
p3 <- ggplot(data=df, aes(x=x1, y=x2, colour=cl3)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") + 
  ggtitle("average")
p4 <- ggplot(data=df, aes(x=x1, y=x2, colour=cl4)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") + 
  ggtitle("centroid")
p5 <- ggplot(data=df, aes(x=x1, y=x2, colour=cl5)) +
  geom_text(aes(label=lbl)) + 
  scale_color_brewer("", palette="Dark2") +
  xlab("") + ylab("") + theme_bw() + 
  theme(aspect.ratio=1, legend.position="none") + 
  ggtitle("wards")
grid.arrange(p1, p2, p3, p4, p5, ncol=3)


## --------------------------------------------------------
p_hc_w3 <- hclust(dist(p_std[,2:5]), method="ward.D2")
p_std_hc_w3 <- p_std |>
  mutate(cl = factor(cutree(p_hc_w3, 3)))


## --------------------------------------------------------
#| echo: false
#| eval: false
## animate_xy(p_std_hc_w3[,2:5], col=p_std_hc_w3$cl)
## render_gif(p_std_hc_w3[,2:5],
##            grand_tour(),
##            display_xy(col=p_std_hc_w3$cl),
##            width=400,
##            height=400,
##            frames=100,
##            gif_file = "../gifs/p_hc_w3.gif")


## --------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 6
#| out-width: 90%
f.hc <- NULL; f.hc.stats <- NULL
for (i in 2:10) {
  cl <- cutree(p_hc_w3, i)
  x <- cluster.stats(dist(p_std[,2:5]), cl)
  f.hc <- cbind(f.hc, cl)
  f.hc.stats <- rbind(f.hc.stats, 
                      c(x$within.cluster.ss, 
                        x$wb.ratio, x$ch,
                        x$pearsongamma, x$dunn, 
                        x$dunn2))
}
colnames(f.hc.stats) <- c("within.cluster.ss",
                          "wb.ratio", "ch",
                          "pearsongamma", "dunn",
                          "dunn2")
f.hc.stats <- data.frame(f.hc.stats)
f.hc.stats$cl <- 2:10
f.hc.stats.m <- f.hc.stats %>% 
  pivot_longer(`within.cluster.ss`:dunn2,
               names_to="stat", 
               values_to="value") |>
  mutate(stat = factor(stat,
                       levels=colnames(f.hc.stats)))
ggplot(data=f.hc.stats.m) + 
  geom_line(aes(x=cl, y=value)) + xlab("# clusters") + ylab("") +
  facet_wrap(~stat, ncol=3, scales = "free_y") + 
  theme_bw()

