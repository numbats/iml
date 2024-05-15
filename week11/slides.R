## ----include = FALSE-----------------------------------------
source("../setup.R")


## ------------------------------------------------------------
#| echo: false
set.seed(20190512)
df <- data.frame(x1=scale(c(rnorm(50, -4), rnorm(50, 4))),
                   x2=scale(c(rnorm(100))))
df_hc1 <- hclust(dist(df), method="complete")
df_hc2 <- hclust(dist(df), method="ward.D2")
pd1 <- ggdendrogram(df_hc1) + 
  theme(axis.text = element_text(size=0),
        panel.border = element_blank()) +
  ggtitle("Method 1")
pd2 <- ggdendrogram(df_hc2) + 
  theme(axis.text = element_text(size=0),
        panel.border = element_blank()) +
  ggtitle("Method 2")
pd1 + pd2 + plot_layout(ncol=2)


## ------------------------------------------------------------
#| echo: false
df_cl <- df |>
  mutate(cl1 = factor(cutree(df_hc1, 2)),
         cl2 = factor(cutree(df_hc2, 2)))
df_cl |> count(cl1, cl2) |>
  group_by(cl1) |>
  pivot_wider(names_from = cl2, values_from = n) |>
  ungroup()


## ------------------------------------------------------------
#| echo: false
df_cl |> count(cl1, cl2) |>
  arrange(desc(cl1)) |>
  group_by(cl1) |>
  pivot_wider(names_from = cl2, values_from = n)


## ------------------------------------------------------------
#| echo: false
pd3 <- ggplot(df_cl, aes(x1, x2, colour=cl1)) + 
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none")
pd4 <- ggplot(df_cl, aes(x1, x2, colour=cl2)) + 
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none")
pd3 + pd4 + plot_layout(ncol=2)


## ------------------------------------------------------------
#| echo: false
#| fig-width: 10
#| fig-height: 6
p_hc1 <- hclust(dist(p_std[,2:5]), method="average")
p_hc2 <- hclust(dist(p_std[,2:5]), method="ward.D2")
pd5 <- ggdendrogram(p_hc1) + 
  theme(axis.text = element_text(size=0),
        panel.border = element_blank()) +
  ggtitle("Average linkage")
pd6 <- ggdendrogram(p_hc2) + 
  theme(axis.text = element_text(size=0),
        panel.border = element_blank()) +
  ggtitle("Wards linkage")
pd5 + pd6 + plot_layout(ncol=2)
p_std_cl <- p_std |>
  mutate(
    cl1 = factor(cutree(p_hc1, 5), levels=1:5),
    cl2 = factor(cutree(p_hc2, 5), levels=1:5))


## ------------------------------------------------------------
#| echo: false
p_std_cl |> count(cl1, cl2) |>
  arrange(cl1, cl2) |>
  group_by(cl1) |>
  pivot_wider(names_from = cl2, 
              values_from = n, 
              values_fill = 0) |>
  ungroup()


## ------------------------------------------------------------
#| echo: false
w_b <- simple_clusters[c(1:5, 74:78), ]
w_b_edges <- tibble(from = c(1, 2, 3, 4, 1, 2, 3, 1, 2, 1),
                    to =   c(2, 3, 4, 5, 3, 4, 5, 4, 5, 5))
w_b_edges <- bind_rows(w_b_edges,
              tibble(from = w_b_edges$from + 5,
                     to = w_b_edges$to + 5)       
                       )
w_b_segments <- tibble(x = w_b$x1[w_b_edges$from],
                       xend = w_b$x1[w_b_edges$to],
                       y = w_b$x2[w_b_edges$from],
                       yend = w_b$x2[w_b_edges$to])
ggplot() +
  geom_point(data=w_b, aes(x1, x2)) +
  geom_segment(data=w_b_segments, aes(x=x, xend=xend,
                                      y=y, yend=yend),
               colour="#3B99B1") + 
  geom_text(aes(x=-0.8, y=0, label="WITHIN"),
            colour="#3B99B1") +
  geom_text(aes(x=0.8, y=0.8, label="WITHIN"),
            colour="#3B99B1") +
  geom_segment(aes(x=-0.7, xend=0.7, y=-0.8, yend=1.2),
               colour="#F5191C") +
  geom_text(aes(x=0.2, y=0, label="BETWEEN"),
            colour="#F5191C") +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
  


## ------------------------------------------------------------
#| message: false
#| echo: false
#| out-width: 80%
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
p1 + p2 + p3 + p4 + plot_layout(ncol=2)



## ------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 3
set.seed(914)
blob1 <- rmvnorm(n=155, mean=c(0,0), sigma=matrix(c(1, 0, 0, 1), ncol=2, byrow=TRUE)) |> as.tibble()
blob2 <- rmvnorm(n=155, mean=c(0,0), sigma=matrix(c(1, 0.6, 0.6, 1), ncol=2, byrow=TRUE)) |> as.tibble()
blob3 <- rmvnorm(n=155, mean=c(0,0), sigma=matrix(c(1, 0.9, 0.9, 1), ncol=2, byrow=TRUE)) |> as.tibble()
b1 <- ggplot(blob1, aes(V1, V2)) + 
  geom_point() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
b2 <- ggplot(blob2, aes(V1, V2)) + 
  geom_point() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
b3 <- ggplot(blob3, aes(V1, V2)) + 
  geom_point() +
  theme(axis.text = element_blank(),
        axis.title = element_blank())
b1 + b2 + b3 + plot_layout(ncol=3)


## ------------------------------------------------------------
#| echo: false
#| fig-width: 9
#| fig-height: 3
set.seed(855)
b1_km <- kmeans(blob1, 4)
b2_km <- kmeans(blob2, 4)
b3_km <- kmeans(blob3, 4)
blob1_cl <- blob1 |>
  mutate(cl = factor(b1_km$cluster))
blob2_cl <- blob2 |>
  mutate(cl = factor(b2_km$cluster))
blob3_cl <- blob3 |>
  mutate(cl = factor(b3_km$cluster))
b4 <- ggplot(blob1_cl, aes(V1, V2, colour=cl)) + 
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.title = element_blank())
b5 <- ggplot(blob2_cl, aes(V1, V2, colour=cl)) + 
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.title = element_blank())
b6 <- ggplot(blob3_cl, aes(V1, V2, colour=cl)) + 
  geom_point() +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  theme(legend.position = "none", 
        axis.text = element_blank(),
        axis.title = element_blank())
b4 + b5 + b6 + plot_layout(ncol=3)


## ------------------------------------------------------------
#| echo: false
p_std_cl <- p_std |>
  select(-species) |>
  mutate(cl = factor(cutree(p_hc2, 3)))
p_std_cl_m <- p_std_cl |>
  group_by(cl) |>
  dplyr::summarise_at(vars(bl:bm), mean)
p_std_cl_s <- p_std_cl |>
  group_by(cl) |>
  dplyr::summarise_at(vars(bl:bm), sd)
p_std_cl_n <- p_std_cl |> count(cl)
# Organise into a nice table
p_std_cl_m <- p_std_cl_m |>
  pivot_longer(bl:bm, names_to="var", values_to="mean") |>
  pivot_wider(names_from=cl, values_from=mean) |>
  rename(m1 = `1`, m2 = `2`, m3 = `3`)
p_std_cl_s <- p_std_cl_s |>
  pivot_longer(bl:bm, names_to="var", values_to="sd") |>
  pivot_wider(names_from=cl, values_from=sd) |>
  rename(s1 = `1`, s2 = `2`, s3 = `3`)
p_std_cl_n <- p_std_cl_n |>
  pivot_wider(names_from=cl, values_from=n) 
p_std_cl_smry <- bind_cols(p_std_cl_m, p_std_cl_s[,-1]) |>
  pivot_longer(m1:s3, names_to = "stat", 
               values_to = "value") |>
  mutate(cl = str_sub(stat, 2, 2),
         stat = str_sub(stat, 1, 1)) |>
  pivot_wider(names_from=cl, values_from=value) |>
  mutate(stat = ifelse(stat == "m", "mean", "sd"))
p_std_cl_smry <- bind_rows(p_std_cl_smry, 
   tibble(var="", stat="n", p_std_cl_n))
p_std_cl_smry$var[c(2,4,6,8)] <- ""
kable(p_std_cl_smry, 
      col.names=c("Var", "Stat", "1", "2", "3"),
      digits = 2) |>
  kable_minimal() |>
  add_header_above(c(" " = 2, "Clusters" = 3)) |>
  row_spec(0, hline_after = TRUE) |>
  row_spec(1, extra_css = "border-bottom-style: none") |>
  row_spec(3, extra_css = "border-bottom-style: none") |>
  row_spec(5, extra_css = "border-bottom-style: none") |>
  row_spec(7, extra_css = "border-bottom-style: none") |>
  row_spec(2, italic=TRUE, color="#a7a7a7", extra_css = "") |>
  row_spec(4, italic=TRUE, color="#a7a7a7", extra_css = "") |>
  row_spec(6, italic=TRUE, color="#a7a7a7", extra_css = "") |>
  row_spec(8, italic=TRUE, color="#a7a7a7", extra_css = "")  


## ------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 4
p_std_cl_m <- p_std_cl_m |>
  pivot_longer(m1:m3, names_to = "cluster", 
               values_to = "mean") |>
  mutate(cluster = factor(as.numeric(str_sub(cluster, 2, 2))))
p_std_cl_s <- p_std_cl_s |>
  pivot_longer(s1:s3, names_to = "cluster", 
               values_to = "sd") |>
  mutate(cluster = factor(as.numeric(str_sub(cluster, 2, 2))))
p_std_cl_pcp <- bind_cols(p_std_cl_m, p_std_cl_s[,3]) |>
  mutate(var = factor(var))
ggplot(p_std_cl_pcp) +
  geom_ribbon(aes(x=as.numeric(var), 
                  ymin=mean-sd, 
                  ymax=mean+sd, 
                  fill=cluster), alpha=0.5) +
  geom_line(aes(x=as.numeric(var), 
                y=mean, 
                colour=cluster)) +
  scale_x_continuous("", labels=c("bl", "bd", "fl", "bm")) +
  ylab("") +
  scale_color_discrete_divergingx(palette="Zissou 1") +
  scale_fill_discrete_divergingx(palette="Zissou 1") +
  theme(aspect.ratio=0.7)


## ------------------------------------------------------------
#| echo: false
#| fig-width: 8
#| fig-height: 8
#| out-width: 80%
ggscatmat(p_std_cl, columns=1:4, color = "cl") +
  scale_color_discrete_divergingx(palette="Zissou 1")


## ------------------------------------------------------------
#| echo: false
#| eval: false
## render_gif(p_std_cl[,1:4],
##            grand_tour(),
##            display_xy(col=p_std_cl$cl),
##            gif_file = "../gifs/p_clusters.gif",
##            width = 400,
##            height = 400,
##            frames = 300)


## ------------------------------------------------------------
#| echo: false
#| out-width: 70%
#| fig-width: 6
#| fig-height: 6
data(simple_clusters)

# Compute hierarchical clustering with Ward's linkage
cl_hw <- hclust(dist(simple_clusters[,1:2]),
                method="ward.D2")
cl_ggd <- dendro_data(cl_hw, type = "triangle")

# Compute dendrogram in the data
cl_hfly <- hierfly(simple_clusters, cl_hw, scale=FALSE)

# Show result
simple_clusters <- simple_clusters |>
  mutate(clw = factor(cutree(cl_hw, 2)))

# Plot the data
pd <- ggplot(simple_clusters, aes(x=x1, y=x2)) +
  geom_point(colour="#3B99B1", size=2, alpha=0.8) +
  ggtitle("Data") + 
  theme_minimal() +
  theme(aspect.ratio=1) 

# Plot the dendrogram
ph <- ggplot() +
  geom_segment(data=cl_ggd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=cl_ggd$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("Dendrogram") + 
  theme_minimal() +
  theme_dendro()

# Plot the dendrogram on the data
pdh <- ggplot() +
  geom_segment(data=cl_hfly$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=cl_hfly$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  xlab("x1") + ylab("x2") +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  ggtitle("Dendrogram on data") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Plot the resulting clusters
pc <- ggplot(simple_clusters) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=5, rev=TRUE) +
  ggtitle("Result") + 
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

pd + ph + pdh + pc + plot_layout(ncol=2)


## ------------------------------------------------------------
#| echo: false
# Nuisance observations
set.seed(20190514)
x <- (runif(20)-0.5)*4
y <- x
d1 <- data.frame(x1 = c(rnorm(50, -3), 
                            rnorm(50, 3), x),
                 x2 = c(rnorm(50, -3), 
                            rnorm(50, 3), y),
                 cl = factor(c(rep("A", 50), 
                             rep("B", 70))))
d1 <- d1 |> 
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x))

# Compute single linkage
d1_hs <- hclust(dist(d1[,1:2]),
                method="single")
d1_ggds <- dendro_data(d1_hs, type = "triangle")
pd1s <- ggplot() +
  geom_segment(data=d1_ggds$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d1_ggds$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  theme_minimal() +
  ggtitle("Single linkage") +
  theme_dendro()

# Compute dendrogram in data
d1_hflys <- hierfly(d1, d1_hs, scale=FALSE)

pd1hs <- ggplot() +
  geom_segment(data=d1_hflys$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d1_hflys$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d1 <- d1 |>
  mutate(cls = factor(cutree(d1_hs, 2)))
pc_d1s <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=cls), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Compute Wards linkage
d1_hw <- hclust(dist(d1[,1:2]),
                method="ward.D2")
d1_ggdw <- dendro_data(d1_hw, type = "triangle")
pd1w <- ggplot() +
  geom_segment(data=d1_ggdw$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=d1_ggdw$labels, aes(x=x, y=y),
             colour="#3B99B1", alpha=0.8) +
  ggtitle("Ward's linkage") +
  theme_minimal() +
  theme_dendro()

# Compute dendrogram in data
d1_hflyw <- hierfly(d1, d1_hw, scale=FALSE)

pd1hw <- ggplot() +
  geom_segment(data=d1_hflyw$segments, 
                aes(x=x, xend=xend,
                    y=y, yend=yend)) +
  geom_point(data=d1_hflyw$data, 
             aes(x=x1, y=x2,
                 shape=factor(node),
                 colour=factor(node),
                 size=1-node), alpha=0.8) +
  scale_shape_manual(values = c(16, 3)) +
  scale_colour_manual(values = c("#3B99B1", "black")) +
  scale_size(limits=c(0,17)) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

# Show result
d1 <- d1 |>
  mutate(clw = factor(cutree(d1_hw, 2)))
pc_d1w <- ggplot(d1) +
  geom_point(aes(x=x1, y=x2, colour=clw), 
             size=2, alpha=0.8) +
  scale_colour_discrete_divergingx(palette = "Zissou 1",
                                   nmax=4, rev=TRUE) +
  theme_minimal() +
  theme(aspect.ratio=1, legend.position="none")

pd1s + pd1hs + pc_d1s + 
  pd1w + pd1hw + pc_d1w +
  plot_layout(ncol=3)


## ------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 12
#| out-width: 80%
p_dist <- dist(p_std[,2:5])
p_hcw <- hclust(p_dist, method="ward.D2")
p_hcs <- hclust(p_dist, method="single")

p_clw <- p_std |> 
  mutate(cl = factor(cutree(p_hcw, 3))) |>
  as.data.frame()
p_cls <- p_std |> 
  mutate(cl = factor(cutree(p_hcs, 3))) |>
  as.data.frame()

p_w_hfly <- hierfly(p_clw, p_hcw, scale=FALSE)
p_s_hfly <- hierfly(p_cls, p_hcs, scale=FALSE)
# Generate the dendrograms in 2D
p_hcw_dd <- dendro_data(p_hcw)
pw_dd <- ggplot() +
  geom_segment(data=p_hcw_dd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=p_hcw_dd$labels, aes(x=x, y=y),
             alpha=0.8) +
  ggtitle("Ward's linkage") +
  theme_dendro()

p_hcs_dd <- dendro_data(p_hcs)
ps_dd <- ggplot() +
  geom_segment(data=p_hcs_dd$segments, 
               aes(x = x, y = y, 
                   xend = xend, yend = yend)) + 
  geom_point(data=p_hcs_dd$labels, aes(x=x, y=y),
             alpha=0.8) +
  ggtitle("Single linkage") +
  theme_dendro()
ps_dd + pw_dd + plot_layout(ncol=1)


## ------------------------------------------------------------
#| echo: false
p_dist <- dist(p_std[,2:5])
p_hcw <- hclust(p_dist, method="ward.D2")

p_cl <- data.frame(cl_w = cutree(p_hcw, 3))


# Arranging by cluster id is important to define edges 
penguins_cl <- p_std |>
  mutate(cl_w = p_cl$cl_w) |>
  arrange(cl_w)


## ------------------------------------------------------------
#| echo: false
# Penguins in 2D
# Duplicate observations need to be removed for convex hull calculation
psub <- penguins_cl |>
  select(bl, bd) 
dup <- duplicated(psub)
psub <- penguins_cl |>
  select(bl, bd, cl_w) |>
  filter(!dup) |>
  arrange(cl_w)

ncl <- psub |>
  count(cl_w) |>
  arrange(cl_w) |>
  mutate(cumn = cumsum(n))
phull <- NULL
for (i in unique(psub$cl_w)) {
  x <- psub |>
    dplyr::filter(cl_w == i) |>
    select(bl, bd) 
  ph <- cxhull(as.matrix(x))$edges
  if (i > 1) {
    ph <- ph + ncl$cumn[i-1]
  }
  ph <- cbind(ph, rep(i, nrow(ph)))
  phull <- rbind(phull, ph)
}
phull <- as.data.frame(phull)
colnames(phull) <- c("from", "to", "cl_w") 
phull_segs <- data.frame(x = psub$bl[phull$from],
                         y = psub$bd[phull$from],
                         xend = psub$bl[phull$to],
                         yend = psub$bd[phull$to],
                         cl_w = phull$cl_w)
phull_segs$cl_w <- factor(phull$cl_w) 
psub$cl_w <- factor(psub$cl_w)
ggplot() +
  geom_point(data=psub, aes(x=bl, y=bd, 
                            colour=cl_w)) + 
  geom_segment(data=phull_segs, aes(x=x, xend=xend,
                                    y=y, yend=yend,
                                    colour=cl_w)) +
  scale_colour_discrete_divergingx(palette = "Zissou 1") +
  theme_minimal() +
  theme(aspect.ratio = 1)



## ------------------------------------------------------------
#| echo: false
#| eval: false
## ncl <- penguins_cl |>
##   count(cl_w) |>
##   arrange(cl_w) |>
##   mutate(cumn = cumsum(n))
## phull <- NULL
## for (i in unique(penguins_cl$cl_w)) {
##   x <- penguins_cl |>
##     dplyr::filter(cl_w == i)
##   ph <- cxhull(as.matrix(x[,2:5]))$edges
##   if (i > 1) {
##     ph <- ph + ncl$cumn[i-1]
##   }
##   ph <- cbind(ph, rep(i, nrow(ph)))
##   phull <- rbind(phull, ph)
## }
## phull <- as.data.frame(phull)
## colnames(phull) <- c("from", "to", "cl_w")
## phull$cl_w <- factor(phull$cl_w)
## penguins_cl$cl_w <- factor(penguins_cl$cl_w)
## 
## animate_xy(penguins_cl[,2:5], col=penguins_cl$cl_w,
##            edges=as.matrix(phull[,1:2]), edges.col=phull$cl_w)
## render_gif(penguins_cl[,2:5],
##            tour_path = grand_tour(),
##            display = display_xy(col=penguins_cl$cl_w,
##                                 edges=as.matrix(phull[,1:2]),
##                                 edges.col=phull$cl_w),
##            gif_file = "gifs/penguins_chull.gif",
##            frames = 500,
##            width = 400,
##            height = 400)


## ------------------------------------------------------------
#| echo: false
options(digits=2, width=100)
c1_pca <- prcomp(c1, scale = FALSE)
summary(c1_pca)


## ------------------------------------------------------------
#| echo: false
#| fig-width: 6
#| fig-height: 6
#| out-width: 70%
c1_pca_df <- as_tibble(c1_pca$x)
ggscatmat(c1_pca_df) + 
  theme(axis.text = element_blank())

