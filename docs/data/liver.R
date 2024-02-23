# data for Annalisa on liver function
library(mvtnorm)
library(tidyverse)

liver_stats <- tribble(
  ~ALT, ~AST, ~ALP, ~Albumin, ~Bilirubin, ~GGT, ~LD, ~PT, ~sex,
  29,     14,   44,      3.5,        0.1,    5, 140,   0,   "m",
  33,     20,  147,      5.5,        1.2,   40, 280, 1.1,   "m",
  19,     10,   44,      3.5,        0.1,    5, 140,   0,   "f",
  25,     36,  147,      5.5,        1.2,   40, 280, 1.1,   "f",
)

# ALT, ALP IU/L
# AST U/L
# Albumin g/dL
# Bilirubin mg/dL
# GGT, LD U/L

set.seed(1257)
vc_m <- diag(1, 8, 8)
vc_m[upper.tri(vc_m) == TRUE] <- sample(c(0.3, 0.4, 0.5), 28, replace=T)
for (i in 1:7)
  for (j in (i+1):8)
    vc_m[j, i] <- vc_m[i, j]

vc_f <- diag(1, 8, 8)
vc_f[upper.tri(vc_f) == TRUE] <- sample(c(0.3, 0.4, 0.5), 28, replace=T)
for (i in 1:7)
  for (j in (i+1):8)
    vc_f[j, i] <- vc_f[i, j]

liver_f <- rmvnorm(434, mean=rep(0, 8), sigma=vc_f)
liver_stats_f <- as.matrix(liver_stats[,-9])[3:4,]
for (i in 1:8) {
  s <- (liver_stats[2, i]-
          liver_stats[1, i])/2
  m <- (liver_stats[2, i]+liver_stats[1, i])/2
  liver_f[,i] <- liver_f[,i]*s[1, 1]+m[1,1]
}

colnames(liver_f) <- colnames(liver_stats)[-9]
write_csv(as.data.frame(liver_f), file="data/liver_f.csv")
liver_f_std <- apply(liver_f, 2, function(x) (x-mean(x))/sd(x))
write_csv(as.data.frame(liver_f_std), file="data/liver_f_std.csv")

colnames(vc_m) <- colnames(liver_stats)[-9]
write_csv(as.data.frame(vc_m), file="data/liver_f_vc.csv")
