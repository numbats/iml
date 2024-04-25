# Load libraries used everywhere
library(tidyverse)
library(tidymodels)
library(conflicted)
library(colorspace)
library(patchwork)
library(MASS)
library(rpart)
library(randomForest)
library(gridExtra)
library(GGally)
library(geozoo)
library(mulgar)
library(mvtnorm)
library(palmerpenguins)
library(rpart.plot)
library(discrim)
library(tourr)
library(classifly)
library(ggthemes)
library(xgboost)
library(uwot)
library(Rtsne)
library(plotly)
library(keras)
library(forcats)
library(ggbeeswarm)
library(DALEXtra)
library(kernelshap)
library(shapviz)
library(lime)
# devtools::install_github("dandls/counterfactuals")
library(iml)
library(counterfactuals)
library(kernlab)

# Locations
current_file <- knitr::current_input()
basename <- gsub(".[Rq]md$", "", current_file)

# Set up chunk for all slides
knitr::opts_chunk$set(
  fig.path = sprintf("images/%s/", basename),
  fig.width = 6,
  fig.height = 4,
  fig.align = "center",
  out.width = "100%",
  code.line.numbers = FALSE,
  fig.retina = 4,
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  cache = FALSE,
  dev.args = list(pointsize = 11)
)
options(
  digits = 2,
  width = 60,
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2", "#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)
theme_set(theme_bw(base_size = 14) +
   theme(
     aspect.ratio = 1,
     plot.background = element_rect(fill = 'transparent', colour = NA),
     plot.title.position = "plot",
     plot.title = element_text(size = 24),
     panel.background = element_rect(fill = 'transparent', colour = NA),
     legend.background = element_rect(fill = 'transparent', colour = NA),
     legend.key = element_rect(fill = 'transparent', colour = NA)
   )
)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
conflicts_prefer(dplyr::slice)
conflicts_prefer(palmerpenguins::penguins)
conflicts_prefer(tourr::flea)
conflicts_prefer(viridis::viridis_pal)
conflicts_prefer(latex2exp::TeX)
conflicts_prefer(geozoo::simplex)

p_tidy <- penguins |>
  select(species, bill_length_mm:body_mass_g) |>
  rename(bl=bill_length_mm,
         bd=bill_depth_mm,
         fl=flipper_length_mm,
         bm=body_mass_g) |>
  na.omit()

p_std <- p_tidy |>
  mutate_if(is.numeric, function(x) (x-mean(x))/sd(x)) 

