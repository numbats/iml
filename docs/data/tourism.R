library(tsibble)
library(tidyverse)
library(lubridate)
library(ggbeeswarm)

# Data downloaded from Monash A-Z Databases
# "Tourism Research Australia online for students"
# Row: Quarter/SA2
# Column: Stopover reason was Holiday, Visiting friends and relatives, Business, Other reason
# Sum Overnight trips ('000)
# Australia: Domestic Overnight Trips ('000) ----
domestic_trips <- read_csv(
  "data/domestic_trips_2023-10-08.csv",
  skip = 9,
  col_names = c("Quarter", "Region", "Holiday", "Visiting", "Business", "Other", "Total"),
  n_max = 248056
) %>% select(-X8)

# fill NA in "Quarter" using the last obs
fill_na <- domestic_trips %>%
  fill(Quarter, .direction = "down") %>%
  filter(Quarter != "Total")

# gather Stopover purpose of visit
long_data <- fill_na %>%
  pivot_longer(cols=Holiday:Total,
               names_to="Purpose",
               values_to="Trips")

# manipulate Quarter
qtr_data <- long_data %>%
  mutate(
    Quarter = paste(gsub(" quarter", "", Quarter), "01"),
    Quarter = yearquarter(myd(Quarter))
  )

# convert to tsibble
tourism <- qtr_data %>%
  as_tsibble(key = c(Region, Purpose), index = Quarter)
usethis::use_data(tourism, overwrite = TRUE, compress = "xz")

saveRDS(tourism, file="data/tourism.rds")

# Standarise the counts
tourism_max <- tourism |>
  as_tibble() |>
  group_by(Region) |>
  summarise(mx = max(Trips))

tourism_std <- left_join(tourism, tourism_max)
tourism_std <- tourism_std |>
  as_tibble() |>
  mutate(Trips_std = ifelse(mx > 0, 
                            Trips/mx,
                            0)) 
tourism_std |> summarise(m = max(Trips_std))
tourism_std |> group_by(Region) |> summarise(m = max(Trips_std))
tourism_std <- tourism_std |>
  as_tsibble(key = c(Region, Purpose), index = Quarter)

library(feasts)
tourism_features1 <- tourism_std |>
  features(Trips_std, list(mean = mean, 
                           sd = sd))
tourism_features2 <- tourism_std |>
  features(Trips_std, feat_stl)
tourism_features3 <- tourism_std |>
  features(Trips_std, feat_acf) |>
  select(-acf1, -acf10)

tourism_features <- bind_cols(tourism_features1, 
                              tourism_features2[,3:11],
                              tourism_features3[,3:7]) |>
  filter(Purpose != "Total") |>
  mutate(Purpose = factor(Purpose)) |>
  mutate_if(is.numeric, function(x) ifelse(is.nan(x), 0, x))

tourism_features |>
  count(Purpose)

library(randomForest)
tourism_rf <- randomForest(Purpose~., 
                           data=tourism_features[,-1])
tourism_rf
tourism_rf$importance

ggplot(tourism_features, aes(x=mean, y=sd, colour=Purpose)) + 
  geom_point(alpha=0.5)

tourism_features |>
  pivot_longer(mean:`season_acf1`, 
               names_to="var", 
               values_to="value") |>
  ggplot(aes(x=Purpose, y=value, colour=Purpose)) +
    geom_quasirandom() +
    facet_wrap(~var, ncol=6, scales="free") +
    xlab("") + ylab("") + 
    coord_flip() +
    theme(legend.position="none")

