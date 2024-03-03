library(cricketdata)
library(tidyverse)
library(mulgar)
library(tourr)
library(ggfortify)
library(plotly)
auswt20 <- fetch_cricinfo("T20", "Women", country = "Aust")
auswt20bowl <- fetch_cricinfo("T20", "Women", country = "Aust", 
                              activity="bowling")

auswt20 <- auswt20 |>
  full_join(auswt20bowl, by="Player")

auswt20 <- auswt20 |> 
  select(Player, Start.x, End.x, Matches.x, Innings.x, 
         NotOuts, Runs.x, HighScore, Average.x, 
         BallsFaced, StrikeRate.x, Hundreds, Fifties, 
         Ducks, Fours, Sixes, Overs, Maidens, Wickets, 
         Economy, FourWickets, FiveWickets) |>
  rename(Start = Start.x,
         End = End.x,
         Matches = Matches.x,
         Innings = Innings.x,
         Runs = Runs.x,
         Average = Average.x,
         StrikeRate = StrikeRate.x) 
auswt20 <- auswt20 |>  
  filter(!is.na(Innings))
  
auswt20 <- auswt20 |>  
  replace_na(list(Overs=0, Maidens=0,
                    Wickets=0, Economy=0, 
                    FourWickets=0, FiveWickets=0))

auswt20 <- auswt20 |>  
  mutate(Average = ifelse(Average == Inf, 100, Average))

auswt20_pca <- prcomp(auswt20[, 6:22], scale = TRUE)
ggscree(auswt20_pca, q=17)
summary(auswt20_pca)
options(digits=2)
auswt20_pca$rotation[,1:3]

auswt20_pca_pcs <- as_tibble(auswt20_pca$x[,1:2]) |>
  mutate(player=auswt20$Player)
auswt20_pca_var <- tibble(n=1:length(auswt20_pca$sdev), 
                          evl=auswt20_pca$sdev^2)
auswt20_pca_evc <- as_tibble(auswt20_pca$rotation[,1:2]) |> 
  mutate(origin=rep(0, 17), 
         variable=colnames(auswt20)[6:22],
         varname=rownames(auswt20_pca$rotation)) |>
  mutate(PC1s = PC1*(auswt20_pca_var$evl[1]*2.5), 
         PC2s = PC2*(auswt20_pca_var$evl[2]*2.5))
ggplot() + 
  geom_segment(data=auswt20_pca_evc, 
               aes(x=origin, xend=PC1s, 
                   y=origin, yend=PC2s), colour="orange") +
  geom_text(data=auswt20_pca_evc, aes(x=PC1s, y=PC2s,
                                    label=variable),
            colour="orange", nudge_x=0.7) +
  geom_point(data=auswt20_pca_pcs, aes(x=PC1, y=PC2, label=player)) +
  geom_text(data=filter(auswt20_pca_pcs, PC1<(-3)),
            aes(x=PC1, y=PC2, label=player), 
            nudge_y=0.15, nudge_x=0.1) +
  geom_text(data=filter(auswt20_pca_pcs, PC2<(-4)),
            aes(x=PC1, y=PC2, label=player), 
            nudge_y=0.15, nudge_x=0.1) +
  xlab("PC1") + ylab("PC2") 
ggplotly()

# Look at data in a tour
auswt20_std <- auswt20 |>
  mutate_at(vars(NotOuts:FiveWickets), function(x) (x-mean(x))/sd(x))
animate_xy(auswt20_std[, 4:22])


