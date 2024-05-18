library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)
library(gtExtras)
library(nflreadr)
library(dplyr)
pbp <- load_pbp(seasons=most_recent_season())
out_of_pocket <- load_ftn_charting()|>
  filter(is_qb_out_of_pocket)
out_of_pocket <- left_join(out_of_pocket,pbp,by=c("nflverse_game_id"="game_id","nflverse_play_id"="play_id"))|>
  mutate(passer_player_id = case_when(qb_scramble==1~rusher_player_id, qb_scramble!=1~passer_player_id),
         complete_pass = case_when(pass_touchdown==1~0,
                                   pass_touchdown!=1~complete_pass),
         qb_scramble = case_when(rush_touchdown==1~0,
                                 rush_touchdown!=1~qb_scramble))|>
  group_by(passer_player_id)|>
  mutate(outcome = case_when(incomplete_pass==1~"Incompletion",
                             sack==1~"Sack",
                             complete_pass==1~"Completion",
                             interception==1~"Interception",
                             qb_scramble==1~"Scramble",
                             touchdown==1~"Touchdown"))|>
  mutate(passer_player_name = case_when(qb_scramble==1~rusher_player_name, qb_scramble!=1~passer_player_name))|>
  summarize(name = first(passer_player_name),plays=n(),Outcome=outcome)|>
  filter(!is.na(name),
         !is.na(Outcome),
         plays>=75)
write_csv(out_of_pocket,"pocket_data.csv")

  