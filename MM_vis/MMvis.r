library(tidyverse)
library(reshape2)

df = read.csv("NCAATourneyDetailedResults.csv") %>%
  select_at(-c(2:8)) %>%
  filter(Season >= 2009) %>%
  transmute(
    GameID = row_number(),
    Season = Season,
    FGM = WFGM - LFGM,
    FGA = WFGA - LFGA,
    FGM3 = WFGM3 - LFGM3,
    FGA3 = WFGA3 - LFGA3,
    FTM = WFTM- LFTM,
    OR = WOR - LOR,
    DR = WDR - LDR,
    Ast = WAst - LAst,
    TO = WTO - LTO,
    Stl = WStl - LStl,
    Blk = WBlk - LBlk,
    PF = WPF - LPF
  )

df2 = df %>%
  group_by(Season) %>%
  summarise_all(mean) %>%
  select(-GameID)

df_melted = melt(df, id.vars = c("GameID", "Season")) %>%
  arrange(GameID)

df2_melted = melt(df2, id.vars = "Season")

write.csv(df_melted, "game_level.csv")
write.csv(df2_melted, "averages_melted.csv")
