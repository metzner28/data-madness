library(tidyverse)
library(xgboost)
setwd('/Users/Eli 1/Desktop/MM2019')

# clean data
dfReg = read.csv('RegularSeasonDetailedResults.csv')
dfConfs = read.csv('TeamConferences.csv')
dfRanks = read.csv('MasseyOrdinals.csv')

get_conf = function(Season, TeamID) {
  return(dfConfs$ConfAbbrev[dfConfs$Season == Season & dfConfs$TeamID == TeamID])
}

df0 = dfReg %>%
  transmute(FGM = WFGM - LFGM,
            FGA = WFGA - LFGA,
            FGM3 = WFGM3 - LFGM3,
            FGA3 = WFGA3 - LFGA3,
            FTM = WFTM - LFTM,
            FTA = WFTA - LFTA,
            OR = WOR - LOR,
            DR = WDR - LDR,
            Ast = WAst - LAst,
            TO = WTO - LTO,
            Stl = WStl - LStl,
            Blk = WBlk - LBlk,
            PF = WPF - LPF,
            aConf = mapply(get_conf, Season, WTeamID),
            bConf = mapply(get_conf, Season, LTeamID),
            y = 1)

idx_invert = sample(nrow(df0), size = 0.5 * nrow(df0))
df_losses = df0[idx_invert,] %>%
  mutate_if(is.numeric, function(x) return(-x)) %>%
  rename(aConf = bConf,
         bConf = aConf) %>%
  mutate(y = 0)
df_wins = df0[-idx_invert,]

df_clean = rbind(df_wins, df_losses)
idx = sample(nrow(df_clean), size = 0.2 * nrow(df_clean))
df_train = df_clean[-idx,]
df_test = df_clean[idx,]

# fit xgb model

df_train_x = model.matrix(y ~ . - 1, data = df_train)
df_test_x = model.matrix(y ~ . - 1, data = df_test)

model = xgboost(data = df_train_x, label = df_train$y,
                params = list(objective = 'binary:logistic', 
                              eval_metric = 'logloss'),
                eta = 0.2, nrounds = 100, verbose = TRUE)