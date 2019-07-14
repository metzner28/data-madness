library(tidyverse)
library(xgboost)
library(ranger)
setwd('/Users/Eli 1/Desktop/MM2019')

# read in the data
dfStats = read.csv('NCAATourneyDetailedResults.csv')
dfSeeds = read.csv('NCAATourneySeeds.csv') %>%
  mutate(Seed = as.numeric(str_sub(Seed, 2, 3)))
dfConfs = read.csv('TeamConferences.csv')

# modify and clean the data
get_seed = function(Season, TeamID) {
  return(dfSeeds$Seed[dfSeeds$Season == Season & dfSeeds$TeamID == TeamID])
}

get_conf = function(Season, TeamID) {
  return(dfConfs$ConfAbbrev[dfConfs$Season == Season & dfConfs$TeamID == TeamID])
}

df0 = dfStats %>%
  mutate(WSeed = mapply(get_seed, Season, WTeamID)) %>%
  mutate(LSeed = mapply(get_seed, Season, LTeamID)) %>%
  transmute(
            FGM = WFGM - LFGM,
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
            Conf1 = mapply(get_conf, Season, WTeamID),
            Conf2 = mapply(get_conf, Season, LTeamID),
            y = 1)
            
idx_invert = sample(nrow(df0), size = 0.5 * nrow(df0))
df_losses = df0[idx_invert,] %>%
  mutate_if(is.numeric, function(x) return(-x)) %>%
  mutate(y = 0)
df_wins = df0[-idx_invert,]

df_clean = rbind(df_wins, df_losses)
idx = sample(nrow(df_clean), size = 0.2 * nrow(df_clean))
df_train = df_clean[-idx,]
df_test = df_clean[idx,]

# have the machine learn
df_train_x = model.matrix(y ~ . - 1, data = df_train)
df_test_x = model.matrix(y ~ . - 1, data = df_test)
eval_model = function(type) {
  
  # fit
  if (type == 'xgb') {
    model = xgboost(data = df_train_x, label = df_train$y,
                    params = list(objective = 'binary:logistic'),
                    nrounds = 100)
    preds = ifelse(predict(model, newdata = df_test_x) > 0.5, 1, 0)
  } else if (type == 'ranger') {
    model = ranger(y ~ ., data = df_train, probability = TRUE)
    preds = ifelse(predict(model, data = df_test)$predictions > 0.5, 1, 0)
  } else stop ('select model!')
  
  # evaluate
  error = mean(preds != df_test$y)
  return(error)
  
}

models = c('xgb', 'ranger')
errors = sapply(models, eval_model)


