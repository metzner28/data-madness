library(tidyverse)
library(xgboost)
library(ranger)
setwd('/Users/Eli 1/Dropbox/Documents/MM2019')

# clean data into tidy and prediction-ready format
dfReg = read.csv('RegularSeasonDetailedResults.csv')
dfRanks = read.csv('MasseyOrdinals.csv') %>%
  filter(RankingDayNum == 133) %>%
  select(-RankingDayNum) %>%
  spread(SystemName, OrdinalRank) %>%
  select_if(function(x) return(sum(is.na(x)) < 8))
  
dfTourney = read.csv('NCAATourneyCompactResults.csv') %>%
  select(c(Season, WTeamID, LTeamID)) %>%
  filter(Season >= 2003)

dfTeamA = dfReg %>%
  select(c(Season, WTeamID, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA, WOR, WDR,
           WAst, WTO, WStl, WBlk, WPF)) %>%
  filter(Season >= 2003) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x)))) %>%
  rename(Season = eason)

dfTeamB = dfReg %>%
  select(c(Season, LTeamID, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR,
           LAst, LTO, LStl, LBlk, LPF)) %>%
  filter(Season >= 2003) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x)))) %>%
  rename(Season = eason)

df = rbind(dfTeamA, dfTeamB) %>%
  group_by(Season, TeamID) %>%
  summarise_all(function(x) return(sum(x)))

df_final = full_join(dfRanks, df, by = c("Season", "TeamID"))

model_full = data.frame()
for (i in 1:nrow(dfTourney)) {
  teams = as.numeric(dfTourney[i,])
  teamA = df_final %>%
    filter(TeamID == teams[2] & Season == teams[1]) %>%
    rename_all(function(x) return(str_c("a", x)))
  teamB = df_final %>%
    filter(TeamID == teams[3] & Season == teams[1]) %>%
    rename_all(function(x) return(str_c("b", x)))
  matchup = cbind(teamA, teamB)
  model_full = rbind(model_full, matchup)
}

df0 = model_full %>%
  transmute(
      FGM = aFGM -bFGM,
      FGA = aFGA - bFGA,
      FGPct = (aFGM/aFGA) - (bFGM/bFGA),
      FGM3 = aFGM3 - bFGM3,
      FGA3 = aFGA3 - bFGA3,
      FG3Pct = (aFGM3/aFGA3) - (bFGM3/bFGA3),
      FTM = aFTM - bFTM,
      FTA = aFTA - bFTA,
      FTPct = (aFTM/aFTA) - (bFTM/bFTA),
      OR = aOR - bOR,
      DR = aDR - bDR,
      Ast = aAst - bAst,
      TO = aTO - bTO,
      Stl = aStl - bStl,
      Blk = aBlk - bBlk,
      PF = aPF - bPF,
      POM = aPOM - bPOM,
      RPI = aRPI - bRPI,
      SAG = aSAG - bSAG,
      COL = aCOL - bCOL,
      DOL = aDOL - bDOL,
      MOR = aMOR - bMOR,
      RTH = aRTH - bRTH,
      WLK = aWLK - bWLK,
      WOL = aWOL - bWOL,
      y = 1
  )

xgb = vector(mode = 'numeric', length = 100)
rf = vector(mode = 'numeric', length = 100)
glm = vector(mode = 'numeric', length = 100)
for (n in 1:100) {
  idx_invert = sample(nrow(df0), size = 0.5 * nrow(df0))
  df_losses = df0[idx_invert,] %>%
    mutate_if(is.numeric, function(x) return(-x)) %>%
    mutate(y = 0)
  df_wins = df0[-idx_invert,]
  
  df_clean = rbind(df_wins, df_losses)
  idx = sample(nrow(df_clean), size = 0.2 * nrow(df_clean))
  df_train = df_clean[-idx,]
  df_test = df_clean[idx,]
  
  # fit xgb model with CV
  df_train_x = model.matrix(y ~ . - 1, data = df_train)
  df_test_x = model.matrix(y ~ . - 1, data = df_test)
  
  CV = xgb.cv(params = list(objective = 'binary:logistic'),
              data = df_train_x, label = df_train$y,
              eta = 0.1, nrounds = 200, verbose = FALSE, nfold = 5)
  dfCV = data.frame(CV$evaluation_log)
  opt = dfCV$iter[which.min(dfCV$test_error_mean)]
  
  model_xgb = xgboost(data = df_train_x, label = df_train$y,
                      params = list(objective = 'binary:logistic', 
                                    eval_metric = 'logloss',
                                    max_depth = 2),
                      eta = 0.1, nrounds = opt, verbose = FALSE)
  
  preds_xgb = ifelse(predict(model_xgb, newdata = df_test_x) > 0.5, 1, 0)
  loss_xgb = mean(preds_xgb != df_test$y)
  xgb[n] = loss_xgb
  
  # fit random forest model, hyperparameter tuning by hand
  model_rf = ranger(y ~ ., data = df_train, num.trees = 500, max.depth = 2)
  preds_rf = ifelse(predict(model_rf, data = df_test_x)$predictions > 0.5, 1, 0)
  loss_rf = mean(preds_rf != df_test$y)
  rf[n] = loss_rf
  
  # fit basic glm
  model_glmnet = cv.glmnet(x = df_train_x, y = df_train$y)
  preds_glm = ifelse(predict(model_glmnet, newx = df_test_x, s = "lambda.1se") > 0.5, 1, 0)
  loss_glm = mean(preds_glm != df_test$y)
  glm[n] = loss_glm
  
  print(n)
}

loss_xgb = mean(xgb)
loss_rf = mean(rf)
loss_glm = mean(glm)


