set.seed(1)
setwd('/Users/Eli 1/Dropbox/Documents/MM2019/2019')
source('models_trained.r')

# get 2019 data ready
setwd('/Users/Eli 1/Dropbox/Documents/MM2019/2019')
dfReg19 = read.csv('RegularSeasonDetailedResults.csv') %>%
  filter(Season == 2019)

dfRanks19 = read.csv('Prelim2019_MasseyOrdinals.csv') %>%
  filter(Season == 2019) %>%
  filter(RankingDayNum == 128) %>%
  select(-RankingDayNum) %>%
  spread(SystemName, OrdinalRank) %>%
  rename(RPI = NET)

dfTourney19 = read.csv('NCAATourneySeeds.csv') %>%
  select(c(Season, TeamID)) %>%
  filter(Season == 2019)

dfTeamA19 = dfReg19 %>%
  select(c(Season, WTeamID, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA, WOR, WDR,
           WAst, WTO, WStl, WBlk, WPF)) %>%
  filter(Season >= 2003) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x)))) %>%
  rename(Season = eason)

dfTeamB19 = dfReg19 %>%
  select(c(Season, LTeamID, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR,
           LAst, LTO, LStl, LBlk, LPF)) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x)))) %>%
  rename(Season = eason)

df19 = rbind(dfTeamA19, dfTeamB19) %>%
  group_by(Season, TeamID) %>%
  summarise_all(function(x) return(sum(x)))

df_final19 = full_join(dfRanks19, df19, by = c("Season", "TeamID")) %>%
  filter(TeamID %in% dfTourney19$TeamID) %>%
  select_if(function(x) !anyNA(x))

matchups = expand.grid(dfTourney19$TeamID, dfTourney19$TeamID) %>%
  rename(teamA = Var1, teamB = Var2) %>%
  filter(teamA < teamB)

model_2019 = data.frame()
for (i in 1:nrow(matchups)) {
  teams = as.numeric(matchups[i,])
  teamA = df_final19 %>%
    filter(TeamID == teams[1]) %>%
    rename_all(function(x) return(str_c("a", x)))
  teamB = df_final19 %>%
    filter(TeamID == teams[2]) %>%
    rename_all(function(x) return(str_c("b", x)))
  matchup = cbind(teamA, teamB)
  model_2019 = rbind(model_2019, matchup)
}

df2019 = model_2019 %>%
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
    WOL = aWOL - bWOL
  )

df2019_xgb = data.matrix(df2019)

preds_xgb = predict(model_xgb, newdata = df2019_xgb)
preds_rf = predict(model_rf, data = df2019)$predictions

submit_xgb = cbind(matchups, preds_xgb) %>%
  rename(Pred = preds_xgb) %>%
  mutate(year = 2019) %>%
  select(year, teamA, teamB, Pred) %>%
  unite("ID", year, teamA, teamB, sep = "_")

submit_rf = cbind(matchups, preds_rf) %>%
  rename(Pred = preds_rf) %>%
  mutate(year = 2019) %>%
  select(year, teamA, teamB, Pred) %>%
  unite("ID", year, teamA, teamB, sep = "_")

df_bracket = cbind(matchups, preds_xgb, preds_rf)
names = read.csv('Teams.csv') %>%
  select(c(TeamID, TeamName))

get_name = function(ID) {
  return(names$TeamName[names$TeamID == ID])
}

df_bracket_final = df_bracket %>%
  mutate(nameA = sapply(df_bracket$teamA, get_name),
         nameB = sapply(df_bracket$teamB, get_name),
         flip = sample(seq(0, 1, by = 0.00001), size = nrow(df_bracket)),
         winA = ifelse(preds_rf > flip, 1, 0),
         winA_xgb = ifelse(preds_xgb > flip, 1, 0)
  )

output_result = function(A, B) {
  result = df_bracket_final %>% filter(nameA == A & nameB == B)
  if (nrow(result) == 0) {
    result = df_bracket_final %>% filter(nameA == B & nameB == A)
  }
  return(result)
}