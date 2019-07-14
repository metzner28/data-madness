setwd('/Users/Eli 1/Desktop/MM2019')
source('model_train_xgb.r')
# make predictions for 2018

dfT18 = read.csv('NCAATourneyDetailedResults.csv') %>% filter(Season == 2018)
teams18 = unique(c(dfT18$WTeamID, dfT18$LTeamID))

dfTeamA = dfReg %>%
  select(c(Season, WTeamID, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA, WOR, WDR,
           WAst, WTO, WStl, WBlk, WPF)) %>%
  filter(Season == 2018) %>%
  select(-Season) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x))))

dfTeamB = dfReg %>%
  select(c(Season, LTeamID, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR,
           LAst, LTO, LStl, LBlk, LPF)) %>%
  filter(Season == 2018) %>%
  select(-Season) %>%
  rename_all(function(x) return(str_sub(x, 2, length(x))))

df18 = rbind(dfTeamA, dfTeamB) %>%
  group_by(TeamID) %>%
  summarise_all(function(x) return(sum(x))) %>%
  filter(TeamID %in% teams18) %>%
  mutate(Conf = sapply(TeamID, get_conf, Season = 2018))

# 2018 preds
# generate matchups (filter bc teams can't play themselves)
matchups18 = expand.grid(teams18, teams18) %>%
  rename(TeamA = Var1, TeamB = Var2) %>%
  filter(TeamA != TeamB)

model18 = data.frame()
for (i in 1:nrow(matchups18)) {
  teams = as.numeric(matchups18[i,])
  teamA = df18 %>%
    filter(TeamID == teams[1]) %>%
    rename_all(function(x) return(str_c("a", x)))
  teamB = df18 %>%
    filter(TeamID == teams[2]) %>%
    rename_all(function(x) return(str_c("b", x)))
  matchup = cbind(teamA, teamB)
  model18 = rbind(model18, matchup)
}

model18 = model18 %>%
  transmute(
           FGM = aFGM -bFGM,
           FGA = aFGA - bFGA,
           FGM3 = aFGM3 - bFGM3,
           FGA3 = aFGA3 - bFGA3,
           FTM = aFTM - bFTM,
           FTA = aFTA - bFTA,
           OR = aOR - bOR,
           DR = aDR - bDR,
           Ast = aAst - bAst,
           TO = aTO - bTO,
           Stl = aStl - bStl,
           Blk = aBlk - bBlk,
           PF = aPF - bPF,
           aConf = aConf,
           bConf = bConf,
           y = 1
  )

matchups18$prob = round(predict(model, newdata = model18, type = 'response'), 3)
matchups18$rand = sample(seq(0,1, by = 0.00001), size = nrow(matchups18))
matchups18$winA = ifelse(matchups18$prob > matchups18$rand, 1, 0)

names18 = read.csv('Teams.csv')
get_name = function(ID) {
  return(names18$TeamName[names18$TeamID == ID])
}

matchups18 = matchups18 %>%
  mutate(nameA = sapply(TeamA, get_name)) %>%
  mutate(nameB = sapply(TeamB, get_name))

write.csv(matchups18, 'results2018.csv', row.names = FALSE)

output_result = function(teamA, teamB) {
  result = matchups18 %>% filter(nameA == teamA & nameB == teamB)
  return(result)
}
