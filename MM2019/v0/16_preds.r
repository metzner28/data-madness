setwd('/Users/Eli 1/Desktop/MM2019')
source('model_train.r')

generate_predictions = function(year) {
  dfT = read.csv('NCAATourneyDetailedResults.csv') %>% filter(Season == year)
  teams = unique(c(dfT$WTeamID, dfT$LTeamID))
  
  dfTeamA = dfReg %>%
    select(c(Season, WTeamID, WFGM, WFGA, WFGM3, WFGA3, WFTM, WFTA, WOR, WDR,
             WAst, WTO, WStl, WBlk, WPF)) %>%
    filter(Season == year) %>%
    select(-Season) %>%
    rename_all(function(x) return(str_sub(x, 2, length(x))))
  
  dfTeamB = dfReg %>%
    select(c(Season, LTeamID, LFGM, LFGA, LFGM3, LFGA3, LFTM, LFTA, LOR, LDR,
             LAst, LTO, LStl, LBlk, LPF)) %>%
    filter(Season == year) %>%
    select(-Season) %>%
    rename_all(function(x) return(str_sub(x, 2, length(x))))
  
  df = rbind(dfTeamA, dfTeamB) %>%
    group_by(TeamID) %>%
    summarise_all(function(x) return(sum(x))) %>%
    filter(TeamID %in% teams) %>%
    mutate(Conf = sapply(TeamID, get_conf, Season = year))
  
  # 2018 preds
  # generate matchups (filter bc teams can't play themselves)
  matchups = expand.grid(teams, teams) %>%
    rename(TeamA = Var1, TeamB = Var2) %>%
    filter(TeamA != TeamB)
  
  df_model = data.frame()
  for (i in 1:nrow(matchups)) {
    teams = as.numeric(matchups[i,])
    teamA = df %>%
      filter(TeamID == teams[1]) %>%
      rename_all(function(x) return(str_c("a", x)))
    teamB = df %>%
      filter(TeamID == teams[2]) %>%
      rename_all(function(x) return(str_c("b", x)))
    matchup = cbind(teamA, teamB)
    df_model = rbind(df_model, matchup)
  }
  
  df_model = df_model %>%
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
  
  preds = model.matrix(y ~ . - 1, data = df_model)
  matchups$prob = round(predict(model, newdata = preds), 3)
  matchups$rand = sample(seq(0,1, by = 0.00001), size = nrow(matchups))
  matchups$winA = ifelse(matchups$prob > matchups$rand, 1, 0)
  
  names = read.csv('Teams.csv')
  get_name = function(ID) {
    return(names$TeamName[names$TeamID == ID])
  }
  
  matchups = matchups %>%
    mutate(nameA = sapply(TeamA, get_name)) %>%
    mutate(nameB = sapply(TeamB, get_name))
  
  write.csv(matchups, paste('results', year, '.csv', sep = ''), row.names = FALSE)
  return(matchups)
}

output_result = function(teamA, teamB) {
  result = matchups %>% filter(nameA == teamA & nameB == teamB)
  return(result)
}

matchups = generate_predictions(2016)
 