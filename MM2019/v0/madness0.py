import pandas as pd
import numpy as np
import sklearn as skl
import xgboost as xgb

# read in data and format seeds

dfStats = pd.read_csv('NCAATourneyDetailedResults.csv')
dfSeeds = pd.read_csv('NCAATourneySeeds.csv')
type(dfSeeds.Seed)
dfSeeds = dfSeeds.assign(Seed = dfSeeds.Seed.map(lambda x: int(x[1:3])))[dfSeeds.Season >= 2003]
dfStats.head()

def get_seed(TeamID, Season):
    seed = int(dfSeeds.Seed[(dfSeeds.Season == Season) & (dfSeeds.TeamID == TeamID)])
    return seed

WSeed = []
for s, t in zip(dfStats.Season, dfStats.WTeamID):
    WSeed.append(get_seed(t, s))

LSeed = []
for s, t in zip(dfStats.Season, dfStats.LTeamID):
    LSeed.append(get_seed(t, s))
]
df = dfStats.assign(WSeed = pd.Series(WSeed),
                    LSeed = pd.Series(LSeed))
list(df)

dfFinal = df.assign()
