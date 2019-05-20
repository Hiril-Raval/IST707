# Load required packages
require(dplyr)
require(tidyr)
require(lubridate)

# Load csv files
cities <- read.csv('WCities.csv')
gameCities <- read.csv('WGameCities.csv')
compactResults <- read.csv('WNCAATourneyCompactResults.csv')
seeds <- read.csv('WNCAATourneySeeds.csv')
slots <- read.csv('WNCAATourneySlots.csv')
regSeasonResults <- read.csv('WRegularSeasonCompactResults.csv')
season <- read.csv('WSeasons.csv')
teams <- read.csv('WTeams.csv')
teamSpellings <- read.csv('WTeamSpellings.csv')

# Add columns to files
regSeasonResults$gameType <- 'regularSeason'
compactResults$gameType <- 'tournament'

compactResults <- compactResults %>% 
  left_join(seeds, by = c('Season'='Season', 'WTeamID' = 'TeamID'))
colnames(compactResults)[10] <- 'WTeamSeed'

compactResults <- compactResults %>% 
  left_join(seeds, by = c('Season'='Season', 'LTeamID' = 'TeamID'))
colnames(compactResults)[11] <- 'LTeamSeed'

seasonNew <- gather(season, key = Season, value = Region, c(RegionW, RegionY, RegionX, RegionZ))
colnames(seasonNew) <- c('DayZero', 'Region', 'Title')
seasonNew$DayZero <- as.Date(as.character(seasonNew$DayZero), '%m/%d/%Y')

seasonNew <- seasonNew %>% 
  mutate(season = year(DayZero), 
         seed = substr(Region, 7, 7))

# Create a master dataset 
master <- regSeasonResults %>% bind_rows(compactResults)

master <- master %>% 
  left_join(season, by = c('Season' = 'Season')) %>% 
  select(-RegionW, -RegionX, -RegionY, -RegionZ)
master$DayZero <- as.Date(as.character(master$DayZero), format = '%m/%d/%Y')
master$gameDay <- master$DayNum + master$DayZero

master <- master %>% 
  mutate(WRegion = substr(WTeamSeed, 1, 1), 
         LRegion = substr(LTeamSeed, 1, 1)) %>% 
  left_join(seasonNew, by = c('Season'='season', 'WRegion'='seed')) %>% 
  left_join(seasonNew, by = c('Season'='season', 'LRegion'='seed')) %>%
  select(-DayZero, -WRegion, -LRegion, -DayZero.y, -Region.x, -Region.y) %>%
  group_by(Season, WTeamID, LTeamID) %>%
  mutate(meeting = 1:n()) %>% 
  ungroup() %>% 
  mutate(gameId = paste0(Season, '_', WTeamID, '_', LTeamID, '_', meeting)) %>% 
  select(Season, DayZero.x, gameDay, DayNum, gameId, WTeamID, WScore, LTeamID, LScore, WLoc, NumOT, gameType, 
         WTeamSeed, LTeamSeed, WRegion = Title.x, LRegion = Title.y)
colnames(master)[2] <- 'DayZero'

# Now that we have the master file ready, we'll group by team and calculate each team's offensive and defensive ratings, and
# the average point spread by which they beat - or lose to - their opponents.

# We start by selecting the winning team, their score, and the score of the losing team. These last two will be the points
# scored and points allowed, and will be used to calculate each team's offensive rating (OR) and defensive rating (DR).
homeTeam <- master %>%
  select(Season, gameId, Team1 = WTeamID, Team2 = LTeamID, PointsScored = WScore, PointsAllowed = LScore)
awayTeam <- master %>% 
  select(Season, gameId, Team1 = LTeamID, Team2 = WTeamID, PointsScored = LScore, PointsAllowed = WScore)

totalTeam <- homeTeam %>% bind_rows(awayTeam) %>% arrange(gameId)
totalTeamDedup <- totalTeam[!duplicated(totalTeam$gameId), ]

# First we'll calculate average points scored and allowed by the league. This needs to be done by season to account for the 
# offensive and defensive shifts of all the teams. 
totalTeam <- totalTeamDedup %>% 
  group_by(Season) %>% 
  mutate(lgPS = mean(PointsScored), lgPA = mean(PointsAllowed)) %>% 
  ungroup()

# Then, we'll create Team1's OR, DR and point spread... 
totalTeam <- totalTeam %>% 
  group_by(Season, Team1) %>% 
  mutate(T1OR = mean(PointsScored)/lgPS, 
         T1DR = mean(PointsAllowed)/lgPA,
         T1PointSpread = mean(PointsScored) - mean(PointsAllowed)) %>%
  # ...followed by Team2's OR and DR.
  ungroup() %>% 
  group_by(Season, Team2) %>% 
  mutate(T2OR = mean(PointsAllowed)/lgPA,
         T2DR = mean(PointsScored)/lgPS,
         T2PointSpread = mean(PointsAllowed - mean(PointsScored))) %>%
  ungroup()

# We calculate these ratings because it gives us a better idea of how good or bad the team was relative to the league's
# offensive and defensive environment. The mean for both these ratings is 1, meaning that anybody below 1 is below average, 
# while those above are above average. 
# Point spread will give us the average margin of victory or defeat for each team. 

# With each team's OR, DR, and point spread calculated, we'll merge it back into the master file and calculate the expected
# results for each match (what a team should have ideally scored given the other team's neutral stats.)
master <- master %>% 
  left_join(totalTeam, by = c('Season' = 'Season', 'gameId' = 'gameId')) %>% 
  select(Season, gameId, DayZero, gameDay, DayNum, WTeamID, WScore, LTeamID, LScore, WLoc, NumOT, gameType, WTeamSeed,
         LTeamSeed, WRegion, LRegion, lgPS, lgPA, WTeamOR = T1OR, WTeamDR = T1DR, WTeamPointSpread = T1PointSpread,
         LTeamOR = T2OR, LTeamDR = T2DR, LTeamPointSpread = T2PointSpread) %>%
  as.data.frame()

