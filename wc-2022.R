# Importation des données
library(readr)
player_gca <- read_csv("archive/player_gca.csv")
player_keepersadv <- read_csv("archive/player_keepersadv.csv")
player_passing <- read_csv("archive/player_passing.csv")
player_playingtime <- read_csv("archive/player_playingtime.csv", na = "0")
player_shooting <- read_csv("archive/player_shooting.csv")
player_defense <- read_csv("archive/player_defense.csv")
player_keepers <- read_csv("archive/player_keepers.csv")
player_misc <- read_csv("archive/player_misc.csv")
player_passing_types <- read_csv("archive/player_passing_types.csv")
player_possession <- read_csv("archive/player_possession.csv")
player_stats <- read_csv("archive/player_stats.csv")
teams_ranking <- read_csv("archive/FIFA-RANKING")

# Extraction des équipes et de leur classement
teams <- data.frame(Position = teams_ranking$Position, Team = teams_ranking$Team)
positions <- as.data.frame(unique(player_gca$position))

# Suppression des éléments n'ayant pas joué (NA minutes)
player_playingtime <- player_playingtime[complete.cases(player_playingtime$minutes), ]

# Jointure de toutes les tables
players_data <- cbind(player_gca,
                      player_passing,
                      player_playingtime,
                      player_shooting,
                      player_defense,
                      player_misc,
                      player_passing_types,
                      player_possession,
                      player_stats)
# Suppression des colonnes dupliquées
players_data <- subset(players_data, select = !duplicated(names(players_data)))
# Remplacement des valeurs NA par des 0
players_data[is.na(players_data)] <- 0

# Extraction des défenseurs, milieux, attaquants et gardiens
defensives <- subset(players_data, players_data$position == "DF")
midfielders <- subset(players_data, players_data$position == "MF")
forwards <- subset(players_data, players_data$position == "FW")
goalkeepers <- subset(players_data, players_data$position == "GK")

# Ajout des informations spécifiques aux gardiens
goalkeepers <- cbind(goalkeepers,
                     player_keepers,
                     player_keepersadv)
goalkeepers <- subset(goalkeepers, select = !duplicated(names(goalkeepers)))

players_data <- merge(players_data, teams, by.x = "team", by.y = "Team", all.x = T, all.y = T)
goalkeepers <- merge(goalkeepers, teams, by.x = "team", by.y = "Team", all.x = T, all.y = T)

colnames(players_data)[153] <- "Classement"
colnames(goalkeepers)[194] <- "Classement"
