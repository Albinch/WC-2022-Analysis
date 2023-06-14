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
# Récupération des colonnes similaires
same_columns <- intersect(colnames(players_data), colnames(player_keepers))
# Merge des données des joueurs avec les données des gardiens
players_data <- merge(players_data, player_keepers, by = same_columns, all.x = T)

same_columns <- intersect(colnames(players_data), colnames(player_keepersadv))
players_data <- merge(players_data, player_keepersadv, by = same_columns, all.x = T)

# Remplacement des valeurs NA par des 0
players_data[is.na(players_data)] <- 0

# Ajout du classement de l'équipe dans le dataset des joueurs
players_data <- merge(players_data, teams, by.x = "team", by.y = "Team", all.x = T, all.y = T)

players_data <- subset(players_data, players_data$minutes >= 180)

# Extraction des défenseurs, milieux, attaquants et gardiens
defensives <- subset(players_data, players_data$position == "DF")
midfielders <- subset(players_data, players_data$position == "MF")
forwards <- subset(players_data, players_data$position == "FW")
goalkeepers <- subset(players_data, players_data$position == "GK")

rownames(defensives) <- defensives$player
rownames(midfielders) <- midfielders$player
rownames(forwards) <- forwards$player
rownames(goalkeepers) <- goalkeepers$player

defensives.active <- defensives[, -c(1, 2, 3, 4, 5, 9)]
midfielders.active <- midfielders[, -c(1, 2, 3, 4, 5, 9)]
forwards.active <- forwards[, -c(1, 2, 3, 4, 5, 9)]
goalkeepers.active <- goalkeepers[, -c(1, 2, 3, 4, 5, 9)]

min_pos <- min(defensives.active$Position)
max_pos <- max(defensives.active$Position)
defensives.active$Position_scaled <- ((defensives.active$Position - min_pos) / (max_pos - min_pos)) * 9 + 1


cols_to_divide <- colnames(defensives.active)[!grepl("90", colnames(defensives.active))]
for (col in cols_to_divide){
 if(col != "minutes" | col != "Position" | col != "Position_scaled"){
  defensives.active[[col]] <- (defensives.active[[col]] / defensives.active$minutes) * 90
  }
}

cols_to_remove <- grepl("gk", colnames(defensives.active))
defensives.active <- defensives.active[, !cols_to_remove]
#midfielders.active <- midfielders.active[, !cols_to_remove]
#forwards.active <- forwards.active[, !cols_to_remove]

#data_exclu <- defensives.active[, !colnames(defensives.active) %in% "Position"]
#data_exclu <- data_exclu[, !colnames(data_exclu) %in% "Position_scaled"]

library(FactoMineR)
acp <- PCA(defensives.active)

library(plyr)
library(factoextra)
hcpc = HCPC(acp, nb.clust = 3, graph = FALSE)
plot(hcpc, choice="map", draw.tree=F)
plot(hcpc, choice="bar")
fviz_dend(hcpc, 
          cex = 0.7,                     # Taille du text
          palette = "jco",               # Palette de couleur ?ggpubr::ggpar
          rect = TRUE, rect_fill = TRUE, # Rectangle autour des groupes
          rect_border = "jco",           # Couleur du rectangle
          labels_track_height = 0.8      # Augment l'espace pour le texte
          )

fviz_cluster(hcpc,
             repel = TRUE,            # Evite le chevauchement des textes
             show.clust.cent = TRUE, # Montre le centre des clusters
             palette = "jco",         # Palette de couleurs, voir ?ggpubr::ggpar
             ggtheme = theme_minimal(),
             main = "Factor map",
)

hcpc$desc.var$quanti

print(hcpc$desc.ind$para)
