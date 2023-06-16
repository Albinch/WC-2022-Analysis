# Charger les packages requis
library(shiny)
library(readr)
library(FactoMineR)
library(plyr)
library(factoextra)
library(plotly)
library(dplyr)

setwd("/Users/clementalba/Documents/4A/DATAMINING/WC-2022-Analysis/wc-2022")

# Charger votre jeu de données
# (Assurez-vous que votre jeu de données est dans le répertoire de travail ou spécifiez le chemin complet)
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

teams <- data.frame(Position = teams_ranking$Position, Team = teams_ranking$Team)
positions <- as.data.frame(unique(player_gca$position))

player_playingtime <- player_playingtime[complete.cases(player_playingtime$minutes), ]
players_data <- cbind(player_gca,
                      player_passing,
                      player_playingtime,
                      player_shooting,
                      player_defense,
                      player_misc,
                      player_passing_types,
                      player_possession,
                      player_stats)
players_data <- subset(players_data, select = !duplicated(names(players_data)))
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

rownames(players_data) <- players_data$player
players_data <- players_data[, -c(1, 2, 4, 5, 9)]

# Créer l'application Shiny
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Statistiques des joueurs de la Coupe du Monde 2022"),
  
  # Liste déroulante des postes des joueurs
  sidebarLayout(
    sidebarPanel(
      selectInput("poste", "Poste du joueur :", choices = unique(players_data$position))
    ),
    
    # Affichage des statistiques des joueurs sélectionnés
    mainPanel(
      tableOutput("mean_position"),
      plotlyOutput("graph"),
      tableOutput("carac_clusters")
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  output$mean_position <- renderTable({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    players_data.selected <- subset(players_data.selected, select = -c(1))
    
    cols_to_remove <- grepl("minutes", colnames(players_data.selected))
    players_data.selected <- players_data.selected[, !cols_to_remove]
    
    if(input$poste != "GK"){
      cols_to_remove <- grepl("gk", colnames(players_data.selected))
      players_data.selected <- players_data.selected[, !cols_to_remove]
    }
    
    pcaPlayers <- PCA(players_data.selected, graph=FALSE)
    playersHC <- HCPC(pcaPlayers, graph=FALSE)
    
    playersDf <- transform(players_data.selected, cluster_name = paste("Cluster", playersHC$data.clust$clust))
    
    cluster_mean_position <- aggregate(Position ~ cluster_name, data = playersDf, FUN = mean)
    cluster_mean_position <- cluster_mean_position[order(cluster_mean_position$Position), ]
    
    cluster_mean_position
  })
  
  # Afficher les statistiques des joueurs sélectionnés
  output$graph <- renderPlotly({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    # players_data.selected <- subset(players_data.selected, select = -c(1))
    players_data.selected <- subset(players_data.selected, select = !names(players_data.selected) %in% c("position", "Position"))
    
    cols_to_remove <- grepl("minutes", colnames(players_data.selected))
    players_data.selected <- players_data.selected[, !cols_to_remove]
    
    if(input$poste != "GK"){
      cols_to_remove <- grepl("gk", colnames(players_data.selected))
      players_data.selected <- players_data.selected[, !cols_to_remove]
    }
    
    pcaPlayers <- PCA(players_data.selected, graph=FALSE)
    playersHC <- HCPC(pcaPlayers, graph=FALSE)
    
    playersDf <- transform(players_data.selected, cluster_name = playersHC$data.clust$clust)
    
    p <- plot_ly(playersDf, x = pcaPlayers$ind$coord[, 1] , y = pcaPlayers$ind$coord[, 2], text = rownames(playersDf),
                 mode = "markers", color = playersDf$cluster_name, marker = list(size = 11)) 
    
    p <- layout(p, title = "PCA Clusters from Hierarchical Clustering of Cars Data", 
                xaxis = list(title = "PC 1"),
                yaxis = list(title = "PC 2"))
    
    p
  })
  
  output$carac_clusters <- renderTable({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    # players_data.selected <- subset(players_data.selected, select = -c(1))
    players_data.selected <- subset(players_data.selected, select = !names(players_data.selected) %in% c("position", "Position"))

    cols_to_remove <- grepl("minutes", colnames(players_data.selected))
    players_data.selected <- players_data.selected[, !cols_to_remove]
    
    if(input$poste != "GK"){
      cols_to_remove <- grepl("gk", colnames(players_data.selected))
      players_data.selected <- players_data.selected[, !cols_to_remove]
    }
    
    pcaPlayers <- PCA(players_data.selected, graph=FALSE)
    playersHC <- HCPC(pcaPlayers, graph=FALSE)
    
    playersDf <- transform(players_data.selected, cluster_name = playersHC$data.clust$clust)
    
    clusters_numbers <- unique(playersDf$cluster_name)
    
    clusters_carac <- data.frame(matrix(nrow = 15, ncol = 4))
    
    caracs <- c()
    cluster_means <- c()
    overall_means <- c()
    clusters <- c()
    
    for(i in 1:length(clusters_numbers)){
      caracs_names <- rownames(top_n(as.data.frame(playersHC$desc.var$quanti[[i]][, 1]), n = 5))
      c_means <- top_n(as.data.frame(playersHC$desc.var$quanti[[i]]), n = 5)[, 2]
      o_means <- top_n(as.data.frame(playersHC$desc.var$quanti[[i]]), n = 5)[, 3]
      for(j in 1:5){
        clusters <- c(clusters, paste0("Cluster ", i))
      }
      
      caracs <- c(caracs, caracs_names)
      cluster_means <- c(cluster_means, c_means)
      overall_means <- c(overall_means, o_means)
    }
    
    clusters_carac$X1 <- clusters
    clusters_carac$X2 <- caracs
    clusters_carac$X3 <- cluster_means
    clusters_carac$X4 <- overall_means
    
    colnames(clusters_carac) <- c("Cluster", "Caractéristique", "Moyenne cluster", "Moyenne totale")
    
    clusters_carac
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
