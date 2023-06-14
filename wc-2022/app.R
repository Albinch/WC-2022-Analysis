# Charger les packages requis
library(shiny)
library(readr)
library(FactoMineR)
library(plyr)
library(factoextra)
library(plotly)

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
      tableOutput("stats"),
      plotlyOutput("graph")
    )
  )
)

# Définir le serveur
server <- function(input, output) {
  # Filtrer les données en fonction du poste sélectionné
  filteredData <- reactive({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    rownames(players_data.selected) <- players_data.selected$player
    players_data.selected <- players_data.selected[, -c(1, 2, 3, 4, 5, 9)]
    
    cols_to_divide <- colnames(players_data.selected)[!grepl("90", colnames(players_data.selected))]
    for (col in cols_to_divide){
      if(col != "minutes" & col != "Position"){
        players_data.selected[[col]] <- (players_data.selected[[col]] / players_data.selected$minutes) * 90
      }
    }
    
    if(input$poste != "GK"){
      cols_to_remove <- grepl("gk", colnames(players_data.selected))
      players_data.selected <- players_data.selected[, !cols_to_remove]
    }
    
    players_data.selected
    
    acp <- PCA(players_data.selected)
    hcpc = HCPC(acp, graph = FALSE)
    hcpc
  })

  # Afficher les statistiques des joueurs sélectionnés
  output$graph <- renderPlotly({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    rownames(players_data.selected) <- players_data.selected$player
    players_data.selected <- players_data.selected[, -c(1, 2, 3, 4, 5, 9)]
    
    cols_to_divide <- colnames(players_data.selected)[!grepl("90", colnames(players_data.selected))]
    for (col in cols_to_divide){
      if(col != "minutes" & col != "Position"){
        players_data.selected[[col]] <- (players_data.selected[[col]] / players_data.selected$minutes) * 90
      }
    }
    
    if(input$poste != "GK"){
      cols_to_remove <- grepl("gk", colnames(players_data.selected))
      players_data.selected <- players_data.selected[, !cols_to_remove]
    }
    
    pcaPlayers <- prcomp(players_data.selected)
    playersHC <- hclust(dist(pcaPlayers$x), method = "ward.D2")
    playersClusters <- cutree(playersHC, k = 3)
    
    playersDf <- data.frame(pcaPlayers$x, "cluster" = factor(playersClusters))
    playersDf <- transform(playersDf, cluster_name = paste("Cluster",playersClusters))
    
    # Calculer l'importance des variables par cluster
    cluster_importance <- aggregate(players_data.selected, by = list(playersClusters), FUN = mean)
    
    # Afficher les variables les plus importantes par cluster
    for (i in 1:3) {
      cat("Cluster", i, ":\n")
      top_variables <- sort(cluster_importance[i, -1], decreasing = TRUE)
      print(head(top_variables))
      cat("\n")
    }
    
    p <- plot_ly(playersDf, x = pcaPlayers$x[, 1] , y = pcaPlayers$x[, 2], text = rownames(playersDf),
                 mode = "markers", color = playersDf$cluster_name, marker = list(size = 11)) 
    
    p <- layout(p, title = "PCA Clusters from Hierarchical Clustering of Cars Data", 
                xaxis = list(title = "PC 1"),
                yaxis = list(title = "PC 2"))
    
    p
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
