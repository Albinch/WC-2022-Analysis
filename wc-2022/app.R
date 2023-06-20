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
player_passing <- read_csv("archive/player_passing.csv")
player_playingtime <- read_csv("archive/player_playingtime.csv", na = "0")
player_shooting <- read_csv("archive/player_shooting.csv")
player_defense <- read_csv("archive/player_defense.csv")
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

# Remplacement des valeurs NA par des 0
players_data[is.na(players_data)] <- 0

# Ajout du classement de l'équipe dans le dataset des joueurs
players_data <- merge(players_data, teams, by.x = "team", by.y = "Team", all.x = T, all.y = T)
players_data <- players_data[players_data$position != "GK", ]

players_data <- subset(players_data, players_data$minutes >= 180)

rownames(players_data) <- players_data$player
stat <- players_data
players_data <- players_data[, !(names(players_data) %in% c("team", "player", "age", "birth_year", "club"))]

# Créer l'application Shiny
ui <- fluidPage(
  # Titre de l'application
  titlePanel("Statistiques des joueurs de la Coupe du Monde 2022"),
  
  # Liste déroulante des postes des joueurs
  sidebarLayout(
    sidebarPanel(
      selectInput("poste", "Poste du joueur : (pour la partie cluster)", choices = unique(players_data$position)),
    ),
    
    # Affichage des statistiques des joueurs sélectionnés
    mainPanel(
      tabsetPanel(
        tabPanel('Statistiques globales',
                 fluidRow(
                   column(width = 12, plotlyOutput("nbbuts"))
                 ),
                 fluidRow(
                   column(width = 6, plotlyOutput("ppd")),
                   column(width = 6, plotlyOutput("cartons"))
                 ),
                 fluidRow(
                   column(width = 6, plotlyOutput("passes")),
                   column(width = 6, plotlyOutput("annees")),
                 ),
                 fluidRow(
                   column(width = 6, tableOutput("classement"))
                 ),
        ),
        tabPanel('Clusters',
                 tableOutput("mean_position"),
                 tableOutput("desc_ind"),
                 plotlyOutput("graph"),
                 tableOutput("carac_clusters")
        ),
        tabPanel('Rapport',
                 tags$h1('Rapport projet Data Mining'),
                 tags$i('Clément ALBA, Matteo LAMOURET & Loli BOUTILLIER - IS2A4'),
                 tags$br(),tags$br(),
                 tags$li("Présentation du jeu de données"),
                 "babababab", tags$br(),tags$br(),
                 tags$li("Problématique"),
                 "babababab", tags$br(),tags$br(),
                 tags$li("Méthode analytique"),
                 "abnaabbaba", tags$br(),tags$br(),
                 tags$li("Difficultés rencontrées"),
                 "hdhhfhf", tags$br(),tags$br(),
                 tags$li("Conclusion"),
                 "efhjhelhf"
        )
      )
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
    
    pcaPlayers <- PCA(players_data.selected, graph=FALSE)
    playersHC <- HCPC(pcaPlayers, graph=FALSE)
    
    playersDf <- transform(players_data.selected, cluster_name = paste("Cluster", playersHC$data.clust$clust))
    
    cluster_mean_position <- aggregate(Position ~ cluster_name, data = playersDf, FUN = mean)
    cluster_mean_position <- cluster_mean_position[order(cluster_mean_position$Position), ]
    
    cluster_mean_position
  })
  
  output$desc_ind <- renderTable({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    # players_data.selected <- subset(players_data.selected, select = -c(1))
    players_data.selected <- subset(players_data.selected, select = !names(players_data.selected) %in% c("position", "Position"))
    
    cols_to_remove <- grepl("minutes", colnames(players_data.selected))
    players_data.selected <- players_data.selected[, !cols_to_remove]
    
    pcaPlayers <- PCA(players_data.selected, graph=FALSE)
    playersHC <- HCPC(pcaPlayers, graph=FALSE)
    
    playersDf <- transform(players_data.selected, cluster_name = playersHC$data.clust$clust)
    
    clusters_numbers <- unique(playersDf$cluster_name)
    
    inds <- data.frame(matrix(nrow = length(clusters_numbers), ncol = 5))
    
    clusters <- c()
    
    for(i in 1:length(clusters_numbers)){
      inds_names <- rownames(as.data.frame(playersHC$desc.ind$para[[i]]))
      
      if(length(inds_names) < 5){
        elems_restants <- 5 - length(inds_names)
        for(j in elems_restants:5){
          inds_names[j] <- ""
        }
      }
      
      inds[i, ] <- inds_names
      clusters <- c(clusters, paste0("Cluster ", i))
    }
    
    inds$cluster_name <- clusters
    
    inds <- inds[, c(6, 1, 2, 3, 4, 5)]
    colnames(inds) <- c("cluster_name", "Ind 1", "Ind 2", "Ind 3", "Ind 4", "Ind 5")
    
    inds
  })
  
  # Afficher les statistiques des joueurs sélectionnés
  output$graph <- renderPlotly({
    players_data.selected <- subset(players_data, players_data$position == input$poste)
    players_data.selected <- subset(players_data.selected, select = !names(players_data.selected) %in% c("position", "Position"))
    
    cols_to_remove <- grepl("minutes", colnames(players_data.selected))
    players_data.selected <- players_data.selected[, !cols_to_remove]
    
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
      
      print(c_means)
      
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
  
  # Afficher le nombre de buts par équipe
  output$nbbuts <- renderPlotly({
    # Calcul du nb de buts par équipe
    total_but <- aggregate(goals ~ team, data = stat, FUN = sum)
    fig <- plot_ly(data = total_but,  x = ~reorder(team, -goals), y = ~goals, type = 'bar')
    fig <- fig %>% layout(title = "Nombre de buts par équipe",
                          xaxis = list(title = "Equipe"),
                          yaxis = list(title = "Nombre de buts"))
    fig
  })
  
  output$classement <- renderTable({
    position <- aggregate(team ~ Position, data = stat, FUN =  max)
    # Convertir la colonne Position en entier
    position$Position <- as.integer(position$Position)
    colnames(position)[2] <- "Equipe"
    position
  })
  
  # Affichage d'un plot sur les années de naissances
  output$annees <- renderPlotly({
    table_joueurs <- aggregate(player ~ birth_year, stat, FUN = length)
    
    fig <- plot_ly(data = table_joueurs,  x = ~birth_year, y = ~player, type = 'bar')
    fig <- fig %>% layout(title = "Nombre de joueurs par année de naissance",
                          xaxis = list(title = "Année"),
                          yaxis = list(title = "Nombre de joueurs"))
    fig
  })
  
  # Affichage d'un plot sur les tirs
  output$cartons <- renderPlotly({
    jaunes <- aggregate(shots ~ team, data = stat, FUN = sum)
    fig <- plot_ly(data = jaunes,  x = ~reorder(team, -shots), y = ~shots, type = 'bar')
    fig <- fig %>% layout(title = "Nombre de tirs par équipe",
                          xaxis = list(title = "Equipe"),
                          yaxis = list(title = "Nombre de tirs"))
    fig
  })
  
  # Affichage d'un plot sur les passes réussies
  output$passes <- renderPlotly({
    réussies <- aggregate(passes_completed ~ team, data = stat, FUN = sum)
    fig <- plot_ly(data = réussies,  x = ~reorder(team, -passes_completed), y = ~passes_completed, type = 'bar')
    fig <- fig %>% layout(title = "Nombre de passes réussies par équipe",
                          xaxis = list(title = "Equipe"),
                          yaxis = list(title = "Nombre de passes réussies"))
    fig
  })
  
  # Affichage d'un plot sur les progressive passes distances
  output$ppd <- renderPlotly({
    p <- aggregate(passes_progressive_distance ~ team, data = stat, FUN = sum)
    fig <- plot_ly(data = p,  x = ~reorder(team, -passes_progressive_distance), y = ~passes_progressive_distance, type = 'bar')
    fig <- fig %>% layout(title = "Distance de passes progressives par équipe",
                          xaxis = list(title = "Equipe"),
                          yaxis = list(title = "Distance"))
    fig
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)
