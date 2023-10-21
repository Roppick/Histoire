#Test Sankay
library(worldfootballR)
library(tidyverse)
library(dplyr)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(chorddiag)
library(showtext)
library(magick)
library(grid)

#**************************RENNES******************************
SRFC_url <- c("https://fbref.com/en/squads/b3072e00/2022-2023/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2021-2022/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2020-2021/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2019-2020/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2018-2019/all_comps/Rennes-Stats-All-Competitions",
              "https://fbref.com/en/squads/b3072e00/2017-2018/all_comps/Rennes-Stats-All-Competitions"
)
buts_SRFC <- fb_team_goal_logs(team_urls = SRFC_url, for_or_against = "for")
# view(buts_SRFC)

# Nettoyer le tableau pour ne garder que les dates, les buteurs et les passeurs
# buts_SRFC
buts_SRFC_Nettoyé <- buts_SRFC[,c("Scorer","Assist")]
# view(buts_SRFC_Nettoyé)

# Supprimer les prénoms
buts_SRFC_Nettoyé_nom <- buts_SRFC_Nettoyé
buts_SRFC_Nettoyé_nom[] <- lapply(buts_SRFC_Nettoyé, function(x) sub(".*\\b(\\w+)$", "\\1", x))

# Remplacer les passeurs vides par "Pas de passeur décisif"---------------------
buts_SRFC_Nettoyé_2 <- buts_SRFC_Nettoyé_nom
buts_SRFC_Nettoyé_2[buts_SRFC_Nettoyé_2 == c("","")] <- "Pas de passeur décisif"

# Remplacer les buteurs CSC par "CSC--------------------------------------------
buts_SRFC_Nettoyé_3 <- buts_SRFC_Nettoyé_2
buts_SRFC_Nettoyé_3[apply(buts_SRFC_Nettoyé_3, 1:2, function(i) grepl('\\OG', i))] <- "CSC"
# view(buts_SRFC_Nettoyé_3)

# Réparer Nom Composés
buts_SRFC_Nettoyé_4 <- buts_SRFC_Nettoyé_3
buts_SRFC_Nettoyé_4[apply(buts_SRFC_Nettoyé_3, 1:2, function(i) grepl('\\Castillo', i))] <- "Del Castillo"
buts_SRFC_Nettoyé_4[apply(buts_SRFC_Nettoyé_3, 1:2, function(i) grepl('\\Ekambi', i))] <- "Toko Ekambi"
buts_SRFC_Nettoyé_4[apply(buts_SRFC_Nettoyé_4, 1:2, function(i) grepl('\\Arfa', i))] <- "Ben Arfa"
buts_SRFC_Nettoyé_4[apply(buts_SRFC_Nettoyé_4, 1:2, function(i) grepl('\\Silva', i))] <- "Da Silva"
buts_SRFC_Nettoyé_4[apply(buts_SRFC_Nettoyé_4, 1:2, function(i) grepl('\\Siliki', i))] <- "Léa Siliki"
# view(buts_SRFC_Nettoyé_4)

#Trouver le listing des buteurs-------------------------------------------------
buteurs <- unique(buts_SRFC_Nettoyé_4$Scorer)
# view(buteurs)

#Trouver le listing des passeurs------------------------------------------------
passeurs <- unique(buts_SRFC_Nettoyé_4$Assist)
table(passeurs)
# view(passeurs)

#Trouver le listing des passeurs et/ou buteurs----------------------------------
passbut <- c(buts_SRFC_Nettoyé_4$Scorer,buts_SRFC_Nettoyé_4$Assist)
passbut <- unique(passbut)
# view(passbut)

#Trouver le nombre de buts par buteurs------------------------------------------
# xtabs(~Scorer,data = buts_SRFC_Nettoyé_4)

#Trouver le nombre de passes par passeur------------------------------------------
# xtabs(~Assist,data = buts_SRFC_Nettoyé_5)

#Combo des deux
Combo_but_pass_SRFC <- xtabs(~Assist + Scorer,data = buts_SRFC_Nettoyé_4)
# view(Combo_but_pass_SRFC)

#Transformer la sortie de xtabs en un tableau
tableau_complet <- as.data.frame.matrix(Combo_but_pass_SRFC)[unique(buts_SRFC_Nettoyé_4$Assist),]
data.frame(Scorer = row.names(tableau_complet), tableau_complet, row.names = NULL)
# view(tableau_complet)
# write.csv(x= tableau_complet, file = "Passeurs_Buteurs.csv", row.names = TRUE)

#Tentative de comptage de G+PD
Nb_Buts_Buteurs<-colSums(tableau_complet)
Nb_Passes_Passeurs<-rowSums(tableau_complet)
Nb_Buts_Buteurs_tableau<-data.frame(as.list(Nb_Buts_Buteurs))
Nb_Passes_Passeurs_tableau<-data.frame(as.list(Nb_Passes_Passeurs))
Nb_Buts_Passes<-merge(Nb_Passes_Passeurs_tableau,Nb_Buts_Buteurs_tableau,all=TRUE)
Nb_Buts_Passes[is.na(Nb_Buts_Passes)]<-0
Nb_Buts_Passes<-colSums(Nb_Buts_Passes)
Nb_Buts_Passes_tableau<-data.frame(as.list(Nb_Buts_Passes))
# view(Nb_Buts_Passes_tableau)

#Définir la taille du kiki
kiki<-10

#Trouver et supprimer les petits kiki
petits_kikis<-c(names(Nb_Buts_Passes_tableau)[apply(Nb_Buts_Passes_tableau<kiki,FUN=sum,MARGIN=2)>0])

#Supprimer point dans la liste des joueurs car pose problème ensuite
petits_kikis<-gsub(".", " ", petits_kikis, fixed=TRUE)
# view(petits_kikis)

# Supprimer les petits kikis (ceux qui n'ont pas bcp de G+PD)
buts_SRFC_Nettoyé_5 <- buts_SRFC_Nettoyé_4
buts_SRFC_Nettoyé_5$Assist[buts_SRFC_Nettoyé_5$Assist %in% petits_kikis]<-"Autres"
buts_SRFC_Nettoyé_5$Scorer[buts_SRFC_Nettoyé_5$Scorer %in% petits_kikis]<-"Autres"
# view(buts_SRFC_Nettoyé_5)

#Combo des deux
Combo_but_pass_SRFC_2 <- xtabs(~Assist + Scorer,data = buts_SRFC_Nettoyé_5)
# view(Combo_but_pass_SRFC_2)

#Transformer la sortie de xtabs en un tableau
tableau_complet_2 <- as.data.frame.matrix(Combo_but_pass_SRFC_2)[unique(buts_SRFC_Nettoyé_5$Assist),]
data.frame(Scorer = row.names(tableau_complet_2), tableau_complet_2, row.names = NULL)
# view(tableau_complet_2)
# write.csv(x= tableau_complet_2, file = "Passeurs_Buteurs_2.csv", row.names = TRUE)

#Trouver le listing des passeurs et/ou buteurs après nettoyage------------------
passbut_2 <- c(buts_SRFC_Nettoyé_5$Scorer,buts_SRFC_Nettoyé_5$Assist)
passbut_2 <- unique(passbut_2)
# view(passbut)

#Trouver l'ordre pour ranger les joueurs
Nb_Buts_Passes_ranger<-sort(Nb_Buts_Passes, decreasing = TRUE)
gros_kikis<-names(Nb_Buts_Passes_ranger)[Nb_Buts_Passes_ranger>(kiki-1)]
gros_kikis_complet<-c(gros_kikis,"Autres")
# view(gros_kikis_complet)

#Supprimer point dans la liste des joueurs car pose problème ensuite
gros_kikis_complet<-gsub(".", " ", gros_kikis_complet, fixed=TRUE)

#---------------------------Représentation Graphique----------------------------
# 
# font_import()
# 
# # Charger la police
# font_import(paths = "C:\Windows\Fonts")

circos.clear()

font.add("CMU Serif", "cmunrm.otf")
font_paths()
showtext.auto()
font_add(family = "CMU Serif", 
         regular = "cmunrm.otf",
         bold = "cmunbx.otf")
showtext_auto()
# fonts

# dev.off() #--> peut résoudre certains problème d'affichage instantané sur RStudio

# Reste à ajutser la taille du graphique d'export
png(filename = "~/Documents/Stade Rennais/02_Code R/Sankey_22-23/23-10-21_Sankey_Circulaire_5_ans_Bouri_Buts.png",
    width = 10000,
    height = 10000)

circos.clear()
circos.par(start.degree = 90, gap.degree = 3,
           track.margin = c(-0.33, 0.33), #aide à faire de la marge juste pour la zone graphique et donc que la légende ne soit pas rognée
           points.overflow.warning = FALSE)
par(#mar = rep(0, 4),
  # Attention à l'odre, par exemple avoir pin avant mai, ou l'inverse, ça ne donne pas le même résultat
    # adj=0.5,
    family="CMU Serif",
    font=2,
    #mfrow=c(1,1), #Correspond au nombre de graphe 1X2 graphs par exemple c(1,2)
    #oma=c(10,10,5,0),  # Permet de jouer sur le centrage (bas,gauche,haut,droite) dans le cas où on a plusieurs graphs
    mar=c(4,4,2,1)+.1,  # Cela ajuste les marges du graphique (bas, gauche, haut, droite). Vous avez défini ces marges à 4, 4, 2, 1 plus une petite augmentation pour chaque. Ces valeurs contrôlent la quantité d'espace entre les bords du graphique et le contenu du graphique.
    pin=c(120,120), #taille en pouce de la zone de traçage pour le graphe
    mai=c(4,5,13,5),#c(bottom, left, top, right)
    bg = "#f8f7f5", #Couleur de fond
    cex.main = 25,
    lheight = 1.1)  


#---------------------------Couleurs----------------------------

# Pour avoir des couleurs aléatoires
# mycolor <- viridis(length(passbut_2), alpha = 1, begin = 0, end = 1, option = "D")
# mycolor <- mycolor[sample(1:length(passbut_2))]

# Pour choisir précisemment ses couleurs
# mycolor_SRFC <- c("#0000001A","#c00000cc","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A","#0000001A")

# Nota sur les couleurs
# Rouge SRFC      c00000, c00000cc(80% de transpa.)
# Noir SRFC       000000, 00000040(25% de transpa.), 0000001A(10% de transpa.)
# Bleu entre deux 8da9db
# site pour convertire les couleurs https://web-color.aliasdmc.fr/couleur-web-red-avec-transparence.html

# Pour faire un focus sur un joueur
joueur_focus <- "Bourigeaud"

# Faire le focus sur le pourtour
position_joueur_focus <- match(joueur_focus,gros_kikis_complet)
nouvelles_couleurs <- vector( "character" , length(passbut_2))
nouvelles_couleurs <- rep("#0000001A",times=length(passbut_2))
nouvelles_couleurs[position_joueur_focus]<-"#c00000cc"

#Faire le focus dans le centre, pour les passes
position_joueur_focus_matrix <- match(joueur_focus,row.names(Combo_but_pass_SRFC_2))
nouvelles_couleurs_passes <- vector( "character" , length(row.names(Combo_but_pass_SRFC_2)))
nouvelles_couleurs_passes <- rep("#0000001A",times=length(row.names(Combo_but_pass_SRFC_2)))
nouvelles_couleurs_passes[position_joueur_focus_matrix]<-"#c00000cc"

#Diagramme Basique
chordDiagram(Combo_but_pass_SRFC_2,
             grid.col = nouvelles_couleurs,
             # row.col = nouvelles_couleurs_passes, # dans le cas où on se focus sur les passes
             column.col = nouvelles_couleurs_passes, # dans le cas où se focus sur les buts
             transparency = 0.4,
             link.lwd = 1,    # Line width
             link.lty = 5,    # Line type
             # link.border = 1,
             order = gros_kikis_complet,
             directional = 1,
             direction.type = c("arrows", "diffHeight"), 
             diffHeight  = -0.02,
             annotationTrack = "grid", #semble supprimer la légende
             annotationTrackHeight = c(0.02, 0.05), #change la hauteur de la zone pour chaque joueur
             link.arr.type = "big.arrow", 
             link.sort = TRUE, 
             link.largest.ontop = TRUE) # Border color

#Ajustement du Graph
circos.trackPlotRegion(
  # Truc Générique
  track.index = 1, 
  bg.border = NA, 
  panel.fun = function(x, y) {
    # Trucs pour les noms sur les secteurs
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    xplot = get.cell.meta.data("xplot")
    sector.index = get.cell.meta.data("sector.index")
    
    # Nouvelle version
    if(abs(xplot[2] - xplot[1]) < 9) {
      circos.text(
        x = mean(xlim),
        y = 3, #écartement de la légend du cercle, 2.9 par défaut
        labels = sector.index,
        facing = "clockwise",
        adj = c(0, 0.5), #ajuste la position de la légende: première valeur vers le centre du graphe en +;
        cex = 10, #taille des noms de secteurs
        niceFacing = TRUE 
        )
    } else {
      circos.text(
        x = mean(xlim),
        y = 6, #écartement de la légend du cercle, 2.9 par défaut
        labels = sector.index,
        facing = "bending.outside",
        adj = c(0.5, 0), #ajuste la position de la légende: première valeur vers le centre du graphe en +;
        cex = 10, #taille de police des noms de secteurs
        niceFacing = TRUE
      )
    }
    # Exemple
    # if(abs(xplot[2] - xplot[1]) < 10) {
    #     circos.text(mean(xlim),
    #     ylim[1],
    #     sector.name,
    #     facing = "clockwise",
    #     niceFacing = TRUE,
    #     adj = c(0, 0.5),
    #     col = "red")
    #  } else {
    #     circos.text(mean(xlim),
    #     ylim[1],
    #     sector.name,
    #     facing = "inside",
    #     niceFacing = TRUE,
    #     adj = c(0.5, 0),
    #     col= "blue")
    # }
    
    # Ancienne version
    # circos.text(
    #   x = mean(xlim), 
    #   y = 3, #écartement de la légend du cercle, 2.9 par défaut
    #   labels = sector.index,
    #   facing = "clockwise",
    #   adj = c(0, 0.5), #ajuste la position de la légende: première valeur vers le centre du graphe en +;
    #   cex = 10, #taille des noms de secteurs
    #   niceFacing = TRUE #--> je ne vois pas ce que ça change
    # )

    # Trucs pour les graduation
    circos.axis(
      h = "top",
      labels.pos.adjust = c(0,0),
      major.tick = TRUE,
      # labels.pos.adjust = TRUE,
      # major.at = seq(from = 0, to = xlim[2], by = ifelse(test = xlim[2]>10, yes = 20, no = 10)), #permet de définir les graduations, test si la taille de la zone >10°, si oui, alors on mets un point tous les 20, sinon tous les 10
      major.at = c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160),
      minor.ticks = 4, #Number of minor ticks between two close major ticks.
      major.tick.length = 0.5, #Length of the major ticks, measured in "current" data coordinate
      labels.niceFacing = TRUE,
      labels.cex = 6, #taille de police pour la légende (chiffres)
      labels.font = 1,
      lwd = 5) #Line width for ticks.
  }
)

montitre = "Benjamin Bourigeaud - Stade Rennais FC - Saisons 17/18 à 22/23"
monbasdegraph = "@Roppick            Roppick            @roppick.bsky.social"
monsoustitre = "Diagramme de type flux circulaire (ou de Sankey) donnant les buts (B) et les passes décisives (PD) des joueurs du Stade Rennais,\npour les saisons 17/18 à 22/23, toutes compétitions confondues. Seuls les joueurs ayant plus de 10x(B+PD) sont affichés. Les autres \nsont regroupés dans \"Autres\". Les chiffres autour du cercle donnent le nombre de B+PD pour chacun des joueurs. Au centre, une flèche\nsortante indique une passe décisive, une flèche entrante un but, la taille de la flèche est proportionnelle au flux associé."
monsoussoustitre = "Le focus est fait en rouge sur les buts de B. Bourigeaud."
mtext(side=3, #3 pour le top
      line=22,
      # at=0, #semble indiquer où positionner le texte, mais peut foutre le bordel dans l'ajustement ensuite
      adj=0, #Centrage
      cex=15, #Taille de la police
      montitre)

## Sous-Titre ##
mtext(side=3, #3 pour le top
      line=-20, 
      # at=-0.07,
      adj=0,
      cex=9,
      font = 1,
      monsoustitre)

## Sous-Sous-Titre
mtext(side=3, #3 pour le top
      line=-32, 
      # at=-0.07,
      adj=0,
      cex=9,
      monsoussoustitre)

## Pied de page ##
mtext(side=1, line=8, 
      # at=-0.07,
      adj=0.5, cex=7, col = "#747474", monbasdegraph)

## Insérer des images ##

# # Avec ReadPNG --> ne marche pas
# logotwitter <- readPNG("~/Documents/Stade Rennais/Ressources/Twitter_gris.png")
# rasterImage(logotwitter,200, 200, 200, 200,
#             side=1, line=-2,
#             # at=-0.07,
#             adj=0.5, cex=10, col = "#747474", monbasdegraph)
#
# # avec la library "Magick"

# Twitter
logotwitter <- image_read("~/Documents/Stade Rennais/Ressources/Twitter_gris.png")
image_scale(logotwitter, "600") # échelle
image_trim(logotwitter) #supprimer le fond pour un png

#Github
logogithub <- image_read("~/Documents/Stade Rennais/Ressources/Github_gris.png")
image_scale(logogithub, "600") # échelle
image_trim(logogithub) #supprimer le fond pour un png

#Bluesky
logobluesky <- image_read("~/Documents/Stade Rennais/Ressources/Bluesky_gris.png")
image_scale(logobluesky, "600") # échelle
image_trim(logobluesky) #supprimer le fond pour un png

# # Mélanger les images
grid.raster(logotwitter,
            x = 0.328, # latéral, augmenter pour aller à droite
            y = 0.022, # hauteur, augmenter pour aller vers le haut
            width = unit(200, "points"))

grid.raster(logogithub,
            x = 0.434, # latéral, augmenter pour aller à droite
            y = 0.022, # hauteur
            width = unit(200, "points"))

grid.raster(logobluesky,
            x = 0.534, # latéral, augmenter pour aller à droite
            y = 0.022, # hauteur
            width = unit(150, "points"))

# image_noise(logotwitter) #ajoute du grain
# print(logotwitter, info = FALSE) #affiche l'image dans le viewer

dev.off() # Finally, close the “device”, or file

