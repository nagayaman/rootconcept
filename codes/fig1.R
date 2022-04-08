###########################################################################################
## This script is to reproduce the results shown in Fig 1.
## Fig 1. 3D scatter plot of influence and novelty of rappers’ first albums (N = 1,154).
###########################################################################################

rm(list = ls())
#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"scatterplot3d"
)
pacman::p_load(packages, character.only = TRUE, install = T)

# Load data
df.3d_influence = read.csv("data/artist_novelty_influence.csv")

# Exporting the 3D plot image
rappers = df.3d_influence$artist_name_1
selected_rappers = c("Run-D.M.C.", "Dr. Dre", "N.W.A", "Afrika Bambaataa", "Public Enemy", "Eminem"
                     ,"Kurtis Blow", "Ice-T", "Eric B. & Rakim", "The Sugarhill Gang", "Geto Boys"
                     , "Grandmaster Flash", "LL Cool J", "The Last Poets" ,"Beastie Boys"
)
rappers[!rappers %in% selected_rappers] = ""

setEPS()
postscript("Fig1.eps")
s3d = scatterplot3d(x = df.3d_influence$n_followers
                    ,y = df.3d_influence$pagerank
                    ,z = df.3d_influence$novel_pairs
                    ,pch = 16
                    ,highlight.3d = TRUE
                    ,type = "h"
                    ,box = F
                    ,angle = 45
                    ,scale.y = .55
                    ,xlab = "In-Degree Centrality"
                    ,ylab = "PageRank Centrality"
                    ,zlab = "Novel Mood Pairs"
)
text(s3d$xyz.convert(x = df.3d_influence$n_followers, y = df.3d_influence$pagerank, z = df.3d_influence$novel_pairs)
     ,labels = rappers
     ,cex= 0.7
     ,pos = 4
     ,col = "black")
dev.off()
