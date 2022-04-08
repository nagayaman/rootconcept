################################################################################
## This script is to reproduce the results shown in S1 Table and S1 Fig.
## Descriptive Statistics of Influence and Novelty
## Cumulative Distribution of the Root Concepts’Influence and Novelty
################################################################################

rm(list = ls())
#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"gtools"
  ,"cowplot"
)
pacman::p_load(packages, character.only = TRUE, install = T)


# Load data
df.mood.pair.novel = read.csv("data/album_novelty.csv")
df.3d_influence = read.csv("data/artist_novelty_influence.csv")

# The mean of the number of novel pairs of all albums
df.novel = as.data.frame(psych::describe(df.mood.pair.novel[-1:-3])) %>% 
  dplyr::select(n, mean, sd, min, max)

# The root concepts candidates
temp = df.mood.pair.novel %>% filter(year == 1984, artist_name_1 == "Run-D.M.C.")
df.novel$dmc = t(temp[,4:6])
temp = df.mood.pair.novel %>% filter(year == 1988, artist_name_1 == "N.W.A")
df.novel$nwa = t(temp[,4:6])

# Identify the percentile/quantile for canditate roots
n_albums = nrow(df.mood.pair.novel)

for(i in 4:6){
  # 4: n_moods
  # 5: n_pair
  # 6: novel_pairs
  
  # Need to set ascending order to compute the percentile at the distribution
  temp = sort(df.mood.pair.novel[, i])
  t = i - 3
  df.novel[t, 8] = max(which(temp == df.novel$dmc[t])/n_albums)
  df.novel[t, 9] = max(which(temp == df.novel$nwa[t])/n_albums)
}

colnames(df.novel)[8:9] = paste0(c("RD", "NWA"),"_percentile")
df.novel = round(df.novel, 3)
df.novel = df.novel %>% 
  mutate(vars = rownames(df.novel)) %>% 
  select(vars, everything())
df.novel = df.novel[1:3, ]

df.temp2 = df.3d_influence %>% select(n_followers, pagerank)
df.influence2 = as.data.frame(psych::describe(df.temp2)) %>% 
  dplyr::select(n, mean, sd, min, max)

# Root concepts candidates
temp = df.3d_influence %>% filter(year == 1984, artist_name_1 == "Run-D.M.C.") %>% select(n_followers, pagerank)
df.influence2$dmc = t(temp)
temp = df.3d_influence %>% filter(year == 1988, artist_name_1 == "N.W.A") %>% select(n_followers, pagerank)
df.influence2$nwa = t(temp)

# Identify the percentile/quantile for canditate roots
n_artists = nrow(df.temp2)
df.temp2 = as.data.frame(df.temp2)
for(i in 1:2){
  # 1: n_followers
  # 2: pagerank
  
  # Need to set ascending order to compute the percentile at the distribution
  temp = sort(as.vector(df.temp2[, i]))
  df.influence2[i, 8] = max(which(temp == df.influence2$dmc[i])/n_artists)
  df.influence2[i, 9] = max(which(temp == df.influence2$nwa[i])/n_artists)
}

colnames(df.influence2)[8:9] = paste0(c("RD", "NWA"),"_percentile")
df.influence2 = round(df.influence2, 3)
df.influence2 = df.influence2 %>% 
  mutate(vars = rownames(df.influence2)) %>% 
  select(vars, everything())

df.influence2 = rbind(df.novel, df.influence2)

write.csv(df.influence2, "S1_table.csv", row.names = F)


# Reproduce the results of S1 Fig.
df.temp = df.3d_influence %>% filter(!is.na(n_followers))
df.dmc.influence = df.temp %>% filter(artist_name_1 == "Run-D.M.C.")
df.nwa.influence = df.temp %>% filter(artist_name_1 == "N.W.A")

g1 = ggplot(df.temp, aes(log(n_followers+1))) + 
  stat_ecdf(geom = "step") +
  theme_bw() +
  theme(text=element_text(size=14)
        ,axis.text=element_text(size=12)
  ) +
  geom_vline(xintercept = log(df.dmc.influence$n_followers[1]+1),  colour = "black") +
  geom_vline(xintercept = log(df.nwa.influence$n_followers[1]+1), linetype = "dashed", colour = "black") +
  labs(x = "In-Degree Centrality"
       ,y = ""
       ,subtitle = ""
       ,caption = paste0("Number of Artists: ", nrow(df.temp))
  )

g2 = ggplot(df.temp, aes(log(pagerank+1))) + 
  stat_ecdf(geom = "step") +
  theme_bw() +
  theme(text=element_text(size=14)
        ,axis.text=element_text(size=12)
  ) +
  geom_vline(xintercept = log(df.dmc.influence$pagerank[1]+1),  colour = "black") +
  geom_vline(xintercept = log(df.nwa.influence$pagerank[1]+1), linetype = "dashed", colour = "black") +
  labs(x = "PageRank Centrality"
       ,y = ""
       ,subtitle = ""
       ,caption = paste0("Number of Artists: ", nrow(df.temp))
  )

g3 = ggplot(df.mood.pair.novel, aes(log(novel_pairs+1))) + 
  stat_ecdf(geom = "step") +
  theme_bw() +
  theme(text=element_text(size=14)
        ,axis.text=element_text(size=12)
  ) +
  geom_vline(xintercept = log(df.novel$dmc[3]+1), colour = "black") +
  geom_vline(xintercept = log(df.novel$nwa[3]+1), linetype = "dashed", colour = "black") +
  labs(x = "Novel Mood Pairs"
       ,y = ""
       ,subtitle = ""
       ,caption = paste0("Number of Albums: ", nrow(df.mood.pair.novel))
  )

g = plot_grid(g1, g2, g3
              ,ncol = 3
              ,label_size = 12
              ,labels = c("(A)", "(B)", "(C)")
              ,align = "v"
)
print(g)
ggsave("S1_fig.png", g, width = 8, height = 4, dpi = 300)
