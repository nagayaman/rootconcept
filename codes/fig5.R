###############################################################################
## This script is to reproduce the results shown in Fig 5.
## The Effects of Node Removal to Assess Influence-Contagion Associations
###############################################################################

rm(list = ls())

#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"igraph" #ver: 1.2.6
  ,"cowplot"
)

# Change to install = TRUE to install the required packages
pacman::p_load(packages, character.only = TRUE, install = T)

#Load data
focal_origin = read.csv("data/artist_edgelist.csv")
df.nw.dmc = read.csv("data/collaborators_dmc.csv")
df.nw.nwa = read.csv("data/collaborators_nwa.csv")
followers.dmc = read.csv("data/followers_dmc.csv")
followers.nwa = read.csv("data/followers_nwa.csv")

# Create influence networks excluding DMC collaborators
focal_origin.dmc = focal_origin %>% rename(artist_name_1 = focal) %>% 
  anti_join(df.nw.dmc, by = "artist_name_1") %>% rename(focal = artist_name_1) %>% 
  rename(artist_name_1 = origin) %>% anti_join(df.nw.dmc, by = "artist_name_1") %>% rename(origin = artist_name_1) 

# Create influence networks excluding NWA collaborators
focal_origin.nwa = focal_origin %>% rename(artist_name_1 = focal) %>% 
  anti_join(df.nw.nwa, by = "artist_name_1") %>% rename(focal = artist_name_1) %>% 
  rename(artist_name_1 = origin) %>% anti_join(df.nw.dmc, by = "artist_name_1") %>% rename(origin = artist_name_1) 

# Rate of collaborators with influence /followers
length(intersect(followers.dmc$focal, df.nw.dmc$artist_name_1))/nrow(followers.dmc)
length(intersect(followers.nwa$focal, df.nw.nwa$artist_name_1))/nrow(followers.nwa)
# dmc: 0.289
# nwa: 0.229

# Rate of collaborators with influence /collaborators
length(intersect(followers.dmc$focal, df.nw.dmc$artist_name_1))/nrow(df.nw.dmc)
length(intersect(followers.nwa$focal, df.nw.nwa$artist_name_1))/nrow(df.nw.nwa)
# dmc: 0.226
# Nwa: 0.296

# Measure the largest path length, the average path-length, and the number of communities
g = graph.data.frame(focal_origin)
original.diameter = diameter(g, directed = T, weights = NA)
original.path = average.path.length(g, directed = TRUE, unconnected = TRUE)

g = graph.data.frame(focal_origin, directed = F)
original.community = cluster_louvain(g, weights = NULL)
original.community = length(unique(original.community$membership))

# The number of collaborators with RD are 146
random.diameter = c() # Largest path length
random.path = c() # Average path length
random.community = c() # Number of communities

set.seed(1)
for(i in 1:10000){
  removals = data.frame(artist_name_1 = sample(unique(focal_origin$origin), nrow(df.nw.dmc)))
  remove.network = focal_origin %>% rename(artist_name_1 = focal) %>% 
    anti_join(removals, by = "artist_name_1") %>% rename(focal = artist_name_1) %>% 
    rename(artist_name_1 = origin) %>% anti_join(removals, by = "artist_name_1") %>% rename(origin = artist_name_1)
  
  g = graph.data.frame(remove.network, directed = T)
  random.path = c(random.path ,average.path.length(g, directed = TRUE, unconnected = TRUE))
  random.diameter = c(random.diameter, diameter(g, directed = T, weights=NA))
  g = graph.data.frame(remove.network, directed = F)
  temp2 = cluster_louvain(g, weights = NULL)
  random.community = c(random.community, length(unique(temp2$membership)))
}

g = graph.data.frame(focal_origin.dmc)
dmc.diameter = diameter(g, directed = T, weights = NA)
dmc.path = average.path.length(g, directed = TRUE, unconnected = TRUE)
g = graph.data.frame(focal_origin.dmc, directed = F)
dmc.community = cluster_louvain(g, weights = NULL)
dmc.community = length(unique(dmc.community$membership))

# Removals of collaborators with NWA (n=162)
random2.diameter = c()
random2.path = c()
random2.community = c()

set.seed(1)
for(i in 1:10000){
  removals = data.frame(artist_name_1 = sample(unique(focal_origin$origin), nrow(df.nw.nwa)))
  remove.network = focal_origin %>% rename(artist_name_1 = focal) %>% 
    anti_join(removals, by = "artist_name_1") %>% rename(focal = artist_name_1) %>% 
    rename(artist_name_1 = origin) %>% anti_join(removals, by = "artist_name_1") %>% rename(origin = artist_name_1)
  
  g = graph.data.frame(remove.network)
  
  random2.diameter = c(random2.diameter, diameter(g, directed = T, weights = NA))
  random2.path = c(random2.path ,average.path.length(g, directed = TRUE, unconnected = TRUE))
  g = graph.data.frame(remove.network, directed = F)
  temp2 = cluster_louvain(g, weights = NULL)
  random2.community = c(random2.community, length(unique(temp2$membership)))
}

g = graph.data.frame(focal_origin.nwa)
nwa.diameter = diameter(g, directed = T, weights = NA)
nwa.path = average.path.length(g, directed = TRUE, unconnected = TRUE)
g = graph.data.frame(focal_origin.nwa, directed = F)
nwa.community = cluster_louvain(g, weights = NULL)
nwa.community = length(unique(nwa.community$membership))

temp = data.frame(root = factor(c("Initial state", "Random (146)", "Random (162)", "Run-D.M.C.", "N.W.A"),
                                levels = c("N.W.A", "Run-D.M.C.", "Random (162)", "Random (146)","Initial state"))
                  ,diameter = c(original.diameter, mean(random.diameter), mean(random2.diameter), dmc.diameter, nwa.diameter)
                  ,ave.path = c(original.path ,mean(random.path), mean(random2.path), dmc.path, nwa.path)
                  ,community = c(original.community, mean(random.community), mean(random2.community), dmc.community, nwa.community)
)

g1 = ggplot(data = temp, aes(x = root, y = diameter)) +
  geom_bar(stat = "identity"
           ,fill = "white"
           ,colour = "black"
  )+
  geom_text(aes(label = round(diameter, digit = 2)), hjust = -0.3, size = 5)+
  labs(x = "", y = "") +
  ylim(0, 20) +
  theme_bw() +
  theme_linedraw() +
  coord_flip() +
  theme(text=element_text(size=14)
        ,axis.text = element_text(size=14)) +
  labs(title = "")
plot(g1)

g2 = ggplot(data = temp, aes(x = root, y = ave.path)) +
  geom_bar(stat = "identity"
           ,fill = "white"
           ,colour = "black"
  ) +
  geom_text(aes(label = round(ave.path, digit = 2)), hjust = -0.3, size = 5) +
  labs(x = "", y = "") +
  ylim(0,5) +
  theme_bw() +
  theme_linedraw() +
  coord_flip() +
  theme(text=element_text(size=14)
        ,axis.text=element_text(size=14)) +
  labs(title = "")

g3 = ggplot(data = temp, aes(x = root, y = community))+
  geom_bar(stat = "identity"
           ,fill = "white"
           ,colour = "black"
  )+
  geom_text(aes(label = round(community, digit = 2)), hjust = -0.3, size = 5)+
  labs(x = "", y = "") +
  ylim(0, 90) +
  theme_bw() +
  theme_linedraw() +
  coord_flip() +
  theme(text = element_text(size=14)
        ,axis.text = element_text(size=14)) +
  labs(title = "")
plot(g3)

# Cumulative distributions
robustness = list()
robustness[[1]] = random.diameter
robustness[[2]] = random.path
robustness[[3]] = random.community
robustness[[4]] = random2.diameter
robustness[[5]] = random2.path
robustness[[6]] = random2.community

robustness_figure = c(paste0(c("diameter", "avepath", "community"), "_dmc")
                      ,paste0(c("diameter", "avepath", "community"), "_nwa"))

robustness.root = c(dmc.diameter, dmc.path, dmc.community
                    ,nwa.diameter, nwa.path, nwa.community)


# Measure the cumulative distributions
f = list()
for(i in 1:6){
  random.value = sort(robustness[[i]])
  # Find the nearest value
  nearest = which(abs(random.value - robustness.root[i]) == min(abs(random.value - robustness.root[i])))
  
  temp = data.frame(random.value = random.value)
  f[[i]] = ggplot(temp, aes(random.value)) + 
    stat_ecdf(geom = "step") + 
    theme_bw() +
    theme(text=element_text(size=14)
          ,axis.text=element_text(size=14)
    )+
    geom_vline(xintercept = random.value[nearest], colour = "black", size = 1) +
    labs(x = ""
         ,y = ""
         ,title = ""
    )
  print(robustness_figure[i])
  print(max(nearest/length(random.value)))
}  
g = plot_grid(g1, g2, g3, f[[1]], f[[2]], f[[3]], f[[4]], f[[5]], f[[6]]
              ,ncol = 3
              ,label_size = 12
              ,label_x = 0
              ,align = "v"
              ,labels = c("(A) Largest path length", "(B) Average path length", "(C) Detected communites"
                          , "(D) Largest path length: Run-D.M.C."
                          , "(E) Average path length: Run-D.M.C."
                          , "(F) Detected communites: Run-D.M.C."
                          , "(G) Largest path length: N.W.A"
                          , "(H) Average path length: N.W.A" 
                          , "(I) Detected communites: N.W.A"
              )
)
print(g)
ggsave("Fig5.png", g, width = 15, height = 8, dpi=300)