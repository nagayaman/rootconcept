#####################################################################################
## Supplemental info fig 2. Distance Between Root Concepts' Topic Vectors
#####################################################################################
rm(list = ls())

#install.packages("pacman")
packages = c(
  "tidyverse"
)
pacman::p_load(packages, character.only = TRUE, install = T)

df.topic = read.csv("data/album_topic.csv")

topic.dist =  list()
for(i in 1:nrow(df.topic)){
  temp2 = NULL
  for(j in (i+1):nrow(df.topic)){
    temp = dist(rbind(as.numeric(df.topic[i,c(4:7)]), as.numeric(df.topic[j,c(4:7)])), method = "euclidean")
    temp2 = c(temp2, temp)
  }
  print(i)
  topic.dist[[i]] = temp2
}
topic.dist = Reduce(c, topic.dist) # list of vectors into one vector
topic.dist = topic.dist[!is.na(topic.dist)] 
topic.dist = sort(topic.dist)
length(topic.dist)
# Number of album pairs are 19753756

# Identify the percentile/quantile
DMC = filter(df.topic, album_name == "Run-D.M.C.")[,4:7]
NWA = filter(df.topic, album_name == "Straight Outta Compton")[,4:7]
root_dist = dist(rbind(DMC, NWA), method = "euclidean")
root_dist = as.numeric(root_dist)
# Topic distance between the roots is 0.175
root_percentile = which(topic.dist == root_dist)/length(topic.dist)
# percentile = 0.896

mean(topic.dist)
# Mean topic distance is 0.105

t.test(topic.dist, mu = root_dist)


temp = as.data.frame(topic.dist)

# Cumulative distribution
g = ggplot(temp, aes(log(topic.dist + 1))) + 
  stat_ecdf(geom = "step") + 
  theme_bw() +
  theme(text=element_text(size=14)
        ,axis.text=element_text(size=12)
  ) +
  geom_vline(xintercept = log(root_dist+1), colour = "black") +
  geom_vline(xintercept = log(mean(temp$topic.dist)+1), linetype="dashed", colour = "black") +
  labs(x = ""# "Euclidean Distance of Topic Vector between Run-D.M.C. and N.W.A"
       ,y = ""
       # ,caption = paste0("Number of Album Pairs: ", nrow(temp))
  )
# print(g)
#ggsave("Figure5_topic_distance.png", g, width = 7, height = 5, dpi = 300)
ggsave("S2_Fig.png", g, width = 5, height = 5, dpi = 300)


