###################################################################################
## Reproducing the results of topic modeling using music moods
###################################################################################
rm(list = ls())

#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"tm" 
  ,"topicmodels"
  ,"ldatuning"
  ,"gridExtra"
)
pacman::p_load(packages, character.only = TRUE, install = T)

# Load data
df.album = read.csv("data/albums.csv")

# Topic modeling with Moods
df.temp = df.album %>% dplyr::select(album_name, contains("album_mood_"))
df.temp$moods = NULL
df.temp$moods = apply(df.temp[,2:length(df.temp)], 1,paste, collapse = " ")
df.temp$moods = gsub(" +$", "", df.temp$moods) # delete all empty elements in the end of words
df.temp = df.temp %>% mutate(moods = str_replace_all(moods, " NA", ""))
df.temp = df.temp %>% dplyr::select(album_name, moods)

mood.source = VectorSource(df.temp$moods)
corpus = Corpus(mood.source)

dtm.mood = DocumentTermMatrix(corpus)
rownames(dtm.mood) = df.temp$album_name
freq = colSums(as.matrix(dtm.mood))
ord = order(freq,decreasing=TRUE)
df.mood.freq = data.frame(mood = names(freq), freq = freq)

# Identifying the optimal number of topics
# Set parameters
burnin = 1000
iter = 3000
thin = 500
nstart = 5
best = TRUE
k = seq(from = 3, to = 30, by = 1)

result = FindTopicsNumber(dtm.mood,
                          topics = k,
                          metrics = c("CaoJuan2009", "Deveaud2014"),
                          method = "Gibbs",
                          control = list(seed = 77),
                          mc.cores = 4L,
                          verbose = TRUE
)
FindTopicsNumber_plot(result)

# Set parameters
k = 4 # The number of identified topics are 4
burnin = 1000
iter = 3000
thin = 500
seed =list(2003, 5, 63, 100001, 765)
nstart = 5
best = TRUE

ldaOut =LDA(dtm.mood, k, method="Gibbs"
            , control = list(nstart=nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

# Exporting the results of topic modeling
ldaOut.topics = as.matrix(topics(ldaOut))
ldaOut.terms = as.matrix(terms(ldaOut, 10))
#probabilities associated with each topic assignment
topicProbabilities = as.data.frame(ldaOut@gamma)

write.csv(ldaOut.topics,file=paste0("Mood", "LDAGibbs", k, "_", "DocsToTopics.csv"))
write.csv(ldaOut.terms,file=paste0("associatedmoods.csv"), fileEncoding = "CP932")
write.csv(topicProbabilities,file=paste0("Mood", "LDAGibbs", k, "_", "TopicProbabilities.csv"))

# Exporting data of the album-topic association
df.topic = select(df.album, year, artist_name_1, album_name) %>%
  cbind(topicProbabilities)
write.csv(df.topic, "album_topic.csv", row.names = F)


# Yearly_topic trends
df.temp = topicProbabilities
df.temp$year = df.album$year
df.temp$artist_name = df.album$artist_name_1
df.temp$album_name = df.album$album_name
df.temp = df.temp %>% dplyr::select(year, artist_name, album_name, everything())

g1 = ggplot(df.temp, aes(x=year, y=V1, group = 1)) +
  stat_smooth(fullrange = T, se = T, colour = "black") +
  ylim(0, 0.5) + 
  labs(x = "Year", y = "Weight", title = "Topic 1: Tough")

g2 = ggplot(df.temp, aes(x=year, y=V1, group = 1)) +
  stat_smooth(fullrange = T, se = T, colour = "black") +
  ylim(0, 0.5) + 
  labs(x = "Year", y = "Weight", title = "Topic 2: Energetic")

g3 = ggplot(df.temp, aes(x=year, y=V3, group = 1)) +
  stat_smooth(fullrange = T, se = T, colour = "black") +
  ylim(0, 0.5) + 
  labs(x = "Year", y = "Weight", title = "Topic 3: Introspective")

g4 = ggplot(df.temp, aes(x=year, y=V4, group = 1)) +
  stat_smooth(fullrange = T, se = T, colour = "black") +
  ylim(0, 0.5) + 
  labs(x = "Year", y = "Weight", title = "Topic 4: Violent")

g = grid.arrange(g1, g2, g3, g4, ncol = 4)
ggsave("topictrends.png", g, width = 12, height = 4, dpi=300
       ,bg = "transparent")