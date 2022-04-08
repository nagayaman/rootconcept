####################################################################
## This script is to reproduce the results shown in Fig 4.
## Fig 4. Differences in root concepts’ lyric tacitness.
####################################################################

rm(list = ls())
#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"readxl"
  ,"cowplot"
  ,"ggpubr"
)
pacman::p_load(packages, character.only = TRUE, install = T)


df.temp = read_xlsx("data/lyrics_LIWCResults.xlsx")
colnames(df.temp)[1:5] = c("year", "artist_name", "album_name", "track_name", "track_lyric")
df.temp = df.temp %>% select(year:track_lyric, min, sec, duration, WC, everything())
colnames(df.temp)[10:ncol(df.temp)] = paste0("LIWC_", colnames(df.temp)[10:ncol(df.temp)])

df.temp = df.temp %>% filter(!is.na(track_lyric), artist_name != "Public Enemy") %>% 
  mutate(dummy = as.factor(ifelse(artist_name == "N.W.A", 1, 0))
         ,artist_name = as.factor(artist_name)
         ,artist_name = factor(artist_name, levels = c("Run-D.M.C.", "N.W.A"))
         ,WC_per_sec = (WC/duration)*1000
  ) %>% filter(duration >= 120000) %>% 
  filter(WC >0) # to eliminate prelude or interlude

# Divide words features with word count per second
df.temp = df.temp %>% mutate_at(vars(contains("LIWC_")), .funs=funs(.*WC_per_sec/100)) # ms = 1000 and % 100

my_comparisons = list(c("Run-D.M.C.", "N.W.A"))

g1 = ggbarplot(df.temp
               ,x = "artist_name"
               ,y = "WC_per_sec"
               ,ylab = "The Number of Words per Second"
               ,xlab = ""
               ,add = "mean_se",  add.params = list(color = "black")
               ,label = TRUE
               ,lab.vjust = -2
               ,lab.hjust = -0.05
               ,lab.col = "black"
               ,color = "gray50"
               ,width = 0.4
               ,orientation = "horizontal"
               ,order = c("N.W.A", "Run-D.M.C.")
               ,lab.nb.digits = 3
) + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test"
                     ,na.rm = T
                     ,bracket.size = 1
                     ,label.y = 3.2
  )

g2 = ggbarplot(df.temp
               ,x = "artist_name"
               ,y = "LIWC_informal"
               ,ylab = "The Number of Informal Words per Second"
               ,xlab = ""
               ,add = "mean_se",  add.params = list(color = "black")
               ,label = TRUE
               ,lab.vjust = -2
               ,lab.hjust = -0.05
               ,lab.col = "black"          
               ,color = "gray50"
               ,width = 0.4
               ,orientation = "horizontal"
               ,order = c("N.W.A", "Run-D.M.C.")
               ,lab.nb.digits = 3
) + 
  stat_compare_means(comparisons = my_comparisons, method = "t.test"
                     ,na.rm = T
                     ,bracket.size = 1
                     ,label.y = 0.3
  )
g = plot_grid(g1, g2
              ,ncol = 2 
              ,label_size = 12
              ,align = "v"
              ,labels = c('(A)', '(B)')
)
print(g)
ggsave("Fig4.png", g, width = 12, height = 4, dpi=300)

length(unlist(filter(df.temp, dummy == 0)$WC_per_sec))
length(unlist(filter(df.temp, dummy == 1)$WC_per_sec))
mean(unlist(filter(df.temp, dummy == 0)$WC_per_sec))
mean(unlist(filter(df.temp, dummy == 1)$WC_per_sec))
# Mean word per second of Run-D.M.C. is 1.950
# Mean word per second of N.W.A is 2.800
t.test(unlist(filter(df.temp, dummy == 0)$WC_per_sec), unlist(filter(df.temp, dummy == 1)$WC_per_sec))


mean(unlist(filter(df.temp, dummy == 0)$LIWC_informal))
mean(unlist(filter(df.temp, dummy == 1)$LIWC_informal))
# Mean informal words of Run-D.M.C. is 0.0394
# Mean informal words of N.W.A is 0.236
t.test(unlist(filter(df.temp, dummy == 0)$LIWC_informal), unlist(filter(df.temp, dummy == 1)$LIWC_informal))
