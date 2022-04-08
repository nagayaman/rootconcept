#############################################################################################################
## This script is to reproduce the regression results.
## The reproduced figures and tables are Fig 2, Fig 3, Fig 6., S2, S3, S4, and S5 tables. shown in the paper.
#############################################################################################################
rm(list = ls())

#install.packages("pacman")
packages = c(
  "tidyverse"
  ,"igraph"
  ,"gtools"
  ,"geepack"
  ,"dummies"
  ,"haven"
  ,"labelled"
  ,"forcats"
  ,"latex2exp"
  ,"tm"
  ,"dotwhisker"
  ,"cowplot"
  ,"modelsummary"
  ,"psych"
  ,"tibble"
  ,"plm"
)
pacman::p_load(packages, character.only = TRUE, install = T)


#############################################################################################################
## Fig 2. Coefficient plots—A mood expressed by Run-D.M.C. has greater combined elements, element diversity,
## and lower combinatory strength than a mood expressed by N.W.A (N = 4,040).
#############################################################################################################

# Load data
df.reg.raw = read.csv("data/mood_level_regression.csv")
df.reg = df.reg.raw
df.reg = plm::pdata.frame(df.reg, index=c("mood","year"))

# Standardization of variables
df.reg = df.reg %>% 
         ,hhi = scale(hhi)
         ,tie_str = scale(tie_str)
         ,mood_tenure = scale(mood_tenure)
         ,popularity = scale(popularity)
         ,total_nodes = scale(total_nodes)
         ,n_titles = scale(n_titles)
  )

m1 = geeglm(n_alters ~ dmc1 + nwa1 +
              mood_tenure + popularity + total_nodes + n_titles +
              cohort1 + cohort2 + cohort3 + cohort4 + cohort5
            , id = mood
            , waves = year
            ,corstr = "independence"
            , data = df.reg)

m2 = update(m1, hhi ~ .)
m3 = geeglm(tie_str ~ dmc1 + nwa1 +
              mood_tenure + popularity + total_nodes + n_titles +
              cohort1 + cohort2 + cohort3 + cohort4 + cohort5
            , id = mood 
            , waves = year
            ,corstr = "exchangeable", data = df.reg)

summary(m1)
summary(m2)
summary(m3)

m1_tidy = tidy(m1) %>% 
  mutate(model = "Unique Elements") %>% 
  filter(term == "dmc1" | term == "nwa1") %>% 
  relabel_predictors(c(
    nwa1 = "Moods Expressed by N.W.A"
    ,dmc1 = "Moods Expressed by Run-D.M.C."
  )
  )

m2_tidy = tidy(m2) %>% 
  mutate(model = "Element Diversity") %>% 
  filter(term == "dmc1" | term == "nwa1") %>% 
  relabel_predictors(c(
    nwa1 = "Moods Expressed by N.W.A"
    ,dmc1 = "Moods Expressed by Run-D.M.C."
  )
  )

m3_tidy = tidy(m3) %>% 
  mutate(model = "Tie Strentgh") %>% 
  filter(term == "dmc1" | term == "nwa1") %>% 
  relabel_predictors(c(
    nwa1 = "Moods Expressed by N.W.A"
    ,dmc1 = "Moods Expressed by Run-D.M.C."
  )
  )

# A function for producing the coefficient plots
coef_func = function(tidy_reg, indep_name, ylim, breaks, position){
  df = tidy_reg %>%
    mutate(
      lower_bound = estimate - 1.96 * std.error
      ,upper_bound = estimate + 1.96 * std.error
      ,p_value = 2 * pnorm(-abs(estimate / std.error))
    )  %>%
    mutate_if(is.double, round, digits = 2) %>% 
    mutate(p_value = format(p_value, nsmall = 2)
    )
  
  g = ggplot(df, aes(x = term, y = estimate)) + 
    geom_linerange(
      aes(ymin = lower_bound
          ,ymax = upper_bound)
      ,alpha = 0.8, size = 0.6
    ) +
    geom_dotplot(
      binaxis = "y"
      ,stackdir = "center"
      ,dotsize = 0.3) +
    coord_flip(ylim = ylim) +
    scale_y_continuous(breaks = breaks) +
    geom_hline(yintercept = 0, size = 0.1, alpha = 0.5) +
    theme_classic() +
    labs(x = ""
         ,y = ""
         ,title = indep_name
    )+
    theme(
      text=element_text(family="Arial"
      )
      ,axis.text=element_text(size=12)
      ,axis.line = element_blank()
      ,axis.ticks.y = element_blank()
      ,legend.title = element_blank()
      ,axis.title.y = element_blank()
      ,axis.title.x = element_text(hjust = -0.1)
      ,legend.position = "bottom"
      ,panel.border = element_rect(fill = NA, size=1)
      ,panel.spacing = unit(5, "lines")
    )
  
  ci_str = c()
  for (i in seq_along(df$lower_bound)){
    temp1 = format(df$lower_bound[i], nsmall = 2)
    temp2 = format(df$upper_bound[i], nsmall = 2)
    ci_str = c(ci_str, str_interp("[${temp1}, ${temp2}]"))
  }
  dots_xaxis = ggplot_build(g)$data[[1]]['x']
  
  g = g + ggplot2::annotate("text"
                            ,x = dots_xaxis$x
                            , y = position[1]
                            ,label = format(df$estimate, nsmall = 2)
                            ,size = 4)+
    ggplot2::annotate("text"
                      , x = dots_xaxis$x
                      , y = position[2]
                      ,label = ci_str
                      , size = 4)+
    ggplot2::annotate("text"
                      , x = dots_xaxis$x
                      , y = position[3]
                      ,label = df$p_value
                      , size = 4) +
    ggplot2::annotate("text"
                      , x = max(dots_xaxis$x) + 0.40
                      , y = position[1]
                      ,label = "β", size =  4) + 
    ggplot2::annotate("text"
                      , x = max(dots_xaxis$x) + 0.40
                      , y = position[2]
                      ,label = "95% CI", size = 4) +
    ggplot2::annotate("text"
                      , x = max(dots_xaxis$x) + 0.40
                      , y = position[3]
                      ,label = "p", size = 4)
  return(g)
}

y_limit = c(-0.1, 0.7) # The range of coefficient size
breaks = c(0, 0.2, 0.4) # Indication of X-axis
position = c(0.5, 0.6, 0.7) # Positions of the effect size, confidence intervals, and p-values


# Plot the size of the coefficient of the main variables
g1 = coef_func(m1_tidy, "(A) Combined Elements", y_limit, breaks, position)
g2 = coef_func(m2_tidy, "(B) Element Diversity", y_limit, breaks, position)
g3 = coef_func(m3_tidy, "(C) Combinatory Strength", y_limit, breaks ,position)

g = plot_grid(g1, g2, g3
              ,nrow = 3
              ,label_size = 8
              ,align = "v"
)
print(g)
ggsave("Fig2.png", g, width = 12, height = 6, dpi=300)


######################################################################################################
## S2 Table. Descriptive statistics and correlations for analyzing differences in the root concepts’ 
## generative capacity (N = 4,040).
######################################################################################################

# Mean, standard deviation, min., and max.
df.descriptive = df.reg.raw %>%
  select(n_alters
         ,hhi
         ,tie_str
         ,dmc1
         ,nwa1
         ,mood_tenure
         ,popularity
         ,total_nodes
         ,n_titles
  ) 

label_descriptive = c("Combined elements"
                      ,"Element diversity"
                      ,"Combinatory strength"
                      ,"Run-D.M.C dummies"
                      ,"N.W.A dummies"
                      ,"Mood age"
                      ,"Mood popularity"
                      ,"Total number of moods"
                      ,"Total number of released albums"
                      )

stargazer(df.descriptive
          ,covariate.labels = label_descriptive
          ,digits = 2
          ,type="html"
          ,out = "S2_table_stats.html"
)

# Correlation matrix
temp = cor(df.descriptive)
temp = round(cor(df.descriptive), 2)
cor_mat = temp
cor_mat[upper.tri(temp)] = ""
cor_mat = as.data.frame(cor_mat)
cor_mat$Variables = label_descriptive
cor_mat = cor_mat %>% dplyr::select(Variables, everything())
write.csv(cor_mat, "S2_table_correlation.csv", row.names = T)


##############################################################################################
## S3 Table. Regressions for analyzing differences in the root concepts’ generative capacity.
##############################################################################################

reg_table = list()
reg_table[["Combined elements"]] = m1
reg_table[["Element diversity"]] = m2
reg_table[["Combinatory strength"]] = m3

var_nam = c("dmc1" = "Run-D.M.C. dummies"
            ,"nwa1" = "N.W.A dummies"
            ,"mood_tenure" = "Mood age"
            ,"popularity" = "Mood popurarity"
            ,"total_nodes" = "Total number of moods"
            ,"n_titles" = "Total number of released albums"
            )

rows <- tribble(~term, ~m1, ~m2, ~m3,
  "5 year cohort dummies", "Y", "Y", "Y")
attr(rows, 'position') <- c(13, 3)
msummary(reg_table
             ,coef_map  = var_nam
             ,fmt = '%.2f'
             ,stars = c("+" = .10,"*" = .05, "**" = .01, "***" = .001)
             ,gof_omit = "max.cluster.size|n.clusters|alpha|gamma"
             ,add_rows = rows
             ,output = "S3_table.html"
)


##########################################################################################################
## Fig 3. Coefficient plots—In an album, an artist who works with N.W.A collaborators tends to express 
## moods similar to those expressed by N.W.A (N = 6,111). 
##########################################################################################################

# Load data
df.album.sim = read.csv("data/album_similarity_regression.csv")

# Regression analysis
reg1 = lm(scale(sim_dmc) ~ nw_dmc + nw_nwa + gang + production_res_d + Loc_NY_pure + Loc_CA_pure +
            products + teamsize + cohort2 + cohort3 + cohort4 + cohort5
          , data = df.album.sim)
reg2 = update(reg1, scale(sim_nwa) ~ .)

regs = list()
regs[["Similarity (Run-D.M.C.)"]] = reg1
regs[["Similarity (N.W.A)"]] = reg2

var_nam = c("nw_dmc" ="Run-D.M.C. networks"
            ,"nw_nwa" ="N.W.A networks"
            ,"gang" = "Gang affiliation"
            ,"production_res" = "Atypical production resources"
            ,"production_res_d" = "Atypical production resources (dummy)"
            ,"Loc_NY_pure" = "Location NY"
            ,"Loc_CA_pure" = "Location CA"
            ,"products" = "Previous releases"
            ,"teamsize" = "Team size"
            ,"(Intercept)" = "Constant")

m1_tidy = tidy(reg1) %>% 
  mutate(model = "Similarity (Run-D.M.C.)") %>% 
  filter(term == "nw_dmc" | term == "nw_nwa"
  ) %>% 
  relabel_predictors(c(
    nw_nwa = "Working with the N.W.A's Collaborators"
    ,nw_dmc = "Working with the Run-D.M.C.'s Collaborators"
  )
  )

m2_tidy = tidy(reg2) %>% 
  mutate(model = "Similarity (N.W.A)") %>% 
  filter(term == "nw_dmc" | term == "nw_nwa"
  ) %>% 
  relabel_predictors(c(
    nw_nwa = "Working with the N.W.A's Collaborators"
    ,nw_dmc = "Working with the Run-D.M.C.'s Collaborators"
  )
  )

y_limit = c(-0.3, 0.8)
breaks = c(-0.2, 0, 0.2, 0.4)
position = c(0.6, 0.7, 0.8) # Positions of the effect size, confidence intervals, p-value

g1 = coef_func(m1_tidy, "(A) Expressing the Run-D.M.C.-like Moods", y_limit, breaks, position)
g2 = coef_func(m2_tidy, "(B) Expressing the N.W.A-like Moods", y_limit, breaks, position)

g = plot_grid(g1, g2
              ,nrow = 2
              ,label_size = 8
              ,align = "v"
)
print(g)
ggsave("Fig3.png", g, width = 12, height = 4, dpi=300)


#######################################################################################################
## Fig 6. Coefficient plots—In an album, an artist who has a corresponding socio-economic identity
## tends to express moods similar to those expressed by Run-D.M.C. or N.W.A (N = 6,111).
#######################################################################################################

m1_tidy = tidy(reg1) %>% 
  mutate(model = "Expressing the Run-D.M.C.-like Moods") %>% 
  filter(term == "production_res_d"|term == "gang" 
  ) %>% 
  relabel_predictors(c(
    production_res_d = "Affluent Production Resources"
    ,gang = "Gang Affiliation"
  )
  )

m2_tidy = tidy(reg2) %>% 
  mutate(model = "Expressing the N.W.A-like Moods") %>% 
  filter(term == "production_res_d"|term == "gang" 
  ) %>% 
  relabel_predictors(c(
    production_res_d = "Affluent Production Resources"
    ,gang = "Gang Affiliation"
  )
  )

y_limit = c(-0.3, 0.8)
breaks = c(-0.2, 0, 0.2, 0.4)
position = c(0.6, 0.7, 0.8) # Positions of the effect size, confidence intervals, p-value

g1 = coef_func(m1_tidy, "(A) Expressing the Run-D.M.C.-like Moods", y_limit, breaks, position)
g2 = coef_func(m2_tidy, "(B) Expressing the N.W.A-like Moods", y_limit, breaks, position)

g = plot_grid(g1, g2
              ,nrow = 2
              ,label_size = 10
              ,align = "v"
)
print(g)
ggsave("Fig6.png", g, width = 12, height = 4, dpi=300)



#######################################################################################################################
## S4 Table. Descriptive statistics and correlations for analyzing the contagion and identity requirements (N = 6,111).
#######################################################################################################################

df.descriptive.albums = df.album.sim %>%
  select(sim_dmc
         ,sim_nwa
         ,nw_dmc
         ,nw_nwa
         ,gang
         ,production_res
         ,Loc_NY_pure
         ,Loc_CA_pure
         ,products
         ,teamsize
  )

label_descriptive = c("Similarity (Run-D.M.C.)"
                      ,"Similarity (N.W.A)"
                      ,"Working with Run-D.M.C.'s collaborators"
                      ,"Working with N.W.A's collaborators"
                      ,"Gang affiliation"
                      ,"Atypical production resources"
                      ,"Location NY"
                      ,"Location CA"
                      ,"Previous releases"
                      ,"Team size"
)

stargazer(df.descriptive.albums
          ,covariate.labels = label_descriptive
          ,digits = 2
          ,type="html"
          ,out = "S4_table_stats.html"
)

# Correlation matrix
temp = round(cor(df.descriptive.albums), 2)
cor_mat = temp
cor_mat[upper.tri(temp)] = ""
cor_mat = as.data.frame(cor_mat)
cor_mat$Variables = label_descriptive
cor_mat = cor_mat %>% dplyr::select(Variables, everything())
write.csv(cor_mat, "S4_table_correlation.csv", row.names = T)


####################################################################################
## S5 Table. Regressions for analyzing the contagion and identity requirements.
####################################################################################
rows <- tribble(~term, ~m1, ~m2,
                "5 year Cohort dummies", "Y", "Y")
attr(rows, 'position') <- c(19, 3)
msummary(regs
         ,coef_map = var_nam
         ,fmt = '%.2f'
         ,stars = c("*" = .05, "**" = .01, "***" = .001)
         ,gof_omit = "AIC|BIC|R2 Adj.|Log.Lik."
         ,add_rows = rows
         ,vcov = sandwich::vcovHC
         #,notes = list("Robust standard errors are in parentheses")
         ,'S5_table.html'
)
