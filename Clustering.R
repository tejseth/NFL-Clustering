library(tidyverse)
library(ggrepel)
library(ggimage)
library(nflfastR)
library(stats)
library(mclust)
library(mdthemes)
library(gghighlight)
library(na.tools)
library(gghighlight)

theme_tej <- function() {
  theme(axis.title = element_text(face='bold'), # make all axis titles bold
        plot.title = element_text(face='bold', hjust=0.5, size = 20), # make all plot titles bold
        legend.title = element_text(face='bold'), # make all legend titles bold
        plot.subtitle = element_text(face='italic', hjust=0.5)) # make all subtitles italic
}



pbp_20 <- readRDS(url('https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_2020.rds'))

pbp_rp <- pbp_20 %>% 
  filter(!is_na(epa), play_type=="no_play" | play_type=="pass" | play_type=="run")

pbp_rp <- pbp_rp %>%
  mutate(
    pass = if_else(str_detect(desc, "( pass)|(sacked)|(scramble)"), 1, 0),
    rush = if_else(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)") & pass == 0, 1, 0),
    success = ifelse(epa>0, 1 , 0)
  ) 

pbp_rp <- pbp_rp %>% filter(pass==1 | rush==1)

pbp_rp <- pbp_rp %>% 
  add_xpass() %>%
  filter(!is.na(pass_oe))

pbp_rp <- pbp_rp %>%
  mutate(outside_run = ifelse(run_gap == "end", 1, 0),
         inside_run = ifelse(run_gap != "end", 1, 0),
         yards_past_sticks = air_yards - ydstogo)

team_stats <- pbp_rp %>%
  group_by(posteam) %>%
  summarize(plays = n(),
            proe_avg = mean(pass_oe, na.rm = T),
            outside_run_rate = mean(outside_run, na.rm = T),
            inside_run_rate = mean(inside_run, na.rm = T),
            yards_per_attempt = mean(air_yards, na.rm = T),
            yac_avg = mean(yards_after_catch, na.rm = T),
            qb_scramble_rate = mean(qb_scramble, na.rm = T),
            qb_hit_rate = mean(qb_hit, na.rm = T),
            qb_sack_rate = mean(sack, na.rm = T),
            first_down_pass_rate = mean(first_down_pass, na.rm = T),
            yards_over_sticks = mean(yards_past_sticks, na.rm = T))

no_team_name <- team_stats %>%
  select(-plays, -posteam) %>%
  scale()

set.seed(222) # set seed to ensure reproduceability b/c k-means relies on random states for initialization 
MAX_K <- 20 # max number of clusters
sse <- c() # vector to hold SSE of each model

for (k in 1:MAX_K) {
  algo_k <- kmeans(no_team_name, centers=k, nstart=22, iter.max=20) # k-means algorithm
  sse <- c(sse, algo_k$tot.withinss) # get SSE
} 

tibble(k = 1:MAX_K, SSE = sse) %>%
  ggplot(aes(x=k, y=SSE)) + 
  geom_point(color= "red") + geom_line(color="navy") + 
  labs(x = "K", y = "SSE", title = "Where does this level off?") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_tej()

tibble(k = 1:MAX_K, SSE_difference = sse-2*lead(sse)+lead(sse, 2)) %>%
  dplyr::filter(k<MAX_K-1) %>%
  ggplot(aes(x=k, y=SSE_difference)) + 
  geom_point(color="#F84C1E") + geom_line(color="#232D4B") + 
  labs(x = "K", y = "SSE Rolling 2-Unit Difference", title = "A More Clear Picture") + 
  scale_x_continuous(breaks=seq(1, MAX_K, 1)) + 
  theme_minimal() + theme_tej()

#It levels off at 6 K's
set.seed(22)

K <- 6
kmeans6 <- kmeans(no_team_name, centers=K, nstart=22, iter.max=20)
km_centers <- as.data.frame(kmeans6$centers) 

km_centers$cluster <- c('Cluster 1', 'Cluster 2', 'Cluster 3',
                        'Cluster 4', 'Cluster 5', 'Cluster 6')

km_centers <- km_centers %>%
  rename(c('PROE'='proe_avg', 'ORR'='outside_run_rate', 
           'IRR'='inside_run_rate', 'YPA'='yards_per_attempt',
           'YAC'='yac_avg', 'QBSR'='qb_scramble_rate',
           'SaR'='qb_sack_rate', 'QBHR'='qb_hit_rate', 'FDPR'='first_down_pass_rate',
           'YPS'='yards_over_sticks')) %>% 
  pivot_longer(!cluster, names_to = 'feature', values_to = 'z_val')

km_centers$feature <- factor(km_centers$feature, levels=c('PROE', 'ORR', 'IRR', 'YPA', 
                                                          'YAC','QBSR', 'SaR', 'QBHR', 
                                                          'FDPR', 'YPS'))

km_centers$cluster <- factor(km_centers$cluster, levels=c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4',
                                                          'Cluster 5', 'Cluster 6'))

team_clusters <- tibble(cluster=kmeans6$cluster, name=team_stats$posteam)

team_clusters <- team_clusters %>%
  left_join(teams_colors_logos, by = c("name" = "team_abbr"))

km_centers %>% 
  ggplot(aes(x=feature, y=z_val, color=cluster)) + 
  geom_point(size = 3) + 
  scale_color_brewer(palette="Set1") + 
  gghighlight(use_direct_label = FALSE) + 
  facet_wrap(~ cluster, ncol=3) + 
  labs(x = "Feature", y = "Cluster Center", 
       title = "Each Cluster's Features") + 
  theme_minimal() + theme_tej() + 
  theme(legend.position = "none", strip.text = element_text(face='bold'),
        axis.text.x = element_text(angle=90, size=8),
        panel.grid.minor = element_blank())
ggsave('cluster4.png', height = 10, width = 16, dpi = 300)

pca <- prcomp(no_team_name) # perform Principle Component Analysis 
pca_summary <- summary(pca) # summary of PCA model

pc2 <- as.data.frame(pca$x[,1:2]) # extract first two PCs
pc2$cluster <- as.factor(kmeans6$cluster) 
cluster1_var <- round(pca_summary$importance[2,1], 4) * 100 # get variance explained by cluster 1
cluster2_var <- round(pca_summary$importance[2,2], 4) * 100 # get variance explained by cluster 2

pc2 <- pc2 %>%
  arrange(cluster)

team_clusters <- team_clusters %>%
  arrange(cluster)

pc3 <- cbind(pc2, team_clusters)
write.csv(pc3, "pc3.csv")
pc3 <- read.csv("~/NFL Clusters/pc3.csv") #For some reason it names cluster twice so I changed it on Excel

pc3$cluster <- as.factor(pc3$cluster)

