pc3 %>% 
  ggplot(aes(x=PC1, y=PC2, shape = cluster)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  theme_minimal() +
  geom_text(aes(x=PC1, y=PC2, label = as.integer(cluster)), nudge_x = 0.18, nudge_y = -0.18) +
  scale_shape_identity() +
  labs(x = paste0('PC1 (Accounts for ', cluster1_var, '% of Variance)'), # define cluster 1 % of variance
       y = paste0('PC2 (Accounts for ', cluster2_var, '% of Variance)'), # define cluster 2 % of variance
       title = 'Cluster Analysis of NFL Offensive Schemes in 2020',
       subtitle = "Text next to the team's logo is the cluster they're in",
       caption = "By Tej Seth | @mfbanalytics with help from Alex Stern") + 
  theme_tej()
ggsave('cluster1.png', height = 10, width = 16, dpi = 300)

pc3 %>% 
  ggplot(aes(x=cluster, y=order)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  theme_minimal() +
  labs(x = "Cluster", 
       y = "",
       title = 'The Offensive Schemes in Each Cluster',
       subtitle = "Cluster analysis of offensive schemes for all 32 NFL ",
       caption = "By Tej Seth | @mfbanalytics with help from Alex Stern") + 
  theme_tej() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(size=14)) + 
  scale_x_continuous(breaks=seq(1, 6, 1))
ggsave('cluster2.png', height = 10, width = 16, dpi = 300)

off_epa <- pbp_rp %>%
  group_by(posteam) %>%
  summarize(epa_epa_play = mean(epa, na.rm = T))

pc4 <- pc3 %>%
  left_join(off_epa, by = c("name" = "posteam"))

pc5 <- pc4 %>%
  group_by(cluster) %>%
  summarize(avg_epa = mean(epa_epa_play)) %>%
  ungroup()

pc4 <- pc4 %>%
  left_join(pc5, by = c("cluster"))

write.csv(pc4, "pc4.csv") #Had to make my own geom_jitter
pc4 <- read.csv("~/NFL Clusters/pc4.csv")

pc4 %>% 
  ggplot(aes(x=cluster, y=avg_epa)) + 
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16 / 9) +
  theme_minimal() +
  labs(x = "Cluster", 
       y = "Cluster's Average Offensive EPA/Play",
       title = 'The Average Offensive EPA/Play of Each Cluster',
       subtitle = "Cluster analysis of offensive schemes for all 32 NFL, correlation does not equal causation warning",
       caption = "By Tej Seth | @mfbanalytics with help from Alex Stern") + 
  theme_tej() +
  theme(axis.text.x = element_text(size=14)) + 
  scale_x_continuous(breaks=seq(1, 6, 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('cluster3.png', height = 10, width = 16, dpi = 300)  