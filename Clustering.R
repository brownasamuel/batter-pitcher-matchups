library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

pitchers = read_csv("Pitchers.csv")

scale_vec = function(vec) {
  vec_mean = mean(vec)
  vec_sd = sd(vec)
  
  vec2 = vec - vec_mean
  vec3 = vec2 / vec_sd
  return(vec3)
}

pitchers_clust = pitchers %>%
  select(avg_velo, offspeed_rate, release_angle_ground) %>%
  mutate(across(where(is.numeric), scale_vec))


## hierarchical ----
pitch_d = dist(pitchers_clust, method = "euclidean")

pitch_model = hclust(pitch_d, method = "complete")

plot(pitch_model,  cex = 0.6, hang = -1)

summary(pitch_model)

## k-means ----
kmeans_pitch = kmeans(pitchers_clust, centers = 5)

kmeans_pitch

# Cluster 1: more vertical delivery, average velo, one dominant off-speed
# Cluster 2: more horizontal delivery, average velo, one primary off-speed
# Cluster 3: standard delivery, below average velo, mix of off-speeds
# Cluster 4: standard delivery, above average velo, mix of off-speeds
# Cluster 5: very horizontal delivery, very below average velo, standard off-speeds

kmeans_desc = data.frame(
  clusters = seq(1,5,1),
  desc = c("more vertical delivery, average velo, one dominant off-speed",
           "more horizontal delivery, average velo, one primary off-speed",
           "standard delivery, below average velo, mix of off-speeds",
           "standard delivery, above average velo, mix of off-speeds",
           "very horizontal delivery, very below average velo, standard off-speeds")
)

pitchers_clustered <- pitchers %>%
  mutate(cluster = kmeans_pitch$cluster)

pitchers_examples <- pitchers_clustered %>%
  group_by(cluster) %>%
  top_n(10, pitches)

pitchers_examples_reformat <- pitchers_examples %>%
  left_join(kmeans_desc, by = c("cluster" = "clusters")) %>%
  select(name, pitches, hand, fastball, primary_offspeed,
         avg_velo, offspeed_rate, release_angle_ground, cluster, desc) %>%
  mutate(cluster_desc = paste0("Cluster ", cluster,": ", desc)) %>%
  mutate(cluster = factor(cluster, levels = c("1","2","3","4","5"))) %>%
  select( -desc) %>%
  group_by(cluster_desc) %>%
  arrange(cluster, -pitches) %>%
  select(-cluster)

library(gt)
library(gtExtras)

gt(pitchers_examples_reformat) %>%
  gt_theme_espn() %>%
  fmt_number(pitches, decimals = 0) %>%
  fmt_number(avg_velo, decimals = 1) %>%
  fmt_percent(offspeed_rate, decimals = 1) %>%
  fmt(release_angle_ground, fns = function(x) paste0(round(x,1), "°")) %>%
  cols_label(name ~ "pitcher",
             fastball ~ "main fastball", 
             primary_offspeed ~ "main offspeed",
             avg_velo ~ "fastball velo",
             offspeed_rate ~ "offspeed pct",
             release_angle_ground ~ "release angle") %>%
  cols_align("center", columns = pitches:release_angle_ground) %>%
  tab_header("K-Means Clustering: Pitcher Subtypes")
 
