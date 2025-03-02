# Titel: Fallstudie in Unsupervised Machine Learning



# Libraries laden
library(readr)
library(dplyr)
library(tidyr)
library(cluster)
library(ggplot2)

# Daten laden
flaschenpost_df <- read_rds('Flaschenpost/flaschenpost.rds')
article_df <- read_rds('Flaschenpost/article.rds')
distanzmatrix <- read_rds('Flaschenpost/dist.rds')


# Hierarchische Clusteranalyse durchführen
hc <- hclust(distanzmatrix, method = "ward.D2")

# Cluster extrahieren
k <- 5
clusters <- cutree(hc, k = k)

# Zuordnung der Cluster zu den Artikeln
article_clusters <- data.frame(article_id = as.numeric(rownames(as.matrix(distanzmatrix))), cluster = clusters)
article_clusters_info <- article_clusters %>%
  left_join(article_df, by = c("article_id" = "article_id"))

# Analyse der Hauptunterschiede zwischen den ersten 2 Clustern
cluster_1_2 <- article_clusters_info %>% filter(cluster %in% c(1, 2))

# Vergleich von Cluster 1 und Cluster 2
compare_1_2 <- cluster_1_2 %>%
  group_by(cluster) %>%
  summarise(
    avg_alcohol = mean(alcohol, na.rm = TRUE),
    avg_package_units = mean(package_units, na.rm = TRUE),
    avg_itm_volume = mean(itm_volume, na.rm = TRUE),
    most_common_category = names(sort(table(category), decreasing = TRUE)[1]),
    most_common_category2 = names(sort(table(category2), decreasing = TRUE)[1]),
    most_common_Gebinde = names(sort(table(Gebinde), decreasing = TRUE)[1]),
    most_common_bottle_type = names(sort(table(bottle_type), decreasing = TRUE)[1]),
    count = n()
  )

print(compare_1_2)

# Analyse der weiteren Cluster (3, 4, 5)
cluster_3_5 <- article_clusters_info %>% filter(cluster %in% c(3, 4, 5))

# Vergleich der Cluster 3, 4 und 5
compare_3_5 <- cluster_3_5 %>%
  group_by(cluster) %>%
  summarise(
    avg_alcohol = mean(alcohol, na.rm = TRUE),
    avg_package_units = mean(package_units, na.rm = TRUE),
    avg_itm_volume = mean(itm_volume, na.rm = TRUE),
    most_common_category = names(sort(table(category), decreasing = TRUE)[1]),
    most_common_category2 = names(sort(table(category2), decreasing = TRUE)[1]),
    most_common_Gebinde = names(sort(table(Gebinde), decreasing = TRUE)[1]),
    most_common_bottle_type = names(sort(table(bottle_type), decreasing = TRUE)[1]),
    count = n()
  )

print(compare_3_5)


#Visualisierung

# Vergleich der durchschnittlichen Alkoholgehalte
ggplot(article_clusters_info, aes(x = factor(cluster), y = alcohol, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Alkoholgehalt zwischen Clustern",
       x = "Cluster",
       y = "Alkoholgehalt (%)") +
  theme_minimal() +
  theme(legend.position = "none")

# Vergleich der durchschnittlichen Gebindegröße
ggplot(article_clusters_info, aes(x = factor(cluster), y = package_units, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Gebindegröße zwischen Clustern",
       x = "Cluster",
       y = "Gebindegröße (Einheiten)") +
  theme_minimal() +
  theme(legend.position = "none")

# Vergleich des durchschnittlichen Volumens
ggplot(article_clusters_info, aes(x = factor(cluster), y = itm_volume, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Volumen zwischen Clustern",
       x = "Cluster",
       y = "Volumen (ml)") +
  theme_minimal() +
  theme(legend.position = "none")

# Dendrogramm mit hervorgehobenen Clustern
plot(hc, main = "Dendrogramm der hierarchischen Clusteranalyse mit Clustern", xlab = "", sub = "", cex = 0.9)
rect.hclust(hc, k = k, border = 2:6)

# Legende Dendogramm
legend("topright", legend = paste("Cluster", 1:k), fill = 2:6, border = "black")
