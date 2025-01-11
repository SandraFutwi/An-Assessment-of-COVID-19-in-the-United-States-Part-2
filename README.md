# An-Assessment-of-COVID-19-in-the-United-States-Part-2

#' **importing libraries**
library("ggplot2")
library("ggrepel")
library("ggcorrplot")
library("DT")
library("tidyverse")
library(reshape2)
library(readr)
library(knitr)
library(kableExtra)
library(GGally)
library(pheatmap)
library(dplyr)
library(cluster)
library(dbscan)
library(funtimes)
library(clValid)
library(e1071)
library(factoextra)
library(hopkins)
library(patchwork)
library(webshot2)
library(caret)

#'**Reading the file and Prepare the Data for cases**
#'** cases **
#'** cases_TX **
#'** cases_per_1000**
#'** deaths_per_1000**
#'** population_density**
setwd("C:\\Users\*****\Project 2")
cases <- read_csv("COVID-19_cases_plus_census_plus_area_plus_hesidency.csv")
cases <- cases %>% mutate(across(where(is.character), factor))
dim(cases)
cases_TX <- cases %>% filter(state == "TX")
dim(cases_TX)
#'
#'
#' **Used Data**
datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
#'
cases_TX <- cases_TX %>% 
  arrange(desc(confirmed_cases)) %>%   
  select(county_name,median_age,median_income,total_pop,commuters_by_public_transportation,families_with_young_children,worked_at_home,employed_pop,commuters_drove_alone,vaccine_hesidency,confirmed_cases,county_area,deaths,bachelors_degree)
cases_TX <- cases_TX %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000, 
  deaths_per_1000 = deaths/total_pop*1000, 
  death_per_case = deaths/confirmed_cases,
  population_density = total_pop / county_area)

summary(cases_TX)
#'
#'
#'
#'
#' **Used Data**
datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
#'
#'
#'
#' **Making counties_polygon_TX for the map and printing it afterward**
counties_polygon <- as_tibble(map_data("county"))
counties_polygon_TX <- counties_polygon %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion)) 
counties_polygon_TX
#'
#'
#'
#'
#'
#'**Make county name match map county names (lower case + pulling “County”)**
cases_TX <- cases_TX %>% mutate(county = county_name %>% 
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))
cases_TX %>% pull(county)
#'
#'
#'
#'
#' **Cheking the cases_TX making sure all of the variables are present**
datatable(cases_TX) %>% formatRound(c(5, 9, 10), 2) %>% formatPercentage(11, 2)
#'
#'
#'
#'
#'
#'
#'**Use a database join to join cases_TX and counties_polygon_TX. To join we use `by = join_by(county)` **
counties_polygon_TX <- right_join(counties_polygon_TX, cases_TX)
#'
#'
#'
#'
#'**Plotting the map of Texas with deaths_per_1000**
ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = deaths_per_1000)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas death rates by county")
#'**Plotting the map of Texas with Vaccine Hesidency**
ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = vaccine_hesidency)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas Vaccine Hesidency by county")
#'**Plotting the map of Texas with Median Income**
ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = median_income)) +
  coord_quickmap() +
  scale_fill_continuous(type = "viridis") +
  labs(title = "Texas death rates by Median Income")
#'
#'
#'
#'
#'
#' **Cluster using K-Means**
cases_TX_prepared_supervised <- cases_TX %>% 
  select(median_income,bachelors_degree, vaccine_hesidency, commuters_by_public_transportation, median_age, employed_pop) %>% 
  scale() %>% as_tibble()
cases_TX_scaled_kmeans <- scale(cases_TX_prepared_supervised)
cases_TX_scaled_kmeans_summary <- summary(cases_TX_scaled_kmeans)
cases_TX_scaled_kmeans_summary
head(cases_TX_scaled_kmeans)
#'
#'
#'
#'
#' ** Hopkins**
set.seed(123)
hopkins_stat_TX <- hopkins(cases_TX_scaled_kmeans)
print(hopkins_stat_TX)
#' ** Gap Statistics**
gap_stat <- clusGap(cases_TX_scaled_kmeans, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) + labs(title = "Gap Statistic for Optimal Clusters")
#' **Silhouette Analysis:**
k_means_2 <- kmeans(cases_TX_scaled_kmeans, centers = 2, nstart = 10)
sil_2 <- silhouette(k_means_2$cluster, dist(cases_TX_scaled_kmeans))
fviz_silhouette(sil_2) + labs(title = "Silhouette Analysis for k = 2")
k_means_3 <- kmeans(cases_TX_scaled_kmeans, centers = 3, nstart = 10)
sil_3 <- silhouette(k_means_3$cluster, dist(cases_TX_scaled_kmeans))
fviz_silhouette(sil_3) + labs(title = "Silhouette Analysis for k = 3")
#' 
#' 
#' 
#' 
#' 
#' 
#' ***Using distance plot Factoextra and ggplot2 Finding K**
k_means_TX <- kmeans(cases_TX_scaled_kmeans, centers = 5, nstart = 10)
# Calculate Euclidean distance matrix for the dataset
d_TX <- dist(cases_TX_scaled_kmeans)
# Plot the distance matrix using factoextra
fviz_dist(d_TX, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
  ggtitle("Distance Matrix - Texas Counties K-means Clustering") +
  theme_minimal()
# Optional: Visualize clusters with fviz_cluster (alternative to distance matrix)
fviz_cluster(k_means_TX, data = cases_TX_scaled_kmeans) +
  ggtitle("Cluster Plot - Texas Counties") +
  theme_minimal()
#'
#'
#'
#' ** Finding K Part 2**
#' 
ks_TX <- 2:7
d_TX <- dist(cases_TX_scaled_kmeans)
ASW_TX <- sapply(ks_TX, FUN = function(k) {
  kmeans_result <- kmeans(cases_TX_scaled_kmeans, centers = k, nstart = 10)
  silhouette_info <- silhouette(kmeans_result$cluster, d_TX)
  mean(silhouette_info[, 3])  # Extract average silhouette width
})
best_k_TX <- ks_TX[which.max(ASW_TX)]
print(best_k_TX)
ggplot(data.frame(ks_TX, ASW_TX), aes(x = ks_TX, y = ASW_TX)) + 
  geom_line() +
  geom_vline(xintercept = best_k_TX, color = "red", linetype = 2) + 
  ggtitle("Average Silhouette Width Plot (Texas Counties Clustering)") +
  xlab("Number of Clusters (k)") +
  ylab("Average Silhouette Width")
#'
#'
#'
#'
#' ** Finding K Part 3**
library(ggpubr)
library(cluster)
library(factoextra)
library(gridExtra)
k_means_TX_2 <- kmeans(cases_TX_scaled_kmeans, centers = 2, nstart = 10)
k_means_TX_4 <- kmeans(cases_TX_scaled_kmeans, centers = 4, nstart = 10)
k_means_TX_6 <- kmeans(cases_TX_scaled_kmeans, centers = 6, nstart = 10)
k_means_TX_8 <- kmeans(cases_TX_scaled_kmeans, centers = 8, nstart = 10)
d_TX <- dist(cases_TX_scaled_kmeans)
sil_TX_2 <- silhouette(k_means_TX_2$cluster, d_TX)
sil_TX_4 <- silhouette(k_means_TX_4$cluster, d_TX)
sil_TX_6 <- silhouette(k_means_TX_6$cluster, d_TX)
sil_TX_8 <- silhouette(k_means_TX_8$cluster, d_TX)
p1 <- fviz_silhouette(sil_TX_2) + ggtitle("2 Clusters")
p2 <- fviz_silhouette(sil_TX_4) + ggtitle("4 Clusters")
p3 <- fviz_silhouette(sil_TX_6) + ggtitle("6 Clusters")
p4 <- fviz_silhouette(sil_TX_8) + ggtitle("8 Clusters")
combined_silplot_TX <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
print(combined_silplot_TX)
print(p1)
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
#' 
#' 
#' 
#' 
#' 
#' 
#' **Finding K Part 4**

# Define the range for the number of clusters
ks_TX <- 2:10

# Calculate within-cluster sum of squares (WCSS) for each number of clusters
WCSS_TX <- sapply(ks_TX, FUN = function(k) {
  kmeans(cases_TX_scaled_kmeans, centers = k, nstart = 10)$tot.withinss
})

# Plot the Elbow Plot for WCSS
ggplot(tibble(ks_TX, WCSS_TX), aes(ks_TX, WCSS_TX)) + 
  geom_line() +
  geom_vline(xintercept = 5, color = "red", linetype = 2) +
  ggtitle("Elbow Plot (Texas Counties Clustering)") +
  xlab("Number of Clusters (k)") +
  ylab("Within-Cluster Sum of Squares (WCSS)")
#'
#'
#'
#'
#'
#' **Compairing clusters**
k_means_TX_2 <- kmeans(cases_TX_scaled_kmeans, centers = 2)
k_means_TX_3 <- kmeans(cases_TX_scaled_kmeans, centers = 3)
k_means_TX_5 <- kmeans(cases_TX_scaled_kmeans, centers = 5)
k_means_TX_6 <- kmeans(cases_TX_scaled_kmeans, centers = 6)
d_TX <- dist(cases_TX_scaled_kmeans)
cluster_stats_TX <- sapply(
  list(
    km_TX_2 = k_means_TX_2$cluster,
    km_TX_3 = k_means_TX_3$cluster,
    km_TX_5 = k_means_TX_5$cluster,
    km_TX_6 = k_means_TX_6$cluster
  ),
  FUN = function(x) fpc::cluster.stats(d_TX, x)
)
TX_cluster_stats <- cluster_stats_TX[c("within.cluster.ss", "avg.silwidth", "pearsongamma", "dunn"), ]
print(TX_cluster_stats)
#' **Compairing clusters visualizaiton**
kmeans_TX_2 <- kmeans(cases_TX_scaled_kmeans, centers = 2)
plot_kmeans_TX_2 <- fviz_cluster(kmeans_TX_2, data = cases_TX_scaled_kmeans,
                                 centroids = TRUE, geom = "point", ellipse.type = "norm") +
  labs(title = "K-means Clustering with 2 Centers")
kmeans_TX_3 <- kmeans(cases_TX_scaled_kmeans, centers = 3)
plot_kmeans_TX_3 <- fviz_cluster(kmeans_TX_3, data = cases_TX_scaled_kmeans,
                                 centroids = TRUE, geom = "point", ellipse.type = "norm") +
  labs(title = "K-means Clustering with 3 Centers")
kmeans_TX_5 <- kmeans(cases_TX_scaled_kmeans, centers = 5)
plot_kmeans_TX_5 <- fviz_cluster(kmeans_TX_5, data = cases_TX_scaled_kmeans,
                                 centroids = TRUE, geom = "point", ellipse.type = "norm") +
  labs(title = "K-means Clustering with 5 Centers")
combined_plot_TX <- ggarrange(plot_kmeans_TX_2, plot_kmeans_TX_3, plot_kmeans_TX_5, ncol = 2, nrow = 2)
print(combined_plot_TX)
#'
#'
#'
#'
#'
#' **Testing Kmeans**
# Perform K-means clustering with 5 centers and 25 random starts
kmeans_TX_3 <- kmeans(cases_TX_scaled_kmeans, centers = 3, nstart = 25)
fviz_cluster(kmeans_TX_3, data = cases_TX_scaled_kmeans,
             geom = "point", ellipse.type = "norm", show.clust.cent = TRUE) +
  labs(title = "K-means Clustering with 3 Centers (Texas Counties)")
#' 
#' 
#' 
#' 
#' 
#' 
#' **Step 1.1 Determine Optimal epsilon Value**
library(dbscan)
cases_TX_scaled_dbscan <- scale(cases_TX_prepared_supervised)

# Finding optimal epsilon using k-NN plot (with k set to 5)
kNNdistplot(cases_TX_scaled_dbscan, k = 5)
abline(h = 0.5, col = "red")  # Adjust this value based on the observed "elbow" in the plot
#' **Step 1.2 Visualizing the DBSCAN**
# Applying DBSCAN with optimal epsilon and minPts
dbscan_result_TX <- dbscan(cases_TX_scaled_dbscan, eps = 2.5, minPts = 5)
fviz_cluster(dbscan_result_TX, data = cases_TX_scaled_dbscan, geom = "point") +
  labs(title = "DBSCAN Clustering of Texas Counties")
#'
#'
#'
#'
#'
#' **Hirarchical Clustering**
#Ward's method
hclust_res_TX <- hclust(dist(cases_TX_prepared_supervised), method = "ward.D2")
best_k_TX <- 3  # Replace this with the optimal k if it's different
fviz_dend(hclust_res_TX, k = best_k_TX, show_labels = FALSE, rect = TRUE) +
  labs(title = "Hierarchical Clustering Dendrogram for Texas Counties")
#'
#'
#'
#'
#'
#'
#' **Spectral Clustering**
# Load necessary libraries
library(kernlab)
# Perform spectral clustering with the best number of clusters for Texas counties
best_k_TX <- 3  # Testing 3
cluster_spec_TX <- specc(as.matrix(cases_TX_prepared_supervised), centers = best_k_TX)
fviz_cluster(list(data = as.data.frame(cases_TX_prepared_supervised), cluster = cluster_spec_TX), geom = "point") +
  labs(title = "Spectral Clustering of Texas Counties based on Key Variables")
#'
best_k_TX <- 5  # testing 5
cluster_spec_TX <- specc(as.matrix(cases_TX_prepared_supervised), centers = best_k_TX)
fviz_cluster(list(data = as.data.frame(cases_TX_prepared_supervised), cluster = cluster_spec_TX), geom = "point") +
  labs(title = "Spectral Clustering of Texas Counties based on Key Variables")
#'
#'
#'
#'
#'
#' **Puting the data on the map again**
#' 
#' 
cases_TX_prepared_supervised <- cases_TX %>%
  select(county, median_income, bachelors_degree, vaccine_hesidency, confirmed_cases, deaths)
county_clusters <- data.frame(
  county = cases_TX$county, 
  cluster = kmeans_TX_3$cluster
)
counties_polygon_TX <- counties_polygon_TX %>%
  mutate(county = tolower(county))
counties_polygon_TX <- counties_polygon_TX %>%
  left_join(county_clusters, by = "county")
ggplot(counties_polygon_TX, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = as.factor(cluster)), color = "white") +  
  coord_quickmap() +
  scale_fill_viridis_d(option = "C") +  
  labs(title = "Texas Counties by Cluster Grouping (3 Clusters)", fill = "Cluster") +
  theme_minimal()
#'
#' **Supervised Learning** **Supervised Learning** **Supervised Learning** **Supervised Learning** **Supervised Learning**
#'
#'
#'**Kmeans supervised**
# Select the main variables and normalizing them
data_supervised <- cases_TX %>%
  select(median_income, deaths_per_1000, bachelors_degree, median_age, vaccine_hesidency, population_density)
data_supervised_scaled <- scale(data_supervised)
data_supervised_weighted <- as.data.frame(data_supervised_scaled) %>%
  mutate(weight = data_supervised$deaths_per_1000)
set.seed(123)
kmeans_supervised <- kmeans(data_supervised_weighted[, -ncol(data_supervised_weighted)], centers = 3, nstart = 10, iter.max = 100)
data_supervised$cluster <- kmeans_supervised$cluster
fviz_cluster(kmeans_supervised, data = data_supervised_weighted[, -ncol(data_supervised_weighted)],
             geom = "point", ellipse.type = "convex") +
  labs(title = "Supervised Clustering of Texas Counties with Emphasis on COVID-19 Deaths")
#'
#'
#'
#'**GMM**
library(mclust)
library(factoextra)
data_supervised <- cases_TX %>%
  select(median_income, deaths_per_1000, bachelors_degree, median_age, vaccine_hesidency, population_density)
gmm_result <- Mclust(data_supervised, G = 3)
summary(gmm_result)
plot(gmm_result, what = "classification")
data_supervised$gmm_cluster <- gmm_result$classification
fviz_cluster(list(data = data_supervised, cluster = data_supervised$gmm_cluster),
             geom = "point") +
  labs(title = "Gaussian Mixture Model Clustering of Texas Counties")
#'
#'
#'**Decision Tree**
library(rpart)
library(rpart.plot)

median_death_rate <- median(data_supervised$deaths_per_1000)
data_supervised$death_rate_label <- ifelse(data_supervised$deaths_per_1000 > median_death_rate, "High", "Low")
decision_tree <- rpart(death_rate_label ~ median_income + bachelors_degree + median_age + 
                         vaccine_hesidency + population_density, data = data_supervised, method = "class")
rpart.plot(decision_tree, main = "Decision Tree for COVID-19 Death Rate Clustering")
predicted_clusters <- predict(decision_tree, data_supervised, type = "class")
data_supervised$tree_cluster <- as.numeric(as.factor(predicted_clusters))
fviz_cluster(list(data = data_supervised %>% select(median_income, bachelors_degree, median_age, 
                                                    vaccine_hesidency, population_density), 
                  cluster = data_supervised$tree_cluster),
             geom = "point") +
  labs(title = "Decision Tree Clustering of Texas Counties Based on COVID-19 Death Rates")

