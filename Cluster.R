#install.packages("VIM")
#install.packages("sjPlot")

pacman::p_load(tidyverse, rlist, skimr, caret, VIM, sjPlot, factoextra, NbClust)


##### Remove missing values and normalize the dataframe #####
cluster <- read_csv("data/Measures_by_Hospital_Acute_Care_New.csv")
glimpse(cluster)

#impute NA values using KNN method
kVal = floor(sqrt(nrow(cluster)))

cluster <- cluster %>%
  kNN(variable = 1:length(cluster), k = kVal, imp_var = FALSE)
glimpse(cluster)

#new data frame for the dataframe with no NA values
cluster_NAR <- cluster

#manually normalize data using the min-max method
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

for(i in 2:length(cluster_NAR)) {
  cluster_NAR[i] <- normalize(cluster_NAR[i])
}
glimpse(cluster_NAR)

write_csv(cluster_NAR, "data/cluster_normalized.csv")

##### implement clustering #####

cluster_norm <- read_csv("data/cluster_normalized.csv")

#heirarchical method
dist_matrix <- dist(cluster_norm, method = 'euclidean')

hclust_avg <- hclust(dist_matrix, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 5)
plot(hclust_avg)
rect.hclust(hclust_avg, k = 5, border = 2:6)
abline(h = 5, col = 'red')

install.packages('package_name', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)

#kmeans method

# methods to determine k:
# Elbow method 
fviz_nbclust(cluster_norm, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
  labs(subtitle = "Elbow method") # add subtitle

# Silhouette method
fviz_nbclust(cluster_norm, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Gap statistic (takes way too long)
set.seed(42)
fviz_nbclust(cluster_norm, kmeans,
             nstart = 25,
             method = "gap_stat",
             nboot = 500 # reduce it for lower computation time (but less precise results)
) +
  labs(subtitle = "Gap statistic method")

nbclust_out <- NbClust(
  data = cluster_NAR,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

#implement and plot clustering
glimpse(cluster_norm)

cluster_norm <- cluster_norm[50:length(cluster_norm)]
k2 <- kmeans(cluster_norm, centers = 3, nstart = 50)
fviz_cluster(k2, data = cluster_norm)
print(k2$cluster)

# #check normalization scale to non-normalized scale
# cluster %>%
#   ggplot() +
#   geom_histogram(mapping = aes(y = Total.Performance.Score, binwidth = 5))
# 
# cluster_norm %>%
#   ggplot() +
#   geom_histogram(mapping = aes(y = Total.Performance.Score , binwidth = 5))
