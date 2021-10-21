#install.packages("VIM")
install.packages("sjPlot")

pacman::p_load(tidyverse, rlist, skimr, caret, VIM, sjPlot, factoextra, NbClust)

cluster <- read_csv("data/Measures_by_Hospital_Acute_Care_New.csv")

glimpse(cluster)

#normalize the measure values on a scale from 0 to 1 using min-max scaling
process = preProcess(as.data.frame(cluster[2:length(cluster)]), method=c("range"))
cluster_norm <- predict(process, as.data.frame(cluster[1:length(cluster)]))
glimpse(cluster_norm)

#resolve missing values using KNN imputation
kVal = floor(sqrt(nrow(cluster_norm)))

cluster_norm <- cluster_norm %>%
  kNN(variable = 1:length(cluster_norm), k = kVal, imp_var = FALSE)

glimpse(cluster_norm)

#implement clustering

cluster_norm <- cluster_norm[2:length(cluster_norm)]

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

#implement and plot clustering
k2 <- kmeans(cluster_norm, centers = 3, nstart = 25)
str(k2)
fviz_cluster(k2, data = cluster_norm)
