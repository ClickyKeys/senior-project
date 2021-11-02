#install.packages("VIM")
#install.packages("sjPlot")

pacman::p_load(tidyverse, rlist, skimr, caret, VIM, sjPlot, factoextra, NbClust, cluster, rgl)

##### Remove missing values and normalize the dataframe #####
#cluster <- read_csv("data/Measures_by_Hospital_Acute_Care_New.csv")
cluster <- read_csv("data/Measures_by_Hospital_Acute_Care_V2.csv")
glimpse(cluster)

#manually normalize data using the min-max method
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

for(i in 2:length(cluster)) {
  cluster[i] <- normalize(cluster[i])
}

#cluster_NAR <- as.data.frame(lapply(cluster[2:length(cluster)], normalize))
cluster_NAR <- cluster
glimpse(cluster_NAR)

#impute NA values using KNN method
kVal = floor(sqrt(nrow(cluster_NAR)))
cluster_NAR <- cluster_NAR %>%
  kNN(variable = 2:length(cluster_NAR), k = kVal, imp_var = FALSE)
glimpse(cluster_NAR)

write_csv(cluster_NAR, "data/cluster_normalized_V2.csv")

##### implement clustering #####

cluster_norm <- read_csv("data/cluster_normalized_V2.csv")
glimpse(cluster_norm)

#group measures
# cluster_norm$MORT <- rowMeans(subset(cluster_norm, select = c(2:13, 43, 47:48)))
# cluster_norm$READM <- rowMeans(subset(cluster_norm, select = c(14:16, 35:36, 39:42)))
# cluster_norm$HCAHPS <- rowMeans(subset(cluster_norm, select = c(17:26)))
# cluster_norm$HVBP <- rowMeans(subset(cluster_norm, select = c(50:64)))
# cluster_norm$HVBP_Total <- rowMeans(subset(cluster_norm, select = c(65)))
# cluster_norm$HAI <- rowMeans(subset(cluster_norm, select = c(28)))
# cluster_norm$OP <- rowMeans(subset(cluster_norm, select = c(29:30, 37:38)))
# cluster_norm$HAC <- rowMeans(subset(cluster_norm, select = c(44)))
# cluster_norm$MSPB <- rowMeans(subset(cluster_norm, select = c(46, 49)))

#separate the Facility ID's from the variables for clustering
rownames(cluster_norm) <- cluster_norm$Facility.ID
cluster_norm_vars <- cluster_norm %>%
  select(c(-"Hospital.overall.rating", -"Facility.ID")) 
glimpse(cluster_norm_vars)

#kmeans method
# methods to determine k:
# Elbow method 
fviz_nbclust(cluster_norm_vars, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") # add subtitle

set.seed(123)
#gap stat
MyKmeansFUN <- function(x,k) list(cluster=kmeans(x, k, iter.max=100))

fviz_nbclust(cluster_norm_vars, FUNcluster=MyKmeansFUN, method="gap_stat") +
  labs(subtitle = "Gap statistic method")

# Silhouette method
fviz_nbclust(cluster_norm_vars, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

#test number of clusters for kmeans
nbclust_out <- NbClust(
  data = cluster_norm_vars,
  distance = "euclidean",
  min.nc = 2, # minimum number of clusters
  max.nc = 5, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)
 
set.seed(123)

# k-means clustering method - normalized
#principle component analysis to see which variables are responsible for most variance
cluster.pca <- prcomp(cluster_norm_vars, center = TRUE, scale. = TRUE)
cluster.pca$center

importance <- as.data.frame(pca$importance[2, ]) %>%
  mutate(names = names) %>%
  rename(PC = `pca$importance[2, ]`) %>%
  mutate(PC = PC*100)

#plot importance to show principle components with most variance
#Geometrically speaking, principal components represent the directions of the data that explain a maximal amount of variance, 
#that is to say, the lines that capture most information of the data.
importance %>%
  ggplot() +
  geom_bar(mapping = aes(x = reorder(x = names, PC), y = PC, fill = names), stat = "identity") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(breaks = seq(min(0), max(40), by = 1)) +
  labs(title = "Principle Component Analysis") +
  geom_hline(yintercept = 39.8, linetype="dotted", color = "red") +
  geom_hline(yintercept = 06.6, linetype="dotted", color = "red") +
  ylab("Percentage of explained variance") + 
  xlab("Principle Compoenent") +
  guides(fill = "none") +
  coord_flip()

# Test cluster optimization with a silhouette plot
# https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/#visualizations
km4 <- kmeans(cluster_norm_vars, centers = 4, nstart = 55)
sil <- silhouette(km4$cluster, dist(cluster_norm_vars))
fviz_silhouette(sil)

# Plot the cluster on 2 dimensions (uses principle component analysis)
fviz_cluster(km4, data = cluster_norm_vars, geom = "point", ellipse.type = c("norm"),
             palette = "Set1", ggtheme = theme_minimal())

?fviz_cluster
# Heirarchical method

#find distance matrix and perform euclidean clustering
dist_matrix <- dist(cluster_norm_vars, method = "euclidean")
hc_euclidean <- hclust(dist_matrix, method = "complete")
plot(hc_euclidean)

#agnes function complete 
#https://www.r-bloggers.com/2017/12/how-to-perform-hierarchical-clustering-using-r/n to get the agglomerative coefficient
hc_agnes <- agnes(cluster_norm_vars, method = "complete")
hc_agnes$ac
plot(hc_agnes)

# Cutting tree by no. of clusters
clust <- cutree(hc_agnes, k = 4 )
rect.hclust(hc_agnes, k = 4, border = 2:10)

fviz_cluster(list(data = cluster_norm_vars, cluster = clust, geom = "point"), geom = "point", repel = TRUE, ellipse.type = "norm")

#Add cluster results to original data frame
cluster_results <- as.data.frame(km4$cluster)
hosp_clusters <- bind_cols(cluster_norm, cluster_results)

glimpse(clust_analysis)
clust_analysis <- hosp_clusters %>%
  group_by(`km4$cluster`) %>%
  summarise_at(vars(Hospital.overall.rating), funs(mean(., na.rm=TRUE)))
 
  
clust_analysis %>%
  ggplot() +
  geom_bar(mapping = aes(x = ""))

star_ratings <- read_csv("")

