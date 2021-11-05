#install.packages("VIM")
#install.packages("sjPlot")
install.packages("fossil")

pacman::p_load(tidyverse, rlist, skimr, caret, VIM, sjPlot, factoextra, NbClust, cluster, rgl, fossil)

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

write_csv(cluster_NAR, "data/cluster_normalized.csv")

##### implement clustering #####

cluster_norm <- read_csv("data/cluster_normalized.csv")
glimpse(cluster_norm)

#separate the Facility ID's from the variables for clustering
rownames(cluster_norm_vars) <- cluster_norm$Facility.ID

cluster_norm_vars <- cluster_norm %>%
  select(c(-"Total.Performance.Score", -"Overall.Rating.of.Hospital.Dimension.Score", -"HCAHPS.Base.Score", -"HCAHPS.Consistency.Score",
           -"Facility.ID", -"Total.HAC.Score", -"H_HSP_RATING_STAR_RATING.x", -"H_STAR_RATING.x", -"Weighted.Normalized.Clinical.Outcomes.Domain.Score",
           -"Unweighted.Person.and.Community.Engagement.Domain.Score", -"Weighted.Person.and.Community.Engagement.Domain.Score", -"Unweighted.Normalized.Efficiency.and.Cost.Reduction.Domain.Score",
           -"Weighted.Efficiency.and.Cost.Reduction.Domain.Score")) 

glimpse(cluster_norm_vars)

set.seed(123)

# k-means clustering method - normalized
#principle component analysis to see which variables are responsible for most variance
cluster.pca <- prcomp(cluster_norm_vars, center = TRUE, scale. = TRUE)
View(cluster.pca$x)
fviz_eig(cluster.pca)

cluster_nomr_vars <- as.data.frame(cluster.pca$x) %>%
  select(c(PC1, PC2))

#evaluate strongly correlated variables for dimensionality reduction
fviz_pca_var(cluster.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE,
             max.overalps = 20
)

#results for variables
cluster.var <- get_pca_var(cluster.pca)
groups <- as.data.frame(cluster.var$coord) %>%
  transform(Measures = row.names(cluster.var$coord)) %>%
  select(c(Measures, Dim.1, Dim.2)) %>%
  arrange(Dim.1, Dim.2)

glimpse(cluster_norm_vars)

#reduce dimensionality by grouping based on similarity
cluster_norm_vars$HCAHPS <- rowMeans(subset(cluster_norm_vars, select = c(16:24, 46:52)))
cluster_norm_vars$SEP <- rowMeans(subset(cluster_norm_vars, select = c(28:31)))
cluster_norm_vars$MORT <- rowMeans(subset(cluster_norm_vars, select = c(1:12, 40)))
cluster_norm_vars$READM <- rowMeans(subset(cluster_norm_vars, select = c(13:15,32:33, 36:39)))
cluster_norm_vars$OP <- rowMeans(subset(cluster_norm_vars, select = c(26:27, 34:35)))

cluster_norm_vars <- cluster_norm_vars %>%
  select(c(25, 41:45, 53:57))

#kmeans method
# methods to determine k:
# Elbow method 
fviz_nbclust(cluster_norm_vars, kmeans, method = "wss") +
  labs(subtitle = "Elbow method") # add subtitle

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
  max.nc = 6, # maximum number of clusters
  method = "kmeans" # one of: "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", "kmeans"
)

# Test cluster optimization with a silhouette plot
# https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/#visualizations
km4 <- kmeans(cluster_norm_vars, centers = 3, nstart = 55)
sil <- silhouette(km4$cluster, dist(cluster_norm_vars))
fviz_silhouette(sil)

# Plot the cluster on 2 dimensions (uses principle component analysis)
fviz_cluster(km4, data = cluster_norm_vars, ellipse.type = c("norm"), geom = "point",
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

View(cluster_norm)
View(cluster_results)
glimpse(hosp_clusters)
glimpse(star_ratings)

hosp_clusters <- hosp_clusters %>%
  select(c("Facility.ID", "km4$cluster")) %>%
  rename(cluster = `km4$cluster`) %>%
  transform(Facility.ID = as.character(Facility.ID))
glimpse(hosp_clusters)

#compare clustering results of ground truth overall star ratings
star_ratings <- read_csv("data/Hospital_General_Information.csv")
star_ratings <- star_ratings %>%
  filter(`Hospital Type` == "Acute Care Hospitals") %>%
  select(c("Facility ID", "Hospital overall rating")) %>%
  rename(Facility.ID = `Facility ID`) %>%
  rename(Star.Rating = `Hospital overall rating`) %>%
  transform(Star.Rating = as.numeric(Star.Rating))

star_ratings$Facility.ID <- star_ratings$Facility.ID %>%
  trimws("left", "0")
glimpse(star_ratings)

hosp_cluster_comparison <- full_join(hosp_clusters, star_ratings)
glimpse(hosp_cluster_comparison)

clust_analysis <- hosp_cluster_comparison %>%
  group_by(cluster, Star.Rating) %>%
  count()
glimpse(clust_analysis)

clust_analysis %>%
  ggplot() +
  geom_bar(mapping = aes(x = Star.Rating, y = n, fill = cluster), stat = "identity") +
  facet_wrap(~ cluster)

#Use rand index to comparison compare clustering accuracy
hosp_clust_comp <- hosp_cluster_comparison %>%
  na.omit()
glimpse(hosp_clust_comp)

rand.index(hosp_clust_comp$Star.Rating,  hosp_clust_comp$cluster)
