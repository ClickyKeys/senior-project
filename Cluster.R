#install.packages("VIM")
#install.packages("sjPlot")
#install.packages("fossil")

pacman::p_load(tidyverse, rlist, skimr, caret, VIM, sjPlot, factoextra, NbClust, cluster, rgl, fossil)

##### Remove missing values and normalize the dataframe #####
cluster <- read_csv("data/Measures_by_Hospital_Acute_Care_New.csv")
glimpse(cluster)

#manually normalize data using the min-max method
normalize <- function(x) {
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

for(i in 2:length(cluster)) {
  cluster[i] <- normalize(cluster[i])
}

#impute NA values using KNN method
kVal = floor(sqrt(nrow(cluster_norm)))

cluster_NAR <- cluster_norm %>%
  kNN(variable = 2:length(cluster_norm), k = kVal, imp_var = FALSE)
glimpse(cluster_NAR)

write_csv(cluster_NAR, "data/cluster_normalized_z-score.csv")

##### implement clustering #####

cluster_norm <- read_csv("data/cluster_normalized.csv")
glimpse(cluster_norm)

#separate the Facility ID's from the variables for clustering
rownames(cluster_norm_vars) <- cluster_norm$Facility.ID

#(almost) all data
cluster_norm_vars <- cluster_norm %>%
  select(c(-"Facility.ID"))
glimpse(cluster_norm_vars)

#with summary scores only
#TO DO#

#with raw scores only
#TO DO#

set.seed(123)

# k-means clustering method - normalized
#principle component analysis to see which variables are responsible for most variance
cluster.pca <- prcomp(cluster_norm_vars, center = TRUE, scale. = TRUE)
fviz_eig(cluster.pca)

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

groups %>%
  ggplot() +
  geom_point(mapping = aes(x = Dim.1, y = Dim.2))
View(groups)

# kmeans clustering method
# methods to determine k:
# Elbow method 
cluster_norm_vars %>%
  fviz_nbclust(FUN = kmeans, method = "wss")

#silhouette method
cluster_norm_vars %>%
  fviz_nbclust(FUN = kmeans, method = "silhouette")

# Test cluster optimization with a silhouette plot
# https://statsandr.com/blog/clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/#visualizations
km4 <- kmeans(cluster_norm_vars, centers = 4, nstart = 55)

# Plot the cluster on 2 dimensions (uses principle component analysis)
fviz_cluster(km4, data = cluster_norm_vars, geom = "point",
             palette = "Set1", ggtheme = theme_minimal())


#Hierarchical Method
#elbow method and silhouette method

cluster_norm_vars %>%
  fviz_nbclust(FUN = hcut, method = "wss")

#suggests 10
cluster_norm_vars %>%
  fviz_nbclust(FUN = hcut, method = "silhouette")

hc <- cluster_norm_vars %>%
  dist %>%
  hclust

hc %>%
  plot(
    hang = -1,
    cex = 0.6
  )

#cut the dendrogrm
hc %>%
  rect.hclust(k = 3, border = 2:5)

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
