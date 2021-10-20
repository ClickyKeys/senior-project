pacman::p_load(tidyverse, rlist, skimr)

cluster <- read_csv("data/Measures_by_Hospital_Acute_Care.csv")

View(cluster)
glimpse(cluster)

#normalize the measure values on a scale from 0 to 1


