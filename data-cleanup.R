#sources:
#https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/

#importing and cleaning data init
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(rlist)

# bind datasets (use later)
# path <- setwd("C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/senior-project/hospitals_current_data/EVALUATED")
# View(path)
# Hospital_Data <-  dir(path)
# data_frame <- do.call(rbind.fill, lapply(Hospital_Data, read.csv))
# View(data_frame)

#import datasets - starting with first one as a test
#make lists for the different data conditions
Hospital_Data <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = TRUE)
Transposed_Data <- NULL
Not_Transposed <- NULL
Duplicates_Exist <- NULL

for(j in 1:length(Hospital_Data)) {
  data = read.csv(Hospital_Data[j])
  #View(data)
  
  #group the data by facility.ID to check for duplicate rows
  data_summary <-
    data %>%
    group_by(Facility.ID) %>%
    dplyr::summarize(count = n())
  
  #check how many duplicates there are in each dataframe
  var_count <- 0
  for(i in 1:nrow(data_summary)) {
    if( data_summary$count[i] > 1 ) {
      var_count = var_count + 1;
    }
  }
  print(paste0("duplicates: ", var_count, " in dataset: ", Hospital_Data[j]))
  
  if( var_count == 0 ) {
    Transposed_Data <- list.append(Transposed_Data, Hospital_Data[j])
  }
  
  if( var_count > 100 ) {
    Not_Transposed <- list.append(Not_Transposed, Hospital_Data[j])
  }

  if( var_count > 0 & var_count < 100 ) {
    Duplicates_Exist <- list.append(Duplicates_Exist, Hospital_Data[j])
  }
}

print(length(Transposed_Data))
print(length(Not_Transposed))
print(length(Duplicates_Exist))
#change...
