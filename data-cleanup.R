#sources:
#https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/

#importing and cleaning data
pacman::p_load(pacman, tidyr, tidyverse, dplyr, rlist, skimr, ggplot2)

#import datasets - starting with first one as a test
#make lists for the different data conditions
Hospital_Data <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = TRUE)
names(Hospital_Data) <- list.files(path = "C:/Users/lkn56/Desktop/School Stuff/Fall 2021/CSCI 487/hospitals_current_data/EVALUATED", pattern = "*.csv", full.names = FALSE)

Transposed_Data <- NULL
Not_Transposed <- NULL
Duplicates_Exist <- NULL
T_Names <- NULL
NT_Names <- NULL

for(j in 1:length(Hospital_Data)) {
  data = read_csv(Hospital_Data[1])
  View(data)
  
  #group the data by facility.ID to check for duplicate rows
  data_summary <-
    data %>%
    group_by(Facility.ID) %>%
    dplyr::summarize(count = n())

  #check how many duplicate facility ID's there are in each dataframe
  var_count <- 0
  for(i in 1:nrow(data_summary)) {
    if( data_summary$count[i] > 1 ) {
      var_count = var_count + 1;
    }
  }

  #print(paste0("duplicates: ", var_count, " in dataset: ", Hospital_Data[j]))
  #separate dataframes with duplicates and without duplicates into separate lists
  
  if( var_count == 0 ) {
    Transposed_Data <- list.append(Transposed_Data, list(data))
    T_Names <- list.append(T_Names, names(Hospital_Data[1]))
  }
  
  if( var_count > 0 ) {
    Not_Transposed <- list.append(Not_Transposed, list(data))
    NT_Names <- list.append(NT_Names, names(Hospital_Data[1]))
  }
}

#Not_Transposed[1] is transposed but has duplicates - Move element 1 to Transposed_Data list
Transposed_Data <- list.append(Transposed_Data, Not_Transposed[1])
Not_Transposed <- Not_Transposed[-1]

View(Not_Transposed)
View(Not_Transposed[[1]])

#Not Transposed data frames
for(j in 1:length(Not_Transposed)) {
  glimpse(Not_Transposed[[j]])
  cat("\n", NT_Names[j], "\n")
}

#Transposed data frames
for(j in 1:length(Transposed_Data)) {
  glimpse(Transposed_Data[[j]])
  cat("\nNext Dataframe...\n")
}

T_Names
print("\nHospital General Information\n")
glimpse(Transposed_Data[[4]])

print("\nHVBP Safety\n")
glimpse(Transposed_Data[[9]])
