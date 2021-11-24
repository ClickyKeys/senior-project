#install.packages("MASS")

pacman::p_load(tidyverse, caret, MASS, cvms)

star_ratings <- read_csv("data/Hospital_General_Information.csv")
hospital_characteristics <- read_csv("data/Provider_of_Services_File_Hospital_Non_Hospital_Facilities_Dataset_2020_Q4.csv")

hospitals <- star_ratings %>%
  filter(`Hospital Type` == "Acute Care Hospitals") %>%
  dplyr::select(c("Facility ID", "Facility Name", 
                  "Emergency Services", 
                  "Hospital overall rating")) %>%
  rename(Facility.ID = "Facility ID") %>%
  rename(star_rating = "Hospital overall rating") %>%
  mutate(star_rating = as.numeric(star_rating)) %>%
  na.omit()

characteristics <- hospital_characteristics %>%
  dplyr::select(c("PRVDR_NUM", "CBSA_URBN_RRL_IND", "ACRDTN_TYPE_CD",
                  "CRTFD_BED_CNT", "MDCL_SCHL_AFLTN_CD", "OPRTG_ROOM_CNT", 
                  "CRNA_CNT", "LPN_LVN_CNT", "DIETN_CNT", "LAB_TCHNCN_CNT", 
                  "NRS_PRCTNR_CNT", "PHYSN_CNT", "REG_PHRMCST_CNT", 
                  "NUCLR_MDCN_TCHNCN_CNT", "TOT_AFLTD_ESRD_CNT", "TOT_AFLTD_HHA_CNT", 
                  "TOT_AFLTD_SNF_CNT", "GNRL_CNTL_TYPE_CD")) %>%
  rename(Facility.ID = "PRVDR_NUM") %>%
  mutate(ACRDTN_TYPE_CD = as.character(ACRDTN_TYPE_CD)) %>%
  mutate(MDCL_SCHL_AFLTN_CD = as.character(MDCL_SCHL_AFLTN_CD))

char_and_rate <- left_join(hospitals, characteristics, by = "Facility.ID")
View(char_and_rate)

#recategorize ownership type variable with sparse data for some categories
char_and_rate <- char_and_rate %>%
  mutate(star_rating = as.factor(star_rating)) %>%
  mutate(GNRL_CNTL_TYPE_CD = case_when(GNRL_CNTL_TYPE_CD == "01" ~ "Private_NFP", 
                                       GNRL_CNTL_TYPE_CD == "02" ~ "Private_NFP", 
                                       GNRL_CNTL_TYPE_CD == "05" ~ "Government", 
                                       GNRL_CNTL_TYPE_CD == "06" ~ "Government", 
                                       GNRL_CNTL_TYPE_CD == "07" ~ "Government", 
                                       GNRL_CNTL_TYPE_CD == "10" ~ "Government", 
                                       GNRL_CNTL_TYPE_CD == "03" ~ "Other", 
                                       GNRL_CNTL_TYPE_CD == "08" ~ "Other", 
                                       GNRL_CNTL_TYPE_CD == "09" ~ "Other", 
                                       TRUE ~ "Private_FP")) %>%
  mutate(ACRDTN_TYPE_CD = case_when(ACRDTN_TYPE_CD == "2" ~ "1", 
                                    ACRDTN_TYPE_CD == "3" ~ "1", 
                                    ACRDTN_TYPE_CD == "9" ~ "1", 
                                    TRUE ~ "0")) %>%
  mutate(MDCL_SCHL_AFLTN_CD = case_when(MDCL_SCHL_AFLTN_CD == "2" ~ "1", 
                                        MDCL_SCHL_AFLTN_CD == "3" ~ "1", 
                                        MDCL_SCHL_AFLTN_CD == "4" ~ "0", 
                                        TRUE ~ "1"))

View(char_and_rate)

char_and_rate %>%
  ggplot() +
  geom_histogram(mapping = aes(star_rating), stat = "count")

#check number of rows per category for categorical variable PRVDR_CTGRY
glimpse(char_and_rate)

set.seed(123)
  
training_data <- char_and_rate %>%
  dplyr::select(c(-"Facility.ID", -"Facility Name")) %>%
  sample_frac(.80)

training_data_actual <- training_data %>%
  dplyr::select(c("star_rating"))

model_fit <- polr(star_rating~., data = training_data, Hess = TRUE)
summary(model_fit)

summary_table <- coef(summary(model_fit))
pval <- pnorm(abs(summary_table[, "t value"]),lower.tail = FALSE)* 2
summary_table <- cbind(summary_table, "p value" = round(pval, 3))
write_csv(as.data.frame(summary_table),"data/regressionResults.csv")


#predict on 20% of the data
testing_data <- anti_join(char_and_rate, training_data)

testing_data_actual <- testing_data %>%
  dplyr::select(c("star_rating"))

testing_data <- testing_data %>%
  dplyr::select(c(-"Facility.ID", -"star_rating", -"Facility Name"))

prediction <- round(predict(model_fit, testing_data, type = "p"), 3)
prediction <- as.data.frame(prediction) 
View(prediction)
  
prediction$prediction_max <- pmax(prediction$`1`, 
                                  prediction$`2`, 
                                  prediction$`3`, prediction$`4`, 
                                  prediction$`5`)
prediction$predicted_class <- NA

prediction_class <- prediction %>%
  mutate(predicted_class = as.factor(ifelse(prediction_max == `4`, "4", 
                                   ifelse(prediction_max == `3`, "3",
                                          ifelse(prediction_max == `2`, "2",
                                                 ifelse(prediction_max == `1`, "1", "5"))))))


prediction_class$predicted_class <- factor(prediction_class$predicted_class, 
                                           levels = c("1", "2", "3", "4", "5"))

View(prediction_class)
View(training_data_ids)

cm2 <- confusion_matrix(as.factor(testing_data_actual$star_rating), 
                        as.factor(prediction_class$predicted_class))
View(cm2)

plot_confusion_matrix(cm2$`Confusion Matrix`[[1]], 
                      place_x_axis_above = FALSE, 
                      palette = "Greens",
                      add_row_percentages = FALSE, 
                      add_col_percentages = FALSE) +
  labs(title = "Star Ratings Prediction", 
       subtitle = "Overall Accuracy: 32%, Balanced Accuracy: 52%")
