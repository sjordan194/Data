library(readxl)
library(dplyr)
library(caret)
library(randomForest)
library(tidyverse)
library(e1071)
library(showtext)
library(treemapify)
font_add_google("Roboto", "roboto")
showtext_auto()
#Loading dataset
seru_data2 <- read_excel("C:/Users/jordan.194/OneDrive - The Ohio State University/MTDA 5401/SERU 2025 Anonymized Raw Data.xlsx")
#Cleaning column names.
names(seru_data2) <- make.names(names(seru_data2), unique = TRUE)

#Converting the continuous GPA variable into a categorical with 3.5 beign the cutoff for high and low GPA.
seru_data2 <- seru_data2 %>% 
  mutate(GPA_category = ifelse(CUMGPA >= 3.5, "High", "Low"))
names(seru_data2)
#Removing the text columns that are not needed for the analysis. 
seru_data_filtered2 <- seru_data2 %>% 
  select(-matches("(_Text|_TXT)$", ignore.case = TRUE))

#removing known text columns
seru_data_filtered2 <- seru_data_filtered2 %>%
  select(-any_of(c(
    "OS001_SW_OtherText", "OS002_SW_OtherText", "OS004_SW_OtherText",
    "OS005_SW_OtherText", "OS006_SW_OtherText", "OS007_SW_OtherText",
    "OS008_SW_OtherText", "OS009_SW_OtherText"
  )))

#Summary of all of the missing values in the dataset. Total NA values 535965 and total 99/98/-1 values 34490.
num_na <- sum(is.na(seru_data_filtered2))
num_standins <- sum(seru_data_filtered2 == 99, na.rm = TRUE) + 
  sum(seru_data_filtered2 == 98, na.rm = TRUE) + 
  sum(seru_data_filtered2 == -1, na.rm = TRUE)
cat("Total NA Values", num_na, "\n")
cat("Total 99/98/-1 Values", num_standins, "\n") #total NA values 535965 and total 99/98/-1 values 34490.


#Imputing missing values with the median for numeric variables and the mode for categorical variables.
seru_imputed2 <- seru_data_filtered2 %>% 
  mutate(across(where(is.numeric), ~ replace(.x, .x %in% c(99, 98, -1), NA))) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)))
#Removing the bio demo and format columns.
seru_imputed2 <- seru_imputed2 %>%
  select(-RECORDEDDATE, -RESPONDENTUNIQUEKEY, -DATE_STARTED, 
         -COMPLETED_DATE, -PROGRESS, -DURATION, -UNIVERSITY, 
         -BROWSER1, -VERSION1, -OS1, -RESOLUTION1, -CONSENT, 
         -GS0101_GSYPPROGNAME, -Academic_Program, -GS0105, 
         -LOCATION, -PROF_DEV_MOD, -TIME_MOD, -WELLBEING_MOD, 
         -WILDCARD_MOD, -CIP_CODE2010, -NATIVE_UNITS, -PROGRAM_TEXT1, 
         -SIS_Plan_Code, -INTL_MOD, -INCIVIL_MOD, -ETHNICITY1, 
         -COLLEGE_NAME1, -CIP_CODE1, -AD_VAR1, -RESOLUTION2, 
         -OS2, -VERSION2, -BROWSER2, -GS1103_Additinl_Comments)

#creating an overall satisfaction score by combining the mean values of "Quality of Instruction"
#"Course Availability", "Access to Advising", "Knowledge Gained", "Financial Support", and "Value of Education".

seru_imputed2 <- seru_imputed2 %>%
  mutate(
    GS_Overall_Satisfaction = rowMeans(
      select(., 
             GS_Quality_of_Instruction,
             GS_Course_Availability,
             GS_Access_To_Advising,
             GS_Availability_Advising,
             GS_Knowledge_Gained,
             GS_Financial_Support,
             GS_Value_of_Education),
      na.rm = TRUE
    )
  )

overall_variables <- c("GS_Quality_of_Instruction", 
                       "GS_Course_Availability", 
                       "GS_Access_To_Advising", 
                       "GS_Availability_Advising", 
                       "GS_Knowledge_Gained", 
                       "GS_Financial_Support", 
                       "GS_Value_of_Education")

na_in_Selected <- sum(is.na(seru_data_filtered2[, overall_variables]))
stand_in_overall <- sum(seru_data_filtered2[, overall_variables] %in% c(99, 98, -1))
cat("Total NA Values in the Overall Satisfaction Variables", na_in_Selected, "\n")
cat("Total 99/98/-1 Values in Overall Satisfaction Variables", stand_in_overall, "\n") 
#189 total NA values over selected variables.

#Creating levels for mean values in overall satisfaction so RF treats this as a classification.
seru_imputed2$Overall_Satisfaction_Cat <- cut(seru_imputed2$GS_Overall_Satisfaction, 
                                              breaks = c(-Inf, 2, 3, 4, Inf), 
                                              labels = c("Low", "Moderate", "High", "Very High"), 
                                              right = TRUE)

#Removing columns that were included in the Overall_Satisfaction variable as to not overly influence the model.
seru_imputed2 <- seru_imputed2 %>% 
  select(-GS_Quality_of_Instruction, -GS_Course_Availability, -GS_Access_To_Advising, 
         -GS_Availability_Advising, -GS_Knowledge_Gained, -GS_Financial_Support, 
         -GS_Value_of_Education, -CUMGPA)

table(seru_imputed2$Overall_Satisfaction_Cat)

library(patchwork)
#Creating a bar plot to show the distribution of the Overall Satisfaction categories.

ggplot(seru_imputed2, aes(x  = Overall_Satisfaction_Cat, fill = Overall_Satisfaction_Cat)) + 
  geom_bar(width = 0.5) + 
  geom_text(stat = "count", 
            aes(label = paste0(after_stat(count), 
                                "\n(", 
                                scales::percent(after_stat(count)/sum(after_stat(count)), accuracy = 1), 
                                ")")), 
                vjust = -0.5, color = "grey30", 
                fontface = "bold", lineheight = 0.6, 
                size = 10) + 
  scale_fill_manual(values = c("Low"= "#D55E00", "Moderate" = "#999999", "High" = "#0072B2")) + 
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
  labs(x = "Overall Satisfaction Categories", 
       y = NULL) + 
  theme_minimal(base_family = "roboto") + 
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 32, lineheight = 1.2),
        axis.text.x = element_text(size = 28, color = "gray30"), , 
        axis.text.y = element_blank(), 
        axis.ticks.x = element_line(linewidth = 0.4), 
        axis.ticks.length.x = unit(8, "pt"), 
        axis.title.x = element_text(margin = ggplot2::margin(t = 15)),
        plot.background = element_rect(fill = "#F9F9F9", color = NA), 
        panel.background = element_rect(fill = "#F9F9F9", color = NA), 
        legend.position = "none")
  
ggsave("SERU Overall Satisfaction Distribution.png", width = 8, height = 6, dpi = 300)

ggsave("SERU Overall Satisfaction Distribution2.png", width = 10, height = 6, dpi = 300, 
       scale = 0.8)
#Dropping the Very high category as there are no entries in this category.
seru_imputed2$Overall_Satisfaction_Cat <- droplevels(seru_imputed2$Overall_Satisfaction_Cat)

#Imputing all other NA values.
seru_imputed2 <- seru_imputed2 %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
#Creating function to identify the most frequently occurring value for text columns.
seru_imputed2 <- seru_imputed2 %>% 
  mutate(across(where(is.character), ~ {
    mode_val <- names(which.max(table(.)))
    replace(., is.na(.), mode_val)
  }))
#Replacing character values that have NAs with the the mode.
seru_imputed2 <- seru_imputed2 %>%  
  mutate(across(where(is.character), ~ {
    mode_val <- names(which.max(table(.)))
    replace(., is.na(.), mode_val)
  }))

#Converting character columns to a factor.
seru_imputed2 <- seru_imputed2 %>% 
  mutate(across(where(is.character), as.factor))

#Converting numeric variables to factor
#Identify numeric variables with <=5 unique values
numeric_vars <- sapply(seru_imputed2, is.numeric)
few_unique <- sapply(seru_imputed2[, numeric_vars], function(x) length(unique(x)) <= 5)

#Review these variables
cat_vars_to_convert <- names(few_unique[few_unique])
#remove known non-categorical numeric variables (e.g., IDs, scores)
cat_vars_to_convert <- setdiff(cat_vars_to_convert, c("ID.Mask", "GS2_PRMYFCTR_Overall", "GS0303_Funding_Type"))
#Convert them to factors (or ordered factors if they have ordinal meaning)
seru_imputed2[cat_vars_to_convert] <- lapply(seru_imputed2[cat_vars_to_convert], factor)
# Check results
str(seru_imputed2[cat_vars_to_convert[1:5]])  # show first few converted variablesinstall

set.seed(42)
seru_split2 <- createDataPartition(seru_imputed2$Overall_Satisfaction_Cat, p = 0.8, list = FALSE)
train_data2 <- seru_imputed2[seru_split2, ]
test_data2 <- seru_imputed2[-seru_split2, ]
#Removing the Overall_Satisfaction numeric score as this may influence the model's performance.
#There was also an errant column INT0002_Specify_Country that had NAs as a result of the data split.
#Also removing Cum GPA and Age as it is a continuous number.
#Removing the Overall_Satisfaction numeric score as this may influence the model's performance.
#There was also an errant column INT0002_Specify_Country that had NAs as a result of the data split.
#Also removing Cum GPA and Age as it is a continuous number.
train_data2 <- train_data2 %>% 
  select(-GS_Overall_Satisfaction)
test_data2 <- test_data2 %>% 
  select(-GS_Overall_Satisfaction)
train_data2 <- train_data2 %>% 
  select(-INT0002_Specify_Country, -AGE, -ID.Mask)
test_data2 <- test_data2 %>%
  select(-INT0002_Specify_Country, -AGE, -ID.Mask)
train_data2$Overall_Satisfaction_Cat <- droplevels(train_data2$Overall_Satisfaction_Cat)
test_data2$Overall_Satisfaction_Cat <- droplevels(test_data2$Overall_Satisfaction_Cat)
#Random Forest model
seru_rf2 <- randomForest(Overall_Satisfaction_Cat ~ ., data = train_data2, ntree = 500, importance = TRUE)
importance(seru_rf2)
varImpPlot(seru_rf2, type = 1, main = "Variable Importance (Mean Decrease in Accuracy)")
varImpPlot(seru_rf2, type = 2, main = "Variable Importance: Gini (IncNodePurity)")
#Creating a tree plot to visualize the decision tree structure of the random forest model.
rf_imp <- importance(seru_rf2)
rf_imp_df <- data.frame(Variable = rownames(rf_imp), Importance = rf_imp[, "MeanDecreaseAccuracy"])
rf_imp_top <- rf_imp_df %>% 
  arrange(desc(Importance)) %>% 
  slice_head(n = 10)
#Correcting variable names for better visualization in the tree plot.
rf_imp_top$Variable <- gsub("_", " ", rf_imp_top$Variable)
rf_imp_top$Variable <- gsub("^GS[0-9]* ", "", rf_imp_top$Variable)

#Keeping colors consistent with the bar plot.
my_pallette <- c("#999999", "#0072B2", "#D55E00")




ggplot(rf_imp_top, aes(area = Importance^2, fill = Importance, 
                       label = Variable)) + 
  geom_treemap(colour = "white", size = 4, start = "bottomright") + 
  geom_treemap_text(colour = "white", 
                    place = "centre", 
                    grow = FALSE,
                    size = 26,
                    fontface = "bold",
                    reflow = TRUE, 
                    family = "roboto", 
                    start = "bottomright") + 
  scale_fill_gradientn(colours = my_pallette, limits = c(10, 25)) + 
  labs(title = "Key Drivers of Student Satisfaction", 
       subtitle = "Random Forest Variable Importance (Mean Decrease in Accuracy)", 
       fill = "Importance") + 
  theme_minimal() + 
  theme(text = element_text(family = "roboto"), 
        plot.title = element_text(size = 30, face = "bold"), 
        plot.subtitle = element_text(size = 24), 
        legend.title = element_text(size = 20), 
        legend.text = element_text(size = 18),
                            legend.position = "bottom", 
                            legend.box = "horizontal") + 
          guides(fill = guide_colorbar(barwidth = 20, barheight = 1, 
                                       title.position = "top", title.hjust = 0.5))

ggsave("SERU_RF_Tree.png", width = 8, height = 6, dpi = 300)
ggsave("SERU_RF_Tree2.png", width = 8, height = 6, dpi = 300, 
       scale = 0.8, bg = "white")


#lollipop plot to show the variable importance of the top 10 variables.
rf_imp_top2 <- rf_imp_top[order(rf_imp_top$Importance), ]
rf_imp_top2$Variable <- factor(rf_imp_top2$Variable, levels = rf_imp_top2$Variable)

ggplot(rf_imp_top2, aes(x = Importance, y = Variable, color = Importance)) + 
  geom_segment(aes(x = 0, xend = Importance, y = Variable, yend = Variable), 
                linewidth = 2.2, alpha = 1) + 
                 geom_point(size = 5) +
                 scale_color_gradientn(colours = my_pallette, limits = c(10, 25)) +
                 labs(title = "Key Drivers of Student Satisfaction",
                      subtitle = "Random Forest Variable Importance (Mean Decrease in Accuracy)", 
                      x = "Importance Score", y = NULL) + 
                 theme_minimal(base_family = "roboto") + 
                 theme(plot.title = element_text(size = 36, face = "bold", hjust = 0, 
                                                 margin = ggplot2::margin(b = 10)), 
                       plot.title.position = "plot",
                       plot.subtitle = element_text(size = 28, hjust = 0, 
                                                    margin = ggplot2::margin(b = 30)),
                       plot.background = element_rect(fill = "#F9F9F9", color = NA), 
                       panel.background = element_rect(fill = "#F9F9F9", color = NA),
                       plot.margin = ggplot2::margin(t = 30, r = 10, b = 10, l = 10),
                       axis.text.y = element_text(size = 28), 
                       axis.text.x = element_text(size = 30),
                       axis.title.x = element_text(size = 28, margin = ggplot2::margin(t = 15)),
                       panel.grid.major.y = element_blank(),
                       legend.position = "NONE")
                        
                        


ggsave("SERU_RF_Lollipop.png", width = 12, height = 8, dpi = 300, 
       scale = 0.7)
