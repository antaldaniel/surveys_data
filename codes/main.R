# Credits ----
# Name: Yousef Ibrahim
# date: 2020-09-12
# ____________
library(retroharmonize)
library(dplyr)
library(tidyr)
ab <- dir ( "~/Downloads/surveys_data/input/afrobarometer_dir" )
afrobarometer_rounds <- file.path(ab)
ab_waves <- read_surveys(afrobarometer_rounds, .f='read_spss')
str(ab_waves)
attr(ab_waves[[1]], "id") <- "Afrobarometer_R5"
attr(ab_waves[[2]], "id") <- "Afrobarometer_R6"
attr(ab_waves[[3]], "id") <- "Afrobarometer_R7"
#attributes(ab_waves[[1]]) to check attributes of first survey data( cols name, row_names, class/type of data,filename and id names which we added))
#metadata_create
documented_ab_waves <- document_waves(ab_waves)
#metadata_creat():Create a metadata table, or a data map, that contains information about the variable names and labels, and where each row correspnds to one variable in the survey data file.
ab_metadata <- lapply(ab_waves, FUN = metadata_create)
ab_metadata <- do.call(rbind, ab_metadata) #reuslt List of 12 rows, 12972 variables
#rownames(ab_metadata)
# [1] "filename"       "id"             "var_name_orig"  "class_orig"     "label_orig"     "labels"        
# [7] "valid_labels"   "na_labels"      "na_range"       "n_labels"       "n_valid_labels" "n_na_labels"   

#colnames(ab_metadata)

#Working with metadata

#1. selection of variables of interst("rowid", "DATEINTR", "COUNTRY", "REGION", "withinwt")
#2. also variables with word Trust
library(dplyr)
to_harmonize <-ab_metadata %>%
  filter ( var_name_orig %in%
             c("rowid", "DATEINTR", "COUNTRY", "REGION", "withinwt") |
             grepl("trust ", label_orig )) %>%
  mutate(var_label=var_label_normalize(label_orig)) %>% # normalizing label_orig values
  mutate(var_label= case_when(grepl("^unique_identifier", var_label) ~ "unique_id",
                              TRUE ~ var_label))%>%
  mutate ( var_name = val_label_normalize(var_label)) # again we normalize the var_label( check its inner code)




