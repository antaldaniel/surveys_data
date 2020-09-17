# testing retroharmonize package on other survey data
library(retroharmonize)
library(dplyr)
library(tidyr)
ab <- dir ( "~/Downloads/surveys_data/input/afrobarometer_dir_round1-4" )
afrobarometer_rounds <- file.path(ab)
ab_waves <- read_surveys(afrobarometer_rounds, .f='read_spss')
attr(ab_waves[[1]], "id") <- "Afrobarometer_R2"
attr(ab_waves[[2]], "id") <- "Afrobarometer_R3"
attr(ab_waves[[3]], "id") <- "Afrobarometer_R4"
documented_ab_waves <- document_waves(ab_waves)
ab_metadata <- lapply(ab_waves, FUN = metadata_create)
ab_metadata <- do.call(rbind, ab_metadata) #reuslt List of 12 rows, 12972 variables
colnames(ab_metadata)

to_harmonize <-ab_metadata %>%
  filter ( var_name_orig %in%
             c("rowid","DATEINTR","dateintr", "COUNTRY","country", "REGION","region", "withinwt","Withinwt")|
             grepl("trust ", label_orig ))%>%
  mutate(var_label=var_label_normalize(label_orig)) %>% # normalizing label_orig values
  mutate(var_label= case_when(grepl("^unique_identifier", var_label) ~ "unique_id",
                              TRUE ~ var_label))%>%
  mutate ( var_name = val_label_normalize(var_label)) # again we normalize the var_label( check its inner code)
# further filtering in var_name variables 
to_harmonize <- to_harmonize %>%
  filter (
    grepl ( "president|parliament|religious|traditional|unique_id|weight|country|date_of_int", var_name)
  )
unique(to_harmonize$var_name) # we have duplicated varialbes names
to_harmonize %>%
  select ( all_of(c("id", "var_name", "var_label")))

merged_ab <- merge_waves(waves=ab_waves, var_harmonization = to_harmonize) # original and harmoized data to 
merged_ab <- lapply ( merged_ab,
                      FUN = function(x) x  %>%
                        mutate( country = as_character(country)))

documenteded_merged_ab <- document_waves(merged_ab)

R4 <- pull_survey ( merged_ab, id = "Afrobarometer_R4")
attributes(R4$trust_president[1:20])

document_survey_item(R4$trust_president)

collect_na_labels( to_harmonize )

collect_val_labels (to_harmonize %>%
                      filter ( grepl( "trust", var_name) ))


harmonize_ab_trust <- function(x) {
  label_list <- list(
    from = c("^not", "^just", "^somewhat",
             "^a", "^don", "^ref", "^miss", "^not", "^inap"),
    to = c("not_at_all", "little", "somewhat",
           "a_lot", "do_not_know", "declined", "inap", "inap",
           "inap"),
    numeric_values = c(0,1,2,3, 99997, 99998, 99999,99999, 99999)
  )
  
  harmonize_values(
    x,
    harmonize_labels = label_list,
    na_values = c("do_not_know"=99997,
                  "declined"=99998,
                  "inap"=99999)
  )
}
# The harmonize_waves() function binds all variables that are present in all surveys.
harmonized_ab_waves <- harmonize_waves (
  waves = merged_ab,
  .f = harmonize_ab_trust )

h_ab_structure <- attributes(harmonized_ab_waves)
h_ab_structure

harmonized_ab_waves <- harmonized_ab_waves %>%
  mutate( year = as.integer(substr(as.character(date_of_interview),1,4)))

harmonized_ab_waves <- harmonized_ab_waves %>%
  filter ( country %in% c("Niger", "Nigeria", "Algeria",
                          "South Africa", "Madagascar"))

harmonized_ab_waves %>%
  mutate_at ( vars(starts_with("trust")),
              ~as_numeric(.)*within_country_weight) %>%
  select ( -all_of("within_country_weight") ) %>%
  group_by ( country, year ) %>%
  summarize_if ( is.numeric, mean, na.rm=TRUE )


harmonized_ab_waves %>%
  select ( -all_of("within_country_weight") ) %>%
  mutate_if ( is.labelled_spss_survey, as_factor) %>%
  pivot_longer ( starts_with("trust"),
                 names_to  = "institution",
                 values_to = "category") %>%
  mutate ( institution = gsub("^trust_", "", institution) ) %>%
  group_by ( country, year, institution, category ) %>%
  summarize ( n = n())

