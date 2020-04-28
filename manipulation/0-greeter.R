# this script imports the raw data described in this shared document
# https://drive.google.com/file/d/10idMxy8eX8nTHr6wr2Q40x4XOP3Y5ck7/view
# and prepares a state of data used as a standard point of departure for any subsequent reproducible analytics

# Lines before the first chunk are invisible to Rmd/Rnw callers
# Run to stitch a tech report of this script (used only in RStudio)
# knitr::stitch_rmd(
#   script = "./manipulation/0-greeter.R",
#   output = "./manipulation/stitched-output/0-greeter.md"
# )
# this command is typically executed by the ./manipulation/governor.R

rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
# This is not called by knitr, because it's above the first chunk.
cat("\f") # clear console when working in RStudio

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) #Pipes
library(ggplot2) # graphs
library(dplyr) # wrangling
library(tidyr) # pivoting
library(forcats) # factors

# ---- declare-globals ---------------------------------------------------------
# path_input <- "./data-unshared/raw/STATS_Uro.xlsx"
# path_input <- "./data-unshared/raw/STATS_Uro_noduplicates.xlsx"
path_input <- "./data-unshared/raw/STATS_Uro_noduplicates2.xlsx"
# ---- load-data ---------------------------------------------------------------
# ds0 <- readxl::read_xlsx(path_input, sheet = "DATA")
ds0 <- readxl::read_xlsx(path_input, sheet = "Sheet1")
ds0 %>% glimpse()

# ---- tweak-data ---------------------
ds <- ds0
names(ds) <- c(
  "patient_id"
  ,"gender"
  ,"reason_for_visit"
  ,"provider"
  ,"insurance"
  ,"history_noshow"
  ,"month_of_appointment"
  ,"pm_appointment"
  ,"preferred_language"
  ,"returned_to_care"
  ,"letter_sent"
)

varnames_varlabels <- c(
  "patient_id"            = "Patient Id"
  ,"male"                 = "Gender = Male"
  ,"reason_for_visit"     = "Reason for Visit"
  ,"provider"             = "Service provider"
  ,"insurance"            = "Insurance"
  ,"history_noshow"       = "History of No-Show"
  ,"month_of_appointment" = "Month of Appoinment"
  ,"pm_appointment"       = "Afternoon Appointment"
  ,"preferred_language"   = "Preferred Language"
  ,"returned_to_care"     = "Returned to Care"
  ,"letter_sent"          = "Was letter sent?"
)
ds %>% glimpse(50)

# ---- duplicate-analysis
# dupicates without stars
# Q: are there duplicates among ids NOT marked by a star?
d <- ds %>%
  select(patient_id) %>%
  mutate(
    has_a_star = grepl("\\d+\\*$", patient_id)
    # ,without_a_star = gsub("\\*$","", patient_id) # remove star from the id
  ) %>%
  filter(!has_a_star) %>%
  group_by(patient_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# A: NO, there are no duplicate
print(d)

# Q: What are the duplicated values?
d <- ds %>%
  select(patient_id) %>%
  arrange(patient_id) %>%
  mutate(
    has_a_star = grepl("\\d+\\*$", patient_id)
  ) %>%
  filter(has_a_star)
d %>% DT::datatable(filter = "top")
# Q: How many ids have at least one dupicate?
d %>% n_distinct("patient_id")
# Q: What is the frequency distribution of number of diplicates?
d %>%
  group_by(patient_id) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  TabularManifest::histogram_discrete("n")

# remove persons with multiple visits

ds <- ds %>%
  mutate(
    marked_by_star = grepl("\\d+\\*$", patient_id)
  ) %>%
  filter(!marked_by_star)
# Q: What is sample size after removing persons with multiple visits?
ds %>% n_distinct("patient_id")

# ---- factor-levels --------------

# Match factor levels to the labels
lvl_reason_for_visit <- c(
  "1"  = "Renal/Ureter"
  ,"2" = "Testies/Scrotum"
  ,"3" = "Penile"
  ,"4" = "Voiding"
  ,"5" = "Bladder"
  ,"6" = "Female gyn"
)
lvl_provider <- c(
  "0"  = "MD"
  ,"1" = "ARNP"
  ,"2" = "PA"
)
lvl_insurance <- c(
  "0"  = "None"
  ,"1" = "Medicaid"
  ,"2" = "Private"
)
lvl_history_noshow <- c(
  "0"  = "None"
  ,"1" = "Urology only"
  ,"2" = "Other"
  ,"3" = "Both"
)
lvl_preferred_language <- c(
  "0"  = "English"
  ,"1" = "Spanish"
  ,"2" = "Other"
)


ds1 <- ds %>%
  dplyr::rename(
    male = gender
  ) %>%
  dplyr::mutate(
    male = (male == 1)
    ,reason_for_visit = factor(
      reason_for_visit
      ,levels = lvl_reason_for_visit %>% names() %>% as.numeric()        ,labels =  lvl_reason_for_visit
    )
    ,provider = factor(
      provider
      ,levels = lvl_provider %>% names() %>% as.numeric()
      ,labels = lvl_provider
    ),
    insurance = factor(
      insurance
      ,levels = lvl_insurance %>% names() %>% as.numeric()
      ,labels = lvl_insurance
    )
    ,history_noshow = factor(
      history_noshow
      ,levels = lvl_history_noshow %>% names() %>% as.numeric()
      ,labels = lvl_history_noshow
    )
    ,month_of_appointment = factor(
      month_of_appointment
      ,levels = 1:12
      ,labels = month.abb
    )
    ,pm_appointment = (pm_appointment == 1)
    ,preferred_language = factor(
      preferred_language
      ,levels = lvl_preferred_language %>% names() %>% as.numeric()
      ,labels = lvl_preferred_language
    )
    ,returned_to_care = (returned_to_care == 1)
    ,letter_sent = (letter_sent == 1)

  )
ds1 %>% glimpse()

# ---- define-utility-functions ---------------

# ---- save-to-disk ----------------------------
ds1 %>%
  saveRDS("./data-unshared/derived/0-greeted.rds")
