rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
library(ggpubr)
# library(TabularManifest) # exploratory data analysis, see https://github.com/Melinae/TabularManifest
requireNamespace("knitr", quietly=TRUE)
requireNamespace("scales", quietly=TRUE) #For formating values in graphs
requireNamespace("RColorBrewer", quietly=TRUE)
requireNamespace("dplyr", quietly=TRUE)
requireNamespace("DT", quietly=TRUE) # for dynamic tables

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
source("./scripts/modeling/model-basic.R")
source("./scripts/graphing/graph-missing.R")
# ----- declare-globals ----------------
varnames_varlabels <- c(
  "patient_id"            = "Patient Id"
  ,"male"                 = "Gender = Male"
  ,"reason_for_visit"     = "Reason for Visit / Diagnosis"
  ,"provider"             = "Service provider"
  ,"insurance"            = "Insurance"
  ,"history_noshow"       = "History of No-Show"
  ,"month_of_appointment" = "Month of Appoinment"
  ,"pm_appointment"       = "Afternoon Appointment"
  ,"preferred_language"   = "Preferred Language"
  ,"returned_to_care"     = "Returned to Care"
  ,"letter_sent"          = "Was letter sent?"
)

# positive Green, negative Orange
letter_sent_colors <- c(
  "Letter Sent" = "#66c2a5"
  ,"Letter Not Sent" = "#fc8d62"
)
# positive Teal, negative Brown
returned_to_care_colors <- c(
  "Returned to care"="#80CDC1"
  ,"No show"="#DFC27D"
)

# # positive Green, negative Purple
# colorsFill <- c("TRUE"="#A6DBA0" ,"FALSE"="#C2A5CF") # The colors for negative and positve values of factor loadings for ggplot
# colorFont <- c("TRUE"="#008837" ,"FALSE"="#7B3294") # The colors for negative and positve values of factor loadings for ggplot

#set default ggplot theme
ggplot2::theme_set(
  ggplot2::theme_bw(
  )+
    theme(
      strip.background = element_rect(fill="grey90", color = NA)
    )
)
# ---- load-data -------------------------------------------------------------
ds <- readRDS("./data-unshared/derived/0-greeted.rds")
# ---- inspect-data -------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------
# Establish reference categories for more straigforward interpretation of parameters
ds1 <- ds %>%
  dplyr::filter(!is.na(reason_for_visit)) %>%
  dplyr::mutate(
    returned_to_care = factor(
      returned_to_care
      , level  = c(FALSE, TRUE)
      , labels = c("No show","Returned to care")
      )
    ,returned_to_care = relevel(factor(returned_to_care),         ref = "No show")
    ,letter_sent = factor(
      letter_sent
      , level  = c(FALSE,TRUE)
      , labels = c("Letter Not Sent", "Letter Sent")
    )
    ,letter_sent          = relevel(factor(letter_sent),          ref = "Letter Sent")
    ,reason_for_visit     = relevel(factor(reason_for_visit),     ref = "Voiding")
    ,provider             = relevel(factor(provider),             ref = "ARNP")
    ,month_of_appointment = relevel(factor(month_of_appointment), ref = "Jan")
    ,pm_appointment       = relevel(factor(pm_appointment),       ref = "TRUE")

  )

# ---- drop-small-groups --------------------
# some of response categories were too smal and destabilized the model fitting
ds1 %>% group_by(reason_for_visit) %>% count()
ds1 %>% group_by(insurance) %>% count()
ds1 %>% group_by(history_noshow) %>% count()
ds1 %>% group_by(preferred_language) %>% count()
# total number of initial observation
(initial_obs <- ds1 %>% count())
# remove observations belonging to underrepresented groups
ds2 <- ds1 %>%
  dplyr::filter(!reason_for_visit   %in% c("Female gyn","Bladder") ) %>%
  dplyr::filter(!insurance          %in% c("None")                 ) %>%
  dplyr::filter(!history_noshow     %in% c("Both")                 ) %>%
  dplyr::filter(!preferred_language %in% c("Other")                )
# remove the empty levels of factor variables
ds2 <- ds2 %>% gdata::drop.levels(reorder=FALSE)
# resulting number of observations
(remaining_obs <- ds2 %>% count())
# number of observations removed
(initial_obs - remaining_obs )%>% neat()
# percent of initial observations removed
((initial_obs - remaining_obs )/initial_obs) %>% pull() %>% scales::percent()

# ds_modeling <- ds1 # all observation included
ds_modeling <- ds2 # groups with small n droped
# ds_modeling %>% glimpse(90)


# ----- fig-1 --------------------

d1 <- ds1 %>%
  dplyr::mutate(
    reason_for_visit = forcats::fct_recode(
      reason_for_visit,
      "Renal/Ureteral" ="Renal/Ureter",
      "Scrotal" = "Testies/Scrotum",
      "Female Genitalia" = "Female gyn"
      )
    ,reason_for_visit = forcats::fct_relevel(
      reason_for_visit,
      "Renal/Ureteral", "Scrotal", "Penile", "Voiding"
      ,"Bladder", "Female Genitalia"
    )
    ,provider = forcats::fct_relevel(provider, "MD","ARNP", "PA")
    # ,month_of_appointment = forcats::fct_rev(month_of_appointment)
    # ,letter_sent = forcats::fct_rev(letter_sent)
  ) %>%
  dplyr::group_by(.dots = c("month_of_appointment", "letter_sent",  "provider", "reason_for_visit")) %>%
  dplyr::summarize(
    n_patients = dplyr::n()
  ) %>%
  dplyr::ungroup()
g1 <- d1 %>%
  ggplot(
    aes(
      x = month_of_appointment
      , y = n_patients
      , fill = letter_sent
      ,group = letter_sent
    )
  ) +
  geom_col( alpha = 1, color = "grey50")+
  facet_grid( provider ~ reason_for_visit)+
  scale_fill_viridis_d(option = "magma")+
  # scale_fill_brewer(palette="OrRd")+
  # coord_flip()+
  labs(fill = NULL, y = "Number of Patients", x = "Month of Missed Appointment")
baseSize = 10
g1 <- g1 +
  theme(
     legend.position = "top"
    ,axis.text.x = element_text(angle = 90, vjust = .2, size = baseSize)
    # ,axis.text.x = element_text(vjust = .2, size = baseSize)
    ,strip.text.y = element_text(size = 15)
    ,strip.text.x = element_text(size = 15)
    ,axis.title =  element_text(size = 15)
    ,legend.text = element_text(size = 15)
  )
# g1

ggplot2::ggsave(
  filename = "fig1.png",
  plot     = g1,
  device   = png,
  path     = "./analysis/manuscript-figures/",
  # width    = 1200,
  # height   = 800,
  width    = 900,
  height   = 600,
  # units    = "in",
  # dpi      = 200,
  limitsize = FALSE
)





# ---- publish ---------------------------------------
path_report_1 <- "./analysis/titanic-separate-layers/titanic.Rmd"
allReports <- c(path_report_1)
pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {

  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document"
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document"
                    ),
                    clean=TRUE)
}
