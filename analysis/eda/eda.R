rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run.
cat("\f") # clear console

# ---- load-packages -----------------------------------------------------------
library(ggplot2) #For graphing
library(magrittr) #Pipes
library(dplyr) # for shorter function names. but still prefer dplyr:: stems
library(knitr) # dynamic documents
library(rmarkdown) # dynamic
library(kableExtra) # enhanced tables, see http://haozhu233.github.io/kableExtra/awesome_table_in_html.html
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


# ---- load-data -------------------------------------------------------------
ds <- readRDS("./data-unshared/derived/0-greeted.rds")
# ---- inspect-data -------------------------------------------------------------
# ds %>% glimpse()
# ---- tweak-data --------------------------------------------------------------
ds <- ds %>%
  dplyr::filter(!is.na(reason_for_visit))

# ---- basic-table --------------------------------------------------------------

# ---- univariate --------------------------------------------------------------
varnames <- setdiff(names(ds),"patient_id")

item_n <- varnames[1]
# item_n_label <- varnames_varlabels[item_n]

for(item_i in varnames){
  cat("\n## ", item_i,"\n" )
  ds %>% TabularManifest::histogram_discrete(
    variable_name = item_i,
    main_title = varnames_varlabels[item_i]
    ) %>% print()
  cat("\n")
}

# ---- bivariate --------------
varnames_bivariate <- setdiff(varnames,"returned_to_care")

ds %>% TabularManifest::histogram_discrete(
  variable_name = "returned_to_care",
  main_title = varnames_varlabels["returned_to_care"]
) %>% print()
cat("\n")

for(item_i in varnames_bivariate){
  cat("\n## ", item_i,"\n" )

  g <- ds %>%
    ggplot(aes_string(x = item_i, fill = "returned_to_care")) +
    geom_bar(position = "fill")+
    coord_flip()+
    labs(
      fill = varnames_varlabels["returned_to_care"]
      ,y = "Percent"
      , x = varnames_varlabels[item_i]
    )+
    guides(
      fill = guide_legend(reverse = TRUE)
    )+
    theme_bw()+
    theme(legend.position = "top")
  g %>% print()
  cat("\n")
}




# ---- publish ---------------------------------------
path_report_1 <- "./analysis/eda/eda.Rmd"
allReports <- c(path_report_1)

pathFilesToBuild <- c(allReports)
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {

  rmarkdown::render(input = pathFile,
                    output_format=c(
                      "html_document" # set print_format <- "html" in seed-study.R
                      # "pdf_document"
                      # ,"md_document"
                      # "word_document" # set print_format <- "pandoc" in seed-study.R
                    ),
                    clean=TRUE)
}

