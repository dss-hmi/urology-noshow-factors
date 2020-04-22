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
  "Letter sent" = "#66c2a5"
  ,"No letter sent" = "#fc8d62"
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
ggplot2::theme_set(ggplot2::theme_bw())

# ---- load-data -------------------------------------------------------------
ds <- readRDS("./data-unshared/derived/0-greeted.rds")
# ---- inspect-data -------------------------------------------------------------
ds %>% glimpse()
# ds %>% explore::describe_all() %>% neat()
# number of missing in each column
sapply(ds, function(x) sum(is.na(x))) # no missing observation
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
      , labels = c("No letter sent", "Letter sent")
    )
    ,letter_sent          = relevel(factor(letter_sent),          ref = "Letter sent")
    ,reason_for_visit     = relevel(factor(reason_for_visit),     ref = "Voiding")
    ,provider             = relevel(factor(provider),             ref = "ARNP")
    ,month_of_appointment = relevel(factor(month_of_appointment), ref = "Sep")
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
(initial_obs - remaining_obs )
# percent of initial observations removed
((initial_obs - remaining_obs )/initial_obs) %>% pull() %>% scales::percent()

# ds_modeling <- ds1 # all observation included
ds_modeling <- ds2 # groups with small n droped
ds_modeling %>% glimpse(90)


# ---- define-function ------------------------------------------------

run_logistic <- function(d,p){
  # d <- ds_modeling
  # p <- predictors_00
  #
  # browser()
  ls_out <- list()
  eq_formula <- as.formula(paste0(outcome, paste(p, collapse = " + ") ) )

  model <- stats::glm(
    formula = eq_formula
    # ,family = "binomial"
    ,family=binomial(link=logit)
    ,data = d %>%
      select(-patient_id)
  )
  # create levels of predictors for which to generate predicted values
  d_predicted <- d %>%
    dplyr::select(p) %>%
    dplyr::distinct()
  # add model prediction
  d_predicted <- d_predicted %>%
    dplyr::mutate(
      log_odds = predict(object = model, newdata = .)
      ,probability = plogis(log_odds)
      # ,prob1 = predict.glm(object = model, newdata = .,type = "response")
    )
  ls_out[["equation"]] <- eq_formula
  ls_out[["model"]] <- model
  ls_out[["predicted"]] <- d_predicted
  return(ls_out)
}
# How to use
# lsm00 <- ds_modeling %>% run_logistic(predictors_00)
# model <- lsm00$model

get_rsquared <- function(m){
  cat("R-Squared, Proportion of Variance Explained = ",
      scales::percent((1 - (summary(m)$deviance/summary(m)$null.deviance)),accuracy = .01)
  , "\n")
}
get_model_fit <- function(m){
  cat("MODEL FIT",
      "\nChi-Square = ", with(m, null.deviance - deviance),
      "\ndf = ", with(m, df.null - df.residual),
      "\np-value = ", with(m, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE)),"\n"
  )
}
# from source("./scripts/modeling/model-basic.R")
# basic_model_insource("./scripts/modeling/model-basic.R")fo()
# make_result_table()
make_facet_graph <- function(d, x_aes, fill_aes, facet_row=NULL, facet_col=NULL, smooth = NULL){
  # d <- ds_modeling
  # x_aes <- "month_of_appointment"
  # fill_aes <- "letter_sent"
  # facet_row <- "provider"
  # facet_col <- "reason_for_visit"
  #
  d1 <- d %>%
    dplyr::group_by(.dots = c(x_aes, fill_aes, facet_row, facet_col)) %>%
    dplyr::summarize(
      n_patients = dplyr::n()
    ) %>%
    dplyr::ungroup()
  y_aes <- "n_patients"

  g1 <- d1 %>%
    ggplot(aes(x = !!ensym(x_aes), y = !!ensym(y_aes), fill = !!ensym(fill_aes))) +
    geom_col()+
    theme(
      axis.text.x = element_text(angle = 90, vjust = .2)
      ,legend.position = "top"
    )


  if(!is.null(facet_row) & !is.null(facet_col)){
    facet_expr <- paste0( facet_row, " ~ ", facet_col)
  }
  if(is.null(facet_row) & !is.null(facet_col)){
    facet_expr <- paste0(  ".  ~ ", facet_col)
  }
  if(!is.null(facet_row) & is.null(facet_col)){
    facet_expr <- paste0( facet_row, " ~ .")
  }
  if( !is.null(facet_row) | !is.null(facet_col) ){
    facet_formula <- enexpr(facet_expr)
    g1 <- g1 +facet_grid(facet_formula)
  }
  return(g1)
}
# How to use
# ds_modeling %>% make_facet_graph(
#   x_asix     = "month_of_appointment"
#   ,fill_aes  = "letter_sent"
#   ,facet_row =  "provider"
#   ,facet_col =  "reason_for_visit"
#   )
#
# ds_modeling %>% make_facet_graph(
#   x_asix     = "month_of_appointment"
#   ,fill_aes  = "letter_sent"
#   # ,facet_row =  "provider"
#   ,facet_col =  "reason_for_visit"
# )


# ---- with-small-groups- ------------------

ds1 %>%
  dplyr::mutate(
    month_of_appointment = factor(month_of_appointment, levels = month.abb)
  ) %>%
  dplyr::filter(!reason_for_visit   %in% c("Female gyn","Bladder") ) %>%
  make_facet_graph(
  x_aes     = "month_of_appointment"
  ,fill_aes  = "letter_sent"
  ,facet_row =  "provider"
  ,facet_col =  "reason_for_visit"
) %>%
  print()

# ---- provider-by-reason ------------------

predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
)

# item_i <- predictors[5]

for(item_i in predictors){
  cat("\n## ", item_i,"\n" )
  g <- ds1 %>%
    dplyr::mutate(
      month_of_appoinment = factor(month_of_appoinment, levels = month.abb)
    ) %>%
    make_facet_graph(
    x_aes     = "month_of_appointment"
    ,fill_aes  = item_i
    ,facet_row =  "provider"
    ,facet_col =  "reason_for_visit"
  )
  g <- g + labs(
    fill = varnames_varlabels[item_i]
  )
  print(g)
  cat("\n")
}

# ---- provider-by-reason-with-small-groups ------------------

for(item_i in predictors){
  cat("\n## ", item_i,"\n" )
  ds_modeling %>% make_facet_graph(
    x_asix     = "month_of_appointment"
    ,fill_aes  = item_i
    ,facet_row =  "provider"
    ,facet_col =  "reason_for_visit"
  )+
    labs(
      fill = varnames_varlabels[item_i]
    ) %>%
    print()
  cat("\n")
}

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
