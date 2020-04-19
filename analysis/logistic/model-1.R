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

# ---- load-data -------------------------------------------------------------
ds <- readRDS("./data-unshared/derived/0-greeted.rds")
# ---- inspect-data -------------------------------------------------------------
ds %>% glimpse()
# ds %>% explore::describe_all() %>% neat()
# ---- tweak-data --------------------------------------------------------------
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
  dplyr::filter(!reason_for_visit %in% c("Female gyn","Bladder") ) %>%
  dplyr::filter(insurance != "None") %>%
  dplyr::filter(history_noshow != "Both") %>%
  dplyr::filter(preferred_language != "Other")
# remove the empty levels of factor variables
ds2 <- ds2 %>% gdata::drop.levels(reorder=FALSE)
# resulting number of observations
(remaining_obs <- ds2 %>% count())
# number of observations removed
(initial_obs - remaining_obs )
# percent of initial observations removed
((initial_obs - remaining_obs )/initial_obs) %>% pull() %>% scales::percent()

ds_modeling <- ds1
ds_modeling %>% glimpse(90)

# ---- part1_chunk1  --------------------------------------------------------------
dt1 <- ds_modeling %>%
  dplyr::group_by(returned_to_care, letter_sent) %>%
  dplyr::summarize(
    n_people = n()
  ) %>%
  dplyr::group_by(returned_to_care) %>%
  dplyr::mutate(
    pct_returned_to_care = sum(n_people, na.rm =T)
    ,pct_returned_to_care = n_people/pct_returned_to_care
    ,pct_returned_to_care = scales::label_percent()(pct_returned_to_care)
  ) %>%
  dplyr::group_by(letter_sent) %>%
  dplyr::mutate(
    pct_letter_sent = sum(n_people, na.rm =T)
    ,pct_letter_sent = scales::label_percent()(n_people/pct_letter_sent)
    # ,pct_returned_to_care = scales::label_percent()(pct_returned_to_care)
  )
dt2 <- table(ds_modeling$letter_sent, ds_modeling$returned_to_care)

dt1 %>%
  ggplot(aes(x = letter_sent, y = n_people, fill = returned_to_care))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "black", size = 5 )+
  geom_text(aes(label = pct_letter_sent),position = position_dodge(.9), vjust = -.5, color = "blue", size = 4)+
  scale_fill_manual(values = returned_to_care_colors)+
  theme_minimal()+
  scale_y_continuous(limit = c(0,450))+
  theme(legend.position = "top")+
  labs(
    title = "Patients returning to care after in-mail follow up"
    ,x = "", y = "Number of patients"
    ,fill = ""
  )
#
dt1 %>%
  ggplot(aes(x = returned_to_care, y = n_people, fill = letter_sent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "white", size = 5 )+
  geom_text(aes(label = pct_returned_to_care),position = position_dodge(.9), vjust = -.5, color = "blue", size = 4)+
  scale_fill_manual(values = letter_sent_colors)+
  scale_y_continuous(limit = c(0,450))+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(
    title = "Patients returning to care after in-mail follow up"
    ,x = "", y = "Number of patients"
    ,fill = ""
  )

mosaicplot(~letter_sent + returned_to_care, data = ds_modeling,
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Follow up Communication", y = "Returned to Care"
           ,shade = TRUE)
#
ds_modeling %>%
  dplyr::select(returned_to_care, letter_sent) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
# Test of independence shows no significant association.
# Patients who recieved a follow-up letter are not more likely to return to care

# ---- part1_chunk2  --------------------------------------------------------------

mosaicplot(~letter_sent + reason_for_visit, data = ds_modeling,
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Follow up Communication", y = "Reason to visit"
           ,shade = TRUE)
#
ds_modeling %>%
  dplyr::select(reason_for_visit, letter_sent) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Reason for visit", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)

# ---- part1_chunk2  --------------------------------------------------------------
mosaicplot(~provider + letter_sent, data = ds_modeling,
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Follow up Communication", y = "Provider"
           ,shade = TRUE)
#
ds_modeling %>%
  # dplyr::select(provider, letter_sent) %>%
  dplyr::select( letter_sent, provider) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Follow-up","Provider"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
# ARNP are signficantly less likely to send out the letter
# PA are signfificantly more likely to sound out the letter

# ---- basic-graph --------------------------------------------------------------

# ds %>%
#   ggplot(aes(x = reason_for_visit))+
#   geom_bar()+
#   coord_flip()+
#   facet_grid(.~letter_sent)


# ds %>%
#   ggplot(aes(x = letter_sent, fill = letter_sent))+
#   geom_bar(position = "dodge")+
#   facet_grid(.~provider)
#


# ---- chi-square -----------------------------



# ---- model-function --------------------------------------------------------------

run_logistic <- function(d,p){
  # d <- ds_modeling
  # p <- predictors_00
  #
  # browser()
  ls_out <- list()
  eq_formula <- as.formula(paste0(stem, paste(p, collapse = " + ") ) )

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

# ---- m00 ------------------
stem <- "returned_to_care ~ "
predictors_00 <- c(
  # "1"
  "insurance"
  ,"history_noshow"
  ,"provider"
  ,"pm_appointment"
  ,"preferred_language"
  ,"month_of_appointment"
  ,"male"
  ,"reason_for_visit"
  ,"letter_sent"
)
lsm00 <- ds_modeling %>% run_logistic(predictors_00)
model <- lsm00$model

print(model$formula, showEnv = F)
cat("R-Squared, Proportion of Variance Explained = ",
    scales::percent((1 - (summary(model)$deviance/summary(model)$null.deviance)),accuracy = .01)
    )
cat("MODEL FIT",
    "\nChi-Square = ", with(model, null.deviance - deviance),
    "\ndf = ", with(model, df.null - df.residual),
    "\np-value = ", with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    )
summary(model) %>% print()
exp(cbind(OR = coef(model), confint(model))) %>% neat()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()

# ---- m01 ------------------
stem <- "letter_sent ~ "
predictors_01 <- c(
  # "1"
  "insurance"
  ,"history_noshow"
  ,"provider"
  ,"pm_appointment"
  ,"preferred_language"
  ,"month_of_appointment"
  ,"male"
  ,"reason_for_visit"
  ,"returned_to_care"
)
lsm01 <- ds_modeling %>%
  dplyr::mutate(
    letter_sent = relevel(factor(letter_sent),  ref = "No letter sent")
  ) %>%
  run_logistic(predictors_01)
model <- lsm01$model

print(model$formula, showEnv = F)
cat("R-Squared, Proportion of Variance Explained = ",
    scales::percent((1 - (summary(model)$deviance/summary(model)$null.deviance)),accuracy = .01)
    )
cat("MODEL FIT",
    "\nChi-Square = ", with(model, null.deviance - deviance),
    "\ndf = ", with(model, df.null - df.residual),
    "\np-value = ", with(model, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
    )
summary(model) %>% print()
exp(cbind(OR = coef(model), confint(model))) #%>% neat()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()

# ---- graph-00 -----------------------------------------------------------------

g00 <- lsm00$predicted %>%
  # dplyr::filter(!reason_for_visit %in% c("female gyn", "bladder") ) %>%
  # dplyr::filter(!history_noshow %in% c("Both") ) %>%
  # dplyr::filter(provider != "PA") %>%
  # ggplot(aes(x = letter_sent, y = probability, fill = letter_sent))+
  # ggplot(aes(x = provider, y = probability, fill = letter_sent))+
  ggplot(aes(x = provider, y = probability, fill = provider))+
  geom_boxplot(alpha = .3)+
  geom_jitter(alpha =.3 )+
  # geom_point(j)
  # facet_grid(reason_for_visit ~ history_noshow)+
  facet_grid(history_noshow ~ reason_for_visit)+
  theme_bw()

g00

g01 <- lsm00$predicted %>%
  # dplyr::filter(!reason_for_visit %in% c("female gyn", "bladder") ) %>%
  # dplyr::filter(!history_noshow %in% c("Both") ) %>%
  # dplyr::filter(provider != "PA") %>%
  # ggplot(aes(x = letter_sent, y = probability, fill = letter_sent))+
  # ggplot(aes(x = provider, y = probability, fill = letter_sent))+
  ggplot(aes(x = insurance, y = probability, fill = letter_sent))+
  geom_boxplot(alpha = .3)+
  geom_jitter(alpha =.3 )+
  # geom_point(j)
  # facet_grid(reason_for_visit ~ history_noshow)+
  facet_grid(provider ~ reason_for_visit)+
  theme_bw()


g01

#  Does letter make a difference in different insurance plans?


# ---- ---------
# Do some provides sent more letters that others?
# ds_modeling %>%
#   dplyr::select(provider, letter_sent) %>%
#   sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Follow-up"),
#                 show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
# mosaicplot(~letter_sent + provider, data = ds_modeling,
#            main = "Patients returning to care after in-mail follow up"
#            ,xlab = "Follow up Communication", y = "Returned to Care"
#            ,shade = TRUE)

#
ds_modeling %>%
  # dplyr::filter(!reason_for_visit %in% c("female gyn", "bladder") ) %>%
  dplyr::select(provider, reason_for_visit) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)

mosaicplot(~reason_for_visit + provider, data = (ds_modeling %>%  dplyr::filter(
  !reason_for_visit %in% c("female gyn", "bladder") )),
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Reason for Visit", y = "Provider of Care"
           ,shade = TRUE)

# ---- model-00a ---------------------
# it appears that the reason_for_visit is a strong predictor of no_show
# does sending letter vary across the levels of reason_for_visi?

ds_modeling %>%
  dplyr::select(reason_for_visit, letter_sent) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned ton Care", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)


dt1 <- ds_modeling %>%
  dplyr::group_by(reason_for_visit, letter_sent) %>%
  dplyr::summarize(
    n_people = n()
  ) %>%
  dplyr::group_by(reason_for_visit) %>%
  dplyr::mutate(
    pct_reason_for_visit = sum(n_people, na.rm =T)
    ,pct_reason_for_visit = n_people/pct_reason_for_visit
    ,pct_reason_for_visit = scales::label_percent()(pct_reason_for_visit)
  ) %>%
  dplyr::group_by(letter_sent) %>%
  dplyr::mutate(
    pct_letter_sent = sum(n_people, na.rm =T)
    ,pct_letter_sent = scales::label_percent()(n_people/pct_letter_sent)
    # ,pct_returned_to_care = scales::label_percent()(pct_returned_to_care)
  )

dt1 %>%
  ggplot(aes(x = letter_sent, y = n_people, fill = reason_for_visit))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "grey80", size = 5 )+
  geom_text(aes(label = pct_letter_sent),position = position_dodge(.9), vjust = -.5, color = "blue", size = 4)+
  scale_fill_viridis_d(option = "magma")+
  scale_y_continuous(limit = c(0,300))+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(
    title = "Patients returning to care after in-mail follow up"
    ,x = "", y = "Number of patients"
    ,fill = ""
  )


dt1 %>%
  ggplot(aes(x = reason_for_visit, y = n_people, fill = letter_sent))+
  geom_col(position = position_dodge())+
  geom_text(aes(label = n_people),position = position_dodge(.9), vjust = 1.5, color = "grey50", size = 5 )+
  geom_text(aes(label = pct_reason_for_visit),position = position_dodge(.9), vjust = -.5, color = "blue", size = 4)+
  scale_fill_manual(values = letter_sent_colors)+
  scale_y_continuous(limit = c(0,300))+
  theme_minimal()+
  theme(legend.position = "top")+
  labs(
    title = "Follow up reminder across reasons for visit"
    ,x = "", y = "Number of patients"
    ,fill = ""
  )

mosaicplot(~letter_sent + reason_for_visit, data = ds_modeling,
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Follow up Communication", y = "Returned to Care"
           ,shade = TRUE)

# dt2 <- table(ds_modeling$reason_for_visit, ds_modeling$letter_sent)
#
#
# dt2 %>% t() %>% gplots::balloonplot( main ="housetasks", xlab ="", ylab="",
#             label = FALSE, show.margins = FALSE)



# ---- model-0 -----------------------------------------------------------------
predictors_0 <- c(
  "letter_sent"
  ,"reason_for_visit"
)
lsm0 <- ds_modeling %>% run_logistic(predictors_0)
# AFter controlling for the effect of the Follow up Communication,
# the effect of the 1Reason for Visit1 still remains significatn (Deviance.Rside= 30.82, df = 5, p < .001)

# ---- graph-0 -----------------------------------------------------------------

lsm0$predicted %>%
  ggplot(aes(x = letter_sent, y = probability))+
  geom_bar(stat = "identity")

# ---- model-1 -----------------------------------------------------------------

# model_1 <- stats::glm(
#   formula = returned_to_care ~  letter_sent + male
#   ,family = "binomial"
#   ,data = ds_modeling
# )
# summary(model_1)
# # create levels of predictors for which to generate predicted values
# ds_predicted_1 <- ds_modeling %>%
#   dplyr::select(letter_sent, male) %>%
#   dplyr::distinct()
# # add model prediction
# ds_predicted_1 <- ds_predicted_1 %>%
#   dplyr::mutate(
#     log_odds     = predict(object = model_1, newdata = .)
#     ,probability = plogis(log_odds)
#   )

# ---- graph-1 -----------------------------------------------------------------

# g1 <- ds_predicted_1 %>%
#   ggplot(aes(x = male, y = probability))+
#   geom_point(aes(color = letter_sent))
# g1


# ---- model-2 -----------------------------------------------------------------



# ---- graph-2 -----------------------------------------------------------------


# ---- model-3 -----------------------------------------------------------------



# ---- graph-3 -----------------------------------------------------------------



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
