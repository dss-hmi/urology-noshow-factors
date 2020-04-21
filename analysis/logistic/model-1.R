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


# ---- letter_1  --------------------------------------------------------------
ds_modeling %>%
  dplyr::select(reason_for_visit, letter_sent) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Reason for visit", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)

mosaicplot(~letter_sent + reason_for_visit, data = ds_modeling,
           main = "Was letter sent after the visit?"
           ,xlab = "Follow up Communication", y = "Reason to visit"
           ,shade = TRUE)


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

# ---- m00 ------------------
outcome <- "returned_to_care ~ "
predictors_00 <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
)
lsm00 <- ds_modeling %>% run_logistic(predictors_00)
model <- lsm00$model
print(model$formula, showEnv = F)
model %>% get_rsquared()
model %>% get_model_fit()
summary(model) %>% print()
exp(cbind(OR = coef(model), confint(model))) #%>% neat()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()

# ---- m00a ---------------
outcome <- "returned_to_care ~"
# modeling the outcome using a single predictor

l <- ds_modeling %>% run_logistic("insurance")
l$model %>% get_rsquared() # 0.00%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("pm_appointment")
l$model %>% get_rsquared() # 0.20%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("preferred_language")
l$model %>% get_rsquared() # 0.34%
l$model %>% get_model_fit()

l <- ds_modeling %>% run_logistic("male")
l$model %>% get_rsquared() # 0.55%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("history_noshow")
l$model %>% get_rsquared() # 0.68%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("month_of_appointment")
l$model %>% get_rsquared() #2.43%
l$model %>% get_model_fit()

l <- ds_modeling %>% run_logistic("provider")
l$model %>% get_rsquared() #2.65%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("reason_for_visit")
l$model %>% get_rsquared() # 3.19%
l$model %>% get_model_fit()

l <- ds_modeling %>% run_logistic("letter_sent")
l$model %>% get_rsquared() # 0.25%
l$model %>% get_model_fit()



# ---- m00b ---------------------
outcome <- "returned_to_care ~"
# modeling the outcome using a combination of predictors

l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit"))
l$model %>% get_rsquared()


l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit", "letter_sent"))
l$model %>% get_rsquared()

l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit", "letter_sent", "history_noshow"))
l$model %>% get_rsquared()


# ----- returned_0 ---------------
ds_modeling %>%
  dplyr::select(returned_to_care, letter_sent) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Follow-up"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~letter_sent + returned_to_care, data = ds_modeling,
           main = "Patients returning to care after in-mail follow up"
           ,xlab = "Follow up Communication", y = "Returned to Care"
           ,shade = TRUE)


# ---- returned_1 -----------------------
# insurance
ds_modeling %>%
  dplyr::select(returned_to_care, insurance) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Insurance"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~insurance + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Insurance"
           ,xlab = "Insurance", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("insurance")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# Insurance does not appear to be related to return (ChiSq = 0,df =1, p = .92)
outcome <- "returned_to_care ~ "
predictors <- c(
  "pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"insurance"              # Variance Explained (on its own) = 0.00%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()

# ---- returned_2 -----------------------
# pm_appointment
ds_modeling %>%
  dplyr::select(returned_to_care, pm_appointment) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "PM Appointment"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~pm_appointment + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by PM Appointment"
           ,xlab = "PM Appointment", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("pm_appointment")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_3 -----------------------
# preferred_language
ds_modeling %>%
  dplyr::select(returned_to_care, preferred_language) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Preferred Language"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~preferred_language + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Preferred Language"
           ,xlab = "Preferred Language", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("preferred_language")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_4 -----------------------
# male
ds_modeling %>%
  dplyr::select(returned_to_care, male) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Gender"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~male + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Gender"
           ,xlab = "Gender", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("male")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"male"                  # Variance Explained (on its own) = 0.55%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_5 -----------------------
# history_noshow
ds_modeling %>%
  dplyr::select(returned_to_care, history_noshow) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "History No-Show"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~history_noshow + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by history of No-Shows"
           ,xlab = "History No-Show", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("history_noshow")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# No show appears to be barely correlated with retured to care
# with history "OTHER" are 1.6 times more likely to return than history "None"

outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_6 -----------------------
# month_of_appointment
ds_modeling %>%
  dplyr::select(returned_to_care, month_of_appointment) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Month of Appointment"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~month_of_appointment + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Month of Appointment"
           ,xlab = "Month of Appointment", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("month_of_appointment")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# month_of_appointment  is significantly associated with return (ChiSq = 20.9,df = 11, p = .03)
# Appointment made in Septermber are least likely to return to care
# Appointment made in May are most likely to return to care, 5.34 times more likely that in Septermber
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_7 -----------------------
# provider
ds_modeling %>%
  dplyr::select(returned_to_care, provider) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Provider"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~provider + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Provider"
           ,xlab = "Provider", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("provider")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# provider is significanlty associated with return
# those seen by MD are 2.62 times more likely to return than those seen by ARNP
# those seen by PA are 2.31 times more likely to return than those seen by ARNP
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"provider"              # Variance Explained (on its own) = 2.65%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- returned_8 -----------------------
# reason_for_visit
ds_modeling %>%
  dplyr::select(returned_to_care, reason_for_visit) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Returned to Care", "Reason for Visit"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~reason_for_visit + returned_to_care, data = ds_modeling,
           main = "Patients returning to care by Reason for Visit"
           ,xlab = "Reason for Visit", y = "Returned to Care"
           ,shade = TRUE)

outcome <- "returned_to_care ~"
l <- ds_modeling %>% run_logistic("reason_for_visit")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# provider is significantly associated with return (ChiSq = 27.3,df = 3, p = .000)
# Patients who came for Renal/Uterer were 4.02 time more likely return than Voiding
# Patients who came for Testies/Scrotum were 2.87 time more likely return than Voiding
# Patients who came for Penile  were 1.97 time more likely return than Voiding
outcome <- "returned_to_care ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.20%
  ,"preferred_language"    # Variance Explained (on its own) = 0.34%
  ,"male"                  # Variance Explained (on its own) = 0.55%
  ,"history_noshow"        # Variance Explained (on its own) = 0.68%
  ,"month_of_appointment"  # Variance Explained (on its own) = 2.43%
  ,"provider"              # Variance Explained (on its own) = 2.65%
  ,"letter_sent"           # Variance Explained (on its own) = 0.25%
  ,"reason_for_visit"      # Variance Explained (on its own) = 3.19%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))




# ---- graph-00 -----------------------------------------------------------------
# MD vs ARNP vs PA predicts return to care when looking at reason for visit and if they've had a prior no show

g00 <- lsm00$predicted %>%
  ggplot(aes(x = provider, y = probability, fill = provider))+
  geom_boxplot(alpha = .3)+
  geom_jitter(alpha =.3 )+
  facet_grid(history_noshow ~ reason_for_visit)+
  theme_bw()
g00



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

# ---- m01 ------------------------
outcome <- "letter_sent ~ "
predictors_01 <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
)
lsm00 <- ds_modeling %>% run_logistic(predictors_01)
model <- lsm00$model
print(model$formula, showEnv = F)
model %>% get_rsquared()
model %>% get_model_fit()
summary(model) %>% print()
exp(cbind(OR = coef(model), confint(model))) #%>% neat()
# summary(model)$coefficients %>% neat(output_format = "pandoc") %>% print()
# https://datascienceplus.com/perform-logistic-regression-in-r/
anova(model, test="Chisq") %>% print()

# ---- m01a ---------------
outcome <- "letter_sent ~"
# modeling the outcome using a single predictor

l <- ds_modeling %>% run_logistic("insurance")
l$model %>% get_rsquared() # 0.00%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("pm_appointment")
l$model %>% get_rsquared() # 0.10%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("preferred_language")
l$model %>% get_rsquared() # 0.02%
l$model %>% get_model_fit()

l <- ds_modeling %>% run_logistic("male")
l$model %>% get_rsquared() # 0.01%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("history_noshow")
l$model %>% get_rsquared() # 0.58%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("month_of_appointment")
l$model %>% get_rsquared() # 10.53%
l$model %>% get_model_fit()

l <- ds_modeling %>% run_logistic("provider")
l$model %>% get_rsquared() # 1.80%
l$model %>% get_model_fit()
exp(cbind(OR = coef(l$model), confint(l$model)))

l <- ds_modeling %>% run_logistic("reason_for_visit")
l$model %>% get_rsquared() # 1.70%
l$model %>% get_model_fit()



# ---- m01b ---------------------
outcome <- "letter_sent ~"
# modeling the outcome using a combination of predictors

l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit"))
l$model %>% get_rsquared()


l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit"))
l$model %>% get_rsquared()

l <- ds_modeling %>% run_logistic(c("month_of_appointment","provider","reason_for_visit", "history_noshow"))
l$model %>% get_rsquared()



# ---- letter_1 -----------------------
# insurance
ds_modeling %>%
  dplyr::select(letter_sent, insurance) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Insurance"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~insurance + letter_sent, data = ds_modeling,
           main = "Patients followed up by Insurance"
           ,xlab = "Insurance", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("insurance")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# Insurance does not appear to be related to return (ChiSq = 0,df =1, p = .92)
outcome <- "letter_sent ~ "
predictors <- c(
  "pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"insurance"              # Variance Explained (on its own) = 0.00%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()

# ---- letter_2 -----------------------
# pm_appointment
ds_modeling %>%
  dplyr::select(letter_sent, pm_appointment) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "PM Appointment"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~pm_appointment + letter_sent, data = ds_modeling,
           main = "Patients followed up by PM Appointment"
           ,xlab = "PM Appointment", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("pm_appointment")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_3 -----------------------
# preferred_language
ds_modeling %>%
  dplyr::select(letter_sent, preferred_language) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Preferred Language"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~preferred_language + letter_sent, data = ds_modeling,
           main = "Patients followed up by Preferred Language"
           ,xlab = "Preferred Language", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("preferred_language")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_4 -----------------------
# male
ds_modeling %>%
  dplyr::select(letter_sent, male) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Gender"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~male + letter_sent, data = ds_modeling,
           main = "Patients followed up by Gender"
           ,xlab = "Gender", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("male")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"male"                  # Variance Explained (on its own) = 0.01%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_5 -----------------------
# history_noshow
ds_modeling %>%
  dplyr::select(letter_sent, history_noshow) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "History No-Show"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~history_noshow + letter_sent, data = ds_modeling,
           main = "Patients followed up by history of No-Shows"
           ,xlab = "History No-Show", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("history_noshow")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_6 -----------------------
# month_of_appointment
ds_modeling %>%
  dplyr::select(letter_sent, month_of_appointment) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Month of Appointment"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~month_of_appointment + letter_sent, data = ds_modeling,
           main = "Patients followed up by Month of Appointment"
           ,xlab = "Month of Appointment", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("month_of_appointment")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_7 -----------------------
# provider
ds_modeling %>%
  dplyr::select(letter_sent, provider) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Provider"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~provider + letter_sent, data = ds_modeling,
           main = "Patients followed up by Provider"
           ,xlab = "Provider", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("provider")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
# provider is significanlty associated with return
# those seen by MD are 2.62 times more likely to return than those seen by ARNP
# those seen by PA are 2.31 times more likely to return than those seen by ARNP
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
  ,"provider"              # Variance Explained (on its own) = 1.80%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))

# ---- letter_8 -----------------------
# reason_for_visit
ds_modeling %>%
  dplyr::select(letter_sent, reason_for_visit) %>%
  sjPlot::sjtab(fun = "xtab", var.labels=c("Letter Sent", "Reason for Visit"),
                show.row.prc=T, show.col.prc=T, show.summary=T, show.exp=T, show.legend=T)
mosaicplot(~reason_for_visit + letter_sent, data = ds_modeling,
           main = "Patients followed up by Reason for Visit"
           ,xlab = "Reason for Visit", y = "Letter Sent"
           ,shade = TRUE)

outcome <- "letter_sent ~"
l <- ds_modeling %>% run_logistic("reason_for_visit")
l$model %>% get_rsquared()
l$model %>% get_model_fit()
summary(l$model) %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))
outcome <- "letter_sent ~ "
predictors <- c(
  "insurance"              # Variance Explained (on its own) = 0.00%
  ,"pm_appointment"        # Variance Explained (on its own) = 0.10%
  ,"preferred_language"    # Variance Explained (on its own) = 0.02%
  ,"male"                  # Variance Explained (on its own) = 0.01%
  ,"history_noshow"        # Variance Explained (on its own) = 0.58%
  ,"month_of_appointment"  # Variance Explained (on its own) = 10.53%
  ,"provider"              # Variance Explained (on its own) = 1.80%
  ,"reason_for_visit"      # Variance Explained (on its own) = 1.70%
)
l <- ds_modeling %>% run_logistic(predictors)
l$model %>% anova(test="Chisq") %>% print()
exp(cbind(OR = coef(l$model), confint(l$model)))






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
