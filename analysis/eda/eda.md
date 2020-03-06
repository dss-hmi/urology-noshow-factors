---
title: "EDA"
output:
  html_document: 
    highlight: tango
    keep_md: yes
    theme: readable
    toc: yes
    toc_float: yes
---
This report conducts basic exploratory data analysis (EDA) of the collected data. 

<!-- These two chunks should be added in the beginning of every .Rmd that you want to source an .R script -->
<!--  The 1st mandatory chunck  -->
<!--  Set the working directory to the repository's base directory -->


<!--  The 2nd mandatory chunck  -->
<!-- Set the report-wide options, and point to the external code file. -->



<!-- Load 'sourced' R files.  Suppress the output when loading packages. --> 



<!-- Load the sources.  Suppress the output when loading sources. --> 



<!-- Load any Global functions and variables declared in the R file.  Suppress the output. --> 


<!-- Declare any global functions specific to a Rmd output.  Suppress the output. --> 


<!-- Load the datasets.   -->


<!-- Inspect the datasets.   -->


<!-- Tweak the datasets.   -->


# Univariate 
<!-- Basic graph view.   -->

##  male 
<img src="figure_rmd/univariate-1.png" width="550px" />

##  reason_for_visit 
<img src="figure_rmd/univariate-2.png" width="550px" />

##  provider 
<img src="figure_rmd/univariate-3.png" width="550px" />

##  insurance 
<img src="figure_rmd/univariate-4.png" width="550px" />

##  history_noshow 
<img src="figure_rmd/univariate-5.png" width="550px" />

##  month_of_appointment 
<img src="figure_rmd/univariate-6.png" width="550px" />

##  pm_appointment 
<img src="figure_rmd/univariate-7.png" width="550px" />

##  preferred_language 
<img src="figure_rmd/univariate-8.png" width="550px" />

##  returned_to_care 
<img src="figure_rmd/univariate-9.png" width="550px" />

##  letter_sent 
<img src="figure_rmd/univariate-10.png" width="550px" />

# Bivariate 
This section shows the proportion of patients have "Returned to Care", broken down for each variables by its levels. 

<!-- Basic graph view.   -->
<img src="figure_rmd/bivariate-1.png" width="550px" />

##  male 
<img src="figure_rmd/bivariate-2.png" width="550px" />

##  reason_for_visit 
<img src="figure_rmd/bivariate-3.png" width="550px" />

##  provider 
<img src="figure_rmd/bivariate-4.png" width="550px" />

##  insurance 
<img src="figure_rmd/bivariate-5.png" width="550px" />

##  history_noshow 
<img src="figure_rmd/bivariate-6.png" width="550px" />

##  month_of_appointment 
<img src="figure_rmd/bivariate-7.png" width="550px" />

##  pm_appointment 
<img src="figure_rmd/bivariate-8.png" width="550px" />

##  preferred_language 
<img src="figure_rmd/bivariate-9.png" width="550px" />

##  letter_sent 
<img src="figure_rmd/bivariate-10.png" width="550px" />



