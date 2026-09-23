#Lab 4 Plant Physiology BIOL 4404/5504L
#code to make a vulnerability curve and derive
#hydraulic parameters from it

library (photosynthesis)
library (tidyverse)

#IMPORT data. Do this together in class.
library(readr)
curve1 <- read_csv("Dropbox/BIOL4404/2026/Lab/Lab 4 -vuln_curve_site_1.csv")
View(Lab_4_vuln_curve_site_1)

#Filter to one individual (or one stem segment), then fit:

curve1_filter <- curve1 |> filter(branch_number == "2")

#convert tibble into a data frame

curve1_filter_df<-as.data.frame(curve1_filter)

#perform the curve fitting using the function 'fit_hydra_vuln_curve'
vuln_curve_1 <- fit_hydra_vuln_curve(
  curve1_filter_df,
  varnames = list(psi = "psi1", PLC = "PLC"),
  title = "vuln curve 1"
)

#return sigmoidal fit summary
summary(vuln_curve_1[[1]]) 

#return Weibull fit summary
summary(vuln_curve_1[[4]]) #expecting a = 4.99, b = 3.22

#return model parameters
vuln_curve_1[[2]] 

#return hydraulic parameters
vuln_curve_1[[3]]

#return graph
vuln_curve_1[[5]]
