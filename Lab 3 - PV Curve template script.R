#BIOL 4405/5504 Lab 3
#Plant Water Relations - Pressure-Volume Curve analysis

#if you have not installed this package do this

install.packages("photosynthsis")

library(photosynthesis)
library (tidyverse)

# Example structure for a Pressure-Volume ("PV") curve:
# psi_leaf: MPa (negative)
# RWC: relative water content (0–1) <or> 
# % mass-based water content depending on your dataset

#This is how you create a tibble with your data.
#you can edit this code, <or>
#read in your own data table

pv <- tibble::tibble(
  psi       = c(-0.2, -0.4, -0.8, -1.2, -1.8, -2.5, -3.2), # MPa
  mass      = c(5.832, 5.801, 5.742, 5.690, 5.612, 5.530, 5.445), #g (bag+leaf)
  leaf_mass = 0.215,   # g (dry mass) CHANGE TO 0.05
  bag_mass  = 5.200,   # g (empty bag) CHANGE TO 0.0
  leaf_area = 18.6     # cm^2
)

#the fit function below converts tibble numeric data into
#string data, which it doesn't like
#so, using this line of code to make your tibble a data frame
#(do not have to do this if you read in your own data (I THINK))

pv_df <- as.data.frame(pv)

#make the P-V curve
#can name "fit" and "Leaf 1 PV Curve" whatever you want

fit <- fit_PV_curve(data = pv_df, title = "Leaf 1 PV curve")

#now look at the data & P-V curve

fit[[1]] # shows you the parameters calculated from the fitted curve
fit[[2]] # water mass vs psi plot; I don't use this very much
fit[[3]] # PV plot (1/psi vs 100-RWC); This is the plot you want