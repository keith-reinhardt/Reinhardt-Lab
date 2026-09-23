
#0 import data 
library(tidyverse)

data<-read_csv(file.choose())

#1 plot all three species on one graph
plot<-ggplot (data=data, aes(x=VPD_kPa, y=Stomatal_Conductance_gs_mol_m2_s)) +
              geom_point(aes(color = Species)) +
              theme_classic ()
              
plot

#2 now subset data and fit Medlyn curves to each
#change fit to fit1, fit2, etc. or xeric_fit, mesic_fit, etc.
subset_data <- subset(data, Species == "Xeric_Species")
xeric_fit <- nls(Stomatal_Conductance_gs_mol_m2_s ~ g0 + 1.6 * (1 + g1 / sqrt(VPD_kPa)) * (Assimilation_A_umol_m2_s / Ca_umol_mol), data = subset_data, start=list(g0=0.01, g1=4))
summary(xeric_fit)
