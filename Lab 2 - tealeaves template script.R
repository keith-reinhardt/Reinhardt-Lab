#1
install.packages("tealeaves")
install.packages("units")    # critical
install.packages("tidyverse")    # dependency


#2
library(tealeaves)
library(units)
library(tidyverse)

#On macOS/Linux, the units package may require udunits2 system libraries.

#3
#Core workflow (always the same)
#Every tealeaves run has four objects:
#Object-->	Purpose
#leafpar-->	leaf traits
#enviropar-->	environment
#constants-->	physical constants
#tleaves()-->	solver

#4
#Create leaf parameters (make_leafpar())
#Minimal example (leaf size + stomatal comnductance only)

leaf_par <- make_leafpar(
  replace = list(
    leafsize = set_units(0.025, "m"),
    g_sw = set_units (0.2, "umol/m^2/Pa/s") # 2.5 cm characteristic dimension
  )
)

#5
#Create environmental parameters (make_enviropar())
#Air temperature sweep (example 2 in the paper)

enviro_par <- make_enviropar(
  replace = list(
    T_air = set_units(seq(275, 310, 5), "K"), #a sequence of Tair from 275->310
    P = set_units (90, "kPa")                 #a typical value for our elevation
  )
)

#Example with radiation and wind:
#(note that this undoes the sequence of T_air in script above)
enviro_par <- make_enviropar(
    replace = list(
      T_air = set_units(298, "K"),
      wind  = set_units(2, "m/s"),
      S_sw  = set_units(800, "W/m^2")
    )
  )

#6
#Create constants
constants <- make_constants()
#You almost never need to modify these unless doing sensitivity tests.

#7
#Solve for leaf temperature (tleaves())
#must re-run these lines of code every time you change a parameter
T_leaves <- tleaves(
  leaf_par,
  enviro_par,
  constants,
  quiet = TRUE
)

#ignore any "deprecation" in dplyr 1.0.0 message
#This returns a tibble with:
#•	leaf temperature
#•	boundary layer conductance
#•	sensible heat flux
#•	latent heat flux
#•	radiation terms

#8
#Inspect:

print(T_leaves)
str(T_leaves) #I don't really use this

#9
#plotting data
#template for a basic line plot
library(ggplot2) #note that ggplot2 is part of tidyverse

ggplot(T_leaves, aes(x = T_air, y = T_leaf)) +
  geom_line()

#IMPORTANT: note that ggplot2 does not like units! You may have to convert first
T_leaves_converted <- T_leaves |>
  mutate(
    T_leaf = drop_units(T_leaf),
    T_air  = drop_units(T_air),
    L = drop_units(L)
  )


#multiple y-axis variables
ggplot(T_leaves_converted, aes(x = T_air)) +
  geom_line(aes(y = T_leaf,  color = "T_leaf")) +
  geom_line(aes(y = L, color = "L")) +
  labs(y = "Value (K or W/m^2)", 
       x= "T_air (K)",
       color = ""
       )

#IMPORTANT: the coding above plots the figure in R Studio, but it doesn't save
#it an as object for you to copy and paste (or just save in a folder)
#To make an object for it, do this:

my_plot<-ggplot(T_leaves_converted, aes(x = T_air)) +
  geom_line(aes(y = T_leaf,  color = "T_leaf")) +
  geom_line(aes(y = L, color = "L")) +
  labs(y = "Value (K or W/m^2)", 
       x= "T_air (K)",
       color = ""
  )

#can use different names for 'my_plot" that describe your figure, e.g.
#leaf_t_air_plot<-
#or
#Leaf_vs_Tair_plot<- (for our first plot that we made)







