#BIOL 4404/5504L
#Lab 6 - A-Ci curve analysis template script

#0 open these packages
library(photosynthesis)
library(dplyr)
library(ggplot2)

#1 read in your data file
#1 reasd.csv NOT read_csv
data<-read.csv(file.choose())

#2 create columns for T_leaf in K and PPFD in umol m-2 s-1)
data_new <- data %>%
  mutate(
    T_leaf = 25 + 273.15,
    PPFD = 1500,
    curve_id = interaction(treatment, rep)
  )

#3 fit multiple curves at once
fits <- fit_many(
  data = data_new,
  group = "curve_id",
  funct = fit_aci_response,
  varnames = list(A_net="A_net", T_leaf="T_leaf", C_i="Ci", PPFD="PPFD"),
  fitTPU = TRUE
)

pars  <- compile_data(fits, output_type="dataframe", list_element=1)
plots <- compile_data(fits, list_element=2)

# to export parameters as a csv
write.csv(pars, "aci_parameters_photosynthesis.csv", row.names = FALSE)
getwd() # in case you don't know where this .csv file got stored

#4a Look at one curve
plots[[1]] #choose 1-6

#4b Look at multiple plots

plots <- compile_data(fits, list_element = 2)

plots[[1]]   # show first curve
plots[[2]]   # second curve
plots[[3]]   #third curve....etc

#4c look at multiple plots all on the same page

install.packages("patchwork")
library(patchwork)

wrap_plots(plots)

labels<-names(fits)

wrap_plots(plots, ncol = 3)   #this specifies 3 columns, which we already had...
  
#4c multiple plots LABELED (not required)
plots_labeled <- Map(
  function(p, lab) p + ggtitle(lab),
  plots,
  data_new %>%
    distinct(curve_id, treatment) %>%
    pull(treatment)
)

wrap_plots(plots_labeled, ncol = 3) +
  plot_annotation(tag_levels = "A")

#5 plot means of curves on one figure
fits_data <- compile_data(
  fits,
  output_type = "dataframe",
  list_element = 3   # element 3 = original data + model predictions
)

head(fits_data)
fits_data_mean <- fits_data %>%
  group_by(treatment, C_i) %>%
  summarise(
    A_obs = mean(A_net),
    A_fit = mean(A_model),
    .groups = "drop"
  )

ggplot(fits_data_mean, aes(C_i, A_fit, color = treatment)) +
  geom_line(linewidth = 1.5) +
  geom_point(aes(y = A_obs), size = 2) +
  theme_classic()


#6 optional!!!
#6 if you want to fit just one curve
fit1 <- fit_aci_response(
  data = data_new %>% filter(curve_ID == 1), #change curve_ID to the curve # you want
  varnames = list(
    A_net = "A_net",
    T_leaf = "T_leaf",
    C_i = "Ci",
    PPFD = "PPFD"),
  fitTPU = TRUE
)

fit1[[1]]   # parameter estimates (Vcmax, Jmax, Rd, TPU if fit, transitions, etc.)
fit1[[2]]   # plot the one curve
head(fit1[[3]]) #shows you all the information used to plot the curve

