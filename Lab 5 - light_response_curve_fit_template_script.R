#0 Call libraries (packages)
library(photosynthesis)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

#1 Import data set .csv
#Name the df you import "data"

#2 CAN SKIP THIS
#If your Q is incident PAR and you want to approximate absorbed light:

alpha_Q <- 0.84
data <- data %>% mutate(Qabs = alpha_Q * Q)

#3 Fit one light response curve
fit_A <- fit_photosynthesis(
  .data = filter(data, group == "A"),
  .photo_fun = "aq_response",
  .vars = list(.A = A, .Q = Q)   # use Qabs if you made Qabs
)

summary(fit_A)
coef(fit_A)
confint(fit_A)


#4 Plot first raw data points and fitted curve
#Throughout this section change Q to Qabs if you made Qabs
b <- coef(fit_A)

df_pred <- data.frame(Q = seq(0, max(data$Q, na.rm = TRUE), length.out = 200)) %>%
  mutate(
    A_pred = marshall_biscoe_1980(
      Q = Q, #Use Qabs if you made it
      k_sat = b["k_sat"],
      phi_J = b["phi_J"],
      theta_J = b["theta_J"]
    ) - b["Rd"]
  )

ggplot(filter(data, group == "A"), aes(Q, A)) +
  geom_point(size = 2) +
  geom_line(data = df_pred, aes(Q, A_pred), linewidth = 1) +
  labs(x = "PAR (µmol m⁻² s⁻¹)", y = "Net photosynthesis A (µmol CO₂ m⁻² s⁻¹)") +
  theme_classic()

#5 Now plot another LRC for Group B by editing the 2 blocks of code above
#I'd change all instances of fit_A to fit_B, A_pred to B_pred
#Also b in line 28 to b2
#Also "A" in line 40 to "B"
#Also "Net photosynthesis A.." to "Net photosynthesis B..." in line 43

#6 This block of code will calculate light compensation point LCP
#for both A and B groups

library (purrr)

fits <- data %>%
  split(~ group) %>%
  map(~ fit_photosynthesis(
    .data = .x,
    .photo_fun = "aq_response",
    .vars = list(.A = A, .Q = Q) #Use Qbas if you made it
  ))

lcp_from_coef <- function(cf){
  Rd <- unname(cf["Rd"])
  theta_J <- unname(cf["theta_J"])
  k_sat <- unname(cf["k_sat"])
  phi_J <- unname(cf["phi_J"])
  LCP <- (Rd * (Rd * theta_J - k_sat)) / (phi_J * (Rd - k_sat))
  LCP
}

imap_dbl(fits, ~ lcp_from_coef(coef(.x)))


#7 This block of code will calculate other curve parameters for both groups

param_tbl <- imap_dfr(fits, ~{
  cf <- coef(.x)
  tibble(group = .y,
         k_sat = cf["k_sat"],
         phi_J = cf["phi_J"],
         theta_J = cf["theta_J"],
         Rd = cf["Rd"])
})

param_tbl


