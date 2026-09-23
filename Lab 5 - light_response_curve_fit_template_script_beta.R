#1 Call libraries (packages)
library(photosynthesis)
library(readr)
library(dplyr)
library(ggplot2)
library(broom)

#2 CAN SKIP THIS
#If your Q is incident PAR and you want to approximate absorbed light:

alpha_Q <- 0.84
dat <- dat %>% mutate(Qabs = alpha_Q * Q)

#3 Fit one light response curve
fit_shade <- fit_photosynthesis(
  .data = filter(dat, group == "Sun"),
  .photo_fun = "aq_response",
  .vars = list(.A = A, .Q = Q)   # use Qabs if you made Qabs
)

summary(fit_shade)
coef(fit_shade)
confint(fit_shade)


#4 Plot first raw data points and fitted curve
#Throughout this section change Q to Qabs if you made Qabs
b <- coef(fit_shade)

df_pred <- data.frame(Q = seq(0, max(dat$Q, na.rm = TRUE), length.out = 200)) %>%
  mutate(
    A_pred = marshall_biscoe_1980(
      Q = Q, #Use Qabs if you made it
      k_sat = b["k_sat"],
      phi_J = b["phi_J"],
      theta_J = b["theta_J"]
    ) - b["Rd"]
  )

ggplot(filter(dat, group == "Sun"), aes(Q, A)) +
  geom_point(size = 2) +
  geom_line(data = df_pred, aes(Q, A_pred), linewidth = 1) +
  labs(x = "PAR (µmol m⁻² s⁻¹)", y = "Net photosynthesis A (µmol CO₂ m⁻² s⁻¹)") +
  theme_classic()

#5 Compute light compensation point

fits <- dat %>%
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

###BETA testing below. Trying to calculate LSP
#6 Compute light saturation point

calc_LSP <- function(fit, percent = 0.90, maxQ = 3000){
  
  cf <- coef(fit)
  
  A_fun <- function(Q){
    marshall_biscoe_1980(
      Q_abs = Q,
      k_sat = cf["k_sat"],
      phi_J = cf["phi_J"],
      theta_J = cf["theta_J"]
    ) - cf["Rd"]
  }
  
  Amax <- max(A_fun(seq(0, maxQ, 1)))
  target <- percent * Amax
  
  uniroot(function(Q) A_fun(Q) - target,
          interval = c(0, maxQ))$root
}

calc_LSP(fit, percent = 0.90)

######
library(photosynthesis)

calc_LSP <- function(fit, percent = 0.90, maxQ = 3000) {
  stopifnot(inherits(fit, "nls"))
  
  cf <- coef(fit)
  # handle possible capitalization differences (k_sat vs K_sat)
  k_sat  <- unname(cf[intersect(names(cf), c("k_sat","K_sat"))][1])
  phi_J  <- unname(cf["phi_J"])
  thetaJ <- unname(cf[intersect(names(cf), c("theta_J","theta_J"))][1])
  Rd     <- unname(cf["Rd"])
  
  A_fun <- function(Q) {
    marshall_biscoe_1980(Q = Q, k_sat = k_sat, phi_J = phi_J, theta_J = thetaJ) - Rd
  }
  
  Qgrid <- 0:maxQ
  Agrid <- A_fun(Qgrid)
  Amax  <- max(Agrid, na.rm = TRUE)
  target <- percent * Amax
  
  uniroot(function(Q) A_fun(Q) - target, interval = c(0, maxQ))$root
}

LSP_90 <- calc_LSP(fit, percent = 0.90)
LSP_95 <- calc_LSP(fit, percent = 0.95)

LSP_90; LSP_95

####KR Amax
Amax<-20 #ENTER YOUR K_SAT VALUE HERE
Rd<-2 #ENTER YOUR R_D VALUE HERE
QY<-0.05 #ENTER YOUR THETA_J VALUE HERE
Curve<-0.80 #ENTER YOUR PHI_J VALUE HERE
Amax90<-0.90*Amax
Ag<-Amax90+Rd

LSP<-((Amax90+Rd)*(Amax-QY*(Amax90+Rd)))/(Curve*(Amax-(Amax90+Rd)))
LSP

LSP2<-(Ag*(Amax-QY*Ag))/(Curve*(Amax-Ag))
LSP2



