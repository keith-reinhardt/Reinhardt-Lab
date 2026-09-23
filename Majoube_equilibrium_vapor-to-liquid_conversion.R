# Convert measured water vapor isotopes to equilibrium liquid-water isotopes
# using Majoube (1971) liquid-vapor equilibrium fractionation

#One way is to create a function
#First, create a function
vapor_to_liquid <- function(d18O_vapor, d2H_vapor, temp_C) {
  
  # Convert temperature to Kelvin
  T <- temp_C + 273.15
  
  # Majoube (1971): 1000 ln(alpha), liquid-vapor
  
  ln_alpha_18O_1000 <- (1.137e6 / T^2) -
    (0.4156e3 / T) -
    2.0667
  
  ln_alpha_2H_1000 <- (24.844e6 / T^2) -
    (76.248e3 / T) +
    52.612
  
  # Convert 1000 ln(alpha) to alpha
  alpha_18O <- exp(ln_alpha_18O_1000 / 1000)
  alpha_2H  <- exp(ln_alpha_2H_1000 / 1000)
  
  # Convert vapor delta values to equilibrium liquid values
  d18O_liquid <- alpha_18O * (d18O_vapor + 1000) - 1000
  
  d2H_liquid <- alpha_2H * (d2H_vapor + 1000) - 1000
 
  #(side note: Rothfuss 2013 has these equations
  #d2H_l = 104.96 -1.0342*temp + 1.0724*d2H_v
  #d18O = 11.45 - 0.0795*temp +1.0012*d18O_v
  #they generate the same values as the eqns in teh function)
   
  return(data.frame(
    temp_C = temp_C,
    alpha_18O = alpha_18O,
    alpha_2H = alpha_2H,
    d18O_vapor = d18O_vapor,
    d18O_liquid = d18O_liquid,
    d2H_vapor = d2H_vapor,
    d2H_liquid = d2H_liquid
  ))
}

#Now, apply your function using your vapor values/isotopic ratios
vapor_to_liquid(
  d18O_vapor = -15,
  d2H_vapor = -120,
  temp_C = 25
)

#Another option is to calculate directly with mutate()
#Here, I'm using a generic df named "borehole_data"

library(dplyr)

borehole_data <- borehole_data %>%
  mutate(
    
    # Temperature in Kelvin
    T_K = temperature + 273.15,
    
    # Equilibrium fractionation factors
    alpha_18O = exp(
      ((1.137e6 / T_K^2) -
         (0.4156e3 / T_K) -
         2.0667) / 1000
    ),
    
    alpha_2H = exp(
      ((24.844e6 / T_K^2) -
         (76.248e3 / T_K) +
         52.612) / 1000
    ),
    
    # Convert vapor isotope values to liquid-water equivalents
    d18O_liquid = alpha_18O * (d18O_vapor + 1000) - 1000,
    
    d2H_liquid = alpha_2H * (d2H_vapor + 1000) - 1000
  )