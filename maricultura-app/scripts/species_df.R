#Species Dataframe 
# Data frame with coefficients for different species
species <- c("Atlantic salmon", "Gilthead seabream", "Rachycentron
canadum")
a1 <- c(0.0264, 0.026, 0.0714) #0.093 may be new slope
a2 <- c(-0.066, -0.0042, -0.1667)
b1 <- c(-0.0396, -0.0308, -1.5714)
b2 <- c(1.254, 0.1388, 5.3333)
T0 <- c(14, 25, 29)
Linf <- c(54.7, 140, 133.3)
time0 <- c(0, 0, -0.13)
a <- c(0, 0, 0.00479)
b <- c(0, 0, 3.11)
A_omega <- c(0, 0, 0.0714*12)
B_omega <- c(0, 0, -1.5714*12)
species_df <- data.frame(species, a1, a2, b1, b2, T0, Linf, time0, a, b, A_omega, B_omega)