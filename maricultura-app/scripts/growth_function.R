# Function for growth model
growth_function = function (species_var, suitable_sst) {
  # Set variables from vector (starts at element 2 because element 1 is species name)
  a1 = species_var[2]
  a2 = species_var[3]
  b1 = species_var[4]
  b2 = species_var[5]
  T0 = species_var[6]
  
  # Separete cells into cells above and below optimal SST
  cells_below_optimal <- suitable_sst < T0
  
  cells_above_optimal <-  suitable_sst > T0
  
  # Apply growth equations
  growth_below_optimal <- a1*cells_below_optimal*suitable_sst - b1*cells_below_optimal
  growth_above_optimal <- a2*cells_above_optimal*suitable_sst + b2*cells_above_optimal
  
  # Add both rasters
  growth_added <- growth_above_optimal + growth_below_optimal
}