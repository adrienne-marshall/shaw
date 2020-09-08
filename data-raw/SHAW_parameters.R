# Save SHAW parameters datafile.


SHAW_parameters <- read_csv("data-raw/SHAW_parameters.csv")
usethis::use_data(SHAW_parameters, internal = T, overwrite = T)
