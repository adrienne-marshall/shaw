#' Function to build SHAW moisture initial temperature conditions for the soil profile.
#'
#' This function allows you to build SHAW files based on desired parameters.
#' @param model_dir This is the directory where all inputs will be written.
#' @param temp_obs Observations available. Must be a data frame with columns named depth and temperature.
#' @param lower_bc Lower boundary condition if known. Not used if NA.
#' @param soil1 First line of the soils.
#' @param site_file This is the site file - will be used to determine saturated or residual values.
#' @keywords hydrology, SHAW.
#' @export
#'
#'
shaw_temp <- function(model_dir,
                     soil1 = 15,
                     lower_bc = NA,
                     temp_obs,
                     site_file = paste0(model_dir, "/", basename(model_dir), ".sit")){
  site <- readLines(site_file)
  start <- unlist(strsplit(site[2], split = " |\t"))[1:3]

  # Make soils into a data frame.
  soils <- lapply(site[soil1:length(site)], FUN = function(x){
    vec <- as.numeric(unlist(strsplit(x, split = " |\t")))
    depth <- vec[1]

    return(depth)
  })
  soils <- data.frame(depth = unlist(soils))

  soils2 <- dplyr::left_join(soils, temp_obs, by = "depth")

  if(!is.na(lower_bc)){soils2$temperature[nrow(soils2)] <- lower_bc}

  temp_vec <- tidyr::fill(soils2, temperature, .direction = "downup")
  temp_vec <- as.numeric(temp_vec$temperature)


  start <- paste(start, collapse = " ")
  temp_vec <- paste(temp_vec, collapse = " ")
  ans <- paste(start, temp_vec, collapse = " ")

  writeLines(ans, paste0(model_dir, "/", basename(model_dir), ".tem"))

  return(ans)

} # end function.


