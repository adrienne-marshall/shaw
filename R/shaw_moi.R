#' Function to build SHAW moisture initial conditions for the soil profile.
#'
#' This function allows you to build SHAW files based on desired parameters.

#' @param model_dir This is the directory where all inputs will be written.
#' @param method Options are "saturated", "residual", "half", "observed". If method is "observed", moi_obs requires a data frame with depths and observed moisture. Will then do stepwise interpolation.
#' @param moi_obs Observed data frame: column names must be depth and VWC.
#' @param soil1 First line of the soils.
#' @param iwrc Flag used to define water retention curve (1 = Campbell; 2 = Brooks-Corey; 3 = van Genuchten).
#' @param site_file This is the site file - will be used to determine saturated or residual values.
#' @keywords hydrology, SHAW.
#' @export
#' @examples
#' observed <- data.frame(depth = c(0.1, 0.5, 1), VWC = c(0.4, 0.2, 0.1))

#' shaw_moi(model_dir = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/test_shaw",
#'         method = "half", iwrc = 3, soil1 = 15)
#'
#' shaw_moi(model_dir = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/test_shaw",
#'         method = "observed", iwrc = 3, soil1 = 15, moi_obs = observed)
#'
shaw_moi <- function(model_dir,
                     method,
                     moi_obs = NULL,
                     soil1,
                     iwrc = 3,
                     site_file = paste0(model_dir, "/", basename(model_dir), ".sit")){
  site <- readLines(site_file)
  start <- unlist(strsplit(site[2], split = " |\t"))[1:3]


  # Make soils into a data frame.
  soils <- lapply(site[soil1:length(site)], FUN = function(x){
    vec <- as.numeric(unlist(strsplit(x, split = " |\t")))
    depth <- vec[1]
    theta_sat <- vec[11] # always the case, regardless of IWRC.


    if(iwrc == 1){theta_res <- NA} else {theta_res <- vec[13]} # depends on IWRC.

    ans <- data.frame(depth, theta_sat, theta_res)

    return(ans)
  })
  soils <- dplyr::bind_rows(soils)
  soils <- dplyr::mutate(soils, mean_moi = round((theta_sat + theta_res) / 2, 3))

  # Make moisture inputs.
  if(method == "saturated"){
    moi_vec <- as.numeric(soils$theta_sat)
  }
  if(method == "residual"){
    moi_vec <- as.numeric(soils$residual)
  }
  if(method == "half"){
    moi_vec <- as.numeric(soils$mean_moi)
  }
  if(method == "observed"){
    soils2 <- dplyr::left_join(soils, moi_obs, by = "depth")
    soils2 <- tidyr::fill(soils2, VWC, .direction = "downup")
    soils2 <- dplyr::mutate(soils2, VWC = ifelse(VWC > theta_sat, theta_sat, VWC))
    soils2 <- dplyr::mutate(soils2, VWC = ifelse(VWC < theta_res, theta_res, VWC))
    moi_vec <- as.numeric(soils2$VWC)
  }

  start <- paste(start, collapse = " ")
  moi_vec <- paste(moi_vec, collapse = " ")
  ans <- paste(start, moi_vec, collapse = " ")

  writeLines(ans, paste0(model_dir, "/", basename(model_dir), ".moi"))

  return(ans)

} # end function.



