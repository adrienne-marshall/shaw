#' Function to build SHAW input files based on parameters
#'
#' This function allows you to build SHAW files based on desired parameters.
#' Most parameters default to NULL, and nothing in the site file changes. If you change any of the parameters from NULL, the parameter will change in the new site file.
#' @import dplyr
#' @import utils
#' @import magrittr
#' @import lubridate
#' @import purrr

#' @param model_dir This is the directory where all inputs will be written.
#' @param existing_site_file The existing site file
#' @param new_site_file The new site file.
#' @param start This is the start date, formatted as a y-m-d character.
#' @param start_hour An integer (0-24) indicating start hour.
#' @param end This is the end date, formatted as a y-m-d. Note there is no end hour.
#' @param nres Number of desired residue nodes if the residue does not change over the simulation (required). if 0, no residue. Set to 1 if residue properties change.
#' @param mcanflg Flag controlling options for input of plant growth curves and node spacing.  (0 = no plant growth, i.e. leaf area index and plant height are constant for simulation, and model will determine node spacing within the canopy; 1 = input files for plant growth are specified for each plant and model will determine node spacing within the canopy; 2 = no plant growth and allows user to input spacing and parameters of  plant canopy layers; 3 = input files for plant growth are specified for each plant and the user can specify desired heights above ground surface for canopy nodes.)  Option 3 is intended for subsequent comparison with measurements of temperature and humidity at specified heights within the canopy.
#' If MCANFLG = 1, plant height, dchar, clumping, biomass, LAI, and rooting are not used, but are taken from the .gro file.
#' @param istomate Flag to select option for computing stomatal resistance; 1 = default computation of stomatal resistance as a function of leaf water potential; 2 = option for additional controls on stomatal conductance using Stewart-Jarvis type functions for solar radiation, air temperature and vapor pressure deficit.
#' @param itype This controls plant type. Everything in Lines F-1 to F1-NPLANT should be a vector of length nplants.
#' @param gro_file Defaults to sharing a name with the site. However, if nplants > 1 and MCANFLG == 1 this can be a vector of gro file names for each plant.
#' @param soils_df Possible variable names are: "depth", "sand", "silt", "clay", "rock", "organic", "bulk_density", "ksat", "klat", "air_entry",
#' "theta_sat", "n", "theta_res", "l", "alpha", "brooks_corey_lambda", "asalt", "dispersion_coefficient", "b".
#' @keywords hydrology, SHAW.
#' @export
#'
# To dos/possibly change:
# 1. Maybe this should just take a site file as R text, rather than the path.
# 2. Consider allowing for different output frequencies - right now requires they're all the same.
# 4. Add errors and warnings.

shaw_modify_sit <- function(site,
                            start = NULL, start_hour = NULL, end = NULL,
                          lat_deg = NULL, lat_min = NULL, slope = NULL, aspect = NULL, elev = NULL,
                     soils_df = NULL, nres = NULL, roughness = NULL, measurement_height = NULL, ponding = NULL, itype = NULL, pintrcp = NULL, xangle = NULL,
                     canalb = NULL, tcrit = NULL, min_stomatal_resistance = NULL, stomatal_exp = NULL, critical_leaf_water = NULL, leaf_resist = NULL, root_resist = NULL,
                     plant_height = NULL, dchar = NULL, clumping = NULL, biomass = NULL, LAI = NULL, rooting = NULL,
                     residue_thickness = NULL, residue_weight = NULL, residue_fraction = NULL,
                     residue_albedo = NULL, lower_bc_temp = NULL, nsalt = NULL, iwrc = NULL,
                     ivlcbc  = NULL, itmpbc  = NULL, dry_soil_albedo  = NULL, soil_albedo_exponent = NULL,
                     residue_coefficient = NULL, residue_krb = NULL,
                     isnotemp = NULL, snotemp = NULL, snow_roughness = NULL,
                     Kst = NULL, Tlower = NULL, Tupper = NULL, Topt = NULL, Kvpd = NULL, r = NULL,
                     mcanflg = NULL, istomate = NULL, canma = NULL, canmb = NULL, wcandt = NULL, hrnoon = NULL,
                     nplants = NULL, nsp = NULL, nsolutes = NULL, error_tol = NULL, time_step = NULL,
                     nrchang = NULL, gmcdt = NULL,
                     debugging_seq = NULL,
                     gro_file = NULL,
                     run_name = NULL
                     ){ # begin function

  # split site file.
  site_lines <- lapply(site, strsplit, split = " |\t")
  site_lines <- lapply(site_lines, unlist)

  # Read in parameters info - line numbers get updated based on flags below.
  par_info <- SHAW_parameters %>%
    mutate(new_value = NA) %>%
    mutate(Line_number = as.list(Line_number))

  # For variables that were not given in function, assign to NULL.
  for(i in 1:nrow(par_info)){
    if(!exists(par_info$R_variable[i])){
      assign(par_info$R_variable[i], NULL)
    }
  }

  # Get relevant flags that determine locations of parameters:
  if(is.null(nplants)){nplants <- as.numeric(site_lines[[4]][1])} # number of plants
  if(is.null(nr)){nr <- as.numeric(site_lines[[4]][3])}  # number of residue nodes.
  if(is.null(ns)){ns <- as.numeric(site_lines[[4]][4])} # number of soil nodes.
  if(is.null(mcanflg)){mcanflg <- as.numeric(site_lines[[6]][1])} # growth type
  if(is.null(istomate)){istomate <- as.numeric(site_lines[[6]][2])} # stomatal conductance flag

  if(nplants > 0){
    par_info$Line_number[par_info$Line_letter == "f"] <- 6:(5 + nplants) # F series
    par_info$Line_number[par_info$Line_letter == "f_1"] <- 7:(6 + nplants) # F-1 series of lines.
    f_1s <- 7:(6+nplants) # duplicate for easy way to reference below
    if(istomate == 2){
      par_info$Line_number[par_info$Line_letter == "fa"] <- (max(f_1s) + 1):(max(f_1s) + nplants) # Fa series
      fas <- (max(f_1s) + 1):(max(f_1s) + nplants)
      }

    if(mcanflg == 0){
      par_info$Line_number[par_info$Line_letter == "f0"] <- (max(fas) + 1):(max(fas) + nplants) # F0 series
      f0s <- (max(fas) + 1):(max(fas) + nplants)
      par_info$Line_number[par_info$Line_letter == "g"] <- max(f0s) + 1 # line G, snow
      g <- max(f0s) + 1
    } else if(mcanflg == 1){
      par_info$Line_number[par_info$Line_letter == "f1"]  <- (max(f_1s) + 1):(max(f_1s) + nplants) # F1 series
      f1s <-(max(f_1s) + 1):(max(f_1s) + nplants)
      par_info$Line_number[par_info$Line_letter == "g"] <- max(f1s) + 1
      g <- max(f1s) + 1
      }
    } else if (nplants == 0){
      par_info$Line_number[par_info$Line_letter == "g"] <- 6
      g <- 6
      }

  # G1 series not included for multiple snow nodes right now.
  if(nr > 0){
    par_info$Line_number[par_info$Line_letter == "h"] <- g + 1
    h <- g + 1 # basic residue info
    par_info$Line_number[par_info$Line_letter == "h1"] <- h + 1
    h1 <- h + 1 # more complicated residue info. # solutes would go next if included.
    par_info$Line_number[par_info$Line_letter == "j1"] <- h1 + 1
    j1 <- h1 + 1 # start soils.
    } else if (nr == 0){
      par_info$Line_number[par_info$Line_letter == "j1"] <- g + 1
      j1 <- g + 1
      }# end residue.

  ivlcbc <- as.numeric(site_lines[[j1]][1]) # water flow at lower boundary flag
  itmpbc <- as.numeric(site_lines[[j1]][2]) # temperature lower boundary flag.

  if(itmpbc == 1){
    par_info$Line_number[par_info$Line_letter == "j2"] <- j1 + 1
    j2 <- j1 + 1 # lower temperature boundary condition.
    j3_1 <- j2 + 1 # j3_1 is first line of soils. not included in SHAW_parameters csv.
  } else {j3_1 <- j1 + 1}

    # Make some warnings for cases we haven't made yet. -------------
  if(!is.null(nsp) && nsp > 0){
    stop("NSP is required to be 0; greater values have not been implemented at this time.
                    Suggest starting in summer so NSP can reasonably be 0 for now.")
  }

  if(mcanflg > 1){
    stop("This function is not yet set up to deal with MCANFLG values > 1.")
  }

  if(!is.null(nsalt) && nsalt > 0){
    stop("This function isn't yet set up to deal with solutes.")
  }

  if(!is.null(nrchang) && nrchang == 1){
    stop("This function isn't set up to deal with changing residue layers.")
  }

  # Function to either extract variables from site file or get from function arguments.
  fun_new_value <- function(x){ # x is a data frame with 1 row.
    var <- x$R_variable[1]
    if(is.null(get(var))){ # if a variable was not given in the function...
      linex <- x$Line_number[[1]]
      positionx <- x$Position[1]
      x$new_value[[1]] <- unlist(purrr::map(site_lines[linex], positionx)) # use data from site file
    } else {
      x$new_value[[1]] <- get(var) # use data given in function
    }
    return(x)
  } # end replacement function.

  # Replace variables in par_info$new_value
  par_info2 <- par_info %>%
    filter(!is.na(Line_number)) %>%
    mutate(new_value = as.list(new_value)) %>%
    split(.$R_variable) %>%
    purrr::map(fun_new_value) %>%
    bind_rows()

  # Write site file.---------------
  # Line B # gets special treatment because of the way these are passed to function.
  if(!is.null(start)){
    start <- lubridate::ymd(start)
    site_lines[[2]][1:3] <- paste(lubridate::yday(start), start_hour, lubridate::year(start))
  }
  if(!is.null(end)){
    end <- lubridate::ymd(start)
    site_lines[[2]][4:5] <- paste(lubridate::yday(end), lubridate::year(end))
  }

  # Write other lines using data from par_info2
  site_lines2 <- site_lines # make a copy to modify
  for(i in 1:nrow(par_info2)){
    line_numbers <- par_info2$Line_number[[i]]
    position <- par_info2$Position[[i]]
    new_values <- par_info2$new_value[[i]]
    for(j in 1:length(line_numbers)){
      site_lines2[[line_numbers[j]]][position] <- new_values[j]
    }
  }

  site_lines2 <- lapply(site_lines2, paste, collapse = " ")

  # Now need to add soils df.
  if(!is.null(soils_df)){
    soils_lines <- soils_df %>%
      split(.$depth) %>%
      purrr::map_chr(~str_c(as.numeric(.[1,]), collapse = "\t"))

    site_lines2[[j3_1:length(site_lines2)]] <- soils_lines
  }

  site_lines2 <- unlist(site_lines2)

  return(site_lines2)

}  # end function
# #
# new_site <- shaw_modify_sit(model_dir = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/test_shaw",
#                             lat_deg = 65)



