# Testing:
model_dir <- "/Volumes/research_storage2/arctic/arctic_point_modeling"
existing_site_file <- paste0(model_dir, "/SHAW/test_shaw/test_shaw.sit")


#' Function to build SHAW input files based on parameters
#'
#' This function allows you to build SHAW files based on desired parameters.
#' Most parameters default to NULL, and nothing in the site file changes. If you change any of the parameters from NULL, the parameter will change in the new site file.
#' @import dplyr
#' @import utils

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

shaw_modify_sit <- function(model_dir,
                            existing_site_file = paste0(model_dir, "/", basename(model_dir), ".sit"),
                            start = NULL, start_hour = NULL, end = NULL, lat_deg = NULL, lat_min = NULL, slope = NULL, aspect = NULL, elev = NULL,
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


  # read site file
  site <- readLines(existing_site_file)
  site_lines <- lapply(site, strsplit, split = " |\t")
  site_lines <- lapply(site_lines, unlist)

  # Get relevant flags that determine locations of parameters:
  nplants <- as.numeric(site_lines[[4]][1]) # number of plants
  nr <- as.numeric(site_lines[[4]][3]) # number of residue nodes.
  ns <- as.numeric(site_lines[[4]][4]) # number of soil nodes.
  mcanflg <- as.numeric(site_lines[[6]][1]) # growth type
  istomate <- as.numeric(site_lines[[6]][2]) # stomatal conductance flag
  max_line <- length(site) # max number of lines
  # to dos: ivlcbc, itmpbc

  if(nplants > 0){
    f_1s <- 7:(6+nplants) # F-1 series of lines.
    if(istomate == 2){fas <- (max(f_1s) + 1):(max(f_1s) + nplants)} # Fa series
    if(mcanflg == 0){
      f0s <- (max(fas) + 1):(max(fas) + nplants) # F0 series
      g <- max(f0s) + 1 # line G, snow
    } else if(mcanflg == 1){
      f1s <- (max(f_1s) + 1):(max(f_1s) + nplants)
      g <- max(f1s) + 1
      }
  } else if (nplants == 0){g <- 6}

  # G1 series not included for multiple snow nodes.

  if(nr > 0){
    h <- g + 1 # basic residue info
    h1 <- h + 1 # more complicated residue info. # solutes would go next if included.
    j1 <- h1 + 1 # start soils.
    } else if (nr == 0){j1 <- g + 1}# end residue.


  ivlcbc <- as.numeric(site_lines[[j1]][1]) # water flow at lower boundary flag
  itmpbc <- as.numeric(site_lines[[j1]][2]) # temperature lower boundary flag.

  if(itmpbc == 1){
    j2 <- j1 + 1 # lower temperature boundary condition.
    j3_1 <- j2 + 1 # j3_1 is first line of soils.
  } else {j3_1 <- j1 + 1}


    # Make some warnings for cases we haven't made yet. -------------
  if(nsp != 0 & !is.null(nsp)){
    stop("NSP is required to be 0; greater values have not been implemented at this time.
                    Suggest starting in summer so NSP can reasonably be 0 for now.")
  }

  if(mcanflg > 1){
    stop("This function is not yet set up to deal with MCANFLG values > 1.")
  }

  if(nsalt > 0){
    stop("This function isn't yet set up to deal with solutes.")
  }

  if(nrchang == 1){
    stop("This function isn't set up to deal with changing residue layers.")
  }

  # Write site file.---------------
  if(!is.null(run_name)){site[1] <- run_name}  # Line A.

  # Line B
  if(!is.null(start)){
    start <- lubridate::ymd(start)
    site_lines[[2]][1:3] <- paste(lubridate::yday(start), start_hour, lubridate::year(start))
  }
  if(!is.null(end)){
    end <- lubridate::ymd(start)
    site_lines[[2]][4:5] <- paste(lubridate::yday(end), lubridate::year(end))
  }

  # For each parameter, extract the value from the site file if not given.
  # Then paste everything together.
  # Possible approach: include data file with line number and position and do this in a loop.
  if(is.null(lat_deg)){lat_deg <- site_lines[[3]][1]}
  if(is.null(lat_min)){lat_min <- site_lines[[3]][2]}
  if(is.null(slope)){slope <- site_lines[[3]][3]}
  if(is.null(aspect)){aspect <- site_lines[[3]][4]}
  if(is.null(hrnoon)){hrnoon <- site_lines[[3]][5]}
  if(is.null(elev)){elev <- site_lines[[3]][6]}

  # Alternative approach:
  par_info <- read_csv("data/SHAW_parameters.csv")

  fun_replace <- function(var){
    if()
  }

  site[3] <- paste(lat_deg, lat_min, slope, aspect, hrnoon, elev) # Line C

  site[4] <- paste(nplants, nsp, nres, nrow(soils_df), nsolutes, error_tol, time_step,
                   paste(debugging_seq, collapse = " "),
                   collapse = " ") # Line D.

  site[5] <- paste(roughness, measurement_height, ponding, sep = " ") # Line E

  site[6] <- paste(mcanflg, istomate, canma, canmb, wcandt) # Line F.

  # Define line numbers for F-1 to F1-NPlants.
  f1_lines <- 7:(7 + nplants-1)
  for(i in 1:length(f1_lines)){
    site[f1_lines[i]] <- paste(itype[i], pintrcp[i], xangle[i], canalb[i], tcrit[i],
                               min_stomatal_resistance[i], stomatal_exp[i], critical_leaf_water[i], leaf_resist[i], root_resist[i])
  }

  # Fa series of lines: (Stewart-Jarvis functions.)
  max_line <- length(site) # determine how many lines we have so far.
  if(istomate == 2){
    fa_lines <- (max_line + 1):(max_line + nplants)
    for(i in 1:fa_lines){
      site[fa_lines[i]] <- paste(Kst[i], Tlower[i], Tupper[i], Topt[i], Kvpd[i], r[i], 0, 0, sep = " ")
    } # end looping through fa_lines
  } # end if Istomate = 2. If Istomate = 1, nothing to do here.

  # F0 series of lines -- info if MCANFLG = 0, no growth.
  max_line <- length(site)
  if(mcanflg == 0){
    f0_lines <- (max_line + 1):(max_line + nplants)
    for(i in 1:f0_lines){
      site[f0_lines[i]] <- paste(plant_height[i], dchar[i], clumping[i], biomass[i], LAI[i], rooting[i])
    } # end looping through f0 lines
  } # end if mcanflg = 0

  # F1 series if mcanflg = 1
  max_line <- length(site)
  if(mcanflg == 1){
    f1_lines <- (max_line + 1):(max_line + nplants)
    for(i in 1:f1_lines){
      site[f1_lines[i]] <- gro_file[i]
    }
  }

  # Line G. (snow)
  site[length(site) + 1] <- paste(isnotemp, snotemp, snow_roughness, 0, collapse = " ")

  # Line H. (residue setup info info)
  if(nres != 0){
    site[length(site) + 1] <- paste(nrchang, gmcdt, collapse = " ")
  }

  # Line H1 (residue details)
  if(nres != 0){
    site[length(site) + 1] <- paste(residue_thickness, residue_weight, residue_fraction,
                                    residue_albedo, residue_coefficient, residue_krb)
  }

  # Line J1 (setup information about soils.)
  site[length(site) + 1] <- paste(ivlcbc, itmpbc, dry_soil_albedo, soil_albedo_exponent, iwrc, 0, collapse = " ")

  # Line J2 (lower bounary condition soil temperature)
  if(itmpbc == 1){
    site[length(site) + 1] <- lower_bc_temp # TSAVG in SHAW.
  }

  # Soils DF:
  if(iwrc == 1){ # Campbell equation.
    soils_df <- soils_df[,c("depth", "sand", "silt", "clay", "rock", "organic", "bulk_density", "ksat", "klat", "air_entry",
                            "theta_sat", "b", "asalt", "dispersion_coef")]
  }
  if(iwrc == 2){ # Brooks-Corey
    soils_df <- soils_df[,c("depth", "sand", "silt", "clay", "rock", "organic", "bulk_density", "ksat", "klat", "air_entry",
                            "theta_sat", "brooks_corey_lambda", "theta_res", "l", "asalt", "dispersion_coefficient")]
  }
  if(iwrc == 3){ # van Genuchten
    soils_df <- soils_df[,c("depth", "sand", "silt", "clay", "rock", "organic", "bulk_density", "ksat", "klat", "air_entry",
                            "theta_sat", "n", "theta_res", "l", "alpha")]
  }

  soils_lines <- apply(soils_df, MARGIN = 1, FUN = paste, collapse = "\t") # make soils lines.
  site[(length(site) + 1):(length(site) + length(soils_lines))] <- soils_lines

  writeLines(site, site_file)


  return(site)

}  # end function

# Notes on line numbers:
# 1. Line A (run timing)
# 2. Line B (start/end)
# 3. Line C (lat, slope aspect.)
# 4. Line D - NPLANT, NR, NS, etc. Right now, we're not including snow and changeable residue nodes.
# But NPLANT and NSOIL affect the run.
# 5. Line E (roughness, measurement height)
# 6. Line F (MCANFLG, ISTOMATE, coefficients)
# 7-7 + NPlant



