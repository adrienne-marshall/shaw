#' Function to build SHAW input files based on parameters
#'
#' This function allows you to build SHAW files based on desired parameters.
#' @import dplyr
#' @import utils

#' @param model_dir This is the directory where all inputs will be written.
#' @param mode "create" or "modify" to create a new site file or modify an existing one.
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
# 1. Consider removing the input file names - just hard code them?
# 2. Consider allowing for different output frequencies - right now requires they're all the same.
# 4. Add errors and warnings.

shaw_sit <- function(model_dir, mode = "create", start, start_hour, end, lat_deg, lat_min, slope, aspect, elev,
                     soils_df, nres, roughness, measurement_height, ponding, itype, pintrcp, xangle,
                     canalb, tcrit, min_stomatal_resistance, stomatal_exp, critical_leaf_water, leaf_resist, root_resist,
                     plant_height, dchar, clumping, biomass, LAI, rooting,
                     residue_thickness, residue_weight, residue_fraction,
                     residue_albedo, lower_bc_temp, nsalt = 0, iwrc = 3,
                     ivlcbc = 1, itmpbc = 1, dry_soil_albedo = 0.25, soil_albedo_exponent = 1,
                     residue_coefficient = 50000, residue_krb = 6,
                     isnotemp = 1, snotemp = 0, snow_roughness = 0.15,
                     Kst = 100, Tlower = -999, Tupper = 999, Topt = 20, Kvpd =0.5, r = 0.5,
                     mcanflg = 0, istomate = 2, canma = -53.72, canmb = 1.32, wcandt = 0, hrnoon = 12,
                     nplants = 1, nsp = 0, nsolutes = 0, error_tol = 0.01, time_step = 1,
                     nrchang = 0, gmcdt = 0,
                     debugging_seq = c(0, 0, 0, 0, 0, 0),
                     site_file = paste0(basename(model_dir), ".sit"),
                     gro_file = paste0(basename(model_dir), ".gro"),
                     run_name = basename(model_dir)
){ # begin function

  # Set up inputs.-------------
  if(!dir.exists(model_dir)){dir.create(model_dir)}
  out_dir <- paste0(model_dir, "/out")
  if(!dir.exists(out_dir)){dir.create(out_dir)}

  # Make some warnings for cases we haven't made yet. -------------

  if(nsp != 0){
    stop("NSP is required to be 0; greater values have not been implemented at this time.
                    Suggest starting in summer so NSP can reasonably be 0 for now.")
  }

  if(mcanflg > 1){
    stop("This function is not yet set up to deal with MCANFLG values > 1.")
  }

  if(nsalt > 0){
    stop("This function isn't yet set up to deal with solutes.")
  }

  if(mode == "create"){

  # Write site file.---------------
  site <- character()
  site[1] <- run_name # Line A.
  start <- lubridate::ymd(start)
  end <- lubridate::ymd(start)
  site[2] <- paste(lubridate::yday(start), start_hour, lubridate::year(start),
                   lubridate::yday(end), lubridate::year(end)) # Line B

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

  writeLines(site, paste0(model_dir, "/", basename(model_dir), ".sit"))

  }

  # If mode is modify:
  if(mode == "modify"){
    stop("I haven't implemented the code yet to modify existing site files. Nothing is being done.")
  }

  return(site)

}  # end function


# Testing:

soils_df <- data.frame(depth = seq(0, 2, by = 0.1),
                       sand = 30,
                       silt = 60,
                       clay = 10,
                       rock = 0,
                       organic = 5,
                       bulk_density = 1200,
                       ksat = 50,
                       klat = 0,
                       air_entry = 0,
                       theta_sat = 0.4,
                       n = 1.4,
                       theta_res = 0.05,
                       l = 0.5,
                       alpha = 1.5)

site_file <- shaw_sit(model_dir = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/test_shaw",
                      start = "2000-01-01", start_hour = 0, end = "2000-12-31",
                      lat_deg = 60, lat_min = 0, slope = 10, aspect = 0, hrnoon = 12, elev = 300,
                      soils_df = soils_df, nres = 5, roughness = 1, measurement_height = 10, ponding = 4,
                      itype = 1, pintrcp = 0.5, xangle = 1,
                      canalb = 0.2, tcrit = 0, min_stomatal_resistance = 150, stomatal_exp = 5, critical_leaf_water = -70,
                      leaf_resist = 10^5, root_resist = 2*10^5,
                      plant_height = 6, dchar = 1, clumping = 0.5, biomass = 4, LAI = 3, rooting = 0.5,
                      residue_thickness = 5, residue_weight = 6000, residue_fraction = 0.8, residue_albedo = 0.25,
                      iwrc = 3, ivlcbc = 1, itmpbc = 1, lower_bc_temp = 1, nsalt = 0)

