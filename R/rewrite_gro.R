#' Rewrites the SHAW gro file based on snow melt timing
#'
#' Takes output frost.txt, cleans, and calculates SDD for each water year.
#' Given a model with sdd as input, calculates first day of leaf out.
#'
#' This function allows you to build SHAW files based on desired parameters.
#' @param model_dir This is the directory where all inputs will be written.
#' @param frost File with snow outputs
#' @param model Some kind of model object wit h SDD and lai_start
#' @param gro_file Name of gro file
#' @param on_duration Period of time over which LAI ramps up from 0 to max, in days
#' @param write_file Only writes file (to same location) if set to true.

#' @keywords hydrology, SHAW.
#' @export
#' @examples
#' observed <- data.frame(depth = c(0.1, 0.5, 1), VWC = c(0.4, 0.2, 0.1))

rewrite_gro <- function(model_dir,
                        frost = paste0(model_dir, "/out/frost.txt"),
                        gro_file = paste0(model_dir, "/", basename(model_dir), ".gro"),
                        model = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/hourly/calibration_data/phenology_gam.rds",
                        on_duration = 30,
                        write_file = F
                        ){

  # Get snow data, phenology model, and current .gro file.
  snow_data <- data.table::fread(frost, fill = T, skip = 5)
  phen_mod <- readRDS(model)

  #just get the day end snow pack variables (snow and swe)
  snow_data <- data.frame(snow_data)
  snow <- snow_data[snow_data[,2] == 24,]
  snow <- snow[,c(1,3,6,7)]
  names(snow) <- c("day", "year", "snow", "swe")
  snow$date <- as.Date(strptime(paste(snow$year, snow$day), format="%Y %j"))

  snow_df <- data.frame(date = seq(snow$date[1], snow$date[nrow(snow)], by="days"))
  snow <- full_join(snow_df, snow, by="date")
  snow$day <- yday(snow$date)
  snow$year <- year(snow$date)
  snow$snow[is.na(snow$snow)] <- 0
  snow$swe[is.na(snow$swe)] <- 0
  snow <- mutate(snow, wy = ifelse(month(date)>=10,year+1,year))
  snow <- mutate(snow, dowy = ifelse(wy!=year, day-273, day + 92) ) %>%
    filter(!is.na(wy))

  #Identify date of snowmelt.
  summary <- data.frame(wy = c(snow$wy[1]:snow$wy[nrow(snow)]))
  for (j in 1:nrow(summary)){ # loop through water years.
    temp <- filter(snow, wy == j + .$wy[1] - 1)
    doms <- temp$dowy[temp$swe == max(temp$swe)]
    zeros <- which(temp$swe == 0)
    zeros <- zeros[zeros > doms[1]]
    summary$sdd[j] <- temp$day[zeros[1]]
  }

  # Make date of lai onset based on summary.
  summary <- mutate(summary,
                    lai_start = round(mgcv::predict.gam(phen_mod, newdata = summary, type = "response")))
  summary <- mutate(summary, mutate(maxlai = lai_start + 30))

  # Fill in if necessary.
  summary <- tidyr::fill(summary, sdd, maxlai, lai_start, .direction = "downup")

    #Rewrite gro file based on snowmelt data. Leaf-on should start at snowmelt, reach max after 30 days.
    gro <- read.table(gro_file, header = F)
    names(gro) <- c("day", "year", "height", "dchar", "clumping", "weight", "lai", "rtdepth")
    gro <- mutate(gro, wy = ifelse(day > 273, year + 1, year))

    # Replace with new dates based on snowmelt.
    gro_new <- filter(gro, day != 1) %>% split(.$year)
    # gro_new <- data.table::split(gro_new, year)
    gro_new <- lapply(gro_new, FUN = function(df){
      lai_start1 <- summary$lai_start[summary$wy == df$year[1]][1]
      lai_max1 <- lai_start1 + 30

      if(!is.na(lai_start1) && length(lai_start1) == 1){
        df$day[1] <- lai_start1
      } else df$day[1] <- 130 # mean from observational record.

      if(!is.na(lai_max1) && length(lai_max1) == 1){
        df$day[2] <- lai_max1
      } else df$day[2] <- 160 # mean from observational record.
      return(df)
    })
    gro_new <- bind_rows(gro_new)


    gro_new <- bind_rows(gro[1,], gro_new) # include starting day.

    gro_new <- dplyr::select(gro_new, -wy)

    # Add a row for the last day of the year.
    gro_new <- dplyr::bind_rows(gro_new, gro_new[nrow(gro_new),])
    gro_new$day[nrow(gro_new)] <- 365

    if(write_file == T){
      write.table(gro_new, file = gro_file,
                  col.names = FALSE, row.names = FALSE,
                  sep = "\t")
    }

}
