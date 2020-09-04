#' Function to build SHAW .inp file.
#'
#' This function allows you to build SHAW files based on desired parameters.
#' @param model_dir This is the directory where all inputs will be written.
#' @param inp_file Name of .inp file. Defaults to basename of the model directory.
#' @param iversion Version of SHAW.Valid versions include Shaw2.3 through Shaw3.0.
#' @param mtstep Time step: 0 = hourly, 1 = daily, 2 = same intervals as NRPDT. Defaults to 0.
#' @param outputs Vector with names of desired outputs: options are "profile", "soil temperature", "soil water",
#' "soil liquid", "soil matric", "canopy temperature", "canopy humidity", "snow temperature", "energy balance", "water balance",
#' "water flow", "root extract", "lateral", "frost", "salt", "soil solution"
#' @param output_freq Frequency in hours for outputs specified in outputs. Same value applied to all.
#' @keywords hydrology, SHAW.
#' @export
#' @examples
#'
# To dos/possibly change:
# 1. Consider removing the input file names - just hard code them?
# 2. Consider allowing for different output frequencies - right now requires they're all the same.
# 3. Split into functions that make input file, site file, etc. separately. Trouble is they're somewhat interdependent?
# 4. Add errors and warnings.

shaw_inp <- function(model_dir,
                     iversion = "Shaw3.0", mtstep = 0, iflagsi = 1, inph20 = 0, mwatrxt = 0,
                     outputs = c("out", "soil temperature", "soil water", "soil liquid", "energy balance", "water balance", "frost"),
                     output_freq = 24,
                     site_file = paste0(basename(model_dir), ".sit"),
                     inp_file = paste0(basename(model_dir), ".inp"),
                     moi_file = paste0(basename(model_dir), ".moi"),
                     temp_file = paste0(basename(model_dir), ".tem"),
                     gro_file = paste0(basename(model_dir), ".gro"),
                     wea_file = paste0(basename(model_dir), ".wea"),
                     sink_file = paste0(basename(model_dir), ".sink")){ # begin function

  # Set up inputs.-------------
  if(!dir.exists(model_dir)){dir.create(model_dir)}
  out_dir <- paste0(model_dir, "/out")
  if(!dir.exists(out_dir)){dir.create(out_dir)}

  # Write input file.------------
  inp <- character()
  inp[1] <- iversion # line A
  inp[2] <- paste(mtstep, iflagsi, inph20, mwatrxt, sep = " ") # line B
  inp[3] <- site_file # line B-1
  inp[4] <- wea_file # Line B-2
  inp[5] <- moi_file # Line B-3
  inp[6] <- temp_file # Line B-4
  if(mwatrxt == 1){inp[7] <- sink_file} # line B-5

  # Set up information about output files.
  x <- length(inp)
  possible_outputs <- c("out", "soil temperature", "soil water", "soil liquid", "soil matric",
                        "canopy temperature", "canopy humidity", "snow temperature", "energy balance", "water balance",
                        "water flow", "root extract", "lateral", "frost", "salt", "soil solution", "profile", "extra1", "canwnd")
  desired_outputs <- which(possible_outputs %in% outputs)
  line_c <- rep(0, length(possible_outputs))
  line_c[desired_outputs] <- output_freq
  # line c also gets four 0s at the end for unused parameters.
  line_c <- c(line_c, rep(0, 4))
  inp[x + 1] <- paste(line_c, collapse = " ")

  # Make names of output files:
  out_files <- gsub(pattern = " ", replacement = "", x = possible_outputs)
  out_files <- paste0("out/", out_files, ".txt")

  inp[(x+2):(x+length(out_files) + 1)] <- out_files

  writeLines(inp, paste0(model_dir, "/", inp_file))

  return(inp)

}  # end function


# Testing:

inputs <- shaw_inp(model_dir = "/Volumes/research_storage2/arctic/arctic_point_modeling/SHAW/test_shaw")

