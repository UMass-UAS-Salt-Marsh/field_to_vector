logger_data_file_pattern <- "[[:digit:]]*_cal.xlsx$"


#' assess_water_logger_errors
#'
#' Processes a folder of calibrated water logger CSV files, 
#' calculating water surface elevation, sd, and mean
#' near high tide using a supplied deployment metadata file, 
#' common high tide times.
#'
#' @param calibrated_dir Character. Path to folder containing calibrated logger files
#' @param deployment_file Character. Path to the deployment metadata CSV file
#' @param min_depth Numeric. Minimum valid depth (meters) (default = 0.1)
#' @param min_count Minimum number of observations above `min_depth` required
#' to include in calibration.
#' @return A data frame combining water surface elevation and logger stats for all valid files.
#' @export
assess_water_logger_errors <- function(calibrated_dir, deployments, 
                                       common_high_tides, min_depth  = 0.1) {
  
   
   # List files
   files <- list.files(calibrated_dir, 
                       pattern = logger_data_file_pattern, 
                       full.names = TRUE)
   files <- gsub("\\\\", "/", files)
   
   serials <- gsub("^.*/([[:digit:]]*)[^/]*$", "\\1", files, perl = TRUE)
   if(any(duplicated(serials))){
      cat("There's more than one file for these serial numbers:\n\t", 
          paste(serials[duplicated(serials)], collapse = "\n\t"), 
          "\n\n")
      
      stop("More than one file per serial number.  Check for duplicated files")
      
   }
   total_files <- length(files)
 
   if(total_files == 0)
      stop("No calibratd logger files identified")
   results_list <- list()
   success_count <- 0
   
   problems <- data.frame(serial = character(0), message = character(0))
   
   for (i in seq_along(files)) {
      file <- files[[i]]
      
      serial_number <- basename(file) |> substring(1, 8)
      
      # message("\n[", i, "/", total_files, "] Processing: ", serial_number)
      
      result <- tryCatch({
         tide_data <- calculate_water_surface_elevation(file, common_high_tides,
                                                        deployments)
         message("→ Rows read: ", nrow(tide_data))
         success_count <- success_count + 1
      }, error = function(e) e )
      
      if(inherits(result, "error")) {
         tide_data <- NULL
         problems <- rbind(problems, data.frame(serial = serial_number, 
                                                error = result$message))
      }
      results_list[[i]] <- tide_data
   }
   
   data <- bind_rows(results_list)
   
   
   
   deep_data <- filter(data, depth > min_depth)
   
   dropped_due_to_depth <- setdiff(data$serial, deep_data$serial)
   
   data <- deep_data
   
   data <- data %>%
      group_by(date_time) %>%
      mutate(
         mean_wse = mean(water_surface_elevation, na.rm = TRUE),
         sd_wse = sd(water_surface_elevation, na.rm = TRUE),
         difference_wse = water_surface_elevation - mean_wse
      ) %>%
      ungroup()
   
   logger_data <- data |> 
      group_by(serial) |>
      summarize(
         mean_wse_diff = mean(difference_wse, na.rm = TRUE),
         sd_wse_diff = sd(difference_wse, na.rm = TRUE),
         n = n()
      ) |>
      mutate(
         ratio = abs(sd_wse_diff / mean_wse_diff)
      )
      
   
   # Add back in loggers where there weren't enough observations
   if(length(dropped_due_to_depth) > 0){
      logger_data <- rbind(logger_data, 
                    data.frame(
                       serial = dropped_due_to_depth,
                       mean_wse_diff = NA,
                       sd_wse_diff = NA,
                       n = 0, 
                       ratio = NA))
   }
   data <- logger_data
   
   # Check for loggers in deployments that weren't processed
   missing_serials <- deployments$serial[!deployments$serial %in% data$serial]
   
   if(length(missing_serials) > 0){
      problems <- rbind(problems,
                        data.frame(serial = missing_serials,
                                   error = "Serial number in deployments file but calibrated file was not processed"))
   }
   
   
   message("✅ Successfully processed ", success_count, " of ", total_files, 
           " files.")
   
   if(nrow(problems) > 0) {
      message("❌ ", nrow(problems), " errors")
      cat("Errors:\n")
      print(problems)
   }
   
   
   
   return(data)
}
