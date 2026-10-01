assess_high_tide_timing <- function(calibrated_dir, deployments,
                                    official_high_tides, min_depth = 0.1,
                                    max_offset = 3600) {

   files <- list.files(calibrated_dir,
                       pattern = logger_data_file_pattern,
                       full.names = TRUE)
   files <- gsub("\\\\", "/", files)

   if(length(files) == 0)
      stop("No calibratd logger files identified")

   tz <- if(inherits(official_high_tides, "POSIXct")) {
      lubridate::tz(official_high_tides)
   } else {
      "UTC"
   }
   if(tz == "") tz <- "UTC"

   consensus <- sort(unique(as.numeric(official_high_tides)))

   serials <- gsub("^([[:digit:]]*).*$", "\\1", basename(files))

   events_list <- vector("list", length(files))
   problems <- data.frame(serial = character(0), error = character(0))

   for (i in seq_along(files)) {
      file <- files[[i]]
      serial_number <- serials[[i]]

      peaks <- tryCatch(find_high_tides(file, deployments, min_depth = min_depth),
                        error = function(e) e)

      if(inherits(peaks, "error")) {
         problems <- rbind(problems, data.frame(serial = serial_number,
                                                error = peaks$message))
         peaks <- numeric(0)
      }

      peaks <- as.numeric(peaks)

      if(length(peaks) == 0) {
         nearest <- rep(NA_real_, length(consensus))
      } else {
         nearest <- vapply(consensus, function(ht) {
            peaks[which.min(abs(peaks - ht))]
         }, numeric(1))
      }

      offset_sec <- nearest - consensus
      keep <- !is.na(offset_sec) & abs(offset_sec) <= max_offset

      events_list[[i]] <- tibble(
         serial = rep(serial_number, sum(keep)),
         consensus_high_tide = as.POSIXct(consensus[keep], origin = "1970-01-01",
                                          tz = tz),
         logger_high_tide = as.POSIXct(nearest[keep], origin = "1970-01-01",
                                       tz = tz),
         offset_minutes = offset_sec[keep] / 60
      )
   }

   events <- bind_rows(events_list)

   summary <- events |>
      group_by(serial) |>
      summarize(
         mean_offset_minutes = mean(offset_minutes, na.rm = TRUE),
         sd_offset_minutes = sd(offset_minutes, na.rm = TRUE),
         n_tides = n()
      )

   summary <- tibble(serial = serials) |>
      left_join(summary, by = "serial") |>
      mutate(n_tides = ifelse(is.na(n_tides), 0L, n_tides))

   if(nrow(problems) > 0) {
      message("❌ ", nrow(problems), " errors finding high tides")
      cat("Errors:\n")
      print(problems)
   }

   return(list(
      summary = summary,
      events = events,
      problems = problems
   ))
}
