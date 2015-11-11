complete <- function(directory, id = 1:332) {
  monitor_files <- 
    dir(path = file.path(getwd(), directory))
  monitor_files = monitor_files[id]
  no_files = length(monitor_files)
  result <- data.frame(id = 1:no_files, nobs = 1:no_files)
  colnames(result) <- c("id", "nobs")
  for (i in 1:no_files){
    file_name <- file.path(getwd(), directory, monitor_files[i])
    pollution_data <- read.csv(file_name)
    good <- complete.cases(pollution_data)
    result[i, 1] = as.integer(pollution_data[1,4])
    result[i, 2] = as.integer(nrow(pollution_data[good, ]))
  }
  result
}
