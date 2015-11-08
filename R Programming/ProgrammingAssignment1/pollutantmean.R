pollutantmean <- function(directory, pollutant, id = 1:332) {
    monitor_files <- 
        dir(path = paste(getwd(), "/", directory, sep = ""))
    monitor_files = monitor_files[id]
    no_files = length(monitor_files)
        
    if (pollutant == "sulfate" || pollutant == "nitrate"){
    for (i in 1:no_files){
        file_name <- paste(getwd(), "/", directory, "/", monitor_files[i], sep = "")
        pollution_data <- read.csv(file_name)
            if(i == 1) 
                data <- pollution_data
            else
                data = rbind(data, pollution_data)
    }
    mean(data[,pollutant], na.rm = T)
    }
}