corr <- function(directory, threashold = 0) {
    if(threashold == 0) threashold = 1
    comp <- complete(directory)
    comp_use <- subset(comp, nobs >= threashold)
    monitor_files <- 
        dir(path = file.path(getwd(), directory))
    monitor_files = monitor_files[comp_use[,1]]
    no_files = length(monitor_files)
    if(no_files > 0)
        for (i in 1:no_files){
            file_name <- file.path(getwd(), directory, monitor_files[i])
            pollution_data <- read.csv(file_name)
            data_sulfate = subset(pollution_data, select = sulfate)
            data_nitrate = subset(pollution_data, select = nitrate)
            c <- cor(data_sulfate, data_nitrate, use = "complete.obs")
            if(i == 1)
                result <- c
            else
                result = append(result, c)
        }
    else
        result <- vector("numeric", length = 0)
    result = result
}