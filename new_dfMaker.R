dfMaker <- function(input.folder, config.path) {
  # Load the required library
  library(arrow)
  
  # Initialize variables to store metadata and final data
  all_data <- list()
  default_config <- list(
    extract_datetime = FALSE,
    extract_time = FALSE,
    extract_exp_search = FALSE,
    extract_country_code = FALSE,
    extract_network_code = FALSE,
    extract_program_name = FALSE,
    extract_time_range = FALSE,
    timezone = "America/Los_Angeles"
  )
  
  # Check if config path is provided and read the configuration; use default if not provided
  if (missing(config.path)) {
    config <- default_config
  } else {
    config <- read_json_arrow(config.path, as_data_frame = TRUE)
    config <- as.list(config)
  }
  
  
  # Metadata variables definition
  datetime <- NA
  exp_search <- NA
  country_code <- NA
  network_code <- NA
  program_name <- NA
  time_range <- NA
  
  
  # List all JSON files in the input directory
  files <- list.files(input.folder, pattern = "*.json", full.names = TRUE)
  
  # Loop through each file to process
  for (frame_file in files) {
    # Read the JSON file and extract the keypoints data
    rawData <- read_json_arrow(frame_file, as_data_frame = TRUE)[[2]][[1]][2:5]
    
    # Function to clean numeric values in keypoints data
    trim_zeros <- function(x) {
      sapply(x, function(num) {
        if (num == 0) {
          NA
        } else {
          sub("\\.?0+$", "", format(num))
        }
      })
    }
    
    # Define the expected number of points for each type of keypoints
    check_points <- c(25, 70, 21, 21)
    
    # Metadata extraction based on the configuration
    metadata <- gsub(".*[\\\\/]", "", frame_file)
    frame <- as.numeric(regmatches(metadata, regexec("[0-9]{12}", metadata)))
    
    # Extract additional metadata if enabled in configuration
    
    if (config$extract_datetime) {
      timezone <- ifelse(is.null(config$timezone), default_config$timezone, config$timezone)
      datetime_str <- sub("^(\\d{4}-\\d{2}-\\d{2})_(\\d{4})_.*$", "\\1 \\2", metadata)
      datetime <- as.POSIXct(datetime_str, format = "%Y-%m-%d %H%M", tz = timezone)
    }
    exp_search <- ifelse(config$extract_exp_search, gsub(".*[0-9]_(.*)_\\d{12}_keypoints\\.json$", "\\1", metadata), NA)
    country_code <- ifelse(config$extract_country_code, sub(".*?_(\\w{2})_.*", "\\1", metadata), NA)
    network_code <- ifelse(config$extract_network_code, sub("^.*_\\d{4}_\\w{2}_([^_]+)_.*$", "\\1", metadata), NA)
    program_name <- ifelse(config$extract_program_name, sub("^.*_\\d{4}_\\w{2}_[^_]+_(.*?)_\\d+-\\d+.*$", "\\1", metadata), NA)
    time_range <- ifelse(config$extract_time_range, sub("^.*_(\\d+-\\d+)_.*$", "\\1", metadata), NA)
    # Process keypoints data and compile into data frames
    for (i in 1:nrow(rawData)) {
      for (j in 1:ncol(rawData)) {
        matrix_data <- matrix(unlist(rawData[i, j]), ncol = 3, nrow = check_points[j], byrow = TRUE)
        matrix_data <- apply(matrix_data, 2, trim_zeros)
        matrix_data <- apply(matrix_data, 2, as.numeric)
        
        # Combine individual keypoints data into a data frame with metadata
        frame_data_list <- list(matrix_data = matrix_data,
                                type_point = gsub("_2d", " ", colnames(rawData[j])),
                                people_id = i,
                                point = c(0:(nrow(matrix_data) - 1)),
                                id = metadata, 
                                frame = frame)
        
        # Aggregate dynamic only no NA variables
        if (!is.na(exp_search)) frame_data_list$exp_search <- exp_search
        if (!is.na(datetime)) frame_data_list$datetime <- datetime
        if (!is.na(country_code)) frame_data_list$country_code <- country_code
        if (!is.na(network_code)) frame_data_list$network_code <- network_code
        if (!is.na(program_name)) frame_data_list$program_name <- program_name
        if (!is.na(time_range)) frame_data_list$time_range <- time_range
        df <- data.frame(frame_data_list)
        all_data[[length(all_data) + 1]] <- df
      }
    }
  }
  
  # Combine all the individual frames into one data frame
  final_data <- do.call(rbind, all_data)
  colnames(final_data)[1:3] <- c("x", "y", "c")
  
  return(final_data)
}


data<-dfMaker(input.folder= "dfMaker/dfMakerExample/exampleVideos/2006-01-14_0600_US_KTTV-FOX_Ten_OClock_News_273-275_ID202_back_then/",config.path = "var_config.json")

write.csv(x = data,file = "new_dfMaker_example.csv",row.names = FALSE)
