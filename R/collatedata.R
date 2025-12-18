#' @title CardiacDP - collatedata()
#' @description Automatically read and collate separate .csv files in chronological order as inferred by the file names and in hierarchy.
#' @param file_path Designate the path to your file, must be a .zip file
#' @return A single collated data table
#' @export collatedata
#' @examples \dontrun{
#' collatedata(file_path = "path/to/your/file.zip")
#' }
collatedata <- function(file_path) {
    ### DATA COLLATION
    if (!file.exists(file_path)) {
        stop("File does not exist at the given path.")
    }
    if (!stringr::str_detect(file_path, "\\.zip$")) {
        stop("File is not a zip file.")
    }

    # get the name of the zipped folder
    dname <- basename(tools::file_path_sans_ext(file_path))
    unzip(file_path)

    print(paste("Zip file name:", dname))
    print(paste("Files in root directory:", paste(list.files("./"), collapse = ", ")))

    # Check if CSV files exist in root directory
    csv_files <- list.files("./", pattern = "\\.csv$")
    if (length(csv_files) > 0) {
        # If CSV files are in root, create a directory and move them there
        dir.create(dname, showWarnings = FALSE)
        file.copy(csv_files, paste0("./", dname, "/"))
        file.remove(csv_files)
    }

    print(paste("Files in extracted directory:", paste(list.files(paste0("./", dname)), collapse = ", ")))

    # names of directories
    folds <- list.dirs(paste0("./", dname), full.names = FALSE)[-1]
    print(paste("Subdirectories found:", paste(folds, collapse = ", ")))

    if (length(folds) == 0) {
        # If no subdirectories found, use the main directory
        print("No subdirectories found, checking root directory")
        folds <- "." # Use current directory instead of dname
        root_files <- list.files(paste0("./", dname))
        print(paste("Files in root:", paste(root_files, collapse = ", ")))
        if (length(root_files) == 0) {
            stop("No files found in the zip archive")
        }
    }

    # Sort directories numerically
    folds <- stringr::str_sort(folds, numeric = TRUE)
    print(paste("Final folders to process:", paste(folds, collapse = ", ")))
    # number of directories
    nfold <- length(folds)
    data_str <- data.table::data.table(
        folders = folds,
        pages = sapply(
            as.list(paste0("./", dname, "/", folds)),
            function(x) {
                files <- list.files(x, full.names = FALSE)
                # Filter out generated output files
                files <- files[!grepl("^Channel_.*\\.(csv|png)$", files)]
                length(files)
            }
        )
    )
    # Initialize ch_names as NULL before the loop
    ch_names <- NULL
    temp <- NULL
    file_format <- NULL

    # Get list of files in first directory, excluding generated output files
    first_dir_files <- list.files(paste0("./", dname, "/", folds[1]), full.names = TRUE)
    # Filter out generated CSV files (those starting with "Channel_" or ending with specific patterns)
    first_dir_files <- first_dir_files[!grepl("^.*/Channel_.*\\.(csv|png)$", first_dir_files)]
    if (length(first_dir_files) == 0) {
        stop("No files found in directory: ", paste0("./", dname, "/", folds[1]))
    }

    print(paste("Attempting to read files from:", paste0("./", dname, "/", folds[1])))
    print(paste("Number of files found:", length(first_dir_files)))

    for (tempn in first_dir_files) {
        print(paste("Trying to read file:", tempn))
        # Try reading with both formats
        temp_result <- tryCatch(
            {
                # Try first format (semicolon/comma separated with multiple header rows)
                print("Attempting semicolon/comma format...")
                # First read just the headers with Windows-1252 encoding
                header_row <- suppressWarnings(readLines(tempn, n = 1L, encoding = "Windows-1252"))
                print(paste("Header row:", header_row))
                # Then read data skipping the headers
                # Read file content as text with Windows-1252 encoding
                file_text <- paste(readLines(tempn, encoding = "Windows-1252"), collapse = "\n")
                temp_data <- suppressWarnings(data.table::fread(text = file_text, skip = 1L))
                if (nrow(temp_data) > 0 && ncol(temp_data) > 0) {
                    # Parse column names from header line
                    ch_names <- unlist(strsplit(header_row, "[,;]"))
                    ch_names <- trimws(ch_names)
                    print(paste("Parsed column names:", paste(ch_names, collapse = ", ")))

                    # Translate Chinese column names to English
                    ch_names <- gsub("^\u65f6\u95f4$", "Time", ch_names) # Chinese "Time" -> Time
                    ch_names <- gsub("^\u901a\u9053\\s*([A-Z])$", "Channel \\1", ch_names) # Chinese "Channel A" -> Channel A
                    ch_names <- gsub("^\u901a\u9053([A-Z])$", "Channel \\1", ch_names) # Chinese "ChannelA" -> Channel A
                    print(paste("Translated column names:", paste(ch_names, collapse = ", ")))

                    print(paste("Number of columns in data:", ncol(temp_data)))
                    # Check if column count matches
                    if (length(ch_names) == ncol(temp_data) && any(ch_names == "Time")) {
                        list(data = temp_data, names = ch_names, format = "semicolon")
                    } else {
                        NULL
                    }
                } else {
                    NULL
                }
            },
            error = function(e) {
                # Try second format (space separated)
                print("Attempting space format...")
                # Read file content as text with Windows-1252 encoding
                file_text <- paste(readLines(tempn, encoding = "Windows-1252"), collapse = "\n")
                temp_data <- suppressWarnings(data.table::fread(text = file_text))
                if (nrow(temp_data) > 0 && ncol(temp_data) > 0) {
                    # Extract column names without units
                    ch_names <- colnames(temp_data)
                    ch_names <- gsub("\\s*\\([^\\)]+\\)", "", ch_names)
                    print(paste("Column names from space format:", paste(ch_names, collapse = ", ")))

                    # Translate Chinese column names to English
                    ch_names <- gsub("^\u65f6\u95f4$", "Time", ch_names) # Chinese "Time" -> Time
                    ch_names <- gsub("^\u901a\u9053\\s*([A-Z])$", "Channel \\1", ch_names) # Chinese "Channel A" -> Channel A
                    ch_names <- gsub("^\u901a\u9053([A-Z])$", "Channel \\1", ch_names) # Chinese "ChannelA" -> Channel A
                    print(paste("Translated column names:", paste(ch_names, collapse = ", ")))

                    # Check if Time column exists
                    if (any(ch_names == "Time")) {
                        list(data = temp_data, names = ch_names, format = "space")
                    } else {
                        NULL
                    }
                } else {
                    NULL
                }
            }
        )

        if (!is.null(temp_result)) {
            ch_names <- temp_result$names
            file_format <- temp_result$format
            temp <- temp_result$data
            colnames(temp) <- ch_names
            print(paste("Successfully read file with format:", file_format))
            rm(tempn, temp_result)
            break
        } else {
            print("Failed to read file with either format")
        }
    }

    # Check if we successfully got the column names
    if (is.null(ch_names)) {
        stop("Could not read column names from any file in the directory. Please check file format and contents.")
    }

    ch_selected <- ch_names[-which(ch_names == "Time")]

    # print data structure for data preview
    print(list(
        paste0("Zipped file name: ", dname),
        paste0("Number of files: ", nfold),
        data_str,
        paste0("Number of channels: ", length(ch_selected)),
        paste0("Names of channels: ", paste(ch_selected, collapse = ", "))
    ))

    ## read data
    raw_res <- as.numeric(temp[2, Time])

    rawmaster <- list() # initialize master data.table
    for (f in 1:nfold) {
        ## for each directory
        # Get all files and filter out generated output files
        dir_files <- list.files(paste0("./", dname, "/", folds[f]), full.names = TRUE)
        dir_files <- dir_files[!grepl("^.*/Channel_.*\\.(csv|png)$", dir_files)]

        rawmaster <- c(rawmaster, lapply(
            as.list(seq_along(dir_files)),
            function(p) {
                tryCatch(
                    {
                        # Read file content as text with Windows-1252 encoding
                        file_text <- paste(readLines(dir_files[p], encoding = "Windows-1252"), collapse = "\n")
                        if (file_format == "semicolon") {
                            data <- data.table::fread(
                                text = file_text,
                                skip = 1L
                            )
                            colnames(data) <- ch_names
                        } else {
                            data <- data.table::fread(
                                text = file_text
                            )
                            colnames(data) <- ch_names
                        }
                        return(data)
                    },
                    warning = function(w) {
                        message(paste("Empty csv file was detected: ", folds[f],
                            sprintf("_%02.0f.csv", p),
                            sep = ""
                        ))
                        return(data.table::data.table(
                            matrix(0, ncol = ncol(temp), nrow = nrow(temp))
                        ))
                    }
                )
            }
        ))
        print(sprintf("Reading data: %.0f%%", f / nfold * 100))
    }

    print("Finalizing...")

    # bind data tables into one master data table
    rawmaster <- data.table::rbindlist(rawmaster)
    # Only assign column names if the count matches
    if (ncol(rawmaster) == length(ch_names)) {
        names(rawmaster) <- ch_names # assign column names
    } else {
        stop(sprintf(
            "Column count mismatch: expected %d columns, but got %d columns after rbindlist.
                     This might indicate inconsistent file formats or column counts in the input files.",
            length(ch_names), ncol(rawmaster)
        ))
    }
    rawmaster <- rawmaster[which(!is.na(rawmaster[, Time])), ] # remove extra rows

    # convert data into numeric class
    suppressWarnings(
        rawmaster[, colnames(rawmaster) := lapply(.SD, as.numeric)]
    )

    # re-assign time
    rawmaster[, Time := seq(0, by = raw_res, length.out = nrow(rawmaster))]

    # Print actual total duration based on final time value
    print(paste0(
        "Total duration: ",
        round(rawmaster[nrow(rawmaster), Time] / 60, digits = 1),
        " mins"
    ))

    # remove extracted files from the working directory
    unlink(paste0("./", dname), recursive = TRUE)
    ## end of read data

    # preview the data
    utils::str(rawmaster)

    # Save rawmaster to a CSV file
    data.table::fwrite(rawmaster, file = paste0(dirname(file_path), "/", dname, ".csv"))
    print(paste0("Collated data table saved as ", dname, ".csv"))

    return(rawmaster)
}
### END OF DATA COLLATION ###
