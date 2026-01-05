#' @title CardiacDP - collatedata()
#' @description Automatically read and collate separate .csv files in chronological order as inferred by the file names and in hierarchy.
#' @param file_path Designate the path to your file, must be a .zip file
#' @param output_file Optional path to write the collated data table as a CSV file. May be either a full file path
#'   (e.g. \code{/path/to/out.csv}) or an output directory (e.g. \code{/path/to/outdir}). If a directory (or a path
#'   without a file extension) is provided, a file named \code{<input_stem>_collated.csv} is written inside it.
#'   Default NULL (no file written).
#' @param verbose Logical; if TRUE, emit progress messages. Default FALSE.
#' @return A single collated data table
#' @export collatedata
#' @examples
#' zip_path <- system.file("extdata", "example.zip", package = "CardiacDP")
#' collated <- collatedata(zip_path)
collatedata <- function(file_path, output_file = NULL, verbose = FALSE) {
    ### DATA COLLATION
    if (!file.exists(file_path)) {
        stop("File does not exist at the given path.")
    }
    if (!stringr::str_detect(file_path, "\\.zip$")) {
        stop("File is not a zip file.")
    }

    inform <- function(...) {
        if (isTRUE(verbose)) message(...)
    }

    # get the name of the zipped folder
    dname <- basename(tools::file_path_sans_ext(file_path))

    # extract to a temporary directory (CRAN-safe; avoids getwd() side-effects)
    exdir <- tempfile(pattern = "CardiacDP-")
    dir.create(exdir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(exdir, recursive = TRUE, force = TRUE), add = TRUE)

    utils::unzip(file_path, exdir = exdir)
    inform("Zip file name: ", dname)

    # determine where the extracted data lives
    data_root <- file.path(exdir, dname)
    if (!dir.exists(data_root)) {
        top_dirs <- list.dirs(exdir, recursive = FALSE, full.names = TRUE)
        top_dirs <- top_dirs[top_dirs != exdir]
        if (length(top_dirs) == 1 && dir.exists(top_dirs[1])) {
            data_root <- top_dirs[1]
        } else {
            data_root <- exdir
        }
    }
    inform("Using extracted directory: ", data_root)

    # names of subdirectories (if any)
    sub_entries <- list.files(data_root, full.names = TRUE, recursive = FALSE, include.dirs = TRUE)
    sub_dirs <- sub_entries[file.info(sub_entries)$isdir]
    folds <- basename(sub_dirs)
    inform("Subdirectories found: ", paste(folds, collapse = ", "))

    if (length(folds) == 0) {
        # If no subdirectories found, use the main directory
        inform("No subdirectories found; using extracted root directory")
        folds <- "." # use data_root directly
        root_files <- list.files(data_root)
        inform("Files in root: ", paste(root_files, collapse = ", "))
        if (length(root_files) == 0) {
            stop("No files found in the zip archive")
        }
    }

    # Sort directories numerically
    folds <- stringr::str_sort(folds, numeric = TRUE)
    inform("Final folders to process: ", paste(folds, collapse = ", "))
    # number of directories
    nfold <- length(folds)
    data_str <- data.table::data.table(
        folders = folds,
        pages = sapply(
            as.list(file.path(data_root, folds)),
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
    first_dir_files <- list.files(file.path(data_root, folds[1]), full.names = TRUE)
    # Filter out generated CSV files (those starting with "Channel_" or ending with specific patterns)
    first_dir_files <- first_dir_files[!grepl("^.*/Channel_.*\\.(csv|png)$", first_dir_files)]
    if (length(first_dir_files) == 0) {
        stop("No files found in directory: ", file.path(data_root, folds[1]))
    }

    inform("Attempting to read files from: ", file.path(data_root, folds[1]))
    inform("Number of files found: ", length(first_dir_files))

    for (tempn in first_dir_files) {
        inform("Trying to read file: ", tempn)
        # Try reading with both formats
        temp_result <- tryCatch(
            {
                # Try first format (semicolon/comma separated with multiple header rows)
                inform("Attempting semicolon/comma format...")
                # First read just the headers with Windows-1252 encoding
                header_row <- suppressWarnings(readLines(tempn, n = 1L, encoding = "Windows-1252"))
                inform("Header row: ", header_row)
                # Then read data skipping the headers
                # Read file content as text with Windows-1252 encoding
                file_text <- paste(readLines(tempn, encoding = "Windows-1252"), collapse = "\n")
                temp_data <- suppressWarnings(data.table::fread(text = file_text, skip = 1L))
                if (nrow(temp_data) > 0 && ncol(temp_data) > 0) {
                    # Parse column names from header line
                    ch_names <- unlist(strsplit(header_row, "[,;]"))
                    ch_names <- trimws(ch_names)
                    inform("Parsed column names: ", paste(ch_names, collapse = ", "))

                    # Translate Chinese column names to English
                    ch_names <- gsub("^\u65f6\u95f4$", "Time", ch_names) # Chinese "Time" -> Time
                    ch_names <- gsub("^\u901a\u9053\\s*([A-Z])$", "Channel \\1", ch_names) # Chinese "Channel A" -> Channel A
                    ch_names <- gsub("^\u901a\u9053([A-Z])$", "Channel \\1", ch_names) # Chinese "ChannelA" -> Channel A
                    inform("Translated column names: ", paste(ch_names, collapse = ", "))

                    inform("Number of columns in data: ", ncol(temp_data))
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
                inform("Attempting space format...")
                # Read file content as text with Windows-1252 encoding
                file_text <- paste(readLines(tempn, encoding = "Windows-1252"), collapse = "\n")
                temp_data <- suppressWarnings(data.table::fread(text = file_text))
                if (nrow(temp_data) > 0 && ncol(temp_data) > 0) {
                    # Extract column names without units
                    ch_names <- colnames(temp_data)
                    ch_names <- gsub("\\s*\\([^\\)]+\\)", "", ch_names)
                    inform("Column names from space format: ", paste(ch_names, collapse = ", "))

                    # Translate Chinese column names to English
                    ch_names <- gsub("^\u65f6\u95f4$", "Time", ch_names) # Chinese "Time" -> Time
                    ch_names <- gsub("^\u901a\u9053\\s*([A-Z])$", "Channel \\1", ch_names) # Chinese "Channel A" -> Channel A
                    ch_names <- gsub("^\u901a\u9053([A-Z])$", "Channel \\1", ch_names) # Chinese "ChannelA" -> Channel A
                    inform("Translated column names: ", paste(ch_names, collapse = ", "))

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
            inform("Successfully read file with format: ", file_format)
            rm(tempn, temp_result)
            break
        } else {
            inform("Failed to read file with either format")
        }
    }

    # Check if we successfully got the column names
    if (is.null(ch_names)) {
        stop("Could not read column names from any file in the directory. Please check file format and contents.")
    }

    ch_selected <- ch_names[-which(ch_names == "Time")]

    if (isTRUE(verbose)) {
        inform("Number of folders: ", nfold)
        inform("Number of channels: ", length(ch_selected))
        inform("Names of channels: ", paste(ch_selected, collapse = ", "))
    }

    ## read data
    raw_res <- as.numeric(temp[2, Time])

    rawmaster <- list() # initialize master data.table
    for (f in 1:nfold) {
        ## for each directory
        # Get all files and filter out generated output files
        dir_files <- list.files(file.path(data_root, folds[f]), full.names = TRUE)
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
        inform(sprintf("Reading data: %.0f%%", f / nfold * 100))
    }

    inform("Finalizing...")

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

    if (isTRUE(verbose)) {
        inform(
            "Total duration: ",
            round(rawmaster[nrow(rawmaster), Time] / 60, digits = 1),
            " mins"
        )
        utils::str(rawmaster)
    }

    # Optionally save collated data
    if (!is.null(output_file)) {
        output_path <- output_file
        output_ext <- tools::file_ext(output_file)
        output_is_dirlike <- isTRUE(dir.exists(output_file)) || identical(output_ext, "")

        if (isTRUE(output_is_dirlike)) {
            # Treat as output directory; create it if needed and derive a filename from the input.
            dir.create(output_file, recursive = TRUE, showWarnings = FALSE)
            input_stem <- tools::file_path_sans_ext(basename(file_path))
            output_path <- file.path(output_file, paste0(input_stem, "_collated.csv"))
        } else {
            # Treat as explicit file path; ensure parent directory exists.
            out_dir <- dirname(output_file)
            dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        }

        data.table::fwrite(rawmaster, file = output_path)
        inform("Collated data table saved to: ", output_path)
    }

    return(rawmaster)
}
### END OF DATA COLLATION ###
