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
    if (!stringr::str_detect(file_path, ".zip")) {
        stop("File is not a zip file.")
    }

    # get the name of the zipped folder
    dname <- strsplit(
        unzip(file_path, list = TRUE)[1, "Name"],
        split = "/"
    )[[1]][1]
    unzip(file_path)
    # names of directories
    folds <- stringr::str_sort(
        list.dirs(paste0("./", dname), full.names = FALSE)[-1],
        numeric = TRUE
    )
    # number of directories
    nfold <- length(folds)
    data_str <- data.table::data.table(
        folders = folds,
        pages = sapply(
            as.list(paste0("./", dname, "/", folds)),
            function(x) length(list.files(x, full.name = FALSE))
        )
    ) # create a data.table displaying the data structure
    for (tempn in list.files(paste0(
        "./", dname, "/", folds[1]
    ), full.name = TRUE)) {
        temp <- suppressWarnings(data.table::fread(tempn, skip = 1L))
        ifelse(nrow(temp) == 0, next, {
            ch_names <- suppressWarnings(
                colnames(data.table::fread(tempn))
            ) # get column names
            colnames(temp) <- ch_names
            rm(tempn)
            break
        })
    }

    ch_selected <- ch_names[-which(ch_names == "Time")]

    # print data structure for data preview
    print(list(
        paste0("Zipped file name: ", dname),
        paste0("Number of files: ", nfold),
        data_str,
        paste0(
            "Total duration: ",
            round(temp$Time[nrow(temp)] * sum(data_str$pages) / 60, digits = 0),
            " mins"
        ),
        paste0("Number of channels: ", length(ch_selected)),
        paste0("Names of channels: ", paste(ch_selected, collapse = ", "))
    ))

    ## read data
    raw_res <- as.numeric(temp[2, Time])

    rawmaster <- list() # initialize master data.table
    for (f in 1:nfold) {
        ## for each directory
        rawmaster <- c(rawmaster, lapply(
            as.list(1:data_str$pages[f]),
            function(p) {
                tryCatch(
                    {
                        data.table::fread(
                            list.files(
                                paste0("./", dname, "/", folds[f]),
                                full.name = TRUE
                            )[p],
                            skip = 1L
                        )
                    },
                    warning = function(w) {
                        message(
                            paste(
                                "Empty csv file was detected: ", folds[f],
                                sprintf("_%02.0f.csv", p),
                                sep = ""
                            )
                        )
                        return(data.table::data.table(
                            matrix(0, ncol = ncol(temp), nrow = nrow(temp))
                        ))
                    }
                )
            }
        )) # read csvs and add to list
        # update progress bar
        print(sprintf("Reading data: %.0f%%", f / nfold * 100))
    }

    print("Finalizing...")

    # bind data tables into one master data table
    rawmaster <- data.table::rbindlist(rawmaster)
    names(rawmaster) <- ch_names # assign column names
    rawmaster <- rawmaster[which(!is.na(rawmaster[, Time])), ] # remove extra rows

    # convert data into numeric class
    suppressWarnings(
        rawmaster[, colnames(rawmaster) := lapply(.SD, as.numeric)]
    )

    # re-assign time
    rawmaster[, Time := seq(0, by = raw_res, length.out = nrow(rawmaster))]

    # remove extracted files from the working directory
    unlink(paste0("./", dname), recursive = TRUE)
    ## end of read data

    # preview the data
    str(rawmaster)

    # Save rawmaster to a CSV file
    data.table::fwrite(rawmaster, file = paste0(dirname(file_path), "/", dname, ".csv"))
    print(paste0("Collated data table saved as ", dname, ".csv"))

    return(rawmaster)
}
### END OF DATA COLLATION ###
