if (getRversion() >= "2.15.1") {
    utils::globalVariables(c(
        "Time", "X2", "tx", "N", "count", "hr", "ACF", "ix", "win",
        "start_int", "s", "e", "ti", "wACF", "whr", "Time_min", "."
    ))
}

#' @title CardiacDP - computeHR()
#' @description Employing the autocorrelation function (ACF) with a genetic algorithm framework to locate periodic sub-sequences within each sequence. From the candidate heart rates of these sub-sequences, the final results are either evaluated based on the autocorrelation value or a tracking index (TI).
#' @importFrom foreach %dopar%
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom dplyr %>%
#' @importFrom dplyr arrange
#' @importFrom dplyr top_n
#' @importFrom dplyr filter
#' @importFrom data.table .SD
#' @importFrom data.table fread
#' @importFrom data.table rbindlist
#' @importFrom utils unzip
#' @importFrom stats lm
#' @importFrom stats qnorm
#' @importFrom stats median
#' @importFrom stats predict
#' @importFrom stats na.action
#' @importFrom stats na.omit
#' @importFrom stats na.pass
#' @importFrom stats runif
#' @importFrom stats acf
#' @importFrom stringr str_detect
#' @importFrom parallel detectCores
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom doParallel stopImplicitCluster
#' @importFrom foreach foreach
#' @importFrom purrr transpose
#' @importFrom RColorBrewer brewer.pal
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 theme_set
#' @importFrom ggplot2 theme_linedraw
#' @importFrom ggplot2 theme_update
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 element_line
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 element_rect
#' @param file_path Designate the path to your file, must be a .zip or .csv file
#' @param reduce_res Time interval of reduced resolution (seconds), by default 0.01
#' @param pop_size Number of populations used in the genetic algorithm, by default 10L
#' @param max_gen Maximum number of generations in the genetic algorithm, by default 20L
#' @param patience Patience threshold (maximum number of generations with no further changes) in the genetic algorithm, by default 2L
#' @param an_in Analysis interval (length of a sequence; in minute), by default 1
#' @param acf_thres Threshold used in ACF to classify periodic oscillations from aperiodic noises, by default 0.5
#' @param lr_thres Linear regression r-sq threshold in extrapolating the tracking index, by default 0.7
#' @param ncore Integer; number of CPU cores to use for the genetic algorithm. If NULL (default), uses `parallel::detectCores() - 1`. During `R CMD check`, cores are clamped to a small number to satisfy check limits.
#' @param output_dir Optional directory to write CSV/PNG outputs when `save_outputs = TRUE`. If NULL, defaults to `tempdir()`.
#' @param save_outputs Logical; if TRUE, write CSV/PNG outputs to `output_dir`. Default FALSE.
#' @param verbose Logical; if TRUE, emit progress messages. Default FALSE.
#' @return The positions (in indices) and durations of the sub-sequences (finalsubseq) and the corresponding candidate HR (candidateHR) obtained from the genetic algorithm, and the final results evaluating the candidates by autocorrelation values (results_ACF) or the tracking index (results_TI), which contains the details of the subsequences after checking for resolution (subseqHR with Time_min column), the weighted heart rate per sequence (weightedHR with Time_min column) and a plot (plot). If `save_outputs = TRUE`, file paths are recorded in `output$files`.
#' @export computeHR
#' @examples \donttest{
#' # use the default parameters to analyse a zip file
#' # the collatedata function will be called automatically
#' zip_path <- system.file("extdata", "example.zip", package = "CardiacDP")
#' computeHR(file_path = zip_path, save_outputs = FALSE)
#' }
#'
#' @examples \donttest{
#' # use the default parameters to analyse a csv file
#' csv_path <- system.file("extdata", "example.csv", package = "CardiacDP")
#' computeHR(file_path = csv_path, reduce_res = 0.1,
#'           save_outputs = FALSE)
#' }
#'
#' @examples \donttest{
#' # use customized parameters to analyse a zip file
#' zip_path <- system.file("extdata", "example.zip", package = "CardiacDP")
#' computeHR(zip_path, reduce_res = 0.1, max_gen = 30L,
#'           lr_thres = 0.8, save_outputs = FALSE)
#' }
#'
#' @examples \donttest{
#' # use custom parameters to analyse a csv file
#' csv_path <- system.file("extdata", "example.csv", package = "CardiacDP")
#' computeHR(csv_path, reduce_res = 0.1, pop_size = 20L,
#'           an_in = 1, acf_thres = 0.6, save_outputs = FALSE)
#' }
computeHR <- function(
  file_path, reduce_res = 0.01, pop_size = 10L, max_gen = 20L,
  patience = 2L, an_in = 1, acf_thres = 0.5, lr_thres = 0.7,
  ncore = NULL,
  output_dir = NULL, save_outputs = FALSE, verbose = FALSE
) {
    # Import required operators
    `%dopar%` <- foreach::`%dopar%`
    `%do%` <- foreach::`%do%`
    `%>%` <- dplyr::`%>%`

    inform <- function(...) {
        if (isTRUE(verbose)) message(...)
    }

    # CRAN check often sets _R_CHECK_LIMIT_CORES_; respect it to avoid spawning too many processes.
    check_limit_raw <- Sys.getenv("_R_CHECK_LIMIT_CORES_", unset = "")
    check_limit_set <- nzchar(check_limit_raw) &&
        !(tolower(check_limit_raw) %in% c("false", "0"))
    check_limit_num <- suppressWarnings(as.integer(check_limit_raw))
    check_limit_max <- if (!is.na(check_limit_num)) check_limit_num else 2L
    # Clamp to 1-2 when the env var is set (per CRAN check expectations)
    check_limit_max <- max(1L, min(2L, check_limit_max))

    if (isTRUE(save_outputs)) {
        if (is.null(output_dir)) output_dir <- tempdir()
        dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # Check if file_path is a CSV or ZIP file
    if (stringr::str_detect(file_path, "\\.csv$")) {
        # Read CSV directly to avoid creating a huge single string
        temp_check <- data.table::fread(file_path, encoding = "Latin-1")
        if (!any(stringr::str_detect(
            colnames(temp_check), "Time"
        ))) {
            stop("File must have a column named 'Time'.")
        }
        # check if file has at least 2 columns
        if (length(colnames(temp_check)) < 2) {
            stop("File must have at least 2 columns.")
        }
        # check if file has at least 2 rows
        if (nrow(temp_check) < 2) {
            stop("File must have at least 2 rows.")
        }
        # If it's a CSV file, reuse the already read data instead of reading again
        rawmaster <- temp_check

        # convert data into numeric class
        suppressWarnings(
            rawmaster[, colnames(rawmaster) := lapply(.SD, as.numeric)]
        )
    } else if (stringr::str_detect(file_path, "\\.zip$")) {
        # If it's a ZIP file, call the data_preparation function
        rawmaster <- collatedata(file_path)
    } else {
        stop("File must be either a .csv or .zip file.")
    }

    # derive resolution and channel names from the collated data table
    raw_res <- as.numeric(rawmaster[2, Time])
    ch_selected <- colnames(rawmaster)[-which(colnames(rawmaster) == "Time")]

    # interval that is the closest to the desired resolution
    closest_interval <- which.min(abs(rawmaster[, Time] - reduce_res)) - 1L
    actual_new_res <- rawmaster[closest_interval + 1L, Time] # new resolution

    # create data table of reduced resolution
    mindex <- seq(1, nrow(rawmaster), by = closest_interval)
    master <- rawmaster[mindex, ] # reduced resolution data.table

    findpeaks <- function(x, nups = 1, ndowns = nups, zero = "+",
                          peakpat = NULL, minpeakheight = -Inf,
                          minpeakdistance = 1,
                          threshold = 0, npeaks = 0, sortstr = FALSE) {
        stopifnot(is.vector(x, mode = "numeric") || length(is.na(x)) == 0)
        if (!zero %in% c("0", "+", "-")) {
            stop("Argument 'zero' can only be '0', '+', or '-'.")
        }
        xc <- paste(as.character(sign(diff(x))), collapse = "")
        xc <- gsub("1", "+", gsub(
            "-1", "-",
            xc
        ))
        if (zero != "0") {
            xc <- gsub("0", zero, xc)
        }
        if (is.null(peakpat)) {
            peakpat <- sprintf("[+]{%d,}[-]{%d,}", nups, ndowns)
        }
        rc <- gregexpr(peakpat, xc)[[1]]
        if (rc[1] < 0) {
            return(NULL)
        }
        x1 <- rc
        x2 <- rc + attr(rc, "match.length")
        attributes(x1) <- NULL
        attributes(x2) <- NULL
        n <- length(x1)
        xv <- xp <- numeric(n)
        for (i in 1:n) {
            # bypass error
            tryCatch(
                {
                    xp[i] <- which.max(x[x1[i]:x2[i]]) + x1[i] - 1
                    xv[i] <- x[xp[i]]
                },
                error = function(e) {}
            )
        }
        inds <- which(
            xv >= minpeakheight & xv - pmax(x[x1], x[x2]) >= threshold
        )
        X <- cbind(xv[inds], xp[inds], x1[inds], x2[inds])
        if (minpeakdistance < 1) {
            warning("Handling 'minpeakdistance < 1' is logically not possible.")
        }
        if (sortstr || minpeakdistance > 1) {
            sl <- sort.list(X[, 1], na.last = NA, decreasing = TRUE)
            X <- X[sl, , drop = FALSE]
        }
        if (length(X) == 0) {
            return(c())
        }
        if (minpeakdistance > 1) {
            no_peaks <- nrow(X)
            badpeaks <- rep(FALSE, no_peaks)
            for (i in 1:no_peaks) {
                ipos <- X[i, 2]
                if (!badpeaks[i]) {
                    dpos <- abs(ipos - X[, 2])
                    badpeaks <- badpeaks | (dpos > 0 & dpos < minpeakdistance)
                }
            }
            X <- X[!badpeaks, , drop = FALSE]
        }
        if (npeaks > 0 && npeaks < nrow(X)) {
            X <- X[1:npeaks, , drop = FALSE]
        }
        return(X)
    }

    fitness <- function(pop, segment, thres) {
        # determine whether populations are viable in the GA

        # return sub-sequence duration if max acf >= threshold, else return 1 (minimal duration)
        return(apply(pop, 1, function(x) {
            ifelse(sum(segment[x["s"]:x["e"]], na.rm = TRUE) == 0, 1, {
                acfs <- acf(
                    segment[x["s"]:x["e"]],
                    lag.max = x["e"] - x["s"], plot = FALSE,
                    na.action = na.pass
                )$acf
                ifelse(
                    max(
                        acfs[-c(1:(which(sign(acfs) == -1)[1]))],
                        na.rm = TRUE
                    ) > thres,
                    x["e"] - x["s"], 1
                )
            })
        }))
    }

    mutate <- function(pop, start, end, step) {
        # mutate gene e/s for each population
        return(
            data.table::data.table(
                t(apply(pop, 1, function(x) {
                    # determine which gene (which can still be mutated) to mutate
                    direction <- runif(1) - 0.5 * (x["s"] == start)
                    +0.5 * (x["e"] == end)
                    if (direction > 0.5) {
                        # if mutate gene s
                        return(
                            c(
                                s = as.integer(round(runif(
                                    1, start, max(start, x["s"] - step)
                                ))),
                                x["e"], x["p"], x["f"]
                            )
                        )
                    } else {
                        # if mutate gene e
                        return(
                            c(x["s"],
                                e = as.integer(round(runif(1, min(end, x["e"] + step), end))),
                                x["p"], x["f"]
                            )
                        )
                    }
                }))
            )
        ) # return mutated e or s
    }

    acffunc <- function(final, segment, thres) {
        # to identify periods (in indices) of final sub-sequences using autocorrelation
        return(apply(final, 1, function(x) {
            acfout <- acf(
                segment[x["s"]:x["e"]],
                lag.max = as.integer(x["f"]), plot = FALSE, na.action = na.pass
            )
            # pair lag with acf
            acfdt <- suppressWarnings(data.table::data.table(
                lag = acfout[["lag"]],
                ACF = acfout[["acf"]]
            ))

            # upper limit of 95% confidence interval
            clim <- qnorm((1 + 0.95) / 2) / sqrt(acfout$n.used)
            # remove the first positive and negative section
            acfdt <- acfdt[-c(1:(which(sign(acfdt$ACF) == -1)[1])), ]
            allpeaks <- dplyr::arrange(
                data.frame(
                    findpeaks(acfdt$ACF, minpeakheight = clim, minpeakdistance = 5)
                )[, 1:2], X2
            )
            localmax <- allpeaks[1:which.max(allpeaks[, 1]), ]
            return(data.table::data.table(
                ACF = localmax[, 1],
                lag = acfdt$lag[localmax[, 2]],
                hr = 60 / actual_new_res / acfdt$lag[localmax[, 2]]
            ))
        }))
        # return a list of local maxima
    }

    ga <- function(master, hrdf, initial_pop, pop_size,
                   max_gen, mutation_step, thres, patience) {
        # genetic algorithm for calculating the heart rate of each sequence in parallel

        # extract the segment of signal
        segment <- master[hrdf["start_int"]:hrdf["end_int"]]
        # initialize population and calculate fitness
        initial_pop$f <- fitness(initial_pop, segment, thres)
        # initialize population
        pop <- initial_pop
        # initialize hall of fame as the fit-enough individuals from the initial population
        hof <- pop[which(pop$f > 1), ]

        wait <- 0 # patience counter

        for (g in 2:max_gen) {
            ## for a number of generations

            # save population (to be compared with the subsequent generation)
            last_pop <- pop
            # select individuals for reproducing the next generation (chance proportional to fitness)
            pop <- pop[sample.int(pop_size, replace = T, prob = pop$f), ]

            # mutate the new generation
            pop <- mutate(pop, 1, length(segment), mutation_step)

            # calculate the fitness of each individual
            pop$f <- fitness(pop, segment, thres)

            # store fit-enough individuals into hall of fame
            hof <- rbind(hof, pop[which(pop$f > 1), ])
            wait <- ifelse(any(sapply(as.list(unique(pop$p)), function(x) {
                ## compare the new generation with the last generation (along each lineage)

                # if there are increments in at least one lineage, reset patience counter, else patience counter + 1
                max(
                    pop$f[which(pop$p == x)]
                ) > max(
                    last_pop$f[which(last_pop$p == x)]
                )
            })), 0, wait + 1)

            # break loop if an individual equals the entire sequence, or the patience counter has reached the tolerance level
            if (any(pop$f == (length(segment) - 1L)) || wait == patience) break

            # if all individuals are unfit, revert to the previous population
            if (all(pop$f == 1)) pop <- last_pop
        }

        # initialize data.tables to store outputs
        final <- data.table::data.table(
            s = integer(0), e = integer(0), p = integer(0), f = integer(0)
        )
        while (nrow(hof) > 0) {
            ## while there are still individuals in hall of fame

            # store the longest individual in final
            final <- rbind(hof[which.max(hof$f), ], final)

            # remove individuals that are completely overlapped by the stored individual
            hof <- hof[-which(hof$s >= final$s[1] & hof$e <= final$e[1]), ]
        }
        if (nrow(final) == 0) {
            final <- data.table::data.table(s = NA, e = NA, p = NA, f = NA)
            counts <- list(data.table::data.table(ACF = NA, lag = NA, hr = NA))
        } else {
            counts <- acffunc(final, segment, thres)
        }
        return(list(
            final = final,
            counts = counts
        ))
    }

    weight <- function(dt) { # calculate hr counts by weighting
        if (nrow(na.omit(dt)) > 1) { # multiple periodic sub-sequences
            seqtally <- stats::setNames(
                data.table::data.table(table(
                    unlist(
                        apply(na.omit(dt), 1, function(x) seq(x[["s"]], x[["e"]], 1))
                    )
                )),
                c("tx", "count")
            ) # count the occurrence
            return(stats::setNames(
                data.table::data.table(t(apply(
                    apply(na.omit(dt), 1, function(x) {
                        c(x[["ACF"]], x[["hr"]]) * (seqtally[tx %in% seq(
                            x[["s"]], x[["e"]], 1
                        ), .N, by = "count"][, sum(N / count)])
                    }), 1, sum
                ) / (seqtally[, length(unique(tx))]))),
                c("wACF", "whr")
            )) # weighted by duration
        } else {
            if (nrow(na.omit(dt)) == 1) { # only one periodic sub-sequence
                return(data.table::data.table(
                    wACF = na.omit(dt[["ACF"]]),
                    whr = na.omit(dt[["hr"]])
                ))
            } else { # no periodic sub-sequences
                return(data.table::data.table(
                    wACF = NA,
                    whr = NA
                ))
            }
        }
    }

    HRbyacf <- function(out) {
        # calculate HR based on ACF only (selecting the time lag with max ACF)
        byacfdt <- cbind(
            ix = rep(seq_along(out[["final"]]), lapply(out[["final"]], nrow)),
            win = unlist(
                lapply(lapply(out[["final"]], nrow), function(x) seq(1, x, 1))
            ),
            data.table::rbindlist(out[["final"]])
        )

        byacfdt <- cbind(
            byacfdt,
            data.table::rbindlist(lapply(out[["counts"]], function(i) {
                data.table::rbindlist(lapply(i, function(w) {
                    if (is.na(w[1, hr])) {
                        w[1, ]
                    } else {
                        w %>% dplyr::top_n(1, ACF)
                    }
                }))
            }))
        )
        return(byacfdt)
    }

    HRbyTI <- function(out) {
        # screen through the counts with a tracking index (past 5 intervals) through time
        byTIdt <- cbind(
            ix = rep(seq_along(out[["final"]]), lapply(out[["final"]], nrow)),
            win = unlist(
                lapply(lapply(out[["final"]], nrow), function(x) seq(1, x, 1))
            ),
            data.table::rbindlist(out[["final"]]),
            ACF = as.numeric(NA), lag = as.numeric(NA), hr = as.numeric(NA)
        )

        r_squared_from_fit <- function(fit, y) {
            yhat <- stats::fitted(fit)
            ss_res <- sum((y - yhat)^2)
            ss_tot <- sum((y - mean(y))^2)
            if (isTRUE(all.equal(ss_tot, 0))) {
                return(1)
            }
            1 - (ss_res / ss_tot)
        }

        for (i in seq_along(out[["counts"]])) {
            prev <- byTIdt[ix %in% (i - 5):(i - 1), .(ix, hr)]
            prev2 <- stats::na.omit(prev)
            if (nrow(prev2) == 0) {
                est <- NA_real_
            } else if (length(unique(prev2[["ix"]])) > 2) {
                fit <- stats::lm(hr ~ ix, prev2)
                r2 <- r_squared_from_fit(fit, prev2[["hr"]])
                if (!is.na(r2) && r2 >= lr_thres) {
                    est <- as.numeric(stats::predict(fit, data.frame(ix = i)))
                } else {
                    est <- stats::median(prev2[["hr"]], na.rm = TRUE)
                }
            } else {
                est <- stats::median(prev2[["hr"]], na.rm = TRUE)
            }
            for (w in seq_along(out[["counts"]][[i]])) {
                if (is.na(out[["counts"]][[i]][[w]][1, hr])) { # NA count
                    byTIdt[
                        ix == i & win == w, names(byTIdt)[3:9] := as.list(rep(NA, 7))
                    ]
                } else {
                    ifelse(is.na(est),
                        byTIdt[
                            ix == i & win == w, c("ACF", "lag", "hr") := as.list(
                                out[["counts"]][[i]][[w]] %>%
                                    dplyr::top_n(1, ACF)
                            )
                        ], # return counts with max acf if no reference from previous counts
                        ifelse(abs(
                            as.numeric(out[["counts"]][[i]][[w]][, .(hr)] %>% dplyr::filter(
                                abs(hr - est) == min(abs(hr - est))
                            )) - est
                        ) / est > 0.4,
                        byTIdt[
                            ix == i & win == w, names(byTIdt)[3:9] := as.list(rep(NA, 7))
                        ],
                        byTIdt[
                            ix == i & win == w, c("ACF", "lag", "hr") := as.list(
                                out[["counts"]][[i]][[w]] %>%
                                    dplyr::filter(
                                        abs(hr - est) == min(abs(hr - est))
                                    )
                            )
                        ]
                        )
                    ) # return counts closest to previous counts
                }
            }
        }
        return(byTIdt)
    }

    reacf <- function(dt, segment) {
        # re-run acf at finest resolution if resolution is coarser than 2% of the bpm count
        acfout <- acf(
            segment,
            lag.max = as.integer(
                (dt$lag + 5) * round(
                    actual_new_res / raw_res
                )
            ), plot = FALSE, na.action = na.pass
        )
        # pair lag with acf
        acfdt <- suppressWarnings(data.table::data.table(
            lag = acfout[["lag"]],
            ACF = acfout[["acf"]]
        ))

        # upper limit of 95% confidence interval
        clim <- qnorm((1 + 0.95) / 2) / sqrt(acfout$n.used)
        # remove the first positive and negative section
        acfdt <- acfdt[-c(1:(which(sign(acfdt$ACF) == -1)[1])), ]

        allpeaks <- dplyr::arrange(
            data.frame(
                findpeaks(acfdt$ACF, minpeakheight = clim, minpeakdistance = 5)
            )[, 1:2], X2
        )
        i <- which.min(
            abs(apply(
                allpeaks, 1, function(x) 60 / raw_res / acfdt$lag[x[2]]
            ) - dt$hr)
        )
        return(data.table::data.table(
            ACF = allpeaks[i, 1],
            lag = acfdt$lag[allpeaks[i, 2]],
            hr = 60 / raw_res / acfdt$lag[allpeaks[i, 2]]
        ))
    }

    pp <- function(dt) {
        palette <- RColorBrewer::brewer.pal(n = 11, name = "RdYlBu")
        names(palette) <- seq(0, 10, 1) / 10
        ggplot2::theme_set(ggplot2::theme_linedraw())
        ggplot2::theme_update(
            text = ggplot2::element_text(size = 12, colour = "black"),
            axis.title = ggplot2::element_text(size = 12, colour = "black"),
            axis.text = ggplot2::element_text(size = 12, colour = "black"),
            axis.ticks = ggplot2::element_line(
                color = "black", linewidth = 0.5, lineend = "square"
            ),
            panel.grid.minor = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(
                color = "black", linewidth = 0.5, linetype = "dotted"
            ),
            panel.border = ggplot2::element_rect(
                fill = NA, colour = "black", linewidth = 1
            ),
            panel.background = ggplot2::element_blank(),
            legend.background = ggplot2::element_blank(),
            legend.key = ggplot2::element_blank(),
            plot.background = ggplot2::element_blank()
        )

        # Check if there's any valid data to plot
        if (nrow(dt) == 0 || all(is.na(dt$hr)) || all(is.na(dt$ACF))) {
            # Create an empty plot with appropriate labels when no data is available
            return(
                ggplot2::ggplot() +
                    ggplot2::annotate("text",
                        x = 0.5, y = 0.5,
                        label = "No periodic sub-sequences found\nNo data to display",
                        size = 5, hjust = 0.5, vjust = 0.5
                    ) +
                    ggplot2::scale_y_continuous(
                        name = "Heart rate (bpm)",
                        breaks = seq(0, 50, 10),
                        limits = c(0, 50),
                        expand = c(0, 0)
                    ) +
                    ggplot2::scale_x_continuous(
                        name = "Time (min)",
                        breaks = seq(0, 30, 10),
                        limits = c(0, 30),
                        expand = c(0, 0)
                    ) +
                    ggplot2::theme(
                        panel.grid = ggplot2::element_blank(),
                        axis.text = ggplot2::element_blank(),
                        axis.ticks = ggplot2::element_blank()
                    )
            )
        }

        return(
            ggplot2::ggplot() +
                ggplot2::geom_point(
                    data = dt,
                    ggplot2::aes(x = Time_min, y = hr, fill = factor(floor(ACF * 10) / 10)),
                    colour = "black", shape = 21, size = 4, alpha = .8
                ) +
                ggplot2::scale_fill_manual(
                    name = "ACF", values = palette, na.value = NA,
                    na.translate = FALSE, guide = "legend"
                ) +
                ggplot2::scale_y_continuous(
                    name = "Heart rate (bpm)",
                    breaks = seq(
                        0, ifelse(
                            sum(!is.na(dt[, hr])) > 0,
                            ceiling(max(dt[, hr], na.rm = TRUE) / 50) * 50, 50
                        ), 50
                    ),
                    limits = c(
                        0, ifelse(
                            sum(!is.na(dt[, hr])) > 0,
                            ceiling(max(dt[, hr], na.rm = TRUE) / 50) * 50, 50
                        )
                    ),
                    expand = c(0, 0)
                ) +
                ggplot2::scale_x_continuous(
                    name = "Time (min)",
                    breaks = seq(0, ceiling(max(dt[, Time_min], na.rm = TRUE) / 30) * 30, 30),
                    limits = c(0, ceiling(max(dt[, Time_min], na.rm = TRUE) / 30) * 30),
                    expand = c(0, 0)
                )
        )
    }

    ## main function
    {
        ## build an index table of sequences for analysis in parallel
        # length (in indices) of a sequence
        an_index_in <- which.min(
            abs(
                master[
                    1:(floor(60 * an_in / actual_new_res) + 200), Time
                ] - (60 * an_in)
            )
        ) - 1L
        # number of sequence
        an_index_length <- floor(master[nrow(master), Time] / (60 * an_in))
        # index table
        windex <- data.table::data.table(
            start_int = seq(1,
                by = an_index_in + 1L,
                length.out = an_index_length
            ),
            end_int = seq(an_index_in + 1L,
                by = an_index_in + 1L,
                length.out = an_index_length
            ),
            start_min = seq(0, by = an_in, length.out = an_index_length),
            end_min = seq(an_in, by = an_in, length.out = an_index_length)
        )

        ## initializing populations of each sequence for genetic algorithm
        wbound <- as.integer(seq(1, an_index_in, length.out = pop_size + 1L))
        # s(starting index) and e(ending index) are genes, p is the lineage
        initial_pop <- data.table::data.table(
            s = wbound[1:pop_size],
            e = wbound[2:(pop_size + 1L)] - 1L,
            p = 1:pop_size
        )

        # determine the minimal step for mutation
        mutation_step <- as.integer(
            ceiling(an_index_in * (1 - 1 / pop_size) / max_gen)
        )

        # lag threshold below which the resolution exceeds 2% of the calculated bpm
        # and acf is re-run using the raw data at finest resolution
        lag_thres <- as.numeric(
            which(
                sapply(
                    seq(1, an_index_in, 1),
                    function(x) {
                        (
                            (60 / actual_new_res / x) - (60 / actual_new_res / (x + 1))
                        ) / (60 / actual_new_res / x)
                    }
                ) <= 0.02
            )[1]
        )

        # empty list to store results
        output <- list()

        for (channel in ch_selected) {
            ## calculate heart rate for each channel

            # Calculate actual duration for this channel
            channel_duration <- round(master[nrow(master), Time] / 60, digits = 1)
            n_sequences <- nrow(windex)

            # indicate which channel is being analyzed with duration info
            inform(sprintf(
                "Calculating heart rate: %s (Duration: %s mins, Sequences: %d)...",
                channel, channel_duration, n_sequences
            ))

            ncore_auto <- max(1L, parallel::detectCores() - 1L)
            ncore_req <- if (is.null(ncore)) ncore_auto else as.integer(ncore)
            if (length(ncore_req) != 1L || is.na(ncore_req) || ncore_req < 1L) {
                stop("`ncore` must be a positive integer or NULL.")
            }

            ncore_eff <- ncore_req
            if (isTRUE(check_limit_set)) ncore_eff <- min(ncore_eff, check_limit_max)
            ncore_eff <- min(ncore_eff, n_sequences)
            use_parallel <- isTRUE(ncore_eff > 1L)

            # employ genetic algorithm to select periodic durations for heart rate computation
            cl <- NULL
            out <- tryCatch(
                {
                    if (use_parallel) {
                        cl <- parallel::makeCluster(ncore_eff)
                        doParallel::registerDoParallel(cl)
                    }
                    purrr::transpose(
                        if (use_parallel) {
                            foreach::foreach(
                                ti = seq_len(nrow(windex)),
                                .packages = c("data.table", "dplyr")
                            ) %dopar% {
                                ga(
                                    master = master[[channel]], hrdf = unlist(windex[ti, ]),
                                    initial_pop = initial_pop, pop_size = pop_size, max_gen = max_gen,
                                    mutation_step = mutation_step, thres = acf_thres, patience = patience
                                )
                            }
                        } else {
                            foreach::foreach(
                                ti = seq_len(nrow(windex)),
                                .packages = c("data.table", "dplyr")
                            ) %do% {
                                ga(
                                    master = master[[channel]], hrdf = unlist(windex[ti, ]),
                                    initial_pop = initial_pop, pop_size = pop_size, max_gen = max_gen,
                                    mutation_step = mutation_step, thres = acf_thres, patience = patience
                                )
                            }
                        }
                    )
                },
                finally = {
                    if (!is.null(cl)) {
                        try(parallel::stopCluster(cl), silent = TRUE)
                    }
                    # Best-effort cleanup (no-op if none registered)
                    try(doParallel::stopImplicitCluster(), silent = TRUE)
                }
            )

            ## Generating the final output
            inform("Generating output...")

            # without tracking index (calculate HR based on max ACF)
            byACFdt <- cbind(
                HRbyacf(out), data.table::data.table(res = actual_new_res)
            )
            for (i in which(byACFdt$lag < lag_thres)) { # re-run acf at the finest resolution if resolution is lower than threshold
                byACFdt[i, 7:10] <- cbind(
                    reacf(
                        dt = byACFdt[i, ],
                        segment = rawmaster[[channel]][mindex[
                            windex[byACFdt[i, ix], start_int]
                            + byACFdt[i, s] - 1
                        ]:mindex[windex[byACFdt[i, ix], start_int] + byACFdt[i, e] - 1]]
                    ),
                    res = raw_res
                )
            }
            # Add Time_min column (start of analysis interval for sub-sequences)
            byACFdt$Time_min <- (byACFdt$ix - 1) * an_in
            wACFdt <- cbind(
                data.table::data.table(ix = unique(byACFdt[["ix"]])),
                data.table::rbindlist(lapply(
                    unique(byACFdt[["ix"]]), function(x) weight(byACFdt[ix == x, ])
                ))
            )
            # Add Time_min column (midpoint of analysis interval)
            wACFdt$Time_min <- (wACFdt$ix - 0.5) * an_in
            ACFplot <- pp(wACFdt[, .(Time_min = Time_min, ACF = wACF, hr = whr)])

            # with tracking index
            byTIdt <- cbind(HRbyTI(out), data.table::data.table(res = actual_new_res))
            for (i in which(byTIdt$lag < lag_thres)) { # re-run acf at the finest resolution if resolution is lower than threshold
                byTIdt[i, 7:10] <- cbind(
                    reacf(
                        dt = byTIdt[i, ],
                        segment = rawmaster[[channel]][mindex[
                            windex[byTIdt[i, ix], start_int]
                            + byTIdt[i, s] - 1
                        ]:mindex[windex[byTIdt[i, ix], start_int] + byTIdt[i, e] - 1]]
                    ),
                    res = raw_res
                )
            }
            # Add Time_min column (start of analysis interval for sub-sequences)
            byTIdt$Time_min <- (byTIdt$ix - 1) * an_in
            wTIdt <- cbind(
                data.table::data.table(ix = unique(byTIdt[["ix"]])),
                data.table::rbindlist(lapply(
                    unique(byTIdt[["ix"]]), function(x) weight(byTIdt[ix == x, ])
                ))
            )
            # Add Time_min column (midpoint of analysis interval)
            wTIdt$Time_min <- (wTIdt$ix - 0.5) * an_in
            TIplot <- pp(wTIdt[, .(Time_min = Time_min, ACF = wACF, hr = whr)])


            output$finalsubseq[[channel]] <- out[["final"]] # indices of the final sub-sequences
            output$candidateHR[[channel]] <- out[["counts"]] # candidate HR corresponding to the sub-sequences
            output$results_ACF[[channel]] <- list(
                subseqHR = byACFdt, # results without tracking
                weightedHR = wACFdt,
                plot = ACFplot
            )
            output$results_TI[[channel]] <- list(
                subseqHR = byTIdt, # results with tracking
                weightedHR = wTIdt,
                plot = TIplot
            )

            # Optionally save CSV/PNG outputs (CRAN-safe: opt-in and default tempdir())
            if (isTRUE(save_outputs)) {
                # Sanitize channel name for file naming (replace spaces with underscores)
                channel_name <- gsub(" ", "_", channel)
                channel_name <- gsub("[^A-Za-z0-9_-]", "_", channel_name)

                acf_subseq_path <- file.path(output_dir, paste0(channel_name, "_ACF_subseqHR.csv"))
                acf_weight_path <- file.path(output_dir, paste0(channel_name, "_ACF_weightedHR.csv"))
                ti_subseq_path <- file.path(output_dir, paste0(channel_name, "_TI_subseqHR.csv"))
                ti_weight_path <- file.path(output_dir, paste0(channel_name, "_TI_weightedHR.csv"))
                acf_plot_path <- file.path(output_dir, paste0(channel_name, "_ACF_plot.png"))
                ti_plot_path <- file.path(output_dir, paste0(channel_name, "_TI_plot.png"))

                data.table::fwrite(byACFdt, file = acf_subseq_path, na = "NA")
                data.table::fwrite(wACFdt, file = acf_weight_path, na = "NA")
                data.table::fwrite(byTIdt, file = ti_subseq_path, na = "NA")
                data.table::fwrite(wTIdt, file = ti_weight_path, na = "NA")

                ggplot2::ggsave(
                    filename = acf_plot_path,
                    plot = ACFplot,
                    width = 10,
                    height = 6,
                    dpi = 300,
                    units = "in"
                )
                ggplot2::ggsave(
                    filename = ti_plot_path,
                    plot = TIplot,
                    width = 10,
                    height = 6,
                    dpi = 300,
                    units = "in"
                )

                if (is.null(output$files)) output$files <- list()
                output$files[[channel]] <- list(
                    ACF_subseqHR = acf_subseq_path,
                    ACF_weightedHR = acf_weight_path,
                    TI_subseqHR = ti_subseq_path,
                    TI_weightedHR = ti_weight_path,
                    ACF_plot = acf_plot_path,
                    TI_plot = ti_plot_path
                )
                inform(sprintf("Saved CSV and PNG files for %s to %s", channel, output_dir))
            }
        }
        return(output)
    }
}
