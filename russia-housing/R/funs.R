





## only numeric (tranform int to numeric too)
intnum <- function(dat) {
    id <- dat$id
    price_doc <- dat$price_doc
    ints <- vapply(dat, is.integer, logical(1), USE.NAMES = FALSE)
    dat[ints] <- lapply(dat[ints], as.numeric)
    dat <- dat[, !duplicated(gsub("\\_|[[:digit:]]", "", names(dat)))]
    nums <- vapply(dat, is.numeric, logical(1), USE.NAMES = FALSE)
    dat <- dat[, nums]
    dat$id <- id
    dat$price_doc <- price_doc
    dat
}

## omit id and dv
var.omit <- function(x, ...) {
    dots <- c(...)
    if (length(dots) == 0) {
        dots <- c("id", "price_doc")
    }
    x[, !names(x) %in% dots]
}





## summary stats
sum_stats <- function(data, digits = 2L) {
    ##data <- var.omit(data, "id")
    ## numerics
    nums <- vapply(data, is.numeric, logical(1))
    data[nums] <- lapply(data[nums], as.numeric)
    ##numdat <- data[nums]

    ## factors
    chrs <- vapply(data, is.character, logical(1))
    data[chrs] <- lapply(data[chrs], as.factor)
    fcts <- vapply(data, is.factor, logical(1))
    if (sum(fcts) > 0) data[fcts] <- lapply(data[fcts], as.factor)
    ##fctdat <- data[fcts]

    ## summary stats
    sum_nums <- function(numdat) {
        out <- lapply(numdat, function(x)
            c("dbl",
              length(x),
              sum(is.na(x)),
              sum(is.na(x)) / length(x),
              min(x, na.rm = TRUE),
              max(x, na.rm = TRUE),
              mean(x, na.rm = TRUE),
              sd(x, na.rm = TRUE)))
        matrix(unlist(out), ncol = 8, byrow = TRUE)
    }
    sum_fcts <- function(fctdat) {
        out <- lapply(fctdat, function(x)
            c("chr",
              length(x),
              sum(is.na(x)),
              sum(is.na(x)) / length(x),
              NA_real_,
              NA_real_,
              NA_real_,
              as.double(length(unique(x)))))
        matrix(unlist(out), ncol = 8, byrow = TRUE)
    }
    ## output matrix
    sum_stats <- matrix(NA_character_, ncol(data), 9)
    sum_stats[, 1] <- names(data)
    summat <- sum_nums(data[nums])
    for (i in seq_len(ncol(summat))) {
        sum_stats[which(nums), (i + 1L)] <- summat[, i]
    }
    if (sum(fcts) > 0) {
        summat <- sum_fcts(data[fcts])
        for (i in seq_len(ncol(summat))) {
            sum_stats[which(fcts), (i + 1L)] <- summat[, i]
        }
    }
    ##
    sum_stats <- structure(
        data.frame(
            sum_stats, stringsAsFactors = FALSE),
        names = c("var", "class", "n", "na", "na.pct", "min", "max", "mean", "sd/uq"))
    dig <- paste0("%.", digits, "f")
    sum_stats[, -c(1:4)] <- lapply(sum_stats[, -c(1:4)], function(x) sprintf(dig, as.numeric(x)))
    sum_stats
}

truncnames <- function(x) {
    names(x) <- substr(names(x), 1, 15)
    x
}
