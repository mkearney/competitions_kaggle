chardiffs <- function(q1, q2) {
    abs(nchar(q1) - nchar(q2))
}
word_overlap <- function(q1, q2) {
    sum(q1 %in% q2) + sum(q2 %in% q1)
}
word_overlap_p <- function(q1, q2) {
    x <- sum(q1 %in% q2, na.rm = TRUE) + sum(q2 %in% q1, na.rm = TRUE)
    x / (length(q1) + length(q2))
}
nchars <- function(q1, q2) {
    nchar(q1) + nchar(q2)
}
cor_abcs <- function(q1, q2) {
    foo <- function(q) strsplit(paste(q, collapse = ""), "")[[1]]
    q1 <- lapply(q1, foo)
    q2 <- lapply(q2, foo)
    mapply(cor_words, q1, q2, n = 16)
}

cor_words <- function(q1, q2, n = 8) {
    if (word_overlap(q1, q2) < n) return(0)
    lvs <- unique(c(q1, q2, use.names = FALSE))
    q1q2 <- lapply(list(q1, q2), factor, levels = lvs)
    f1 <- function(q) {
        q <- as.character(q)
        names(q) <- q
        q <- c(q, structure(rep(NA, sum(!lvs %in% q)),
                            names = lvs[!lvs %in% q]))
        factor(q, levels = lvs)
    }
    q1q2 <- lapply(q1q2, f1)
    f2 <- function(x) ifelse(is.na(x), NA, as.numeric(x))
    q1q2 <- lapply(q1q2, f2)
    f3 <- function(x, n) {
        if (length(x) < n) x <- c(x, rep(NA, n - length(x)))
        x
    }
    n <- vapply(q1q2, length, double(1))
    n <- max(n, na.rm = TRUE)
    q1q2 <- lapply(q1q2, f3, n = n)
    cor(q1q2[[1]], q1q2[[2]], use = "pairwise")
}

## ascii handler function
get_ascii <- function(x, ascii = FALSE) {
    i <- grep("\\+", iconv(x, "latin1", "ASCII", "+"),
              invert = ascii)
    if (all(vapply(i, length, double(1)) == 0))
        return(NA_character_)
    x <- iconv(x, "latin1", "utf-8", " ")
    x[i]
}
## utf8 encoded
get_utf8 <- function(x) iconv(x, "latin1", "utf-8", "")
## tokenize by word
wtoken <- function(q) {
    ## remove periods, comas, question marks
    q <- gsub("\\.|\\,|\\?|\\:|\\;|\\(|\\)|\\[|\\]|\\+|\\*|\\{|\\}",
              " ", tolower(q))
    ## remove spaces
    q <- gsub("[ ]{2,}", " ", gsub("[ ]${1,}", "", q))
    ## remove dashes (assume one word)
    q <- gsub("\u2014|\\-", "", q)
    ## split into words
    q <- strsplit(q, " ")
    ## convert to utf-8 and return
    lapply(q, get_utf8)
}
## filter for stopwords
fullstop <- function(q, fullstop = TRUE) {
    ## if non-null rm stopwords
    if (fullstop) {
        ## remove stopwords
        q <- lapply(q, function(x) x[!x %in% stopwords])
    }
    ## remove remaining punctuation
    q <- lapply(q, function(x) gsub("[[:punct:]]", "", x))
    ## replace empty with blank
    q[vapply(q, length, double(1)) == 0] <- NA_character_
    ## return words
    q
}
## word counts
nword <- function(q) {
    vapply(q, length, double(1))
}
## word count diff
nword_diff <- function(q1, q2) {
    q1 <- nword(q1)
    q2 <- nword(q2)
    q1 <- ifelse(is.na(q1), 0, q1)
    q2 <- ifelse(is.na(q2), 0, q2)
    abs(as.numeric(q1 - q2))
}
## char count diff
nchar_diff <- function(q1, q2) {
    if (any(is.na(c(q1, q2)))) return(0)
    abs(as.numeric(nchar(q1) - nchar(q2)))
}
## count matches
nmatch <- function(q1, q2) {
    ## sum matches fun
    f1 <- function(q1, q2) {
        if (!any(q1 %in% q2)) return(0)
        sums <- c(sum(q1 %in% q2, na.rm = TRUE),
                  sum(q2 %in% q1, na.rm = TRUE))
        sums[which.max(sums)]
    }
    ## return # of matches
    x <- mapply(f1, q1, q2)
    ## return as numeric
    as.numeric(x)
}
## random sample
rsamp <- function(x, n = 100) x[sample(seq_len(nrow(x)), n), ]
## match big words
bigwordmatch <- function(q1, q2) {
    ## function to get biggest word
    bigword <- function(x) x[order(nchar(x))][1]
    ## function to collapse other big words
    pc <- function(x, n = 2) {
        if (length(x) < n) n <- length(x)
        x <- x[order(nchar(x))][seq_len(n)]
        paste(x, collapse = "")
    }
    ## function to match big words
    f1 <- function(q1, q2) {
        all(grepl(bigword(q1), pc(q2)),
            grepl(bigword(q2), pc(q1)))
    }
    ## apply to each pair
    f2 <- function(q1, q2) mapply(f1, q1, q2)
    ## return # of big word matches
    as.numeric(f2(q1, q2))
}
## chunker
chunker <- function(q1, q2) {
    f0 <- function(x) strsplit(paste(x, collapse = ""), "")
    q1 <- lapply(q1, f0)
    q1 <- unlist(q1, recursive = FALSE)
    q2 <- lapply(q2, f0)
    q2 <- unlist(q2, recursive = FALSE)
    f1 <- function(n) {
        n <- n[n != " "]
        x <- lapply(seq(0, length(n), 3), function(x) c(1:3) + x)
        x <- lapply(x, function(i) paste(n[i], collapse = ""))
        grep("NA", x, invert = TRUE, value = TRUE)
    }
    ## apply function
    q1 <- lapply(q1, f1)
    q2 <- lapply(q2, f1)
    ## matching fun
    matching <- function(a, b) {
        sum(a %in% b, na.rm = TRUE)
    }
    x <- mapply(matching, q1, q2)
    as.numeric(x)
}
## who what when where why how
wwwwwh <- function(q1, q2) {
    types <- c("who", "what", "when", "where", "why", "how")
    w <- function(a, b, c) {
        mapply(function(a, b) grepl(c, paste(a, collapse = " ")) &
                              grepl(c, paste(b, collapse = " ")),
               q1, q2, USE.NAMES = FALSE)
    }
    w1 <- w(q1, q2, "who")
    w2 <- w(q1, q2, "what")
    w3 <- w(q1, q2, "when")
    w4 <- w(q1, q2, "where")
    w5 <- w(q1, q2, "why")
    w6 <- w(q1, q2, "how")
    mapply(sum, w1, w2, w3, w4, w5, w6, na.rm = TRUE,
           USE.NAMES = FALSE)
}
## first word match
wfirst <- function(q1, q2) {
    mapply(function(a, b) a[1] == b[1], q1, q2, USE.NAMES = FALSE)
}
## last word match
wlast <- function(q1, q2) {
    mapply(function(a, b) a[length(a)] == b[length(b)],
           q1, q2, USE.NAMES = FALSE)
}
## non words
nonwords <- function(q1, q2) {
    foo <- function(q) vapply(q, function(.)
        grepl("[^[:lower:]]", .), logical(1),
        USE.NAMES = FALSE)
    q1 <- lapply(q1, function(q) sum(foo(q), na.rm = TRUE))
    q2 <- lapply(q2, function(q) sum(foo(q), na.rm = TRUE))
    mapply(function(a, b) min(c(a, b), na.rm = TRUE), q1, q2)
}
## rankcor
wordorder <- function(q1, q2) {
    foo <- function(a, b) {
        x <- list(a = unique(a[a %in% b]),
                  b = unique(b[b %in% a]))
        x <- list(factor(x[[1]], levels = x[[1]]),
                  factor(x[[2]], levels = x[[1]]))
        x <- lapply(x, as.integer)
        x <- cor(x[[1]], x[[2]], method = "kendall")
        x[is.na(x)] <- 0
        x
    }
    mapply(foo, q1, q2)
}
## samesies
samesies <- function(q1, q2) {
    foo <- function(q1, q2) {
        x <- all(q1 %in% q2)
        if (x) return(x)
        q1 <- fullstop(q1)
        q2 <- fullstop(q2)
        if (length(q1) > 0 & length(q2) > 0) return(all(q1 %in% q2))
        FALSE
    }
    mapply(foo, q1, q2)
}
## convert text to numbers
txt2num <- function(x) {
    x <- iconv(x, "latin1", "ascii", "")
    x <- gsub("[^[:alnum:]_]", "", x)
    x <- gsub("[ ]{1,}", "", x)
    x <- tolower(x)
    x <- strsplit(x, "")
    lvls <- c(letters, as.character(0:9))
    x <- lapply(x, factor, levels = lvls)
    x <- lapply(x, as.numeric)
    x
}

## function to return min for each question pair
retmin <- function(a, b) {
    mapply(function(a, b) ifelse(a < b, a, b), a, b)
}

## logloss function
logloss <- function(actual, predicted, eps = 1e-15) {
    predicted = pmin(pmax(predicted, eps, na.rm = TRUE),
                     1 - eps, na.rm = TRUE)
    - (sum(actual * log(predicted) + (1 - actual) *
           log(1 - predicted), na.rm = TRUE)) / length(actual)
}

## convert chars to factors then numeric and correlate
abcors <- function(q1, q2) {
    ## numeric version
    n1 <- txt2num(q1)
    n2 <- txt2num(q2)

    ## length of each question
    l1 <- vapply(n1, length, double(1))
    l2 <- vapply(n2, length, double(1))

    n1zeros <- which(l1==0)
    n2zeros <- which(l2==0)

    ## apply to length vectors
    n12min <- retmin(l1, l2)

    ## zeroes
    zeros <- unique(c(n1zeros, n2zeros,
                      which(n12min==0)))

    ## replace zeros with random
    n1[zeros] <- replicate(length(zeros),
                           list(sample(0:20, 5)))
    n2[zeros] <- replicate(length(zeros),
                           list(sample(0:20, 5)))
    ## change length
    n12min[zeros] <- 5

    ## get cors values
    abcor <- mapply(function(a, b, n)
        cor(a[seq_len(n)], b[seq_len(n)], use = "pairwise"),
        n1, n2, n12min)
    abcor
}

count_commas <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("\\,", x), f, double(1))
}
count_periods <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("\\.", x), f, double(1))
}
count_uppers <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("[A-Z]", x), f, double(1))
}
count_numbers <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("[0-9]", x), f, double(1))
}
count_punts <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("[[:punct:]]", x), f, double(1))
}
count_pasttense <- function(x) {
    f <- function(x) sum(x > 0, na.rm = TRUE)
    vapply(gregexpr("[a-z]+ed | was | did| were", x), f, double(1))
}
