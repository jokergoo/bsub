

### format in JS ### 

#' @importFrom utils getFromNamespace
formatFileSize = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplFileSize)
}

formatTimeDiff = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplTimeDiff)
}

formatNumber = function(table, columns) {
	getFromNamespace("formatColumns", ns = "DT")(table, columns, tplNumber)
}


# file size should be in bytes
tplFileSize = function(...) {
  "DTWidget.formatFileSize(data);"
}


# time diff should be in secs
tplTimeDiff = function(...) {
  "DTWidget.formatTimeDiff(data);"
}

tplNumber = function(...) {
  "DTWidget.formatNumber(data);"
}

### format in terminal table ###

convert_to_byte = function(x) {
    num = as.numeric(gsub("\\D", "", x))
    v = ifelse(grepl("K", x), num*1024, ifelse(grepl("M", x), num*1024^2, ifelse(grepl("G", x), num*1024^3, x)))
    suppressWarnings(as.numeric(v))
}

format_mem = function(x) {
    gsub(" (.)bytes", "\\1b", x)
}

format_difftime = function(x, add_unit = FALSE) {
    units(x) = "hours"
    t = as.numeric(x)

    hour = floor(t)
    min = floor((t - hour)*60)
    
    l = is.na(x)
    if(add_unit) {
        txt = paste0(hour, "h", ifelse(min < 10, paste0("0", min), min), "m")
    } else {
        txt = paste0(hour, ":", ifelse(min < 10, paste0("0", min), min))
    }
    txt[l] = "-"
    txt[t == 0] = "-"
    txt
}


convert_to_POSIXlt = function(x) {

    if(is.null(bsub_opt$parse_time)) {

        if(any(grepl("^\\w+\\s+\\d+\\s+\\d+:\\d+$", x))) { # Dec 1 18:00
            t = as.POSIXlt(x, format = "%b %d %H:%M")
        } else if(any(grepl("^\\w+\\s+\\d+\\s+\\d+:\\d+:\\d+$", x))) { # Dec 1 18:00:00
            t = as.POSIXlt(x, format = "%b %d %H:%M:%S")
        } else {                                        # Dec 1 18:00:00 2019
            t = as.POSIXlt(x, format = "%b %d %H:%M:%S %Y")
        }
    } else {
        t = bsub_opt$parse_time(x)
    }

    if(any(is.na(t) & x != "-")) {
        stop_wrap(qq("Cannot convert time string (e.g. '@{x[which(is.na(t))[1]]}'') to a `POSIXlt` object. Please set a proper parsing function for `bsub_opt$parse_time`. See ?bsub_opt for more details."))
    }

    if(inherits(t, "POSIXct")) t = as.POSIXlt(t)

    current_t = as.POSIXlt(Sys.time())
    l = t$year > current_t$year
    l[is.na(l)] = FALSE
    if(any(l)) {
        t[l]$year = t[l]$year - 1
    }

    return(t)
}

