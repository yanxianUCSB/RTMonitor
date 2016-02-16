readline0 <- function(fmt = '', ...) {
    return(readline(prompt = sprintf(fmt, ...)))
}  # readline modified @ver1.2
input.number <-
    function(fmt = 'Please Enter a Number >> ', default = NA, ...) {
        while (T) {
            input.anything <- readline(prompt = sprintf(fmt, ...))
            if (input.anything == '')
                return(default)
            if (!is.na(as.numeric(input.anything)))
                return (as.numeric(input.anything))
            tryagain()
        }
    }
input.string <-
    function(fmt = 'Please Enter a String >> ', default = '', no.input = T, ...) {
        while (T) {
            input.anything <- readline0(fmt, ...)
            if (no.input & input.anything == '')
                return(default)
            if (!no.input & input.anything == '')
                tryagain()
            return(input.anything)
        }
    }
tryagain <- function() {
    cat('Please try again...\n')
}
waitasec <- function() {
    readline("Press Enter to continue >>")
}
printcursetup <- function(var.list, val.list, connection = '=') {
    # print current setup
    # Ver 1.1
    cat('                        \n')
    cat('     Current Setup      \n')
    cat('------------------------\n')
    for (i in 1:length(var.list)) {
        cat(sprintf(
            '%2d. %-20s %s %s\n',
            i,
            paste0(collapse = ' ', var.list[[i]]),
            connection,
            paste0(collapse = ' ', val.list[[i]])
        ))
    }
}  # print current setup @ver1.1
choosefrom <-
    function(from, prompt = '', default.index = 1, return.index = F) {
        while (T) {
            if (prompt == '')
                cat(sprintf("Choose item from list below\n"))
            cat(prompt)
            
            for (i in 1:length(from)) {
                cat(sprintf("%s. %s\n", i, from[[i]]))
            }
            
            index <-
                as.integer(as.numeric(input.number('>> ', default = default.index)))
            if (!(index %in% 1:length(from))) {
                tryagain()
                next
            }
            if (return.index)
                return(index)
            return(from[[index]])
        }
    }
read.csv0 <- function(path, skip = 0) {
    df <- NULL
    try({
        df <- read.csv(path[length(path)], skip = skip)
    }, silent = T)
    if (is.null(df)) {
        cat(sprintf('File access failed @ \n %s', basename(path)))
    }
    return(df)
}
ifelse0 <- function(logic, yes, no) {
    if (logic) {
        return(yes)
    } else {
        return(no)
    }
}
assign0 <- function(var, val, default = NA) {
    if (length(var) > length(val)) {
        assign0(var[1:length(val)], val)
        assign0(var[(length(val) + 1):length(var)], rep(default, length(var) -
                                                            length(val)))
    }else{
        for (i in 1:length(val)) {
            assign(var[i], val[i], envir = sys.frame())
        }
    }
    
}

## plot
plot.raw.n.fit <- function(df, config, mark, screen.n) {
    # VER 1.3.0
    screen(screen.n)
    erase.screen()
    flush.console()
    par(mar = config$margin)
    mode <- config$mode
    #     df <- NULL
    #     try({
    #         df <- read.csv(path, skip = skip)
    #     }, silent = T)
    #     if (is.null(df)) {
    #         cat(sprintf(
    #             'plot.raw.n.fit() > File access failed @ \n %s', basename(path)
    #         ))
    #         return(NULL)
    #     }
    #
    fc <- NULL
    try({
        fc <- fit.curve(df[,2], mode, config)
    }, silent = T)
    if (is.null(fc)) {
        cat(
            sprintf(
                'plot.raw.n.fit() > fit.curve() > Curve fit failed @ \n %s', basename(path)
            )
        )
        return(NULL)
    }
    plot(
        df[,1], df[,2],
        type = 'l',
        col = 'blue',
        main = mark,
        xlab = 'Voltage',
        ylab = 'Current'
    )
    par(new = T)
    plot(
        df[,1], fc,
        type = 'l',
        col = 2,
        axes = F,
        main = '',
        xlab = '',
        ylab = ''
    )
    par(new = F)
}
get.mark <- function(filename, marks, split) {
    # VER 1.0
    # mark this.file
    filename.parts <- strsplit0(filename, split = split)
    mark.index <- which(marks %in% filename.parts)
    if (length(mark.index) == 0)
        return(NA)
    mark <- marks[[mark.index]]
    return(mark)
}
plot.calculate <- function(data4plot, config, screen.n) {
    if (nrow(data4plot) == 0)
        return
    
    with(data4plot, {
        now <- timepassed[length(timepassed)]
        window.t <- config$window.t
        
        if (!is.numeric(window.t)) {
            timelim <- NULL
            
        } else {
            x.lo <- now - window.t
            x.hi <- now
            timelim <- c(x.lo, x.hi)
            
        }
        
        screen(screen.n)
        par(mar = config$margin)
        
        plot(
            timepassed, peak.y,
            type = 'l',
            xlim = timelim,
            main = paste0(marks[2], ' - ', marks[1]),
            xlab = 'time',
            ylab = 'Peak Current'
        )
        
        points(now, peak.y[length(peak.y)], col = 'red', pch = 16)
    })
}
is.nothing <- function(x){
    if(is.null(x)) return(T)
    else if (length(x)==0) return(T)
    else if (is.na(x)) return(T)
    else if (is.nan(x)) return(T)
    else return(F)
}
anyNothing <- function(v) {
    for(x in v){
        if(is.nothing(x)) return(T)
    }
    return(F)
}
