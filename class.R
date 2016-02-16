Stack <- function(data = NULL) {
    # ver 3.0
    nc = list(
        data <- list(),
        top <- function() {
            return(data[length(data)])
        },
        pop <- function() {
            d <- nc$top()
            data[length(data)] <- NULL
            return(d)
        },
        push <- function(e) {
            data[length(data) + 1] <- e
        },
        empty <- function() {
            data <- list()
        },
        isempty <- function() {
            return(is.null(nc$data))
        },
        main = function(data) {
            nc$data <- data
        }
    )
    nc$main(data)
    nc <- list2env(nc)
    class(nc) <- "Config"
    return(nc)
}
Config <- function(presetpath = './config.R') {
    # ver1.3.3
    nc = list(
        var.list = list(),
        val.list = list(),
        presetpath = NULL,
        get = function(x)
            nc[[x]],
        set = function(x, value)
            nc[[x]] <<- value,
        loadpreset = function(presetpath) {
            # Config setting
            # Read saved config.R file, if can't find, then use default.
            cat('Reading CONFIG.R as preset config...')
            if (file.exists(presetpath)) {
                config.file <- file(presetpath)
                config.lines <- readLines(config.file)
                close(config.file)
                
                # Seperate variables with values and store in var.list and val.list
                var.lines <- config.lines[1:(length(config.lines))]
                var.lines.list <-
                    strsplit0(var.lines, split = '=', simplify = FALSE)
                var.list <- list()
                val.list <- list()
                for (i in 1:length(var.lines.list)) {
                    var.list[[length(var.list) + 1]] <- var.lines.list[[i]][1]
                    val.list[[length(val.list) + 1]] <-
                        strsplit0(var.lines.list[[i]][2], split = ' ')
                }
            }
            nc$set('var.list', var.list)
            nc$set('val.list', val.list)
        },
        changevarval = function(var.list, val.list) {
            # Source
            var.list = nc$var.list
            val.list = nc$val.list
            # making config from user input
            printcursetup(var.list, val.list, '=')
            while (T)
                switch (
                    as.character(menu.config()),
                    '1' = {
                        printcursetup(var.list, val.list, '=')
                        choice <-
                            menu.edit.modify(length(var.list))
                        if (choice == 0)
                            next
                        i <- choice
                        cat(sprintf('Input your new configs\n'))
                        cat(sprintf(
                            'Previously, %d. %s = %s\n', i,
                            paste0(var.list[[i]]), paste0(collapse = ' ', val.list[[i]])
                        ))
                        input <- readline(prompt =
                                              sprintf('Now,        %d. %s = ', i, paste0(var.list[[i]])))
                        val.list[[i]] <-
                            strsplit0(input, split = ' ')
                        printcursetup(var.list, val.list, '=')
                        nc$set('var.list', var.list)
                        nc$set('val.list', val.list)
                    },  # Modify
                    '2' = {
                        printcursetup(var.list, val.list, '=')
                        while (T)
                            switch (
                                as.character(menu.save()),
                                '1' = {
                                    cat("-------Save_and_Overwrite-------\n")
                                    zz <-
                                        file('./config.R', 'w')
                                    for (i in 1:length(val.list)) {
                                        writeLines(
                                            sprintf(
                                                '%s=%s',
                                                paste0(collapse = ' ', var.list[[i]]),
                                                paste0(collapse = ' ', val.list[[i]])
                                            ),
                                            con = zz,sep =
                                                "\n"
                                        )
                                    }
                                    close(zz)
                                    printcursetup(var.list, val.list, '=')
                                    break
                                },  # Save_and_Overwrite
                                '2' = {
                                    cat("----------------Save_as---------------\n")
                                    cat("Input file name. Leave empty as default\n")
                                    config.filename <-
                                        readline('Save as: ')
                                    if (config.filename == '') {
                                        zz = file(paste0(
                                            'config-', format(Sys.time(), "%Y%m%d%H%M%S"), '.R'
                                        ), 'w')
                                    }
                                    else {
                                        zz = file(paste0(config.filename, '.R'), 'w')
                                    }
                                    for (i in 1:length(val.list)) {
                                        writeLines(
                                            sprintf(
                                                '%s=%s',
                                                paste0(collapse = ' ', var.list[[i]]),
                                                paste0(collapse = ' ', val.list[[i]])
                                            ),
                                            con = zz,sep =
                                                "\n"
                                        )
                                    }
                                    l <-
                                        length(config.lines)
                                    writeLines(as.character(config.lines[(l -
                                                                              1):l]), con = zz, sep = '\n')
                                    close(zz)
                                    printcursetup(var.list, val.list, '=')
                                    break
                                },  # Save as
                                '0' = break
                            )
                        
                    },  # Save
                    '3' = {
                        cat('Choose your config file to load')
                        waitasec()
                        config.to.load <- file(choose.files())
                        config.lines <- readLines(config.to.load)
                        
                        # Seperate variables with values and store in var.list and val.list
                        var.lines <-
                            config.lines[1:(length(config.lines))]
                        var.lines.list <-
                            strsplit0(var.lines, split = '=', simplify = FALSE)
                        var.list <- list()
                        val.list <- list()
                        for (i in 1:length(var.lines.list)) {
                            var.list[[length(var.list) + 1]] <- var.lines.list[[i]][1]
                            val.list[[length(val.list) + 1]] <-
                                strsplit0(var.lines.list[[i]][2], split = ' ')
                        }
                        printcursetup(var.list, val.list, '=')
                        nc$set('var.list', var.list)
                        nc$set('val.list', val.list)
                        next
                    },  # Load
                    '9' = break,  # Continue
                    '0' = quit()   # Back
                )
        },
        recovernumeric = function(var.list = nc$var.list, val.list = nc$val.list) {
            if (is.null(var.list))
                return(null)
            for (index in 1:length(val.list)) {
                if (!is.na(as.numeric(val.list[[index]]))) {
                    val.list[[index]] <- as.numeric(val.list[[index]])
                }
            }  # recovering numeric
            nc$set('var.list', var.list)
            nc$set('val.list', val.list)
        },
        assignconfig = function(envir = sys.frame(which = -1L)) {
            nc$recovernumeric()
            val.list <- nc$val.list
            var.list <- nc$var.list
            for (index in 1:length(var.list))
                assign(var.list[[index]], val.list[[index]], envir = envir)
        },
        setup = function(presetpath = nc$presetpath) {
            nc$loadpreset(presetpath)
            nc$changevarval(nc$var.list, nc$val.list)
            nc$recovernumeric()
            # get var.list to namespace of Config
            for (i in 1:length(nc$var.list))
                nc$set(nc$var.list[[i]], nc$val.list[[i]])
        },
        main = function(presetpath) {
            nc$set('presetpath', presetpath)
        }
    )
    nc$main(presetpath)
    nc <- list2env(nc)
    class(nc) <- "Config"
    return(nc)
}
