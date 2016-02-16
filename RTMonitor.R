# VER 3.4.2

rm(list = ls())

### Functions Definitions
source('./class.R')

source('./copyright.R')

source('./menu.R')

# string
source('./string.R')

# input and output
source('./io.R')

## math
source('./math.R')

# monitoring


### Initialization
{
### Working Directory
# workingDirectory <- dirname(sys.frame(1)$ofile)  # Test
# workingDirectory <- 'C:\\Users\\Jones Penn\\Google Drive\\Github\\R\\RTMonitor'
# setwd(workingDirectory)

## Copyright and Instruction
readme.copyright()
readme.instruction()

## Setting up configs
preset.path <- './config.R'
config = Config(preset.path)
config$setup()

### Monitoring Ver1.3.3

# Initialize
marks <- config$marks
Data.unsorted <- data.frame()
Data.sorted <- data.frame()
data4plot <- data.frame()
time.start <- Sys.time()
cur.file.list <- list()
psv.null.files <- list()  # Test Only

options(stringsAsFactors = FALSE)
scan.num <-
    as.integer(config$watching.time / config$watching.interval)


# split screen
#
screen.split <- config$screen.split
try(dev.off())
split.screen(screen.split)

if (config$plotMode == 1) {
    split.screen(screen = 1, c(1, length(marks)))
    split.screen(screen = 2, c(1, length(marks)))
}

if (config$plotMode == 2) {
    split.screen(screen = 1, c(1, length(marks)))
}

if (config$plotMode == 3) {
    split.screen(screen = 1, c(1, length(marks)))
    split.screen(screen = 2, c(1, length(marks)))
}
}



### scan

cur.file.list <- list()
if.first.sort <- TRUE
Data.normed <- data.frame()
df.stack <- data.frame()
first.peak <- NULL
timepassed <- c()
peak.y <- c()

for (scani in 1:scan.num) {
    cat('>')
    Sys.sleep(config$watching.interval)
    
    monitor.path <- paste0(config$monitor.path, collapse = ' ')
    
    # building Data.unsorted from file.list = setdiff(...)
    
    new.file.list <-
        list.files(monitor.path, pattern = '.txt', full.names = T)
    file.list <- setdiff(new.file.list, cur.file.list)
    cur.file.list <- new.file.list
    
    if (length(file.list) == 0)
        next
    
    
    # file.list --> Data.unsorted VER 3.1
    # file.list --> peak.y.us, peak.x.us VER 4.0
    data <- data.frame()
    for (this.file in file.list) {
        # Load data  VER 3.1
        #
        df <- NULL
        if (is.null(df))
            for (i in 1:3) {
                try({
                    df <- read.csv(this.file, skip = config$skipRow)
                }, silent = T)
            }
        if (is.null(df)) {
            next
        }
        
        # find peak and build Data.unsorted VER 1.3
        #
        peak <- find.peak(df[,2],
                          mode = config$mode,
                          config = config)
        
#         if(abs(peak[1])<1e-10) { # TEST
#             next
#         }
            
        # area <- 0  # TEST
        area <- find.area(df[,2],
                          mode = config$mode,
                          config = config)  # VER 3.2.2
        if (is.null(peak)) {
            psv.null.files <- append(psv.null.files, this.file)
            cat(this.file)
            cat('\n peak is null \n')
            next
        }
        if (is.null(area)) {
            cat(this.file)
            cat('\n area is null \n')
            area <- 0
        }

        
        ctime <- file.info(this.file)$ctime
        this.y <- peak[1]
        this.x <- peak[2]
        filename <- basename(this.file)
        this.mark <- get.mark(filename, marks, split = config$split)
        this.resi <- sub(this.mark, '', filename)
        this.timepassed <-
            difftime(ctime, time.start, units = config$unit.time)
        data <- rbind(
            data, list(
                time = ctime, peak.x = this.x, peak.y = this.y,
                mark = this.mark, filename = this.resi,
                timepassed = this.timepassed,
                path = this.file,
                area = area  # VER 3.2.2
            )
        )
    } # file.list --> Data.unsorted
    if (nrow(data) == 0) {
        next
    }
    
    
    # Data.unsorted <- rbind(Data.unsorted, data)
    Data.unsorted <- data
    print(basename(tail(data$path)))  # Test
    if (is.null(Data.unsorted$timepassed)) {
        cat('NON DATA TXT FILES EXISTS')
        next
    }
    
    
    # Sorting Data.unsorted --> Data.sorted
    #
    Data.sorted <- Data.unsorted[order(Data.unsorted$timepassed),]
    # Data.sorted <- rbind(Data.sorted, Data.unsorted[order(Data.unsorted$timepassed),])
    if (nrow(Data.sorted) == 0)
        next
    
    
    # Normalize Data.sorted VER 3.1
    #
    if (if.first.sort) {
        print('FIRST SORT')  # TEST
        
        for (i in 1:length(marks)) {
            this.mark <- marks[i]
            this.first.peak <-
                Data.sorted[which(Data.sorted$mark == this.mark),]$peak.y[1]
            if (!any(is.na(this.first.peak)) &
                !any(is.null(this.first.peak)))
                if (any(is.na(first.peak[i])) |
                    any(is.null(first.peak[i])))
                    first.peak[i] <- this.first.peak
        }
        if (length(marks) == sum(!is.na(first.peak)))
            if.first.sort <- F
    }
    # VER 3.1
    if (config$normalized == 1) {
        some.df <- data.frame()
        for (i in 1:length(marks)) {
            mark <- marks[i]
            subset.index <-
                !is.na(Data.sorted$mark) &
                (Data.sorted$mark == mark)
            this.df <- Data.sorted[subset.index,]
            this.df$peak.y <- this.df$peak.y / first.peak[i]
            some.df <- rbind(some.df, this.df)
        }
        Data.sorted <- some.df
    }
    
    # Ver 3.3
    if (config$plotMode == 1) {
        Data.normed <- rbind(Data.normed, Data.sorted)
        print(nrow(Data.normed))  # TEST
        
        # plot uncalculated data VER 1.2.6
        #
        for (this.mark in marks) {
            this.df.sub <-
                subset(x = Data.normed, mark == this.mark)
            if (nrow(this.df.sub) == 0)
                next
            
            # timepassed <- rbind(timepassed, this.df.sub$timepassed)
            # path <- rbind(path, this.df.sub$path)
            # peak.y <- rbind(peak.y, this.df.sub$peak.y)
            timepassed <- this.df.sub$timepassed
            path <- this.df.sub$path
            peak.y <- this.df.sub$peak.y
            now <- timepassed[length(timepassed)]
            window.t <- config$window.t
            if (!is.numeric(window.t)) {
                timelim <- NULL
            } else {
                x.lo <- now - window.t
                x.hi <- now
                timelim <- c(x.lo, x.hi)
            }
            
            df <-
                read.csv0(path[length(path)], skip = config$skipRow)
            if (is.null(df))
                next
            
            # plot raw data and fitting curve
            screen.n <-
                which(this.mark == marks) + screen.split[1] * screen.split[2]
            suc <- F
            try({
                plot.raw.n.fit(
                    df = df, config = config, mark = this.mark, screen.n = screen.n
                )
                suc <- T
            })
            if (!suc)
                next
            
            # plot history peak current
            screen.n <-
                which(this.mark == marks) + length(marks) + screen.split[1] * screen.split[2]
            this.sc.n <- NULL
            try({
                this.sc.n <- screen(screen.n)
            }, silent = T)
            if (is.null(this.sc.n)) {
                next
            } else {
                erase.screen()
                flush.console()
                par(mar = config$margin)
                plot(
                    timepassed, peak.y, type = 'l', xlim = timelim,
                    ylim = ifelse0(config$normalized == 1, config$ylim, NULL),  # VER 1.2.6
                    # main = this.mark,
                    xlab = 'time', ylab = 'Peak Current', axes = F
                )
                grid()
                at.time <- pretty(timepassed)
                labels.time <-
                    pretty(timepassed / config$scale.time)
                axis(1, at = at.time, labels = labels.time)
                at.y <- pretty(peak.y)
                labels.y <- pretty(peak.y / config$scale.y)
                axis(2, at = at.y, labels = labels.y)
                points(now, peak.y[length(peak.y)], col = 'red', pch = 16)
            }
        }
        
        next
    }
    
    # Ver 3.3.1  TODO:
    if (config$plotMode == 0) {
        print(paste('Data.sorted', nrow(Data.sorted)))  # TEST
        
        while (nrow(Data.sorted)!=0) {
            
            # Get current row
            # 
            this.row <- Data.sorted[1,]
            Data.sorted <- Data.sorted[-1,]
            
            # Get current area
            # Get current mark
            # Plot current curve and raw data
            # Plot current area with its corresponding history data
            # 
            
        }

    }
    
    
    if (config$plotMode == 2) {
        print(paste('Data.sorted', nrow(Data.sorted)))  # TEST
        
        while (nrow(Data.sorted) != 0) {
            # Data.sorted as a queue for current scan
            
            this.item <- Data.sorted[1,]
            Data.sorted <- Data.sorted[-1,]
            Data.normed <-
                rbind(Data.normed, this.item)  # Data.normed as a stack for future processing
            
            # In Data.normed get rows labeled with current filename -> thisFilename.df
            thisFilenameIndex <-
                which(Data.normed$filename == this.item$filename)
            thisFilename.df <- Data.normed[thisFilenameIndex,]
            thisdfmark1 <-
                with(thisFilename.df, thisFilename.df[which(mark == marks[1]),])
            thisdfmark2 <-
                with(thisFilename.df, thisFilename.df[which(mark == marks[2]),])
            
            
            # In thisFilename.df process and remove rows with marks[1] and marks[2]
            if (nrow(thisdfmark1) != 0 && nrow(thisdfmark2) != 0) {
                data4plot <-
                    rbind(
                        data4plot, list(
                            peak.y = thisdfmark2$peak.y - thisdfmark1$peak.y,
                            peak.x = mean(thisdfmark2$peak.x, thisdfmark1$peak.x),
                            timepassed = mean(
                                thisdfmark2$timepassed, thisdfmark1$timepassed
                            ),
                            mark = 'mark2 - mark1',
                            filename = this.item$filename
                        )
                    )
                Data.normed <- Data.normed[-thisFilenameIndex,]
            }
            
            
            #             # In Data.normed clean rows that are not labeled within the marks
            #             Data.normed.noMarkIndex <-
            #                 with(Data.normed, which(mark != marks[1]) & which(mark != marks[2]))
            #             Data.normed <- Data.normed[-Data.normed.noMarkIndex, ]
            
            print(paste('Data.normed', nrow(Data.normed))) # TEST
            
            
            ## VER 3.2
            #         Data.normed <- rbind(Data.normed, Data.sorted)
            #         print(nrow(Data.normed))  # TEST
            #
            #         # calculate difference
            #         # Data.normed --> data4plot
            #         #
            #         data4plot <- data.frame()
            #         filename.factor <- as.factor(Data.normed$filename)
            #
            #         for (thisfnf in levels(filename.factor)) {
            #             thisFilename.df <-
            #                 Data.normed[which(Data.normed$filename == thisfnf),]
            #             thisdfmark1 <-
            #                 with(thisFilename.df, thisFilename.df[which(mark == marks[1]),])
            #             thisdfmark2 <-
            #                 with(thisFilename.df, thisFilename.df[which(mark == marks[2]),])
            #             if (nrow(thisdfmark1) == 0 | nrow(thisdfmark2) == 0)
            #                 next
            #
            #             data4plot <-
            #                 rbind(
            #                     data4plot, list(
            #                         peak.y = thisdfmark2$peak.y - thisdfmark1$peak.y,
            #                         peak.x = mean(thisdfmark2$peak.x, thisdfmark1$peak.x),
            #                         timepassed = mean(thisdfmark2$timepassed, thisdfmark1$timepassed),
            #                         mark = 'mark2 - mark1',
            #                         filename = thisfnf
            #                     )
            #                 )
            #         }
            #         if (nrow(data4plot) == 0)
            #             next
            
            # plot raw data and fitting curve 1
            #
            if (nrow(thisdfmark1) == 0 | nrow(thisdfmark2) == 0)
                next
            
            screen.n <- 1 + screen.split[1] * screen.split[2]
            df <- read.csv0(thisdfmark1$path, skip = config$skipRow)
            if (is.null(df))
                next
            try({
                plot.raw.n.fit(
                    df = df, config = config, mark = thisdfmark1$mark, screen.n = screen.n
                )
            }, silent = F)
            
            # plot raw data and fitting curve 2
            #
            screen.n <- 2 + screen.split[1] * screen.split[2]
            df <- read.csv0(thisdfmark2$path, skip = config$skipRow)
            if (is.null(df))
                next
            try({
                plot.raw.n.fit(
                    df = df, config = config, mark = thisdfmark2$mark, screen.n = screen.n
                )
            }, silent = F)
            
            # Plot data4plot
            #
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
                
                screen.n <- 2
                
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
        
    }
    
    
    if (config$plotMode == 3) {
        print(paste('Data.sorted', nrow(Data.sorted)))  # TEST
        print(paste('Data.normed', nrow(Data.normed)))  # TEST
        
        while (nrow(Data.sorted) != 0) {
            # Data.sorted as a queue for current scan
            
            this.item <- Data.sorted[1,]
            Data.sorted <- Data.sorted[-1,]
            
            this.mark <- this.item$mark
            if (!this.mark %in% marks)
                next
            
            # Fetch data with this.mark in Data.normed
            #
            timepassed <- c(timepassed, this.item$timepassed)
            path <- this.item$path
            peak.y <- c(peak.y, this.item$area)
            now <- this.item$timepassed
            window.t <- config$window.t
            if (!is.numeric(window.t)) {
                timelim <- NULL
            } else {
                x.lo <- now - window.t
                x.hi <- now
                timelim <- c(x.lo, x.hi)
            }
            
            # Read current data.file
            #
            df <- read.csv0(path, skip = config$skipRow)
            if (is.null(df))
                next
            
            # plot raw data and fitting curve
            screen.n <-
                which(this.mark == marks) + screen.split[1] * screen.split[2]
            suc <- F
            try({
                plot.raw.n.fit(
                    df = df, config = config, mark = this.mark, screen.n = screen.n
                )
                suc <- T
            })
            if (!suc)
                next
            
            
            # plot history peak current
            #
            screen.n <-
                which(this.mark == marks) + length(marks) + screen.split[1] * screen.split[2]
            this.sc.n <- NULL
            try({
                this.sc.n <- screen(screen.n)
            }, silent = T)
            if (is.null(this.sc.n)) {
                next
            } else {
                erase.screen()
                flush.console()
                par(mar = config$margin)
                plot(
                    timepassed, peak.y, type = 'l', xlim = timelim,
                    ylim = ifelse0(config$normalized == 1, config$ylim, NULL),  # VER 1.2.6
                    # main = this.mark,
                    xlab = 'time', ylab = 'Peak Current', axes = F
                )
                grid()
                at.time <- pretty(timepassed)
                labels.time <-
                    pretty(timepassed / config$scale.time)
                axis(1, at = at.time, labels = labels.time)
                at.y <- pretty(peak.y)
                labels.y <- pretty(peak.y / config$scale.y)
                axis(2, at = at.y, labels = labels.y)
                points(now, peak.y[length(peak.y)], col = 'red', pch = 16)
            }
            
        }
        
    }
    
}
