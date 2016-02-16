# finiteIntegrate <- function(x, y, r = seq_along(y)) {
#     # VER 3.1
#     # return integral(0->x) y(r)
#     A <- 0
#     t <- 0
#     B <- 0
#     result <- 0
#     for (i in r[-length(r)]) {
#         A[i] <- (y[i] + y[i + 1]) / 2
#         t[i] <- (r[i] + r[i + 1]) / 2
#         B[i + 1] <- B[i] + A[i] * t[i]
#     }
#     B <- B[-1]
#     for (i in seq_along(x)) {
#         ind <- which(x[i] <= t)[1]
#         if (is.na(ind)) {
#             ind <- length(t)
#             x[i] <- t[ind - 1]
#         }
#         if (ind == 1) {
#             result[i] <- 0
#             next
#         }
#         t2 <- t[ind]
#         B2 <- B[ind]
#         t1 <- t[ind - 1]
#         B1 <- B[ind - 1]
#         result[i] <- B1 + (B2 - B1) * (x[i] - t1) / (t2 - t1)
#     }
#     return(result)
# }

fit.2norm <- function(y, x = seq_along(y), para, area.output = F) {
    # VER 3.1
    # Normal distribution + Normal distribution + linear baseline
    
    # Verify para
    if (class(para) != "numeric" | length(para) <= 7)
        warning('Function fit.2norm has no enough initial parameters')
    return(NULL)
    
    # First present the data in a data-frame
    r <- y
    tab <- data.frame(x = x, r = r)
    
    #Apply function nls
    # Load para
    mu1 <- para[1]
    sigma1 <- para[2]
    k1 <- para[3]
    mu2 <- para[4]
    sigma2 <- para[5]
    k2 <- para[6]
    a <- para[7]
    b <- para[8]
    res <- NULL
    try({
        res <-
            (nls(
                r ~ k1 * exp(-1 / 2 * (x - mu1) ^ 2 / sigma1 ^ 2) +
                    k2 * exp(-1 / 2 * (x - mu2) ^ 2 / sigma2 ^ 2) +
                    a * x + b,
                start = c(
                    k1 = k1, mu1 = mu1, sigma1 = sigma1,
                    k2 = k2, mu2 = mu2, sigma2 = sigma2,
                    a = a, b = x[1]
                ) ,  # a = 1e-10,
                data = tab
            ))
    }, silent = T)
    if (is.null(res)) {
        cat(sprintf('nls curve fit failed @ \n'))
        return(NULL)
    }
    v <- summary(res)$parameters[,"Estimate"]
    fit <-
        function(x)
            v[1] * exp(-1 / 2 * (x - v[2]) ^ 2 / v[3] ^ 2) +
        v[4] * exp(-1 / 2 * (x - v[5]) ^ 2 / v[6] ^ 2) +
        v[7] * x + v[8]
    y <- fit(seq_along(x))
    if (!area.output)
        return(y)
    fit.baseline <- function(x)
        v[4] * x + v[5]
    baseline <- fit.baseline(x)
    area <- finiteIntegrate(x = x, y = y - baseline)
    return(area)
}

fit.norm <- function(y, x = seq_along(y), para, area.output = F) {
    # VER 3.1
    # Normal distribution + linear baseline
    
    # Verify para
    if (class(para) != "numeric" | length(para) <= 4) {
        warning('Function fit.2norm has no enough initial parameters')
        return(NULL)
    }
    
    # First present the data in a data-frame
    r <- y
    tab <- data.frame(x = x, r = r)
    
    #Apply function nls
    # Load para
    mu <- para[1]
    sigma <- para[2]
    k <- para[3]
    a <- para[4]
    b <- para[5]
    res <- NULL
    try({
        res <-
            (nls(
                r ~ k * exp(-1 / 2 * (x - mu) ^ 2 / sigma ^ 2) + a * x + b,  # +a*x
                start = c(
                    mu = mu, sigma = sigma, k = k, a = a, b = x[1]
                ) ,  # a = 1e-10,
                data = tab
            ))
    }, silent = T)
    
    if (is.null(res)) {
        cat(sprintf('nls curve fit failed @ \n'))
        return(NULL)
    }
    
    v <- summary(res)$parameters[,"Estimate"]
    fit <-
        function(x)
            v[3] * exp(-1 / 2 * (x - v[1]) ^ 2 / v[2] ^ 2) +  v[4] * x + v[5]  # v[4]*x +
    y <- fit(x)
    if (!area.output)
        return(y)
    fit.baseline <- function(x)
        v[4] * x + v[5]
    baseline <- fit.baseline(x)
    area <- finiteIntegrate(x = x, y = y - baseline)
    return(area)
}

fit.smooth <-
    function(y, x = seq_along(y), r = x, para = NULL) {
        # VER 3.3.1
        smooth.y <- NULL
        try({
            smooth.y <- smooth.spline(x, y)$y
        }, silent = T)
        if (is.null(smooth.y)) {
            cat(sprintf('Fit.smooth Failed'))
            return(NULL)
        }
        #         if (!area.output)
        return(smooth.y)
        #         area <- finiteIntegrate(x = x, y = smooth.y, r = x)
        #         return(area)
    }

fit.curve <- function(x, mode = 'fit.smooth', config) {
    # Ver 3.1
    curve <- NULL
    if (mode == 'fit.norm') {
        para <-
            c(config$mu1, config$sigma1, config$k1, config$a, config$b)
        curve <- fit.norm(x, para = para)
    } else if (mode == 'fit.smooth') {
        curve <- fit.smooth(x)
    } else if (mode == 'fit.2norm') {
        para <- c(
            config$mu1, config$sigma1, config$k1,
            config$mu2, config$sigma2, config$k2,
            config$a, config$b
        )
        curve <- fit.2norm(x, para = para)
    } else {
        cat('curve fit model does not exist\n')
        return(NULL)
    }
    # return NULL if no peaks or no velleys
    if (is.null(curve)) {
        warning('Curve not fit\n')
        return(NULL)
    } else {
        return(curve)
    }
}


get.first.valley <- function(curve){
    # VER 3.4.1
    # get valley --> valley1
    if(is.null(valley1 <- get.valley(curve))) {
        return(NULL)
    } else {
        # get peak to the left --> peak1
        if(is.null(peak <- get.peak(curve[ 1:valley1 ])))
            # -> no peak return valley1
            return(valley1)
        else {
            # -> has peak get valley to the left
            if(is.null(valley2 <- get.valley(curve[ 1:peak ])))
                # -> no valley return valley1
                return(valley1)
            else
                # -> has valley get first valley [1, peak]
                return(get.first.valley(curve[1:peak]))
        }
    }
}

get.first.peak <- function(curve){
    # ver 3.4.1
    # get peak --> peak1
    if(!is.null(peak1 <- get.peak(curve))) {
        # get valley to the left --> valley1
        if(!is.null(valley <- get.valley(curve[ 1:peak1 ]))) {
            # -> has valley get peak to the left
            if(!is.null(peak2 <- get.peak(curve[ 1:valley ]))) {
                # -> has peak get first peak [1, valley.x]
                return(get.first.peak(curve[ 1:valley ]))
            } else {
                # -> no peak return peak1
                return(peak1)
            }
        } else {
            # -> no valley return peak1 
            return(peak1)
        }
    } else {
        return(NULL)
    }
}

get.valley <- function(curve, x = seq_along(curve)) {
    #Ver 3.4.1
    curve.min <- min(curve)
    len = length(curve)
    if (len == 1)
        return (NULL)
    if (curve.min == curve[len])
        return(get.valley(curve[1:len - 1], x[1:len - 1]))
    if (curve.min == curve[1])
        return(get.valley(curve[2:len], x[2:len]))
    return(x[which(curve == curve.min)])
}

get.peak <- function(curve, x = seq_along(curve)) {
    #Ver 3.4.1
    curve.max <- max(curve)
    len = length(curve)
    if (len == 1)
        return (NULL)
    if (curve.max == curve[len])
        return(get.peak(curve[1:len - 1], x[1:len - 1]))
    if (curve.max == curve[1])
        return(get.peak(curve[2:len], x[2:len]))
    return(x[which(curve == curve.max)])
}

get.2valleys <- function(curve) {
    # VER 3.4.1
    # 
    if(is.null(valley <- get.valley(curve))) return(NULL)
    
    if(is.null(peak <- get.peak(curve))) return(NULL)

    if(peak < valley) {
        if(!is.null(y <- get.valley(curve[1:peak])))
            valley[2] <- y
        else return()
    } else {
        if(!is.null(u <- get.valley(curve[peak:length(curve)])))
            valley[2] <- u
        else return()
    }
    
    return(valley)
}

find.peak <- function(x, mode = 'fit.smooth', config) {
    # Ver 3.4.1
    curve <- fit.curve(x, mode = mode, config)
    if (is.null(curve)) {
        cat(sprintf('Curve fit failed, no peak found!'))
        return(NULL)
    }
    
    # Ver 3.4.1
    curve.max <- curve[curve.max.p <- get.peak(curve)]
    curve.min <- curve[curve.min.p <- get.valley(curve)]
    
    if(is.nothing(curve.max)|is.nothing(curve.min)) return(NULL)
    return(c(curve.max - curve.min, curve.min.p, curve.max.p))
#     poi <- floor((curve.max.p + curve.min.p) / 2)
#     voi <- curve[poi]
#     if(is.na(poi)|is.na(voi)) return(NULL) # VER 3.3.2
#     if (curve[1] < voi) {
#         # Positive Peak
#         return(c(curve.max - curve.min, curve.max.p))
#     } else if (voi < curve[1]) {
#         # Negative Peak
#         return(c(curve.max - curve.min, curve.min.p))
#     } else {
#         warning('find.peak() UNKNOWN ERROR \n')
#         return(NULL)
#     }
}

# fit.area <-
#     function(y, x = seq_along(y), mode = 'fit.smooth', config) {
#         # Ver 3.4
#         area <- NULL
#         if (mode == 'fit.smooth') {
#             area <- fit.smooth(y, x = x)
#             # smooth.y <- fit.smooth(y, x = x)
#             # valleys <- get.2valleys(smooth.y)
#         } else {
#             cat('area fit model does not exist\n')
#             return(NULL)
#         }
#         # return NULL if no peaks or no velleys
#         if (is.null(area)) {
#             warning('area not fit\n')
#             return(NULL)
#         } else {
#             return(area)
#         }
#     }
#    

# Ver 3.3.2

finiteIntegrate <- function(f, lower, upper, ..., subdivisions = 200L) {
    f <- match.fun(f)
    if(!is.finite(lower) | !is.finite(upper)) {
        # cat('finiteIntegrate infinite boundary!')  # TEST
        return(NULL)
    }
    dx <- (upper - lower) / subdivisions
    x <- seq(lower,upper,length=subdivisions)
    return(sum(f(x, ...)*dx))
}

get.baseline <- function(x, min.x, max.x, min.y, max.y) {
    baseline <- (max.y - min.y) / (max.x - min.x) * (x - max.x) + max.y
    return(baseline)
}

get.curve.y <- function(curve, x = seq_along(curve)) {
    if(sum(which(x < 1 | x > length(curve)))) return(NULL)
    curve.y <- c()
    ce <- ceiling(x)
    fl <- floor(x)
    curve.y[which(ce - fl == 0)] <- curve[fl][which(ce - fl == 0)]
    curve.y[which(ce - fl != 0)] <- 
        ((curve[ce] - curve[fl]) / (ce - fl) * (x - fl) + curve[fl])[which(ce - fl != 0)]
    return(curve.y)
}

areaFun <- function(r, curve, min.x, max.x, min.y, max.y, x = seq_along(1:max(ceiling(r)))) {
    baseline <- get.baseline(r, min.x, max.x, min.y, max.y)
    curve.y <- c()
    curve.y[which(r < min(x) | max(x) < r)] <- 0
    curve.y[which(min(x) <= r & r <= max(x))] <- 
        get.curve.y(curve, x = r[which(min(x) <= r & r <= max(x))])
    return(curve.y - baseline)
}

find.area <- function(y, x = seq_along(y), mode = 'fit.smooth', config) {
    # Ver 3.3.4
    
    curve <- fit.curve(y, mode = mode, config = config)
    
    if(config$area.method == 'full.peak') {
        valleys <- get.2valleys(curve)
        if(anyNothing(valleys)) return(N)
        min.x <- min(valleys)
        max.x <- max(valleys)
        min.y <- curve[min.x]
        max.y <- curve[max.x]
    } else if (config$ area.method == 'half.peak') {
        valley <- get.valley(curve)
        peak <- get.peak(curve)
        if(is.nothing(valley)|is.nothing(peak)) return(NULL)
        min.x <- ifelse0(valley < peak, valley, peak)
        max.x <- ifelse0(valley < peak, peak, valley)
        min.y <- curve[valley]
        max.y <- min.y
    }
    area <- NULL
    
    # VER 3.4.2
    baseline <- get.baseline(x, min.x, max.x, min.y, max.y)
    area <- sum((y - baseline)*diff(x))
    if (is.null(area)) {
        cat(sprintf('area fit failed, no area found!'))
        return(NULL)
    }
    return(area)
}

odd <- function(x)
    x[(1:length(x)) %% 2 != 0]

even <- function(x)
    x[(1:length(x)) %% 2 == 0]

evenb <- function(x)
    x[!odd(x)]
