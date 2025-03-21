cd <- function(text) {
    setwd(text)
}
#Sys.setenv(R_INTERACTIVE_DEVICE="quartz")
# source ("~/Documents/DBDA/openGraphSaveGraph.R")

aad <- function(x, na.rm=TRUE) {
    ## adapted from the lsr package
    if (na.rm) {
        x <- x[!is.na(x)]
    }
    y <- mean(abs(x - mean(x)))
    return(y)
}

retrodesign <- function(A, s, alpha=.05, df=Inf, n.sims=10000) {
    ## Code by Gelman & Carlin (2014): Persp. on Psy Science
    z <- qt(1-alpha/2, df)
    p.hi <- 1 - pt(z-A/s, df)
    p.lo <- pt(-z-A/s, df)
    power <- p.hi + p.lo
    typeS <- p.lo/power
    estimate <- A + s*rt(n.sims,df)
    significant <- abs(estimate) > s*z
    exaggeration <- mean(abs(estimate)[significant]/A)
    return(list(power=power,typeS=typeS,exaggeration=exaggeration))
}

standardizeCols = function( dataMat ) {
    zDataMat = dataMat
    for ( colIdx in 1:NCOL( dataMat ) ) {
        mCol = mean( dataMat[,colIdx] )
        sdCol = sd( dataMat[,colIdx] )
        zDataMat[,colIdx] = ( dataMat[,colIdx] - mCol ) / sdCol
    }
    return( zDataMat )
}

## basic mode calculation
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

## density-based mode calculation
estimate_mode <- function(x) {
    d <- density(x)
    d$x[which.max(d$y)]
}

lm.beta <- function (MOD) {
    b <- summary(MOD)$coef[-1, 1]
    sx <- sd(MOD$model[-1])
    sy <- sd(MOD$model[1])
    beta <- b * sx/sy
    return(beta)
}

## Seth's  preferred way of indicating significance in correlation tables.
minimum.sig.r <- function(N) {
    t.stat <- qt(.975,df=(N-2))
    r <- sqrt(t.stat^2/(t.stat^2 + N - 2))
    return(r)
}

## glm.beta <- function(MOD) {
##     b <- summary(MOD)$coef[-1,1]
##     sx <- sd(MOD$model[-1])
##     beta <- b*sx
##     return(beta)
## }

rad.2.degrees <- function(rad) rad*(360/(2*pi))
deg.2.rad <- function(deg) deg*((2*pi)/360)
