##------------------------------------------------------------------
## <function> :: scaleData
##------------------------------------------------------------------
## med subtract and normalize by the standard deviation (but
## compensate for the presence of missing obserations in the data)
##------------------------------------------------------------------
scaleData   <- function(x) {
    x.scaled    <- (x - mean(x, na.rm=TRUE))/sd(x,  na.rm=TRUE)
    return(x.scaled)
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: createDateFlags
##------------------------------------------------------------------
## A simple procedure to load a flag vector corresponding to a series
## of dates (+/- a given lag [in weeks])
##------------------------------------------------------------------
createDateFlags	<- function(d, t, n.lag=0) {
    
	d		<- as.Date(d)
	nd		<- length(d)
	idx		<- vector("numeric", length=nd)
	
	t.orig	<- as.Date(t)
	t		<- 0*(1:length(t.orig))
	
	for (i in 1:nd) {
		idx[i] 		<- min( which(t.orig >= d[i]) )
		t[idx[i]] 	<- 1
	}
	
	if (n.lag != 0) {
		t <- Lag(t, shift=n.lag)
	}
	return(t)
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: maskClean
##------------------------------------------------------------------
## Expects data in the form of the walmart dataset (mydata) and a
## index vector of pixels to replace (mymask).  The function will
## replace the bad pixels with the median observation of the
## remaining data from that particular week in the year (nweek)
##------------------------------------------------------------------
maskClean   <- function(mymask, mydata) {
    
    ## extract the raw weekly_sales and nweek data
    ws   <- mydata$weekly_sales[-mymask]
    nw   <- mydata$nweek[-mymask]
    
    ## aggregate sales by nweek
    agg  <- aggregate(ws ~ nw, FUN=median)
    
    ## relace the bad data with the aggregated sales
    ws.c            <- mydata$weekly_sales
    nw.c            <- mydata$nweek
    ws.c[mymask]    <- agg$ws[mydata$nweek[mymask]]
    
    ## return the cleaned data
    return(ws.c)
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcMedianFit
##------------------------------------------------------------------
## Forecast the median of a timeseries forward h timesteps
##------------------------------------------------------------------
calcMedianFit <- function(x, h=39) {
    
	orig.x		<- x
	fitted		<- rep(median(x, na.rm=TRUE), length(x))
	forecast	<- rep(median(x, na.rm=TRUE), h)
	
	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=NULL, mae=mae))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcMeanFit
##------------------------------------------------------------------
## Forecast the mean of a timeseries forward h timesteps
##------------------------------------------------------------------
calcMeanFit <- function(x, h=39) {
    
	orig.x		<- x
	fitted		<- rep(mean(x, na.rm=TRUE), length(x))
	forecast	<- rep(mean(x, na.rm=TRUE), h)
	
	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=NULL, mae=mae))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcZeroFit
##------------------------------------------------------------------
## Forecast zero forward h timesteps
##------------------------------------------------------------------
calZeroFit <- function(x, h=39) {
    
	orig.x		<- x
	fitted		<- rep(length(x), 0)
	forecast	<- rep(h, 0)
	
	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=NULL, mae=mae))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcStlFit
##------------------------------------------------------------------
## Forecast a timeseries forward h timesteps using stl() to
## decompose the timeseries into its season, trend, level
##------------------------------------------------------------------
calcStlFit <- function(x, h=39) {
    
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
    ## create a timeseries
    x.ts        <- ts(x, frequency=52, start=c(2010,5))

    ## compute the fit/forecast
    fit.stl     <- stlf(x.ts, h=h, s.window=7, robust=FALSE, lambda=lambda)
    fitted      <- as.vector(InvBoxCox(fit.stl$fitted, lambda=lambda))
    forecast    <- as.vector(InvBoxCox(fit.stl$mean, lambda=lambda))

	## compute the mean absolute error (in-sample)
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}

	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae, aic=fit.stl$model$aic, aicc=fit.stl$model$aicc, bic=fit.stl$model$bic))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcTslmFit
##------------------------------------------------------------------
## Forecast the median of a timeseries forward h timesteps
##------------------------------------------------------------------
calcTslmFit <- function(x, h=39) {
    
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
    ## create a timeseries
    x.ts        <- ts(x, frequency=52, start=c(2010,5))
    
    ## compute the fit/forecast
    fit.tslm    <- tslm(x.ts ~ trend + season, lambda=lambda)
    fitted      <- as.vector(InvBoxCox(fit.tslm$fitted, lambda=lambda))
    fc.tslm     <- forecast(fit.tslm, h=h)
    forecast    <- as.vector(InvBoxCox(fc.tslm$mean, lambda=lambda))

    
	## compute the mean absolute error (in-sample)
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
    
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae, aic=fit.tslm$model$aic, aicc=fit.tslm$model$aicc, bic=fit.tslm$model$bic))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcLmFit
##------------------------------------------------------------------
## Forecast a timeseries forward h timesteps
##------------------------------------------------------------------
calcLmFit <- function(mydata, h=39) {
    
    ## assign the assign vector
    x <- mydata[(mydata$tr_fl == 1), c("ws.min01")]
    
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
    ## map the vector back to the dataset
    mydata[(mydata$tr_fl == 1), c("x")]    <- x
    
    #    ## compute the fit/forecast
    h           <- mydata[ (mydata$tr_fl == 1), ]
    p           <- mydata[ (mydata$tr_fl == 0), ]
    fit.lm      <- lm(x ~ factor(nweek) + time, data=h)
    fitted      <- as.vector(InvBoxCox(fit.lm$fitted, lambda=lambda))
    fc.lm       <- predict(fit.lm, newdata=p)
    forecast    <- as.vector(InvBoxCox(fc.lm, lambda=lambda))
    
    
	## compute the mean absolute error (in-sample)
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
    
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcLmMultiFit
##------------------------------------------------------------------
## Forecast a timeseries forward h timesteps
##------------------------------------------------------------------
#calcLmMultiFit <- function(mydata, wgt=NULL, regs=NULL, k=5, h=39) {
#
#    ## assign the assign vector
#    x <- mydata[(mydata$tr_fl == 1), c("ws.min10")]
#
#    ## do a box-cox transformation if all x > 0
#    if ( any(x < 0) ) {
#        orig.x <- x
#    } else {
#        orig.x  <- x
#        lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
#        x	    <- BoxCox(x, lambda)
#    }
#
#	## define the timeseries
#    y   <- ts(x, start=c(2010,5), freq=365.25/7)
#
#    ## compute a complete set of orders
#    z 	<- fourier(y, K=k)
#    zf	<- fourierf(y, K=k, h=h)
#
#    ## map the vector back to the dataset
#    mydata[(mydata$tr_fl == 1), c("x")]    <- x
#
#    ## append the fourier series
#    mydata  <- cbind(mydata, rbind(z, zf))
#
#    ## create the formula
#    tmp.frm     <- c("x ~ ")
#    if (!is.null(regs)) {
#        tmp.frm     <- paste(tmp.frm, paste(regs,collapse=" + "), sep="+")
#    }
#    tmp.frm     <- as.formula(tmp.frm)
#
#    #    ## compute the fit/forecast
#    h           <- mydata[ (mydata$tr_fl == 1), ]
#    p           <- mydata[ (mydata$tr_fl == 0), ]
#    fit.lm      <- lm(tmp.frm, data=h)
#    fit.lm.2    <- stepAIC(fit.lm)
#    fitted      <- as.vector(InvBoxCox(fit.lm.2$fitted, lambda=lambda))
#    fc.lm       <- predict(fit.lm.2, newdata=p)
#    forecast    <- as.vector(InvBoxCox(fc.lm, lambda=lambda))
#
#
#	## compute the mean absolute error (in-sample)
#	if ( !is.null(forecast) ) {
#		mae	<- sum(abs(orig.x-fitted))/length(x)
#	}
#
#	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
#	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae, coeffs=coefficients(fit.lm.2)))
#}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcQrMultiFit
##------------------------------------------------------------------
## Forecast a timeseries forward h timesteps
##------------------------------------------------------------------
calcQrMultiFit <- function(mydata, regs=NULL, wgt=NULL, h=39) {
    
    ## assign the assign vector
    x <- mydata[(mydata$tr_fl == 1), c("ws.min01")]
    
	## *** EFFECTIVELY NO BOX COX ***
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		#lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
        lambda	<- 1
		x	    <- BoxCox(x, lambda)
	}
    
    ## map the vector back to the dataset
    mydata[(mydata$tr_fl == 1), c("x")]    <- x
    
    ## create the formula
    tmp.frm     <- c("x ~ factor(nweek) + time")
    if (!is.null(regs)) {
        tmp.frm     <- paste(tmp.frm, paste(regs,collapse=" + "), sep="+")
    }
    tmp.frm     <- as.formula(tmp.frm)
    
    ## compute the weight vector
    if ( is.null(wgt) ) {
        w <- rep(1, nrow(mydata[(mydata$tr_fl == 1), c("x")]))
        w <- w / sum(w)
    } else {
        w <- wgt[(mydata$tr_fl == 1)]
        w <- w / sum(w)
    }
    
    ## compute the fit/forecast
    h           <- mydata[ (mydata$tr_fl == 1), ]
    p           <- mydata[ (mydata$tr_fl == 0), ]
    fit.rq      <- rq(tmp.frm, tau=0.5, weights=w, data=h)
    fit.rq.2    <- stepAIC(fit.rq)
    fitted      <- as.vector(InvBoxCox(fit.rq.2$fitted, lambda=lambda))
    fc.rq       <- predict(fit.rq.2, newdata=p)
    forecast    <- as.vector(InvBoxCox(fc.rq, lambda=lambda))
    
    
	## compute the mean absolute error (in-sample)
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	}
    
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae, coeffs=coefficients(fit.rq.2)))
}


##########################################################################################

##------------------------------------------------------------------
## <function> :: genFourier
##------------------------------------------------------------------
## Generate fourier terms; Similar to the forecast::fourier
## funciton, but the period is explicitly passed (from Hyndsight)
##------------------------------------------------------------------
genFourier <- function(t, terms, period) {
    n <- length(t)
    x <- matrix(, nrow=n, ncol=2*terms)
    for (i in 1:terms) {
        x[,2*i-1]   <- sin(2*pi*i*t/period)
        x[,2*i]     <- cos(2*pi*i*t/period)
    }
    colnames(x) <- paste(c("S","C"), rep(1:terms, rep(2,terms)), sep="")
    return(x)
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcFourierOrder
##------------------------------------------------------------------
calcFourierOrder <- function(x, id="s/d", regs.hist=NULL, min.order=5, max.order=30, min.boxcox=0, max.boxcox=1, wgt=NULL, lambda=NULL) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x  <- x
        lambda  <- 1
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
	
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
    
    ## define a placeholder matrix for results
    bestmat <- matrix(, nrow=(max.order-min.order+1), ncol=4)
    
    ## compute fits for a range of harmonics
    for (i in seq(from=min.order, to=max.order, by=1)) {

        ## generate a data frame for the fit: x ~ fourier(k) + regressors
        if ( !is.null(regs.hist) ) {
            fit.df  <- data.frame(fourier(y, K=i), regs.hist)
        } else {
            fit.df  <- data.frame(fourier(y, K=i))
        }
            
        ## compute the arima fit
        fit  <- auto.arima(y, xreg=as.matrix(fit.df), seasonal=FALSE, trace=TRUE)
        
        ## record results
        bestmat[(i-min.order+1),] <- c(i, fit$aicc, fit$aic, fit$bic)
        
    }
    
    ## identify the order that minimized the aicc & refit
    k       <- bestmat[ which( (bestmat[,2] == min(bestmat[,2])) ) , 1][1]
    
    ## generate a data frame for the fit: x ~ t + fourier(k) + regressors
    if ( !is.null(regs.hist) ) {
        fit.df  <- data.frame(fourier(y, K=k), regs.hist)
    } else {
        fit.df  <- data.frame(fourier(y, K=k))
    }
    
    ## perform the fit
    bestfit <- auto.arima(y, xreg=as.matrix(fit.df), seasonal=FALSE, trace=TRUE)
    fitted	<- InvBoxCox(as.vector(fitted(bestfit)), lambda)
    
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(    x=orig.x,
                    id=id,
                    fitted=fitted,
                    lambda=lambda,
                    k=k,
                    residuals=(orig.x-fitted),
                    res=bestmat,
                    regs=regs.hist,
                    arma=bestfit$arma))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcGlmOrderSearch
##------------------------------------------------------------------
## Use linear regression to get a ballpark estimate of the number of
## harmonics to use in subsequent fits.  This approach is based on
## the procedure outlined in the de Silvo (?) thesis.
##------------------------------------------------------------------
calcGlmOrderSearch <- function(x, min.order=5, max.order=30, min.boxcox=0, max.boxcox=1, wgt=NULL) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=min.boxcox, upper=max.boxcox)
		x	    <- BoxCox(x, lambda)
	}
	
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
    
    ## define a placeholder matrix for results
    bestmat <- matrix(, nrow=(max.order-min.order+1), ncol=2)
    
    ## increment the number of harmonics and fit (see the de Silvo thesis)
    for (i in seq(from=min.order, to=max.order, by=1)) {
    
        ## generate a data frame for the fit: x ~ t + fourier(k)
        fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=i))
        
        ## do a simple linear regression of the fourier terms
        fit2     <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")

        ## record results
        bestmat[(i-min.order+1),] <- c(i, fit2$aic)
    }
    
    ## identify the order that minimized the aicc & refit
    k       <- bestmat[ which( (bestmat[,2] == min(bestmat[,2])) ) , 1][1]
    fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=k))
    bestfit <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")
    fitted	<- InvBoxCox(as.vector(fitted(bestfit)), lambda)
    fit.sum <- summary(bestfit)
    
	## return the results
	return(list(    x=orig.x,
                    fitted=fitted,
                    resid=fitted-orig.x,
                    lambda=lambda,
                    k=k,
                    res=bestmat,
                    aic=bestfit$aic,
                    coef=coefficients(bestfit),
                    coef.names=names(coefficients(bestfit)),
                    sderr=fit.sum$coefficients[,2],
                    tval=fit.sum$coefficients[,3]))
}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcGlmVariableSearch
##------------------------------------------------------------------
calcGlmVariableSearch <- function(x, regs.hist=NULL, min.order=5, max.order=30, min.boxcox=0, max.boxcox=1, wgt=NULL) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=min.boxcox, upper=max.boxcox)
		x	    <- BoxCox(x, lambda)
	}
	
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
    
    ## define a placeholder matrix for results
    bestmat <- matrix(, nrow=(max.order-min.order+1), ncol=2)
    
    ## compute even orders only
    for (i in seq(from=min.order, to=max.order, by=1)) {
        
        ## generate a data frame for the fit: x ~ t + fourier(k) + regressors
        if ( !is.null(regs.hist) ) {
            fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=i), regs.hist)
        } else {
            fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=i))
        }
        
        ## do a simple linear regression of the fourier terms
        fit2     <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")
        
        ## record results
        bestmat[(i-min.order+1),] <- c(i, fit2$aic)
    }
    
    ## identify the order that minimized the aicc & refit
    k       <- bestmat[ which( (bestmat[,2] == min(bestmat[,2])) ) , 1][1]
    
    ## generate a data frame for the fit: x ~ t + fourier(k) + regressors
    if ( !is.null(regs.hist) ) {
        fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=k), regs.hist)
    } else {
        fit.df  <- data.frame(x=x, t=1:length(x), fourier(y, K=k))
    }
    bestfit <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")
    fitted	<- InvBoxCox(as.vector(fitted(bestfit)), lambda)
    fit.sum <- summary(bestfit)
    
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(    x=orig.x,
                    fitted=fitted,
                    lambda=lambda,
                    k=k,
                    res=bestmat,
                    aic=bestfit$aic,
                    coef=coefficients(bestfit),
                    coef.names=names(coefficients(bestfit)),
                    sderr=fit.sum$coefficients[,2],
                    tval=fit.sum$coefficients[,3]))

}

##########################################################################################

##------------------------------------------------------------------
## <function> :: calcGlmForecast
##------------------------------------------------------------------
calcGlmForecast <- function(x, coeffs=NULL, regs.hist=NULL, regs.proj=NULL, min.order=5, max.order=30, min.boxcox=0, max.boxcox=1, wgt=NULL, k=40, h=39) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
	
    ## compute a complete set of fourier orders
    z 	<- fourier(y, K=k)
    zf	<- fourierf(y, K=k, h=h)
    
    ## isolate relevant fourier coefficients
    if ( !is.null(coeffs)) {
        if ( !any(is.na(coeffs)) ) {
            if ( length(which(colnames(z) %in% coeffs)) > 0 ) {
                z   <- z[ , coeffs ]
                zf	<- zf[ , coeffs ]
            }
        }
    }
    
    ## append regressors if they exist
	if ( !is.null(regs.hist) ) {
        fit.df      <- data.frame(x=x, regs.hist, z)
        proj.df     <- data.frame(regs.proj, zf)
    } else {
        fit.df      <- data.frame(x=x, z)
        proj.df     <- data.frame(zf)
    }
    
    ## do the fit
    fit2        <- stepAIC(glm(x ~ . , data=fit.df, weights=wgt, family="gaussian"), direction="both")
    fitted      <- InvBoxCox(as.vector(fitted(fit2)), lambda)
    fc          <- predict(fit2, newdata=proj.df)
    forecast    <- as.vector(InvBoxCox(fc, lambda=lambda))

	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	} else {
		mae <- NULL
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, residuals=(fitted-orig.x), lambda=lambda, mae=mae))
}


##########################################################################################

##------------------------------------------------------------------
## <function> :: calcArimaForecast
##------------------------------------------------------------------
calcArimaForecast <- function(x, coeffs=NULL, regs.hist=NULL, regs.proj=NULL, k=5, h=39) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x  <- x
        lambda  <- 1
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
	
	## append regressors if they exist & do the fit
	if ( is.null(regs.hist) ) {
        fit         <- auto.arima(y, seasonal=FALSE, approximation=TRUE, allowdrift=TRUE, trace=FALSE)
		fitted		<- InvBoxCox(as.vector(fitted(fit)), lambda)
		fc 			<- forecast(fit, h=h)
		forecast	<- InvBoxCox(as.vector(fc$mean), lambda)
	} else {
        fit         <- auto.arima(y, xreg=regs.hist, seasonal=FALSE, approximation=TRUE, allowdrift=TRUE, trace=FALSE)
		fitted		<- InvBoxCox(as.vector(fitted(fit)), lambda)
		fc			<- forecast(fit, xreg=regs.proj, h=h)
		forecast	<- InvBoxCox(as.vector(fc$mean), lambda)
	}
	
    ## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	} else {
		mae <- NULL
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae))
}


##########################################################################################

##------------------------------------------------------------------
## <function> :: calcFourierFit
##------------------------------------------------------------------
calcFourierFit <- function(x, coeffs=NULL, regs.hist=NULL, regs.proj=NULL, k=5, h=39) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
	
    ## compute a complete set of orders
    z 	<- fourier(y, K=k)
    zf	<- fourierf(y, K=k, h=h)
    
    ## isolate relevant fourier coefficients
    if ( !is.null(coeffs)) {
        if ( !any(is.na(coeffs)) ) {
            if ( length(which(colnames(z) %in% coeffs)) > 0 ) {
                z   <- z[ , coeffs ]
                zf	<- zf[ , coeffs ]
            }
        }
    }
	
	## no regressors
	if ( is.null(regs.hist) ) {
        fit         <- auto.arima(y, xreg=z, seasonal=FALSE, approximation=TRUE, allowdrift=TRUE, trace=TRUE)
		fitted		<- InvBoxCox(as.vector(fitted(fit)), lambda)
		fc 			<- forecast(fit, xreg=zf, h=h)
		forecast	<- InvBoxCox(as.vector(fc$mean), lambda)
    ## regressors
	} else {
        fit         <- auto.arima(y,xreg=cbind(z,regs.hist), seasonal=FALSE, approximation=TRUE, allowdrift=TRUE, trace=TRUE)
		fitted		<- InvBoxCox(as.vector(fitted(fit)), lambda)
		fc			<- forecast(fit, xreg=cbind(zf,regs.proj), h=h)
		forecast	<- InvBoxCox(as.vector(fc$mean), lambda)
	}
	
	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	} else {
		mae <- NULL
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(
                x=orig.x,
                fitted=fitted,
                forecast=forecast,
                lambda=lambda,
                mae=mae,
                coeffs=coeffs,
                regs=regs.hist,
                k=length(coeffs)/2))
}


##########################################################################################

##------------------------------------------------------------------
## <function> :: calcFourierNoArima
##------------------------------------------------------------------
calcFourierNoArima <- function(x, coeffs=NULL, regs.hist=NULL, regs.proj=NULL, wgt=NULL, k=40, h=39) {
	
	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0, upper=1)
		x	    <- BoxCox(x, lambda)
	}
    
	## define the timeseries
    y   <- ts(x, start=c(2010,5), freq=365.25/7)
	
    ## compute a complete set of orders
    z 	<- fourier(y, K=k)
    zf	<- fourierf(y, K=k, h=h)
    
    ## isolate relevant fourier coefficients
    if ( !is.null(coeffs)) {
        if ( !any(is.na(coeffs)) ) {
            if ( length(which(colnames(z) %in% coeffs)) > 0 ) {
                z   <- z[ , coeffs ]
                zf	<- zf[ , coeffs ]
            }
        }
    }
    
    ## append regressors if they exist
	if ( !is.null(regs.hist) ) {
        fit.df      <- data.frame(x=x, regs.hist, z)
        fit2         <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")
        #fit2        <- stepAIC(fit, direction="backward", trace=2)
        fitted      <- InvBoxCox(as.vector(fitted(fit2)), lambda)
        proj.df     <- data.frame(regs.proj, zf)
        fc          <- predict(fit2, newdata=proj.df)
        forecast    <- as.vector(InvBoxCox(fc, lambda=lambda))
    } else {
        fit.df      <- data.frame(x=x, z)
        fit2        <- glm(x ~ . , data=fit.df, weights=wgt, family="gaussian")
        #fit2        <- stepAIC(fit, direction="backward", trace=2)
        fitted      <- InvBoxCox(as.vector(fitted(fit2)), lambda)
        proj.df     <- data.frame(zf)
        fc          <- predict(fit2, newdata=proj.df)
        forecast    <- as.vector(InvBoxCox(fc, lambda=lambda))
    }

	## compute the mean absolute error
	if ( !is.null(forecast) ) {
		mae	<- sum(abs(orig.x-fitted))/length(x)
	} else {
		mae <- NULL
	}
	
	## return the original vector, the in-sample, out-of-sample, and box-cox parameter
	return(list(x=orig.x, fitted=fitted, forecast=forecast, lambda=lambda, mae=mae, k=k, coef=coefficients(fit2), aic=fit2$aic))
}




##########################################################################################

##------------------------------------------------------------------
## <function> :: calcRegressionFit
##------------------------------------------------------------------
## The purpose of this function is to calculate a simple regression-
## based forecast. The regressors inlude a linear time component
## as well as optional binary variables to handle holidays.  
##
## Initial Verison: 02/28/2014
##------------------------------------------------------------------
calcRegressionFit <- function(x, t, do.nweek=NULL, regs=NULL, ft=NULL, fregs=NULL, fh=39) {

	#browser()
	
	## define the seasonality period
	weeks.per.year	<- 52

	## do a box-cox transformation if all x > 0
	if ( any(x < 0) ) {
		orig.x <- x
	} else {
		orig.x  <- x
		lambda	<- BoxCox.lambda(x, method="guerrero", lower=0)
		x	    <- BoxCox(x, lambda)
	}
	
	## create a data frame and formula for the fit; check first for the do.nweek flag
	if ( !is.null(do.nweek) ) {
	
	
	} else {
		if ( is.null(regs) ) {
			tmp.df		<- data.frame(x=x, t=t)
			tmp.formula	<- as.formula("x ~ t")
		} else {
			tmp.df		<- data.frame(x=x, t=t, regs)
			tmp.formula	<- as.formula(paste("x ~ t + ", paste(names(regs), collapse=" + "), sep=""))
		}
	
	}
	
	## compute the regression
	fit.lm 		<- lm(tmp.formula,  data=tmp.df)
	resid.lm	<- resid(fit.lm)

	## compute the arima fit
	resid.ts		<- ts(as.vector(resid.lm), frequency=weeks.per.year)
	fit.arima		<- auto.arima(resid.ts)
	
	## calcualte the in-sample fitted values
	fitted.lm		<- as.vector(fitted(fit.lm))
	fitted.arima	<- as.vector(fitted(fit.arima))
	fitted.all		<- InvBoxCox((fitted.lm + fitted.arima), lambda=lambda)
	
	if ( !is.null(ft) & !is.null(fregs) ) {
	
		## construct the forecast data frame
		proj.df		<- data.frame(t=ft, fregs)
		proj.df$x	<- 0
		
		fc.lm		<- predict(fit.lm, newdata=proj.df)
		fc.arima	<- forecast(fit.arima, h=fh)
		
		forecast.all <- InvBoxCox( (fc.lm + as.vector(fc.arima$med)), lambda=lambda)
	} else {
		forecast.all <- NULL	
	}
	
	if (!is.null(forecast.all) ) { 
		mae	<- sum(abs(orig.x-fitted.all))/length(x)
	}
	
	return(list(x=orig.x, fitted=fitted.all, forecast=forecast.all, lambda=lambda, farima=fitted.arima, flm=fitted.lm, mae=mae))
}
