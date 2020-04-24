############################################################
# Function to perform the ADF test for debt sustainability #
############################################################
##########
# INPUTS #
##########
# data = The vector of the time series to be analized. Debt to GDP Ratio 
#        for a given country.
# critlev = The criteria level to perform the ADF test. By default 5%
# ... = More parameters to be passed to the ADF Test.

adfSust <- function(data, critlev = 0.05, ...) {
        # 1. Verify if fUnitRoots is installed if not install it
        if (!("fUnitRoots" %in% rownames(installed.packages()))) {
                install.packages("fUnitRoots")
        }
        
        # 2. Loading the package
        library(fUnitRoots)
        
        # 3. Eliminating NA observations in the data
        data <- as.numeric(data)
        data <- data[!is.na(data)]
        
        # 4. Computing the optimal number of lags based on the BIC criteria
        bic_val <- rep(0,10)
        for (i in 1:10) {
                ar_mod <- arima(data, c(i,1,0), method = "ML")
                bic_val[i] <- BIC(ar_mod)
        }
        opt_lag <- which(bic_val == min(bic_val))
        
        # 5. Compute the ADF Test
        adf_test <- adfTest(data, lags = opt_lag, ...)
        
        # 6. Show Results
        if (critlev > 0 && critlev < 1) {
                sust <- adf_test@test$p.value < critlev
                names(adf_test@test$lm$coefficients) <- NULL
                rho <- adf_test@test$lm$coefficients[1]
                names(adf_test@test$statistic) <- NULL
                test_val <- adf_test@test$statistic
                results <- list(Rho = rho, Statistic = test_val, 
                                p.value = adf_test@test$p.value, Sustainability = sust)
                return(results)
        } else {
                stop("Critical Level must be between 0 and 1")
        }
        
        
}