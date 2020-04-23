####################################################################
# Function to estimate the Bayesian Uni Root Test proposed by Sims #
####################################################################
##########
# INPUTS #
##########
# data = The vector containing the time series of Debt/GDP ratio for a given country
# alpha = Hyperparameter that represents the a priori pobability alpha ~ U(0,1) 
#         where if alpha < 1 then the debt follows a sustainable path 
#         and if alpha = 1 the debt is not sustainable.
# S = periocidicity for the critical value for small samples. Could be "yearly", 
#     "quarterly" or "monthly".
# smallSample = A binary variable. By default it computes the critical value for small
#               samples.
# ... = Other inputs for the arima function in R
simsTest <- function(data, alpha = 0.5, S = "yearly", 
                     smallSample = TRUE, ...) {
        # 1. Remove NA observations
        data <- as.numeric(data)
        data <- data[!is.na(data)]
        
        # 2. Estimate the AR(1) model with intercept
        ar1 <- arima(data, c(1,0,0), ...)
        
        # 3. Estimate tau
        names(ar1$coef) <- NULL
        rho <- ar1$coef[1]
        sigma2 <- var(ar1$residuals)
        sum_y2_lag <- sum(data[1:(length(data)-1)]^2)
        tau <- (1 - rho) / sqrt(sigma2 / sum_y2_lag)
        
        # 4. Compute critical value
        if (smallSample) {
                if (alpha <= 1 && alpha > 0) {
                        periodicity <- c("yearly", "quarterly", "monthly")
                        if (S %in% periodicity) {
                                if (S == "yearly") {
                                        critical <- 2 * log((1 - alpha) / alpha) -
                                                log(sigma2 / sum_y2_lag) +
                                                log(1 - 2^(-1))
                                } else if (S == "quarterly") {
                                        critical <- 2 * log((1 - alpha) / alpha) -
                                                log(sigma2 / sum_y2_lag) +
                                                log(1 - 2^(-1 / 4))
                                } else {
                                        critical <- 2 * log((1 - alpha) / alpha) -
                                                log(sigma2 / sum_y2_lag) +
                                                log(1 - 2^(-1 / 12))
                                }
                        } else {
                                stop("Periodicity only: yearly, quarterly or monthly")
                        }
                } else {
                        stop("alpha is bigger than 0 and less or equal to 1")
                }
        } else {
                critical <- - log(sigma2 / sum_y2_lag)
        }
        
        # 5. Compare results
        t2 <- tau^2
        is_sust <- t2 > critical
        results <- list(Rho = rho, Tau2 = t2, Critical = critical, Sustainability = is_sust)
        return(results)
}