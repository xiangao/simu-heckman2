library(MASS)
library(sampleSelection)
# Set up data
# 1. expand grid to get data frame with bunch of parameters
# 2. use apply and function from interior of loop to calculate bias
# for each row, and save that to the table.
# 3. write results to csv file.
# write.csv(simulations, 'simulations.csv')

# Server
# 1. simulations <- read.csv('simulations.csv')
# 2. each call to the server subsets the data frame
#    keep <- (simulations$cr1w == input$cr1w) & ...
#    plot(simulations[keep, 'heckman.bias'], simulations[keep, 'lm.bias'])

    set.seed(6)
    nDim = 3
    sd11 = 1
    sdww=1
    crwu=0
    sduu=1

gen.sim <- function(df){
    # covariance matrix with cr1w, cr1u from input.
    covarMat = matrix( c(sd11^2, df['cr1w']^2, df['cr1u']^2, df['cr1w']^2, sdww^2, crwu^2,  df['cr1u']^2, crwu^2, sduu^2 ) , nrow=nDim , ncol=nDim )
    # this part is to make sure the covariance matrix be positive definite.
    eS <- eigen(covarMat, symmetric = TRUE)
    ev <- eS$values
    tol = 1e-06
    if (!all(ev >= -tol * abs(ev[1L])))
        return(c(lm=NA, heck.noinst=NA, heck.inst=NA))
else {
    # generate data based on covariance matrix.
    data  = as.data.frame(mvrnorm(n=df['nobs'] , mu=rep(0,nDim), Sigma=covarMat ))
    names(data) <- c('x1','w','u')
    # dgp
    data$y <- data$x1 +  data$u
    data$rn <- rnorm(dim(data)[1], 0,1)
    # selection process
    data$select <- (data$y + data$w + data$rn > df['sel.cri'])
    data$y <- ifelse(data$select==1,data$y,NA)
    heckit1 <- heckit(selection = select ~ x1, outcome = y ~ x1 , method = "ml", data=data)
    heckit1.x1 <- summary(heckit1)$estimate['x1','Estimate']-1
    heckit2 <- heckit(selection = select ~ x1 + w, outcome = y ~ x1 , method = "ml", data=data)
    heckit2.x1 <- summary(heckit2)$estimate['x1','Estimate']-1
    lm1 <- lm(y ~ x1 , data=data)
    lm.x1 <- summary(lm1)$coefficients['x1','Estimate']-1
    return(c(lm=lm.x1, heck.noinst=heckit1.x1, heck.inst=heckit2.x1))
}
}

# set parameter space
sim.grid = seq(1,100,1)
corr.grid = seq(0, .9, .3)
nobs.grid = ceiling(exp(seq(6, 9, 1))/100)*100
sel.grid = seq(0,1,1)
data.grid <- expand.grid(nobs.grid, sim.grid, corr.grid, corr.grid, sel.grid)
names(data.grid) <- c('nobs', 'nsim','cr1w','cr1u','sel.cri')
results <- t(apply(data.grid, 1, gen.sim))
forshiny <- cbind(data.grid, results)
# write out for use in shiny.
write.csv(forshiny, 'results.csv')