# This script contains all the user written functions necessary for the dissertation



#### Terror Cleaning Functions (Mainly used on Script2) ####
# We have a problem where there are multiple factor columns when I only want one. i.e. we have weapon1/2/3 but these are separate and list the weapon used. If I want a 'used a gun' dummy I 
# need to convert all these factor columns into a dummy column and then aggregate across them.


clean.column <- function(column){
  # Cleans the factors in a single column to remove and brackets that interfere with our regex later
  cleaned.column <- gsub("\\(|\\)", "", column)
  return(cleaned.column)
}
create.dummy.colnames <- function(...){
  # Creates a list of unique names for use in our dummy columns
  dots.list <- list(...)
  col.names <- dots.list %>% 
    map(unique) %>% 
    unlist %>% 
    as.tibble %>% 
    unique %>% 
    na.omit
  
  return(col.names)
}

find.a.similar.dummy.column <- function(dummy.col, other.dummy.matrix, regex){
  # Uses regex to find a similar column and return the similar column
  similar.col <- other.dummy.matrix[, grepl(regex, colnames(other.dummy.matrix))]
  return(similar.col)
  
}

add.similar.dummies <- function(dummies.df, dummy.colnames){
  # This function takes a dummies.df object where all the dummies with the same name are. It then adds them all up and uses an indicator function
  # to check if they're greater than 1 in which case they're set to 1
  empty.list <- rep(0, nrow(dummies.df))
  summed.dummies.df <- data.frame(original = empty.list)
  L = nrow(dummy.colnames)
  for (l in 1:L){
    
    sim.dummies <- dummies.df[, grepl(dummy.colnames[l, ], colnames(dummies.df))]
    summed.dummy <- data.frame(rowSums(sim.dummies))
    name.to.set <- dummy.colnames[l,]
    colnames(summed.dummy) <- name.to.set
    summed.dummy <- apply(summed.dummy, 2, function(x)ifelse((x>=1),1,0))
    summed.dummies.df<- cbind(summed.dummies.df, summed.dummy)
  }
  
  return(summed.dummies.df)
}

create.unique.colnames <- function(df){
  #Creates unique column names for our completed dummy variables so we know which dummy corresponds to which factor
  colnames(df) <- rep(deparse(substitute(df)), length(df)) %>% 
    paste(., colnames(df))
  return(df)
}



create.dummies <- function(...){
  # Creates a list of dummy variable columns that crucially share the same name. Due to the way the for-loop works I have each dummy column 'pair' ('original' and 'similar') repeated 4 times
  # This is because the first match is the original column with itself and the second match is the dummy with itself. Then the two match each other separately twice.
  dot.list <- list(...)
  dummy.col.names <- create.dummy.colnames(...)
  
  dummy.matrices <- dot.list %>% 
    map(dummy)
  K <- length(dummy.matrices)
  L <- nrow(dummy.col.names)
  dummies.data.frame <- data.frame(matrix('empty', nrow = nrow(dummy.matrices[[1]]), ncol = 1))
  for (j in 1:K){
    for (i in 1:K){
      for (l in 1:L){
        col <- find.a.similar.dummy.column(dummy.matrices[[j]], dummy.matrices[[i]], dummy.col.names[l, ])
        if (length(col) != 0){
          new.col.name <- dummy.col.names[l,]
          col.df <- data.frame(col)
          colnames(col.df) <- new.col.name
          dummies.data.frame <- cbind(dummies.data.frame, col.df)
        }
      }}}
  # Now we have a dataframe of dummies but the repeated observations share the same name so we can add them up
  complete.dummies <- add.similar.dummies(dummies.df = dummies.data.frame, dummy.colnames = dummy.col.names) 
  complete.dummies <- select(complete.dummies, subset = -c(original)) %>% 
    as.tibble
  
  
  return(complete.dummies)
  
}

#### Finite Moment Test Functions ####

# Testing for finite fourth moment as described by Trapani (2015) <https://doi.org/10.1016/j.jeconom.2015.08.006>

calculate.mu.star <- function(data){
  data <- na.omit(data)
  n <- length(data)
  
  mu.hat.k <- (1/n)*sum(abs(data)^4)
  mu.hat.phi <- (1/n)*sum(abs(data)^2)  # Setting phi = 2 i.e. the variance
  mu.hat.star <- mu.hat.k/(mu.hat.phi^2) # Essentially rescaling by the variance
  return(mu.hat.star)
}

calculate.mu.star.star <- function(mu.hat.star){
  mu.phi.norm <- 2*gamma(1.5)*pi^(-1/2) # The phi'th absolute moment of the normal distribution - here the 2nd abs moment
  mu.k.norm <- 4*gamma(2.5)*pi^(-1/2)
  
  mu.hat.star.star <- mu.hat.star*(mu.phi.norm^2)/mu.k.norm # Now rescaling AS IF our X's were standard normal
  return(mu.hat.star.star)
}


generate.sample <- function(r, mu.hat){
  normal.var <- rnorm(n=r, mean = 0, sd = 1)
  sample <- normal.var*sqrt(exp(mu.hat))
  return(sample)
}

test.integrand <- function(generated.sample){ # This function looks a little complicated because we're integrating over u whilst holding the sample used to
  fu.sample <- function(u){                   # generate theta_u constant
    zeta <- data.frame(generated.sample)
    zeta <- mutate(zeta, zeta.indicator = as.numeric(zeta < u))
    zeta <- mutate(zeta, zeta.next = zeta.indicator - 0.5)
    theta_u <- (2/sqrt(length(generated.sample)))*sum(zeta$zeta.next)
    integrand <- (theta_u^2)*(1-u)
    return(integrand)
  }
  return(fu.sample)
}

generate.test.statistic<- function(generated.sample){
  integrate(Vectorize(test.integrand(generated.sample)), lower = -1, upper = 1, rel.tol=.Machine$double.eps^.05)$value # Integrating the test statistic
}

perform.finite.fourth.moment.check <- function(pre.whitened.residuals){
  mu.hat <- calculate.mu.star(pre.whitened.residuals) %>% 
    calculate.mu.star.star
  
  test.sample <- generate.sample(r = 10000, mu.hat = mu.hat)
  test.statistic <- generate.test.statistic(test.sample)
  p.value <- pchisq(test.statistic, df = 1, lower.tail = FALSE) #Under the null the test statistic has a chi squared distribution with 1 degree of freedom
  return(p.value)
}

#### Event Study Analysis Functions #####

# To run our analysis we need to remove NA values however this creates a problem when the attacks we want to study occur on the weekend. To overcome this the function
# checks if the event date is missing from the omitted zoo. If it is, it adds one day and checks again, if it's still missing it adds another day and checks again. If a check
# is successful it returns the relevant index number. Essentially we're replacing the row index number of events happening on weekend with the row index of the following
# Monday.
find.NA.index.number <- function(index.data, events, n){
  single.zoo <- index.data
  date.event <- events[[1]][n]
  if (length(which(attributes(na.omit(single.zoo))$index == date.event)) == 0){
    if (length(which(attributes(na.omit(single.zoo))$index == date.event + 1)) == 0){
      
      index.number <- which(attributes(na.omit(single.zoo))$index == date.event + 2)
    } else {
      index.number <- which(attributes(na.omit(single.zoo))$index == date.event + 1)
    }
    
  } else {
    index.number <- which(attributes(na.omit(single.zoo))$index == date.event)
  }
  return(index.number)
}
# t-test calculator. Take the values in the estimation window and find their standard deviation and calculate root N to adjust degrees of freedom
# then divide the CAR given in the event window by the standard error. N.B. we test against the null of CAR = 0 as we don't subtract anything from ev.window
calculate.CAR.t.test <- function(esti.window, ev.window){
  esti.window <- na.omit(esti.window)
  ev.window <- na.omit(ev.window)
  
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  
  test <- ev.window/(estimation.stdev/root.n)
  return(test)
}


# Bootstrapped standard errors for use in t tests
calculate.boot.se <- function(data, indices){
  d <- na.omit(data[indices]) # allow boot to select sample
  estimation.sd <- sd(d)
  root.n <- sqrt(length(d))
  se <- estimation.sd/root.n
  return(se)
}
calculate.boot.t.test <- function(se, ev.window){
  test <- ev.window/se
  return(test)
}



# Calculating confidence intervals
calculate.CI <- function(esti.window){
  esti.window <- na.omit(esti.window)
  estimation.stdev <- sd(esti.window)
  root.n <- sqrt(length(esti.window))
  ci <- qt(0.975, df = length(esti.window) - 1)*(estimation.stdev/root.n)
  return(ci)
}

calculate.boot.CI <- function(esti.window, se){
  esti.window <- na.omit(esti.window)
  ci <- qt(0.975, df = length(esti.window) - 1)*se
  return(ci)
}


# Selecting the estimation window used in the constant mean return model
find.estimation.window <- function(index, events, n, window.end, window.length){
  row.index <- find.NA.index.number(index.data = index,
                                    events = events,
                                    n = n)
  estimation.window.start <- attributes(index[row.index - (window.end + window.length)])$index
  estimation.window.end <- attributes(index[row.index - (window.end + 1)])$index
  estimation.window <- window(index, start = estimation.window.start, end = estimation.window.end)
  return(estimation.window)
}

# Selecting the event window
find.event.window <- function(index, events, n, window.length){
  row.index <- find.NA.index.number(index.data = index,
                                    events = events,
                                    n = n)
  event.window.start <- attributes(index[row.index])$index
  event.window.end <- attributes(index[row.index + window.length])$index
  event.window <- window(index, start = event.window.start, end = event.window.end)
  return(event.window)
}

# Calculate AR
calculate.AR <- function(window, mean.return){
  ar <- window - mean.return
  return(ar)
}


# Calculating cumulative abnormal returns under the constant mean return model.
calculate.CAR <- function(window, mean.return, car.length){
  ar <- window - mean.return
  car <- rollsum(ar, car.length, fill = NA, align = 'right')
}


# Calculating p value based off t-statistic and estimation window df
calculate.p.value <- function(t.stat, estimation.car){
  p <- p.value <- 2*pt(-abs(t.stat),df=length(na.omit(estimation.car))-1)
  return(p)
}

# Converts event date into market days since attack
calculate.attack.time.delta <- function(event.study){
  event.study$time.delta <- seq(length(event.study[, 1])) - 1
  return(event.study)
}

# Pulling all the above together into a single function to run an event study. This will give an n-day CAR observation only.
perform.one.day.event.study <- function(index, events, n, car.length = 11, estimation.window.length = 20, estimation.window.end = 10, boot = FALSE){
  
  event.market.date <- events$Date[n]
  
  estimation.window <- find.estimation.window(index = index,
                                              events = events,
                                              n = n,
                                              window.end = estimation.window.end,
                                              window.length = estimation.window.length)
  event.window <- find.event.window(index = index,
                                    events = events,
                                    n = n,
                                    window.length = car.length)
  
  constant.mean.return <- mean(estimation.window)
  
  estimation.car <- calculate.CAR(window = estimation.window,
                                  mean.return = constant.mean.return,
                                  car.length = car.length)
  
  event.ar <- calculate.AR(window = event.window,
                           mean.return = constant.mean.return)
  
  event.car <- calculate.CAR(window = event.window,
                             mean.return = constant.mean.return,
                             car.length = car.length)
  
  event.t.stat <- calculate.CAR.t.test(esti.window = estimation.car,
                                       ev.window = event.car)
  
  event.p.value <- round(calculate.p.value(t.stat = event.t.stat,
                                           estimation.car = estimation.car), 4)
  
  
  event.confidence.interval <- calculate.CI(esti.window = estimation.car)
  
  
  return.zoo <- merge.zoo(event.ar,
                          event.car,
                          event.t.stat,
                          event.p.value,
                          event.confidence.interval)
  
  if (boot == TRUE){
    event.boot.se <- boot(data = estimation.car,
                          statistic = calculate.boot.se,
                          R = 10000,
                          parallel = 'snow', 
                          ncpus = 2)
    mean.boot.se <- mean(event.boot.se$t)
    boot.t.stat <- round(calculate.boot.t.test(se = mean.boot.se,
                                               ev.window = event.car), 4)
    boot.p.value <- round(calculate.p.value(t.stat = boot.t.stat,
                                            estimation.car = estimation.car), 4)
    boot.confidence.interval <- calculate.boot.CI(esti.window = estimation.car,
                                                  se = mean.boot.se)
    return.zoo <- merge.zoo(event.ar,
                            event.car,
                            event.t.stat,
                            boot.t.stat,
                            event.p.value,
                            boot.p.value,
                            event.confidence.interval,
                            boot.confidence.interval)
  }
  
  
  return.df <- data.frame(return.zoo[car.length, ])
  return.df <- tibble::rownames_to_column(return.df, 'Date')
  return.df$Date <- as.Date(return.df$Date)
  return.df$stars <- gtools::stars.pval(return.df$event.p.value)
  return.df$market.date <- event.market.date
  return(return.df)
}

# Calculates the CAR for every day in the event window
perform.event.study <- function(index, events, n, car.length = 11, estimation.window.length = 20, estimation.window.end = 10, boot = FALSE){
  
  full.event.study <- perform.one.day.event.study(index = index,
                                                  events = events,
                                                  n = n,
                                                  car.length = 1,
                                                  estimation.window.length = estimation.window.length,
                                                  estimation.window.end = estimation.window.end,
                                                  boot = boot)
  for (i in 2:car.length){
    single.event.study <- perform.one.day.event.study(n = n,
                                                      index = index,
                                                      events = events,
                                                      car.length = i,
                                                      estimation.window.length = estimation.window.length,
                                                      estimation.window.end = estimation.window.end,
                                                      boot = boot)
    
    full.event.study <- rbind(full.event.study, single.event.study)
  }
  full.event.study <- calculate.attack.time.delta(full.event.study)
  return(full.event.study)
}

# Using purrrs's map() function instead of a for loop to calculate CAR for every event in a given list. This function and the above function
# produce identical results although require slightly different arguments. calculate.car takes an event day argument, perform.event.study takes a
# list of events and an indexer n for the specific event wanted. This second function is meant to be marginally quicker but the effect is 
# negligible or non-existent
calculate.car <- function(events, index, estimation.window.length = 20, estimation.window.end = 10, car.length = 11, boot = FALSE){
  
  
  n.vector <- seq(nrow(events))
  all.CARS.CAARS <-
    n.vector %>% 
    map_dfr(perform.one.day.event.study,
            index = index,
            events = events,
            estimation.window.length = estimation.window.length,
            estimation.window.end = estimation.window.end,
            car.length = car.length,
            boot = boot)
  
  
  
  return(all.CARS.CAARS)
  
}

# Gives rolling Cumulative Average Abnormal Returns for use in a plot of CAAR vs number of events included
calculate.rolling.CAAR <- function(all.events.CAR){
  all.events.CAR$rolling.CAAR <- cumsum(all.events.CAR$event.car)/seq(along = all.events.CAR$event.car)
  n <- c(1:nrow(all.events.CAR))
  all.events.CAR$n <- n
  return(all.events.CAR)
}

# Confidence intervals for the above function
calculate.CI.rolling.CAAR <- function(all.events.CAR){
  all.events.CAR$df <- all.events.CAR$n - 1
  all.events.CAR <- mutate(all.events.CAR, 
                           temp.diff = (event.car - rolling.CAAR)^2,
                           rolling.sd = sqrt(cumsum(temp.diff)/df),
                           rolling.ci = qt(0.975, df = df)*rolling.sd/(sqrt(n)),
                           rolling.t = rolling.CAAR/(rolling.sd/sqrt(n)))
  # all.events.CAR$temp.diff <- (all.events.CAR$event.CAR - all.events.CAR$rolling.CAAR)^2
  # # all.events.CAR$rolling.sd <- sqrt(cumsum(all.events.CAR$temp.diff)/all.events.CAR$df)
  # # all.events.CAR$ci <- qt(0.975, df = df)*(all.events.CAR$rolling.sd/(sqrt(all.events.CAR$n)))
  return(all.events.CAR)                                         
}

calculate.boot.CAAR <- function(data, indices){
  CAR <- data[indices]
  return(CAR)
}

# Given a dataframe with n-day CARs calculated, will calculate n-day CAAR with confidence intervals etc.
calculate.CAAR <- function(events, index, estimation.window.length = 20, estimation.window.end = 10, car.length = 11){
  
  CAAR <-
    calculate.car(events,
                  index,
                  estimation.window.length,
                  estimation.window.end,
                  car.length) %>%
    calculate.rolling.CAAR %>%
    calculate.CI.rolling.CAAR %>% 
    select(c(event.car, rolling.CAAR, n, rolling.sd, rolling.ci, rolling.t))
  
  colnames(CAAR) <- c('event.car', 'CAAR', 'number of events', 'st.dev', 'CI width', 'T statistic')
  boot.CAARs <- boot(data = CAAR$event.car,
                     statistic = calculate.boot.CAAR,
                     R = 10000)
  CAAR <- CAAR[nrow(CAAR), ]
  CAAR$boot.ci.lower <- boot.ci(boot.CAARs, type = 'bca')$bca[4]
  CAAR$boot.ci.upper <- boot.ci(boot.CAARs, type = 'bca')$bca[5]
  return(CAAR)
}


#### Probability Analysis Functions ####
## Functions used for probability methods


calculate.event.day.return <- function(event.date, n, index){
  
  terror.event.date <- event.date[[n, 'Date']]
  
  
  row.index <- find.NA.index.number(index.data = index,
                                    events = event.date,
                                    n = n)
  
  event.day.return <- index[row.index]
  event.day.return.L1 <- index[(row.index - 1)]
  
  
  return.list <- list(event.day.return, event.day.return.L1, terror.event.date)
  return(return.list)
  
}



# Rescaling X variable (lagged returns for logit regression) according to Gelman et al (2008)
# (A Weakly Informative Default Prior Distribution for Logistic and Other Regression Models)
rescale.X.variable <- function(X.data, target.sd = 0.5){
  original.sd <- sd(X.data)
  original.mean <- mean(X.data)
  sd.rescale.factor <- target.sd/original.sd
  
  rescaled.data <- (X.data - original.mean)/sd.rescale.factor
  return(rescaled.data)
}



# Performs the data transformations in order to calculate conditional probability as laid out by Marc Chesney, Ganna Reshetar, Mustafa Karaman
# at https://doi.org/10.1016/j.jbankfin.2010.07.026

# The indicator function: Y = I(R < r) where R is observed return and r is the return the day of the attack
# The X.temp variable is just R lagged
# The X.L1.conditioned variable is X - r(t-1) i.e X minus the return the day before the terror attack.
# The X.mean.conditioned variable is X - mean(R) i.e. conditioning on the mean of estimation.length observations before the attack

# A regression of Y on X.L1/.mean is equivalent to finding the conditional probability of observing a return as bad or worse on the 
# day of the attack.
calculate.variables <- function(event.day.return.vector, index,  estimation.length = 200, rescale = TRUE, target.sd = 0.5){
  
  event.day.return <- event.day.return.vector[[1]]
  event.day.return.L1 <- event.day.return.vector[[2]]
  event.date <- event.day.return.vector[[3]]
  
  estimation.window <- find.estimation.window(index = index,
                                              events = event.date,
                                              n = 1,
                                              window.end = 0,
                                              window.length = estimation.length)
  
  estimation.window.returns <- coredata(estimation.window)
  index.df <- data.frame(estimation.window.returns)
  index.df$event.day.return <-  event.day.return
  index.df$event.day.return.L1 <- event.day.return.L1
  
  index.df <- mutate(index.df, Y = as.numeric(estimation.window.returns < event.day.return))
  index.df$X.temp <- coredata(dplyr::lag(estimation.window, 1))
  index.df$X.L1.conditioned <- index.df$X.temp - index.df$event.day.return.L1
  
  index.df <- mutate(index.df, X.mean.conditioned = index.df$X.temp - mean(estimation.window.returns) )
  index.df <- na.omit(index.df)
  #Rescaling variables
  if (rescale == TRUE){
    index.df <- mutate(index.df,
                       rescaled.X.L1 = rescale.X.variable(X.L1.conditioned, target.sd),
                       rescaled.X.mean = rescale.X.variable(X.mean.conditioned, target.sd),
                       rescaled.terror.return = (event.day.return - mean(X.mean.conditioned))/sd(X.mean.conditioned))
  }
  
  return(index.df)
}


# Prepares data for stan to perform regression. Variables specificed in stan file should be N, returns, Y and terror_return
prepare.model.data <- function(data, condition.on = 'mean'){
  if (condition.on == 'lag'){
    model.data <- list(returns = data$rescaled.X.L1,
                       N = nrow(data),
                       Y = data$Y,
                       terror_return = data$rescaled.terror.return[1])
  } 
  else{
    model.data  <- list(
      returns = data$rescaled.X.mean,
      N = nrow(data),
      Y = data$Y,
      terror_return = data$rescaled.terror.return[1])
  }
  return(model.data)
}

# Returns paramets from a list of stanfit models
extract.parameters <- function(fitted.models, parameter){
  
  model.params <- lapply(fitted.models, function(x) summary(x, pars = c(parameter))$summary) %>% 
    map_dfr(data.frame) %>% 
    mutate(parameter = parameter)
  return(model.params)
}


# Formats data in a style that makes it easy to form stan data lists
prepare.stan.data <- function(n.events, events, index){
  number.events <- 1:n.events
  stan.data <- number.events %>% 
    map(calculate.event.day.return, event.date = events, index = index) %>% 
    map(calculate.variables, index = index) %>% 
    map(prepare.model.data)
  
  return(stan.data)
}

# Extracting stan results using the tidy() function from broom
collect.stan.results <- function(stan.fit, parameter, decade, model){
  results <- stan.fit %>% 
    map_dfr(tidy, pars = parameter, conf.int = TRUE) %>% 
    mutate(event = 1:n(),
           decade = decade,
           model = model) %>% 
    as.tibble
  return(results)
}
