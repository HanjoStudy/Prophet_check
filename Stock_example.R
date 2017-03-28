library(prophet)
library(BatchGetSymbols)
library(purrr)

my.stocks <- GetSP500Stocks()$ticker

# first.date <- as.Date('2015-01-01')
# last.date <- Sys.Date()
# df.stocks <- BatchGetSymbols(my.stocks, 
#                              first.date = first.date, 
#                              last.date = last.date)[[2]]

load("SP500.RData")

set.seed(10)
n.stocks <- 10
my.stocks <- sample(unique(df.stocks$ticker), n.stocks)

df.stocks <- df.stocks[df.stocks$ticker %in% my.stocks, ]

est.model.and.forecast <- function(df.in, nfor = 5){
  # Estimated model using prophet and forecast it
  #
  # Args:
  #   df.in - A dataframe with columns price.adjusted and ref.date
  #   nfor - Number of out-of-sample forecasts
  #
  # Returns:
  #   A dataframe with forecasts and errors for each horizon.
  
  require(prophet)
  require(dplyr)
  
  my.ticker <- df.in$ticker[1]
  #df.in <-  df.stocks %>% filter(ticker == my.ticker)
  #cat('\nProcessing ', my.ticker)
  
  n.row <- nrow(df.in)
  df.in$ret <- with(df.in, c(NA,price.adjusted[2:n.row]/price.adjusted[1:(n.row - 1)] - 1))
  
  df.in <- select(df.in, ref.date, price.adjusted)
  names(df.in) <- c('ds', 'y')
  
  idx <- nrow(df.in) - nfor
  
  df.est <- df.in[1:idx, ]
  df.for <- df.in[(idx + 1):nrow(df.in), ]
  
  capture.output(
    m <- prophet(df = df.est, growth = "linear", changepoints = NULL,
                 n.changepoints = 25, yearly.seasonality = FALSE,
                 weekly.seasonality = TRUE, holidays = NULL,
                 seasonality.prior.scale = 10, changepoint.prior.scale = 0.05,
                 holidays.prior.scale = 10, mcmc.samples = 0, interval.width = 0.8,
                 uncertainty.samples = 1000, fit = TRUE)
  )
  
  # forecast 50 days ahead (it also includes non trading days)
  df.pred <- predict(m,
                     make_future_dataframe(m,
                                           periods = nfor))
  
  p1 <- plot(m, df.pred)
  
  
  df.for <- merge(df.for, df.pred, by = 'ds')
  df.for <- select(df.for, ds, y, yhat)
  
  # forecast statistics
  df.for$eps <- with(df.for,y - yhat)
  df.for$abs.eps <- with(df.for,abs(y - yhat))
  df.for$perc.eps <- with(df.for,(y - yhat)/y)
  df.for$nfor <- 1:nrow(df.for)
  df.for$ticker <- my.ticker
  
  return(list(df.for, p1))
  
}

# In this object you’ll find the forecasts (yhat), the actual values (y), the absolute and normalized error (abs.eps, perc.eps).

out.l <- df.stocks %>% split(., "ticker") %>% 
  map(~.x %>% est.model.and.forecast(., nfor = 10))



library(ggplot2)

p <- ggplot(my.result, aes(x=factor(nfor), 
                           y=abs.eps, color = ticker))

p <- p + geom_boxplot() + facet_wrap(~ticker)

print(p)

# Mincer-zaronowitz test

lm.model <- lm(formula = y ~yhat, data = my.result)
summary(lm.model)

