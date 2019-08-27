library("tidyverse")
library("BoomSpikeSlab")
library("bsts")
library("lubridate")

##
data(iclaims)
.data <- initial.claims
claims <- .data$iclaimsNSA
tibble(date = ymd(attributes(claims)$index), claims = coredata(claims)) %>% 
ggplot(aes(x = date, y = claims))+
         geom_line()

(model_components <- list())

## Trend Component
# ?AddAr
# ?AddAutoAr
# ?AddLocalLevel
# ?AddLocalLinearTrend
# ?AddSemilocalLinearTrend
# ?AddStudentLocalLinearTrend
# ?AddGeneralizedLocalLinearTrend

summary(model_components <- AddLocalLinearTrend(model_components, y = claims))

## Seasonal Component
# ?AddTrig # Trigonometric seasonal
# ?AddSeasonal
# ?AddNamedHolidays
# ?AddFixedDateHoliday
# ?AddNthWeekdayInMonthHoliday
# ?AddLastWeekdayInMonthHoliday

summary(model_components <- AddSeasonal(model_components, y = claims, 
                                        nseasons  = 52))


fit <- bsts(claims, model_components, niter = 2000)


burnin <- 500 # Throw away first 500 
tibble(
  date = as.Date(time(claims)),
  trend = colMeans(fit$state.contributions[-(1:burnin),"trend",]),
  seasonality = colMeans(fit$state.contributions[-(1:burnin),"seasonal.52.1",])) %>%
  gather("component", "value", trend, seasonality) %>%
  ggplot(aes(x = date, y= value)) + 
  geom_line() + theme_bw() + 
  theme(legend.title = element_blank()) + ylab("") + xlab("") +
  facet_grid(component ~ ., scales="free") + guides(colour=FALSE) +
  theme(axis.text.x=element_text(angle = -90, hjust = 0))


pred <- predict(fit, horizon = 100, burn = burnin, quantiles = c(.05, .95))
plot(pred)


errors <- bsts.prediction.errors(fit, burn = 1000)
PlotDynamicDistribution(errors)


fit2 <- bsts(iclaimsNSA ~ ., state.specification = model_components, 
             data = initial.claims, niter = 1000)

colMeans(fit2$coefficients)


## SPIKE AND SLAB

??Boom






