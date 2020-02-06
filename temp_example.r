

# laod libraires
library(tidyverse)
library(lubridate)
library(gam)
library(survival)
# library(broom)
# library(purrr)

# read in the global land temperature data
gltd <- read_csv('/Users/kirkw/projects/kaggle_climate/climate-change-earth-surface-temperature-data/GlobalLandTemperaturesByMajorCity.csv')
# look at the data
summary(gltd)
str(gltd)
names(gltd)

# make lowercasae
gltd <- rename_all(gltd, tolower)
names(gltd)

# nest data, 100 major cities
by_city_month <- gltd %>% 
  filter(year(dt) >= 1900) %>%
  mutate(year = year(dt)) %>% 
  mutate(month = month(dt)) %>% 
  mutate(mon = month(dt)) %>% 
  mutate(yr1900 = year(dt) - 1900) %>%
  group_by(city, country, mon) %>%
  nest()

by_city_month

# define function for linear model
city_model_lm <- function(df) {
  lm(averagetemperature ~ yr1900, data = df)
}

# define function for GAM
city_model_gam <- function(df) {
  gam(averagetemperature ~ s(yr1900), data = df)
}

# define function for ols
city_model_ols <- function(df) {
  lm(averagetemperature ~ year + month, data = df)
}

# create columns for the models
cmodels <- by_city_month %>%
  mutate(model = map(data, city_model_lm), 
         modelgam = map(data, city_model_gam),
         modelols = map(data, city_model_ols)
  )
cmodels
cmodels_details <- cmodels %>%
  mutate(
    glance_lm = model %>% map(glance),  #model summary: rsquared...
    rsq = glance_lm %>% map_dbl("r.squared"),  #extract rsquared

    glance_gam = modelgam %>% map(broom::glance), #GAM model summary
    aic_gam = glance_gam %>% map_dbl("AIC"), #extract AIC

    tidy_lm = model %>% map(tidy), #model estimate: coeff...

    augment_lm = model %>% map(augment), #observation stats: resid,hat...
    res = augment_lm %>% map(".resid") #extract resid
  )

coefs <- cmodels_details %>%
  transmute(
    city = city,
    month = mon,
    p_val = map_dbl(glance_lm, "p.value"),
    coefs = map(tidy_lm, "estimate")
  ) %>%
  unnest(coefs) %>% 
  mutate(x = rep(c("intercept", "x1"), nrow(.) / 2)) %>% 
  spread(x, coefs)

cmodels_details_coefs <- left_join(cmodels_details, coefs, by = c('city', 'mon' = 'month'))

cmodels_details_coefs_plots <- cmodels_details_coefs %>%
  mutate(plot = map2(data, city, ~ ggplot(data = .x) +
                       geom_point(aes(x = yr1900, y = averagetemperature)) + 
                       geom_abline(intercept = intercept, slope = x1, color = 'red') + 
                       # stat_smooth(aes(x = yr1900, y = averagetemperature), method = "lm", formula = y ~ x, size = 1, se = FALSE, colour = "black") + 
                       # #####stat_smooth(aes(x = yr1900, y = averagetemperature), method = "lm", formula = y ~ x + I(x^2), size = 1, se = FALSE, colour = "blue") +
                       # # stat_smooth(method = "loess", formula = y ~  x, size = 1, se = FALSE, colour = "red") +
                       # # stat_smooth(method = "gam", formula = y ~  s(x), size = 1, se = FALSE, colour = "green") +
                       # stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = FALSE, colour = "violet") + 
                     
                       
                       # geom_smooth(aes(x = yr1900, y = averagetemperature),
                       #             method = lm, color = 'blue') +
                       # geom_smooth(aes(x = yr1900, y = averagetemperature),
                       #             color = 'green') +
                       
                       ggtitle(.y) +
                       ylab('Temp') +
                       xlab('Years since 1900')))

print(cmodels_details_coefs_plots$plot[1:12])

# sapply(cmodels_details$glance_lm, '[[', 'p.value')

ggplot(cmodels_details, aes(x = rsq)) + 
  geom_histogram(binwidth = 0.05, fill = 'lightblue', color = 'black') + 
  labs(title = 'R '^2~'of cities') +
  theme(strip.text.y = element_text(angle = 0))
