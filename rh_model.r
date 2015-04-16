##### load 2012 - 2013 data and remove unnessessary data
dt <- fread("~/Desktop/USER_FIELDSEASON_2012-2013_hourly.csv") # read in data
dt1 <- subset(dt, block == "b" | block == "d" | block == "k" | block == "i") # keep blocks with rh sensors
dt2 <- subset(dt1, filled_airtc > -30) # remove crazy low values
columns2keep <- c("time2", "doy", "site", "canopy", "filled_rh2", "filled_airtc") # create key as list of columns to keep
dt3 <- subset(dt2, select = columns2keep) # subset based on list of columns to keep

# dual overlaid density plots with semi-transparent fill
ggplot(dt3, aes(x = filled_rh2, fill = canopy)) + geom_density(alpha = 0.3)
ggplot(dt3, aes(x = filled_rh2, fill = site)) + geom_density(alpha = 0.3)

##### create separate data tables for "open" and "closed" canopy data
dt3_open <- dt3[canopy == "open", ] 
dt3_closed <- dt3[canopy == "closed", ]

##### so we dont't get confused about what it what
rename(dt3_open, c("filled_rh2"="rh2_open", "filled_airtc"="airtc_open")) # rename columns 
rename(dt3_closed, c("filled_rh2"="rh2_closed", "filled_airtc"="airtc_closed")) # rename columns
dt3_open[, canopy:=NULL] # remove column
dt3_closed[,canopy:=NULL] # remove column

##### create keys, and remove duplicate values from plots with identical data 
setkey(dt3_open, "time2", "site")
setkey(dt3_closed, "time2", "site")
dt3_open <- unique(dt3_open)
dt3_closed <- unique(dt3_closed)

##### perform data table join and add a binomial "leaf on" column as true bewteen doy 161 and 279
dt4 <- dt3_open[dt3_closed]
# dt4$leafon <- ifelse(dt4$doy >= 161 & dt4$doy <= 279, "TRUE", "FALSE") # data frame way
dt4[,leafon:= as.integer(doy>150 & doy < 280)]

# plot the rh data
xyplot(rh2_closed ~ rh2_open | site, data = dt4, type=c('p','r'))
# plot the airtc data
xyplot(airtc_closed ~ airtc_open | site, data = dt4, type=c('p','r'))

# plot with both grouos on one fig with regression lines
xyplot(rh2_closed ~ rh2_open, groups = site, data = dt4, 
       auto.key = list(columns = 2, title = 'Relative Humidity', 
                       lines = TRUE, points = FALSE, cex=0.5), 
       type=c('p','r'), ylab = 'closed canopy', xlab = 'open canopy')

xyplot(rh2_closed ~ rh2_open, groups = leafon, data = dt4, 
       auto.key = list(columns = 2, title = 'Relative Humidity', 
                       lines = TRUE, points = FALSE, cex=0.5), 
       type=c('p','r'), ylab = 'closed canopy', xlab = 'open canopy')

##### analyize slopes and interceps by site
d.l <- split(dt4, dt4$site) # split by factor
fits <- lapply(d.l, function(d_i) {lm(rh2_closed ~ rh2_open, data=d_i)}) # fit model for each level of factor
est <- lapply(fits, coef) # extract coefs
ci <- lapply(fits, confint) # compute confints
ci.mat <- do.call('rbind', ci)
est.mat <- do.call('rbind', est)
ci.df <- data.frame(f=rep(colnames(sapply(ci, '[')), each=2))
ci.df$lower <- ci.mat[,1] 
ci.df$upper <- ci.mat[,2]
ci.df$which <- row.names(ci.mat) # re-attach estimate label
ci.df$estimate <- NA # add dummy column for estimate

##### make a data frame for the estimates
est.df <- data.frame(which=rep(colnames(est.mat), each=nrow(est.mat))) 
est.df$estimate <- as.vector(c(est.mat[,1], est.mat[,2]))
est.df$f <- rep(row.names(est.mat), 2)

##### add dummy columns for upper and lower conf ints
est.df$upper <- NA
est.df$lower <- NA

##### combine estimate with confints
combined <- rbind(est.df, ci.df)

##### combined plot of estimate +/- confint
dotplot(f ~ estimate + lower + upper | which, data=combined, scales=list(relation='free'), xlab="Estimate", ylab="Group", auto.key=list(columns=3),
        par.settings=list(superpose.symbol=list(col=c(1), pch=c(16,1,1), cex=c(1,0.75,0.75))))

# xy scatter closed vs open with 1:1 line by site
ggplot(dt4, aes(x = rh2_open, y = rh2_closed)) + 
  geom_point(shape = 1, size = 0.5, alpha = 0.02) + 
  geom_abline(intercept = 0, slope = 1, color = "red") +   # add 1:1 line with a zero intercept 
  geom_smooth(method = "lm", fill = NA) + 
  facet_grid(. ~ site)
# xy scaller closed vs open with 1:1 line
ggplot(dt4, aes(x = rh2_open, y = rh2_closed)) + 
  geom_point(shape = 1, size = 0.5, alpha = 0.03) + 
  geom_abline(intercept = 0, slope = 1, color = "red") +   # add 1:1 line with a zero intercept 
  geom_smooth(method = "lm", fill = NA)

##### analyize slopes and interceps by leafon
d.l <- split(dt4, dt4$leafon) # split by factor
fits <- lapply(d.l, function(d_i) {lm(rh2_closed ~ rh2_open, data=d_i)}) # fit model for each level of factor
est <- lapply(fits, coef) # extract coefs
ci <- lapply(fits, confint) # compute confints
ci.mat <- do.call('rbind', ci)
est.mat <- do.call('rbind', est)
ci.df <- data.frame(f=rep(colnames(sapply(ci, '[')), each=2))
ci.df$lower <- ci.mat[,1] 
ci.df$upper <- ci.mat[,2]
ci.df$which <- row.names(ci.mat) # re-attach estimate label
ci.df$estimate <- NA # add dummy column for estimate

##### make a data frame for the estimates
est.df <- data.frame(which=rep(colnames(est.mat), each=nrow(est.mat))) 
est.df$estimate <- as.vector(c(est.mat[,1], est.mat[,2]))
est.df$f <- rep(row.names(est.mat), 2)

##### add dummy columns for upper and lower conf ints
est.df$upper <- NA
est.df$lower <- NA

##### combine estimate with confints
combined <- rbind(est.df, ci.df)

##### combined plot of estimate +/- confint
dotplot(f ~ estimate + lower + upper | which, data=combined, scales=list(relation='free'), xlab="Estimate", ylab="Group", auto.key=list(columns=3),
        par.settings=list(superpose.symbol=list(col=c(1), pch=c(16,1,1), cex=c(1,0.75,0.75))))

# xy scatter closed vs open with 1:1 line by leafon
ggplot(dt4, aes(x = rh2_open, y = rh2_closed)) + 
  geom_point(shape = 1, size = 0.5, alpha = 0.02) + 
  geom_abline(intercept = 0, slope = 1, color = "red") +   # add 1:1 line with a zero intercept 
  geom_smooth(method = "lm", fill = NA) + 
  facet_grid(. ~ leafon)
# xy scaller closed vs open with 1:1 line
ggplot(dt4, aes(x = rh2_open, y = rh2_closed)) + 
  geom_point(shape = 1, size = 0.5, alpha = 0.03) + 
  geom_abline(intercept = 0, slope = 1, color = "red") +   # add 1:1 line with a zero intercept 
  geom_smooth(method = "lm", fill = NA)

# xy scaller closed vs open with 1:1 line
ggplot(dt4, aes(x = airtc_open, y = airtc_closed)) + 
  geom_point(shape = 1, size = 0.5, alpha = 0.03) + 
  geom_abline(intercept = 0, slope = 1, color = "red") +   # add 1:1 line with a zero intercept 
  geom_smooth(method = "lm", fill = NA)

# summary of lm()
dt4[, lm(rh2_open ~ rh2_closed)]

##### save models for comparison
rh <- lm(rh2_closed ~ rh2_open, data = dt4)
rh_site <- lm(rh2_closed ~ rh2_open + site, data = dt4)
rh_leafon <- lm(rh2_closed ~ rh2_open + leafon, data = dt4)
rh_site_leafon <- lm(rh2_closed ~ rh2_open + site + leafon, data = dt4)

airtc <- lm(airtc_closed ~ airtc_open, data = dt4)
airtc_site_leafon <- lm(formula = airtc_closed ~ airtc_open + site + leafon, data = dt4)

require(boot)

rh_g1 <- glm(rh2_open ~ rh2_closed + site + leafon, data = dt4)
rhCV1 <- cv.glm(dt4, rh_g1, k = 5)





