#' Modification of Columbia.R from STCOS Github package
#' Special Thanks to Andrew Raim and Scott Holan for their assistance 

#+ echo = FALSE
options(width = 80)

set.seed(1234)

#' # Loading Fine-level Support
library(jsonlite)
library(sf)
library(tigris)
library(dplyr)
library(ggplot2)

#' Load the fine-level support via shapefile using the `tigris` package.
#' Convert it to an `sf` object, and transform to the projection with EPSG
#' code 3857.
#+ message=FALSE
options(tigris_use_cache = TRUE)
options(tigris_refresh = FALSE)
dom.fine <- block_groups(state = '29', county = '019', year = 2015) %>%
	st_as_sf() %>%
	st_transform(crs = 3857) %>%
	mutate(geoid = GEOID) %>%
	select(-GEOID)

#' plot of the fine-level domain.
ggplot(dom.fine) +
	geom_sf(colour = "black", size = 0.05) +
	ggtitle("Boone County, Missouri") +
	theme_bw()

#' # Assemble Source Supports
#' The following loop pulls ACS direct estimates and associated MOEs from the
#' Census Bureau's Data API, and merges them into a single data frame. 
year.levels <- 2015:2017
dat.list <- list()
dat.missing <- list()

for (idx in 1:length(year.levels)) {

	year <- year.levels[idx]

	est_url <- paste("https://api.census.gov/data/", year, "/acs/acs5?get=NAME,B19013_001E&for=block%20group:*&in=state:29+county:019", sep='')
	json_data <- fromJSON(est_url)
	est_dat <- data.frame(json_data[-1,])
	colnames(est_dat) <- json_data[1,]

	moe_url <- paste('https://api.census.gov/data/', year,
		'/acs/acs5?get=NAME,B19013_001M&for=block%20group:*&in=state:29+county:019', sep = '')
	json_data <- fromJSON(moe_url)
	moe_dat <- data.frame(json_data[-1,])
	colnames(moe_dat) <- json_data[1,]

	my_dat <- est_dat %>%
		inner_join(moe_dat, by = c('state' = 'state', 'county' = 'county',
			'tract' = 'tract', 'block group' = 'block group')) %>%
		select(state, county, tract, blockgroup = `block group`,
			DirectEst = B19013_001E, DirectMOE = B19013_001M) %>%
		mutate(state = as.character(state)) %>%
		mutate(county = as.character(county)) %>%
		mutate(tract = as.character(tract)) %>%
		mutate(blockgroup = as.character(blockgroup)) %>%
		mutate(DirectEst = as.numeric(as.character(DirectEst))) %>%
		mutate(DirectMOE = as.numeric(as.character(DirectMOE))) %>%
		mutate(DirectVar = (DirectMOE / qnorm(0.95))^2)

	my_shp <- block_groups(state = '29', county = '019', year = year) %>%
		st_as_sf() %>%
		st_transform(crs = 3857)

	my_sf <- my_shp %>%
		inner_join(my_dat, by = c('STATEFP' = 'state', 'COUNTYFP' = 'county',
			'TRACTCE' = 'tract', 'BLKGRPCE' = 'blockgroup')) %>%
		select(geoid = GEOID, state = STATEFP, county = COUNTYFP,
			tract = TRACTCE, blockgroup = BLKGRPCE,
			DirectEst, DirectMOE, DirectVar)

	dat.list[[idx]] <- my_sf %>%
		filter(!is.na(DirectEst)) %>%
		filter(DirectEst >= 0)

	dat.missing[[idx]] <- my_sf %>%
		filter(is.na(DirectEst)) %>%
		filter(DirectEst >= 0)
}

#' Our assembled source supports
acs5.2015 <- dat.list[[1]]
acs5.2016 <- dat.list[[2]]
acs5.2017 <- dat.list[[3]]
rm(dat.list)

#' plot of one of the source supports.
ggplot(acs5.2015) +
	geom_sf(colour = "black", size = 0.05, aes(fill = DirectEst)) +
	scale_fill_distiller("DirectEst", palette = "RdYlBu") +
	theme_bw()

#' # Load Target Supports
#' Make sure to transform to the same projection as the fine-level support.
data(columbia_neighbs)
neighbs <- columbia_neighbs %>%
	st_transform(crs = st_crs(dom.fine))

#' plot of the neighborhoods.
ggplot(neighbs) +
	geom_sf(colour = "black", size = 0.05) +
	theme_bw()

#' # Prepare to fit the model
#+ message=FALSE
library(fields)
library(stcos)
library(ggforce)

#' Select spatial knots via space-filling design. Start with uniformly
#' drawn points from the fine-level geography and use `cover.design`
#' to select a subset of those points for knots.
u <- st_sample(dom.fine, size = 2000)
plot(u)
M <- matrix(unlist(u), length(u), 2, byrow = TRUE)
out <- cover.design(M, 500)
knots.sp <- out$design

#' Select temporal knots to be evenly spaced over the years relevant
#' to the source support years.
knots.t <- seq(2011, 2017, by = 0.5)

#' Use Cartesian join of spatial and temporal knots to obtain spatio-temporal knots.
knots <- merge(knots.sp, knots.t)
basis <- SpaceTimeBisquareBasis$new(knots[,1], knots[,2], knots[,3], w.s = 1, w.t = 1)

#' Here is a plot of the spatial knots. We plot a circle around one of the
#' points to illustrate the choice of the `w.s` argument.
knots.sp.dat <- data.frame(x = knots.sp[,1], y = knots.sp[,2], r = basis$get_rl())
g <- ggplot(dom.fine) +
	geom_sf(colour = "black", size = 0.25, fill = NA) +
	geom_point(data = knots.sp.dat, aes(x, y), lwd = 1, col = "red") +
	geom_point(data = knots.sp.dat[1,], aes(x, y), lwd = 3, col = "blue") +
	geom_circle(data = knots.sp.dat[1,], aes(x0=x, y0=y, r=r), fill = NA,
		lwd = 0.5, col = "blue") +
	labs(x = "", y = "") +
	theme_bw()
print(g)

#' # Build terms for STCOS model
#' Create an STCOSPrep object and add source supports
sp <- STCOSPrep$new(fine_domain = dom.fine, fine_domain_geo_name = "geoid",
	basis = basis, basis_mc_reps = 500)
sp$add_obs(acs5.2015, period = 2011:2015, estimate_name = "DirectEst",
	variance_name = "DirectVar", geo_name = "geoid")
sp$add_obs(acs5.2016, period = 2012:2016, estimate_name = "DirectEst",
	variance_name = "DirectVar", geo_name = "geoid")
sp$add_obs(acs5.2017, period = 2013:2017, estimate_name = "DirectEst",
	variance_name = "DirectVar", geo_name = "geoid")

#' Read the objects needed for MCMC
z <- sp$get_z()
v <- sp$get_v()
H <- sp$get_H()
S <- sp$get_S()

#' Dimension reduction of `S` matrix via PCA.
eig <- eigen(t(S) %*% S) # TAKES A WHILE
rho = eig$values

library("RSpectra")
eig2 <- eigs_sym(t(S) %*% S, k = 4, which = "LA") # Know 4 from past run
head(eig$values)
head(eig2$values)

idx.S <- which(cumsum(rho) / sum(rho) < 0.65)
Tx <- eig$vectors[,idx.S]
f <- function(S) { S %*% Tx }
sp$set_basis_reduction(f)
S.reduced <- sp$get_reduced_S()

#' Plot the proportion of variation captured by our selection of PCA components.
eigprops <- cumsum(rho) / sum(rho)
plot(eigprops[1:200], xlab = "Dimension", ylab = "Proportion of Variation")
abline(v = max(idx.S), lty = 2)
abline(h = eigprops[max(idx.S)], lty = 2)

#' Pick a covariance structure for random coefficients of basis expansion.
if (FALSE) {
	# Morans I Basis
	K.inv <- sp$get_Kinv(2008:2015, method = "moran")
} else if (FALSE) {
	# Random Walk
	K.inv <- sp$get_Kinv(2008:2015, method = "randomwalk")
} else if (FALSE) {
	# Spatial-only (CAR)
	K.inv <- sp$get_Kinv(2008:2015, method = "car")
} else if (TRUE) {
	# Independence
	K.inv <- sp$get_Kinv(2008:2015, method = "independence")
}

#' Standardize observations before running MCMC.
z.scaled <- (z - mean(z)) / sd(z)
v.scaled <- v / var(z)

#' # Fit the Model
#+ message=FALSE
library(coda)

#' Fit MLE; this will serve as an initial value for MCMC.
K <- solve(K.inv)
mle.out <- mle.stcos(z.scaled, v.scaled, H, S.reduced, K,
	init = list(sig2K = 1, sig2xi = 1))
init <- list(
	sig2K = mle.out$sig2K.hat,
    sig2xi = mle.out$sig2xi.hat,
    mu_B = mle.out$mu.hat
)

#' Run the Gibbs sampler.
gibbs.out <- gibbs.stcos.raw(z.scaled, v.scaled, H, S.reduced, K.inv,
	R = 10000, report.period = 2000, burn = 2000, thin = 10, init = init)
print(gibbs.out)

#' Show some trace plots to assess convergence of the sampler.
plot((mu_B.mcmc <- mcmc(gibbs.out$mu_B.hist))[,1:3])
plot((xi.mcmc <- mcmc(gibbs.out$xi.hist))[,1:3])
plot((eta.mcmc <- mcmc(gibbs.out$eta.hist))[,1:3])

varcomps.mcmc <- mcmc(cbind(
	gibbs.out$sig2mu.hist,
	gibbs.out$sig2xi.hist,
	gibbs.out$sig2K.hist
))
colnames(varcomps.mcmc) <- c("sig2mu", "sig2xi", "sig2K")
plot(varcomps.mcmc)

#' #  Produce Results on target supports
#' Compute `H` and `S` matrices and get summaries of posterior distribution for E(Y).
#' Use 90% significance for all credible intervals and MOEs.

targ.src <- sp$domain2model(acs5.2015, period = 2011:2015, geo_name = "geoid")
E.hat.scaled <- fitted(gibbs.out, targ.src$H, targ.src$S.reduced)
E.hat <- sd(z) * E.hat.scaled + mean(z)                   # Uncenter and unscale
acs5.2015$E.mean <- colMeans(E.hat)                       # Point estimates
acs5.2015$E.sd <- apply(E.hat, 2, sd)                     # SDs
acs5.2015$E.lo <- apply(E.hat, 2, quantile, prob = 0.05)  # Credible interval lo
acs5.2015$E.hi <- apply(E.hat, 2, quantile, prob = 0.95)  # Credible interval hi
acs5.2015$E.median <- apply(E.hat, 2, median)             # Median
acs5.2015$E.moe <- apply(E.hat, 2, sd) * qnorm(0.95)      # MOE

targ.src <- sp$domain2model(acs5.2016, period = 2012:2016, geo_name = "geoid")
E.hat.scaled <- fitted(gibbs.out, targ.src$H, targ.src$S.reduced)
E.hat <- sd(z) * E.hat.scaled + mean(z)                   # Uncenter and unscale
acs5.2016$E.mean <- colMeans(E.hat)                       # Point estimates
acs5.2016$E.sd <- apply(E.hat, 2, sd)                     # SDs
acs5.2016$E.lo <- apply(E.hat, 2, quantile, prob = 0.05)  # Credible interval lo
acs5.2016$E.hi <- apply(E.hat, 2, quantile, prob = 0.95)  # Credible interval hi
acs5.2016$E.median <- apply(E.hat, 2, median)             # Median
acs5.2016$E.moe <- apply(E.hat, 2, sd) * qnorm(0.95)      # MOE

targ.src <- sp$domain2model(acs5.2017, period = 2013:2017, geo_name = "geoid")
E.hat.scaled <- fitted(gibbs.out, targ.src$H, targ.src$S.reduced)
E.hat <- sd(z) * E.hat.scaled + mean(z)                   # Uncenter and unscale
acs5.2017$E.mean <- colMeans(E.hat)                       # Point estimates
acs5.2017$E.sd <- apply(E.hat, 2, sd)                     # SDs
acs5.2017$E.lo <- apply(E.hat, 2, quantile, prob = 0.05)  # Credible interval lo
acs5.2017$E.hi <- apply(E.hat, 2, quantile, prob = 0.95)  # Credible interval hi
acs5.2017$E.median <- apply(E.hat, 2, median)             # Median
acs5.2017$E.moe <- apply(E.hat, 2, sd) * qnorm(0.95)      # MOE


targ.neighbs <- sp$domain2model(neighbs, period = 2013:2017, geo_name = "Region")
E.hat.scaled <- fitted(gibbs.out, targ.neighbs$H, targ.neighbs$S.reduced)
E.hat <- sd(z) * E.hat.scaled + mean(z)              # Uncenter and unscale
neighbs$E.median <- apply(E.hat, 2, median)
neighbs$E.moe <- apply(E.hat, 2, sd) * qnorm(0.95)
neighbs$E.mean <- apply(E.hat, 2, mean)
neighbs$E.sd <- apply(E.hat, 2, sd)
neighbs$E.lo <- apply(E.hat, 2, quantile, prob = 0.05)
neighbs$E.hi <- apply(E.hat, 2, quantile, prob = 0.95)

#' The objective of our analysis - predictions on the four target neighborhoods.
print(neighbs)

#' # Plot Results
#+ message=FALSE
library(gridExtra)
library(ggrepel)

#' Maps of direct and model-based 2017 5-year estimates.
lim.est <- range(acs5.2017$DirectEst, acs5.2017$E.mean)
g <- ggplot(acs5.2017) +
	geom_sf(colour = "black", size = 0.05, aes(fill = DirectEst)) +
	ggtitle("Median Household Income\nfor Boone County",
		subtitle = "2017 5yr ACS Direct Estimates") +
	scale_fill_distiller("DirectEst", palette = "RdYlBu", limits = lim.est) +
	theme_bw()
h <- ggplot(acs5.2017) +
	geom_sf(colour = "black", size = 0.05, aes(fill = E.mean)) +
	ggtitle("Median Household Income\nfor Boone County",
		subtitle = "2017 5yr Model Estimates") +
	scale_fill_distiller("E.mean", palette = "RdYlBu", limits = lim.est) +
	theme_bw()
k <- grid.arrange(g,h, ncol = 2)

#' Scatter plots comparing direct and model-based 5-year estimates for
#' 2012, ..., 2015.
g2015 <- ggplot(acs5.2015, aes(x=DirectEst, y=E.mean)) +
	geom_point(size = 2) +
	geom_abline(intercept = 0, slope = 1, color="red",
				linetype="dashed", size=1.2) +
	ggtitle("2015 5yr ACS Direct Estimates") +
	labs(x = "Direct Estimate", y = "Model-Based Estimate") +
	theme_bw()
g2016 <- ggplot(acs5.2016, aes(x=DirectEst, y=E.mean)) +
	geom_point(size = 2) +
	geom_abline(intercept = 0, slope = 1, color="red",
				linetype="dashed", size=1.2) +
	ggtitle("2016 5yr ACS Direct Estimates") +
	labs(x = "Direct Estimate", y = "Model-Based Estimate") +
	theme_bw()
g2017 <- ggplot(acs5.2017, aes(x=DirectEst, y=E.mean)) +
	geom_point(size = 2) +
	geom_abline(intercept = 0, slope = 1, color="red",
				linetype="dashed", size=1.2) +
	ggtitle("2017 5yr ACS Direct Estimates") +
	labs(x = "Direct Estimate", y = "Model-Based Estimate") +
	theme_bw()
k <- grid.arrange(g2015, g2016, g2017, nrow = 2, ncol = 2)



#' Plot neighborhood areas (target supports) among ACS 5-year direct estimates;
#' this gives a sense of whether the model-based esimtates are reasonable.
#' This map takes a bit of preparation.

Central <- neighbs[1,]
East <- neighbs[2,]
North <- neighbs[3,]
Paris <- neighbs[4,]

# Prevent `sf` package warnings like "st_centroid assumes attributes are
# constant over geometries of x"
st_agr(Central) <- "constant"
st_agr(East) <- "constant"
st_agr(North) <- "constant"
st_agr(Paris) <- "constant"

Central.coord <- st_coordinates(st_centroid(Central))
East.coord <- st_coordinates(st_centroid(East))
North.coord <- st_coordinates(st_centroid(North))
Paris.coord <- st_coordinates(st_centroid(Paris))

g <- ggplot(acs5.2017) +
	geom_sf(colour = "black", size = 0.05, aes(fill = DirectEst)) +
	ggtitle("Median Household Income for Boone County",
		subtitle = "ACS 2017 5yr Direct Estimates") +
	scale_fill_distiller("DirectEst", palette = "RdYlBu", limits = lim.est) +
	geom_sf(data = neighbs, fill = "black") +
	geom_label_repel(data = st_centroid(East), nudge_x = 20000, nudge_y = 130000,
		aes(x=East.coord[1], y=East.coord[2], label="East")) +
	geom_label_repel(data = st_centroid(Central), nudge_x = -10000, nudge_y = 130000,
		aes(x=Central.coord[1], y=Central.coord[2], label="Central")) +
	geom_label_repel(data = st_centroid(North), nudge_x = 0, nudge_y = 100000,
		aes(x=North.coord[1], y=North.coord[2], label="North")) +
	geom_label_repel(data = st_centroid(Paris), nudge_x = 10000, nudge_y = 100000,
		aes(x=Paris.coord[1], y=Paris.coord[2], label="Paris")) +
	xlab(NULL) +
	ylab(NULL) +
	theme_bw()
print(g)



#' Plot of the neighborhoods with the Model based estimate
g.est <- ggplot(neighbs) +
	geom_sf(colour = "black", size = 0.05, aes(fill = E.mean)) +
	ggtitle("Median Household Income for Boone County",
			subtitle = "ACS 2017 5yr Model Based Estimates") +
	scale_fill_distiller("ModelEst", palette = "RdYlBu", limits = lim.est) +
	theme_bw()
g.moe <- ggplot(neighbs) +
	geom_sf(colour = "black", size = 0.05, aes(fill = E.moe)) +
	ggtitle("Median Household Income for Boone County",
			subtitle = "ACS 2017 5yr Model Based MOE") +
	scale_fill_distiller("MOE", palette = "RdYlBu", limits = c(2000, 12000)) +
	theme_bw()
grid.arrange(g.est, g.moe, ncol = 2)	
