#setup
library(sdmTMB)
library(dplyr)
library(ggplot2)
library(raster)
library(Mar.data)
library(sf)
library(terra)
library(marmap)
library(PBSmapping)
options(ggplot2.continuous.colour = "viridis")
options(ggplot2.continuous.fill = "viridis")
options(ggplot2.discrete.colour = RColorBrewer::brewer.pal(8, "Set2"))
options(ggplot2.discrete.fill = RColorBrewer::brewer.pal(8, "Set2"))
theme_set(theme_light())
source(file.path(getwd(), "directories.r"))
source(file.path(wd, "passwords.r"))


#functions
plot_map <- function(data, column_name) {
  ggplot(data, aes(X, Y, fill = {{ column_name }})) +
    facet_wrap(~year) +
    geom_raster() +
    coord_fixed()
}

##### Data ######
#syear <- 1970
d <- read.csv(file.path(datadir,"RandomHalibutSurveyData_LF.csv"))
d <- read.csv(file.path(datadir,"HalibutSurveyHookData.csv")) # hook data
d$empty_baited[d$empty_baited==0]<-1
d$prop_baited<-d$empty_baited / d$total_sampled

d<-subset(d,select=c("YEAR","LONGITUDE","LATITUDE","total_target_species","NUM_HOOK_HAUL","DEPTH","prop_baited"))
names(d)<-c("year", "longitude", "latitude", "number","hooks", "depth","prop_baited")
d$depth[d$depth==0]<-NA
d<-na.omit(d)
d$log_depth<-log(d$depth)
yrs <- sort(unique(d$year))
all_yrs <- seq(min(yrs), max(yrs))

#plot
ggplot(d, aes(longitude, latitude, size = number, colour = number)) +
  geom_point(pch = 21) +
  scale_size_area() +
  facet_wrap(~year) +
  labs(title = "Halibut Density")

# Add UTM coordinates
d <- add_utm_columns(d, utm_crs = 22820)
d$rowid<-as.factor(1:nrow(d))
d$prop<-d$number/d$hooks

#plot
ggplot(d, aes(X, Y, size = number, colour = number)) +
  geom_point(pch = 21) +
  scale_size_area() +
  facet_wrap(~year) +
  coord_equal() +
  labs(title = "Halibut Density")


d$hook_adj_factor<- -log(d$prop_baited)/(1-d$prop_baited)
d<-na.omit(d)
d$adj_prop<-(d$number * d$hook_adj_factor)/d$hooks

### Mesh ###
# Create a finite element mesh
mesh <- make_mesh(d, c("X", "Y"), cutoff = 50)
plot(mesh)

##### Models #####

  # Model 1: IID spatiotemporal fields neg binom
  fit_n_iid_nb <- sdmTMB(
    number ~ 0 + factor(year)+ poly(log_depth, 2) ,
    data = d,
    mesh = mesh,
    offset = log(d$hooks),
    time = "year",
    spatiotemporal='iid',#ar1, #rw
    family = nbinom2(),
    #family = poisson(),
    anisotropy = F,
    silent=F
  )
  sanity(fit_n_iid_nb)
  fit_n_iid_nb


  # Model 2: IID spatiotemporal fields beta binom
  fit_n_iid_bb <- sdmTMB(
    prop ~ 0 + factor(year)+ poly(log_depth, 2) ,
    data = d,
    mesh = mesh,
    time = "year",
    weights=d$hooks,
    spatiotemporal='iid',#ar1, #rw
    family = betabinomial(link="cloglog"),
    #family = poisson(),
    anisotropy = F,
    silent=F
  )
  sanity(fit_n_iid_bb)
  fit_n_iid_bb


  fit_n_iid_ps <- sdmTMB(
    number ~ 0 + factor(year)+ poly(log_depth, 2) + (1|rowid) ,
    data = d,
    mesh = mesh,
    offset = log(d$hooks),
    time = "year",
    spatiotemporal='iid',#ar1, #rw
    #family = nbinom2(),
    family = poisson(),
    anisotropy = F,
    silent=F
  )
  sanity(fit_n_iid_ps)
  fit_n_iid_ps


  AIC(fit_n_iid_nb,fit_n_iid_bb,fit_n_iid_ps)

  # Model 2: IID spatiotemporal fields beta binom
  fit_n_iid_bb_hookadj <- sdmTMB(
    adj_prop ~ 0 + factor(year)+ poly(log_depth, 2) ,
    data = d,
    mesh = mesh,
    time = "year",
    weights=d$hooks,
    spatiotemporal='iid',#ar1, #rw
    family = betabinomial(link="cloglog"),
    #family = poisson(),
    anisotropy = F,
    silent=F
  )
  sanity(fit_n_iid_bb_hookadj)
  fit_n_iid_bb_hookadj





  # Model 1: IID spatiotemporal fields
  fit_b_iid_tw <- sdmTMB(
    weight_kg ~ 0 + factor(year)+ poly(log_depth, 2),
    data = d,
    mesh = mesh,
    time = "year",
    spatiotemporal='iid',
    family = tweedie(),
    anisotropy = F,
    silent=F
  )
  sanity(fit_b_iid_tw)
  fit_b_iid_tw


  # Model 1: IID spatiotemporal fields
  fit_n_iid_gg <- sdmTMB(
    number ~ 0 + factor(year)+ poly(log_depth, 2) + survey_abbrev,
    data = d,
    mesh = mesh,
    time = "year",
    spatiotemporal='iid',#ar1, #rw
    #family = tweedie(),
    family = delta_gengamma(type = "poisson-link"),
    anisotropy = F,
    silent=F
  )
  sanity(fit_n_iid_gg)
  fit_n_iid_gg


  # Model 1: IID spatiotemporal fields
  fit_b_iid_gg <- sdmTMB(
    weight_kg ~ 0 + factor(year)+ poly(log_depth, 2),
    data = d,
    mesh = mesh,
    time = "year",
    spatiotemporal='iid',
    #family = tweedie(),
    family = delta_gengamma(type = "poisson-link"),
    anisotropy = F,
    silent=F
  )
  sanity(fit_b_iid_gg)
  fit_b_iid_gg

  #


##### Grid for predictions #####

# grid 1 : the original halibut survey grid

  g <- read.csv(file.path(datadir,"Survey","blockIDkey_ZeroStrata2023.csv"))

  #Canada Albers Equal Area Conic Projection (ESRI 102001, resources.arcgis.com)
  g = st_as_sf(g, coords = c("xAEAm", "yAEAm"), crs = "ESRI:102001") #original data as a sf object

  g <- st_transform(g, crs = 22820)
  grid <- data.frame(st_coordinates(g)/1000,depth=g$blockMidDepth_m )
  grid$log_depth<-log(grid$depth)

  # Expand the grid to all sampled years
  grid1 <- replicate_df(grid, "year", unique(d$year))

  ggplot(grid, aes(X, Y)) +
    geom_tile(width = 2, height = 2, fill = "grey50") +
    geom_point(data = d, size = 2, pch = ".", colour = "red") +
    geom_point(data=surveyPoly,col='red') +
    coord_equal()

# grid 2 : a square grid


  grid <- expand.grid(
    X = seq(floor(min(d$X)), ceiling(max(d$X)), 4)*1000,
    Y = seq(floor(min(d$Y)), ceiling(max(d$Y)), 4)*1000
  )

  grid <- st_as_sf(grid, coords = c("X", "Y"), crs = 22820)
  grid_ll <- st_transform(grid, crs = 4326) # EPSG:4326 = WGS84 lon/lat

  ## add depth to grid
  NOAAdata <- getNOAA.bathy(lon1 = -69, lon2 = -59,lat1 = 40, lat2 = 46, resolution = 0.5)
  noaa_mat <- as.matrix(NOAAdata)
  class(noaa_mat) <- "matrix"
  noaa_df <- as.data.frame.table(noaa_mat)
  colnames(noaa_df) <- c("lon", "lat", "value")
  noaa_df$lon <- as.numeric(as.character(noaa_df$lon))
  noaa_df$lat <- as.numeric(as.character(noaa_df$lat))

  NOAA_raster <- rasterFromXYZ(noaa_df, crs = CRS("+proj=longlat +datum=WGS84"))

  NOAAterra <- rast(NOAA_raster)
  coords <- st_coordinates(grid_ll) # matrix of lon/lat

  # Interpolate using bilinear method
  vals <- terra::extract(NOAAterra, coords, method = "bilinear") # [,2] for value

  # Assign to your grid
  grid_ll$NOAA_value <- vals *-1
  g <- st_transform(grid_ll, crs = 22820) # EPSG:4326 = WGS84 lon/lat

  # strata to use for index
  load(file.path(datadir,"Survey","SurveyStrata2024.RData"))
  ps <- as.PolySet(surveyPolyLL, projection = "LL")

  # ---- Conversion to sf ----
  # PBSmapping stores polygons as data frames; we need to group by PID
  sf_poly <- ps %>%
    group_by(PID) %>%
    summarise(do_union = FALSE) %>%
    summarise(
      geometry = st_sfc(
        st_polygon(list(as.matrix(select(ps, X, Y))))
      ),
      .groups = "drop"
    ) %>%
    st_as_sf(crs = 4326)  # Set CRS (WGS84 here)

  polygon <- st_as_sf(surveyPolyLL, coords = c("X", "Y"), crs = 4326)
  polygon <- st_union(polygon)

  polygon <- st_transform(polygon, crs = st_crs(g))

  inside <- st_within(g, polygon, sparse = FALSE)

  g <- g[inside[,1], ]
  grid <- data.frame(st_coordinates(g)/1000)
  grid$log_depth<-log(g$NOAA_value$value)

  # Expand the grid to all sampled years
  grid <- replicate_df(grid, "year", unique(d$year))

  head(grid)

  ggplot(grid, aes(X, Y)) +
    geom_tile(width = 2, height = 2, fill = "grey50") +
    geom_point(data = d, size = 2, pch = ".", colour = 'red') +
    coord_equal()

#Generate predictions and calculate an index:

p <- predict(fit_n_iid_bb_hookadj, newdata = grid1, return_tmb_object = TRUE)
pdata<-p$data
ind_iid <- get_index(p, area = 16)
#ind_iid <- get_index_split(fit_iid, newdata = grid, nsplit = 2, bias_correct = F, area = 4)

plot(y=ind_iid$est,x=ind_iid$year,type="l")

# Depth and year effect contribution:
# (Everything not a random field)
max_est <- quantile(pdata$est, 0.99)
pdata <- mutate(pdata, est_trim = if_else(est > max_est, max_est, est))
pdata$X<-pdata$X*1000
pdata$Y<-pdata$X*1000
pdata <- st_as_sf(pdata, coords = c("X", "Y"), crs = 22820) #original data as a sf object
#p$data <- st_transform(p$data, crs = "ESRI:102001")

ggplot(pdata) +
  geom_sf(aes(fill = exp(est)), color = NA) +
  facet_wrap(~year) +
  scale_fill_viridis_c() +
  theme_minimal() +
  coord_sf(crs = st_crs(pdata))  # explicitly set CRS

ggplot(p$data ) +
  geom_sf(aes(fill = exp(est)),color=NA) +
  facet_wrap(~year) +
  coord_sf(crs = st_crs(p$data))
# Spatial random field:
ggplot(p, aes(X, Y, fill = omega_s)) +
  facet_wrap(~year) +
  geom_raster() +
  scale_fill_gradient2() +
  coord_fixed()

# Spatial-temporal random field:
ggplot(p, aes(X, Y, fill = epsilon_st)) +
  facet_wrap(~year) +
  geom_raster() +
  scale_fill_gradient2() +
  coord_fixed()

# Overall estimate of density in link (log) space:
ggplot(p, aes(X, Y, fill = est)) +
  facet_wrap(~year) +
  geom_raster() +
  coord_fixed()

# Overall estimate of density: (with log-distributed colour)
ggplot(p, aes(X, Y, fill = exp(est))) +
  facet_wrap(~year) +
  geom_raster() +
  coord_fixed() +
  scale_fill_viridis_c(trans = "log10")


# Model 2: IID spatiotemporal fields biomass
bfit_iid <- sdmTMB(
  density_kgpkm2  ~ factor(year),
  data = d,
  mesh = mesh,
  time = "year",
  family = tweedie(),
  anisotropy = TRUE
)

sanity(bfit_iid)
bfit_iid

pb <- predict(bfit_iid, newdata = grid, return_tmb_object = TRUE)

bind_iid <- get_index_split(bfit_iid, newdata = grid, nsplit = 2, bias_correct = TRUE)

load("data/model/tmb_dataU3_PAL.RData")
plot(y=RVindex$B,x=RVindex$Year,type="l")
glimpse(p)

plot_map(p, est1) +
  scale_fill_viridis_c()


