
# Test Script

if(!require(pacman)) install.packages("pacman")
pacman::p_load(dplyr, lubridate, tictoc, readr, downloader, readxl, RCurl,
               writexl, tidyr, stringr, tibble, readr, htmlTable, tibbletime,
               devtools, roxygen2, downloader, shiny, plotly, RColorBrewer, ggnewscale,
               ggplot2, tidyverse, fasstr, lme4, sjPlot, sjmisc, RcppRoll, mgcv, rnrfa,
               viridis, gridExtra, naniar, purrr, ggfortify, glmmTMB, shiny)

# Load the package
devtools::load_all()

#devtools::document()

# Read-in metadata file
master.file <- read_excel("data/NDMN Ecology Master Network PROPOSED NEW FORMAT.xlsx", sheet = "Master NDMN")

# Filter for required BIOSYS site IDs
biolsites <- filter(master.file, Area_Code == "HNL")
biolsites <- biolsites$Biosys_Site_ID

# Try biology import function
biol_data <- import_biology(sites = biolsites)

# Try environmental data import function
env_data <- import_env(sites = biolsites)

# Filter master file for required RHS survey IDs
rhssites <- filter(master.file, Area_Code == "HNL")
rhssites <- rhssites$Ecosys_Survey_ID

# Try RHS import function
rhs_data <- import_rhs(surveys = rhssites)

# Try RICT
predict <- predict_indices(env_data = env_data)


#################################################################################

# Try calc_rfrstats

data("site.model.flow")

calc_rfrstats <-calc_rfrstats(data = site.model.flow,
                           site_col = "SITE_ID",
                            date_col = "Date_end",
                            flow_col = "Flow_HIST",
                            ref_col = "Flow_NAT",
                            q = 75,
                            save = FALSE,
                            save_dir = getwd())

##########################################################################

# Test HEV plot functions

data <- read_csv("data/data_HEV.csv")
data <- data %>% rename(Site_Code = SiteCode)
data <- data %>% rename(LIFE_F_OE = LIFE.F.OE)
data$Date2 <- lubridate::dmy(data$Date2)

# Try shiny

shiny_hev(data = data,
          sites_col = "Site_Code",
          flow_stat = c("RFRQ50.L1", "PFR50.L1"),
          biol_metric = c("WHPT_ASPT_OE", "LIFE_F_OE"),
          date_col = "Year")

# Try plot_hev

data_f <- filter(data, Site_Code == "W6-05")

plots <- plot_hev(data = data_f,
         flow_stat = c("RFRQ50.L1", "PFR50.L1"),
         biol_metric = c("WHPT_ASPT_OE", "LIFE_F_OE", "WHPT_NTAXA_OE"),
         date_col = "Date2",
         save = TRUE,
         clr_by = "Season")

########################################################################

# test diag_lmer

sleepstudy <- lme4::sleepstudy
sleepstudy$type <- rep(c("A","B","C"),60)

# run model
mod <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# run diagnostic plots - with facet option
diag_lmer(model = mod, data = sleepstudy, facet_by = "Subject", order_by = "mean",
               order = "ascending", ncol = 4, scales = "fixed", colour_by = "type")


## Example 2 - without facet option, and saving to png file
diag_lmer(model = mod, data = sleepstudy, facet_by <- NULL, order_by = "mean",
               order = "ascending", ncol = 4, scales = "fixed", colour_by = "type", save = TRUE)

########################################################################

# test model_cv

library(lme4)
library(mgcv)

## Example 1: Cross-validation on linear mixed-effects model
model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
out1 <- model_cv(model = model1, data = sleepstudy, group = "Subject", k = 5, r = 1)
out1[[1]] # RMSE
out1[[2]] # predicted values from cross-validation

# more precise estimate of RMSE by increasing r
model_cv(model = model1, data = sleepstudy, group = "Subject", k = 5,  r = 10)

# convergence issues, so try different optimizer
my_control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000))
model1b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, control = my_control)
model_cv(model = model1b, data = sleepstudy, group = "Subject", k = 5, r = 1, control = my_control)

## Example 2: Cross-validation on hierarchical generalised additive model
model2 <- gam(Reaction ~ s(Days) + s(Subject, bs = "re") + s(Days, Subject, bs = "re"), data = sleepstudy)
 model_cv(model = model2, data = sleepstudy, group = "Subject", k = 10, r = 1)

# compare alternative models
model2b <- gam(Reaction ~ s(Days) + s(Subject, bs = "re"), data = sleepstudy)
model2c <- gam(Reaction ~ s(Subject, bs = "re"), data = sleepstudy)
out2 <- model_cv(model = model2, data = sleepstudy, group = "Subject", k = 10, r = 1)
out2b <- model_cv(model = model2b, data = sleepstudy, group = "Subject", k = 10, r = 1)
out2c <- model_cv(model = model2c, data = sleepstudy, group = "Subject", k = 10, r = 1)
out2[[1]]; out2b[[1]] ;out2c[[1]]

# test model_logocv

library(lme4)
library(mgcv)

## Example 1: Cross-validation on linear mixed-effects model
model1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
out1 <- model_logocv(model = model1, data = sleepstudy, group = "Subject")
out1[[1]] # RMSE
out1[[2]] # predicted values from cross-validation

# convergence issues, so try different optimizer
my_control = lmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 10000))
model1b <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, control = my_control)
model_logocv(model = model1b, data = sleepstudy, group = "Subject", control = my_control)

## Example 2: Cross-validation on hierarchical generalised additive model
model2 <- gam(Reaction ~ s(Days) + s(Subject, bs = "re") + s(Days, Subject, bs = "re"), data = sleepstudy)
model_logocv(model = model2, data = sleepstudy, group = "Subject")

# compare alternative models
model2b <- gam(Reaction ~ s(Days) + s(Subject, bs = "re"), data = sleepstudy)
model2c <- gam(Reaction ~ s(Subject, bs = "re"), data = sleepstudy)
out2 <- model_logocv(model = model2, data = sleepstudy, group = "Subject")
out2b <- model_logocv(model = model2b, data = sleepstudy, group = "Subject")
out2c <- model_logocv(model = model2c, data = sleepstudy, group = "Subject")
out2[[1]]; out2b[[1]] ;out2c[[1]]

########################################################################

data("testdata_flowstats")

flow_stats <- calc_flowstats(data = testdata_flowstats,
                           site_col = "site",
                           date_col = "Date",
                           flow_col = "Flow")

# View outputs

flowstats_1 <- flow_stats[[1]]

flowstats_2 <- flowstats_output[[2]]

#######################################################################

# test join_he

data("biol_data")
data("flow_stats")
data("mapping")

flow_stats_1 <- flow_stats[[1]]

join_data <- join_he(biol_data = biol_data,
                     flow_stats = flow_stats_1,
                     mapping = mapping,
                     LS1 = TRUE,
                     LS2 = TRUE)

############################################################################

data("DU3_trimmed")

plot_rngflows(data = DU3_trimmed,
              flow_stats = c("Q95z", "Q10z"),
              biol_metric = "LIFE_F_OE",
              wrap_by = NULL,
              label = "SAMPLE_DATE",
              plotly = TRUE)


#######################################################################

data("data_HEV_pred")

pred_plot <- plot_predictions(data = data_hev_pred,
                  biol_metric = "WHPT_ASPT_OE",
                  time_col = "Date2",
                  site_col = "Site_Code",
                  flow_stat = c("RFRQ50.L1", "PFR50.L1"),
                  pred_col = c("predictions", "lower", "upper"),
                  ncol = 2,
                  save = TRUE)

############################################################################

data(df)

temp<- df
temp$flow[temp$date%in%lubridate::date(c("2000-02-07","2000-02-08","2000-12-12","2001-09-19"))]<-NA
temp$flow[temp$sites%in%c("SS60F015","029004") & temp$date%in%lubridate::date(c("2000-01-02","2001-06-22","2001-10-12"))]<-NA
temp$month <-lubridate::month(df$date)
temp$year <- lubridate::year(df$date)
temp1 <- temp %>% dplyr::group_by(month,year,sites) %>% dplyr::summarise(across(.cols= "flow", list(mean = ~mean(flow, na.rm = TRUE), missing = ~length(which(is.na(flow))), total = ~length(flow), perc_missing = ~(length(which(is.na(flow)))/length(flow))*100), .names = "{.fn}"))
temp1$yy_mm<-paste(temp1$year,temp1$month,sep="_")



# Heatmap of monthly mean flows.
a <- plot_heatmap(data = temp1, x = "yy_mm", y = "sites", fill = "mean",dual = TRUE)
gridExtra::grid.arrange(a[[1]])

# Heatmap of monthly  percentage completeness of the daily flow data
a <- plot_heatmap(data = temp1, x = "yy_mm", y = "sites", fill = "perc_missing")
a[[1]]
############################################################################
# import_flow

import_flow <- import_flow(sites= c("1001", "0130TH", "F1707"),
                      inputs = c("NRFA", "FLOWFILES", "HDE"),
                      start_date="2001-01-01",
                      end_date="2002-01-01",
                      dir="data/wiski",
                      skip_num = 21,
                      col_order = c(1,2,3))


############################################################################

# plot_sitepca

data("df_inv_open_site")

plot <- plot_sitepca(data = df_inv_open_site,
                     vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"))


# Example 2
plot<- plot_sitepca(data = df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), eigenvectors = TRUE)
plot

# Example 3
plot<- plot_sitepca(data = df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), label_by = "SITE_ID")
plot

# Example 4
plot<- plot_sitepca(data = df_inv_open_site, vars = c("ALTITUDE", "SLOPE", "DIST_FROM_SOURCE", "WIDTH", "DEPTH"), colour_by = "WATERBODY_TYPE", plotly=TRUE)
plot

############################################################################


# Run test files
devtools::test()

