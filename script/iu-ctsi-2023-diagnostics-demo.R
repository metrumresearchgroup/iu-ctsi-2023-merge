# ACoP Hands-on Session 1 ------

## Libraries ----------------------------
suppressPackageStartupMessages(library(tidyverse)) 
library(pmplots)
library(pmtables)
library(bbr)
library(yspec)
library(here) 
library(rmarkdown)
library(patchwork)


# read in the spec file  ----------------------------
spec <- load_spec(here("data/spec/analysis3.yml"))

# look at what's in the spec file and view different namespaces
head(spec)
ys_namespace(spec)
spec <- ys_namespace(spec, "plot")
head(spec) # changed TIME units to h


## read in the NM data  ----------------------------
mod <- read_model(here("model", "pk", "106"))

## join NM data and model output into a single file  ----------------------------
data0 <- nm_join(mod)

# for plotting - filter to observations & decode categorical columns to factors
data <- data0 %>% 
  filter(EVID==0) %>% 
  yspec_add_factors(spec, .suffix = "")


## get covariates of interest from the spec file ----------------------------
pull_meta(spec, "flags")$diagContCov

diagContCov <- pull_meta(spec, "flags")$diagContCov
diagCatCov <- pull_meta(spec, "flags")$diagCatCov


# NPDE vs continuous covariates ----------------------------

# get the column names for the continuous covariates from the spec
contCo <- spec %>% 
  ys_select(all_of(diagContCov)) %>%      
  axis_col_labs(title_case = TRUE,   
                short_max = 10) %>%  
  as.list()

contCo

# make the plot for a single covariate
npde_cont(data, contCo$AGE)

# or map over the list of covariates
pList <- purrr::map(contCo, ~ npde_cont(data, x = .x))

# panel plots using patchwork
( pList$AGE + pList$WT ) / (pList$ALB + pList$EGFR)

# or using pmplots

pm_grid(pList, ncol=2)

# similar plots/methods for categorical covariates using npde_cat


# ETAs vs categorical covariates ----------------------------
# plot each categorical covariate vs eta

# one row per id
id <- data %>% distinct(ID, .keep_all = T)

# get the column names for the categorical covariates from the spec
catCo <- spec %>% 
  ys_select(all_of(diagCatCov)) %>%   # select the covariates of interest
  axis_col_labs(title_case=TRUE) %>% 
  as.list()

catCo

# extract the ETAs directly from combined data
etas <- stringr::str_subset(names(data), "ET")
etas

# or use the pmplot syntax to define the ETAs and the new labels for the plots
etas <- c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
etas

# plot each eta vs renal function
eta_cat(id, catCo$RF, etas)

# and put them in a single plot
eta_cat(id, catCo$RF, etas) %>% pm_grid(ncol=1)

# or map over the list of covariates
p <- map(catCo, ~ eta_cat(id, .x, etas) %>% pm_grid(ncol=1)) 

# take a look at the four different plots
p$STUDY
p$RF


## ETAs vs continuous covariates ----------------------------
# for each eta, make a faceted plot with all continuous covariates
contCo

# make a wrapped plot of eta vs age
wrap_eta_cont(id, contCo$AGE, etas)

# fix the panel labels to use the labels provided for ETAs
wrap_eta_cont(id, contCo$AGE, etas, use_labels=T)

# write a simple function to map across all etas and
# and plot eta vs all continuous covariates
map_wrap_eta_cont <- function(.id, .co, .etas){
  p <- wrap_eta_cont(.id,
                     x = .co, y = .etas, 
                     use_labels = TRUE, 
                     scales= "free_x")
}

# Call the map function
p <- purrr::map(.x = etas, ~ map_wrap_eta_cont(id, contCo, .x))
p[[1]]

# we also use these wrap functions in during EDA too, 
# e.g., plot your continuous covariates by renal function
wrap_cont_cat(id, 
              x = "RF//Renal Function Group",
              y = contCo,
              use_labels = TRUE)


## pairs plots ----------------------------

# create a pairs plot of the continuous covariates
pairs_plot(id, contCo)

# notice panels were automatically renamed using the "short (unit)" from your spec 
# you could modify this here if needed. 
# For example, put the units of EGFR on the next line to help it fit in the panel
co2 <- contCo
co2$EGFR <- "EGFR//EGFR \n(mL/min/1.73m2)"

pairs_plot(id, co2)


# similarly create a pairs plot of the etas
eta_pairs(id, etas)

# Make plots from a diagnostics template --------------------

##' Support for parameterized reports can be found
##' https://bookdown.org/yihui/rmarkdown/parameterized-reports.html

## Opt 1. Create diagnostics using our template and helper function -----------------

# load the helper function
source(here("script/functions-diagnostics.R"))

# point to the diagnostic template
rmd_template <- here("script", "diagnostic-templates", "diagnostics-report.Rmd")

# define the specifics of your model
modelSpecifics <- list(
  contCov = c("AGE","WT","ALB","EGFR"), 
  catCov = c("STUDY", "RF", "CP", "DOSE"),
  etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
)

# pass the model object (or path), modelSpecifics and template locations to our
# helper function and pipe the output directly to browseURL to pop open the html 
# version in new browser window
model_diagnostics(
    read_model(here("model", "pk", 106)),
    modelSpecifics,
    template = rmd_template
  ) %>% 
  browseURL()

# pdf and pngs of all plots were also saved to deliv > figure > 106

## Opt 2. Create diagnostics using our template and Rmarkdowns render function ------

# define the model directory 
modelDir <- here("model", "pk")

# point to the diagnostic template
rmd_template <- here("script", "diagnostic-templates", "diagnostics-report.Rmd")

# define the specifics of your model
modelSpecifics <- list(
  run = 106,
  contCov = c("AGE","WT","ALB","EGFR"), 
  catCov = c("STUDY", "RF", "CP", "DOSE"),
  etas = c("ETA1//ETA-KA", "ETA2//ETA-V/F", "ETA3//ETA-CL/F")
)

# render the diagnotics template
render(rmd_template,
       params = modelSpecifics, 
       output_dir = here("model", "pk", "106"), 
       output_file = "diagnostics"
)

# pop the html version open in new browser window
browseURL(here("model", "pk", "106", "diagnostics.html"))

# pdf and pngs of all plots were also saved to deliv > figure > 106
fs::dir_ls(here::here("deliv", "figure", "106"))
