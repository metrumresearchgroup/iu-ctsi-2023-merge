library(dplyr)
library(purrr)
library(pmplots)
library(pmtables)
library(bbr)
library(yspec)
library(here) 
library(patchwork)
library(lastdose)
library(mrggsave)

# yspec ------------
spec <- ys_load(here("data/spec/analysis3.yml"))

spec

spec$WT

map(spec, "unit") %>% compact()

ys_document(spec, build_dir = definetemplate(), type = "regulatory")


# pmplots ------------
data <- pmplots_data_obs()
data <- nm_join(here("model/pk/106"))
data <- filter(data, EVID==0)

dv_pred(data)
cwres_time(data)


# lastdose ------------
data <- nm_data(read_model(here("model/pk/106")))
data$TAD <- NULL
data$LDOS <- NULL

lastdose(data) %>% as.data.frame() %>% head()


# mrggsave ------------
options(mrg.script = "acop-13-package.R", mrggsave.dir = here("deliv/figure"))
data <- nm_join(here("model/pk/106")) %>% filter(EVID==0)
p1 <- dv_pred(data)

mrggsave(p1, stem = "dv-pred") %>% print()


# pmtables ------------
data <- nm_join(here("model/pk/106"))
data <- filter(data, EVID==0)

pt_data_inventory(data) %>% 
  stable() %>%
  st_as_image() 

