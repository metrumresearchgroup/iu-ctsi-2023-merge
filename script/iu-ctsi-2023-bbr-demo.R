
library(bbr)
library(here)
library(pmtables)
library(pmplots)

MODEL_DIR <- here("model/pk")

######################
# model execution    #
######################

# read in parent model
mod106 <- read_model(file.path(MODEL_DIR, 106))
mod106

model_summary(mod106)

# create new model
mod107 <- copy_model_from(mod106, "107") %>%
  update_model_id()

# see difference in control streams
model_diff(mod107)

# submit the new model
submit_model(mod107, .mode = "local", .wait = FALSE)

# check output files while running
tail_lst(mod107)
tail_output(mod107, .tail = 20)

wait_for_nonmem(mod107)

######################
# model summary      #
######################

# look at model summary, once finished
sum107 <- model_summary(mod107)
sum107

# load final parameter estimates as a tibble
param107 <- param_estimates(sum107)
View(param107)

# join input data to output tables
df107 <- nm_join(mod107)
View(df107)

# make some simple diagnostic plots with MetrumRG's pmplots package
dv_pred(df107) 
dv_ipred(df107)
npde_time(df107)

# render the diagnostics with the Expo parameterized Rmd template. More details in: 
# https://merge.metrumrg.com/expo/expo1-nonmem-foce/posts/model-diagnostics-param-reports.html
source(here("script/functions-diagnostics.R"))
model_diagnostics(
  mod107,
  .p = list(diagContCov = c("AGE"), diagCatCov = c("RF", "CP"))
) %>% browseURL()


######################
# model annotation   #
######################

# add tags and notes
mod107 <- mod107 %>%
  add_tags("NewTag") %>%
  add_notes("This was a great model")

tags_diff(mod107)

# create a run log table
log_df <- run_log(MODEL_DIR)
View(log_df)

# add model_summary() columns
log_df <- log_df %>%
  add_summary()
View(log_df)

# show nicely formatted table with MetrumRG's pmtables package
log_df %>%
  select(run, ofv, tags, notes) %>%
  collapse_to_string(tags, notes) %>%
  pmtables::st_new() %>%
  pmtables::st_left(tags = col_ragged(8), notes = col_ragged(6)) %>% # fix column width for text-wrapping
  pmtables::st_as_image()


#######################
# execute in parallel #
#######################

# earlier models are too fast/simple to run in parallel,
# model 200 is intentionally more complex for demonstration
mod200 <- read_model(file.path(MODEL_DIR, 200))

# submits to run on SGE grid by default (without `.mode = "local"`)
# pass `threads` through `.bbi_args` to run in parallel
submit_model(mod200, .bbi_args = list(threads = 8))

# check the queue for the SGE grid
# job will be "pending" until compute nodes come up
system("qstat -f")

# wait to finish and then look at summary
wait_for_nonmem(mod200)
model_summary(mod200)

