library(bbr)

edit_model <- function(.mod){
  get_model_path(.mod) %>% 
    file.edit()
}

view_output_file <- function(.mod, .extension){
  ## grab file with .extension
  tmpdir <- get_output_dir(.mod)
  tmpfile <- list.files(tmpdir, pattern=regex(.extension, ignore_case = TRUE))
  
  ## return NULL if file doesn't exist
  if(length(tmpfile) == 0){
    warning("File does not exist")
    return(NULL)
  }
  
  ## open file
  file.edit(file.path(tmpdir,tmpfile))
}

view_lst <- function(.mod){
  view_output_file(.mod, .extension="lst")
}

view_prderr <- function(.mod){
  view_output_file(.mod, .extension="PRDERR")
}

update_run_number <- function(
  .mod,
  .suffixes = c(
    '_its.ext',
    '_saem.ext',
    '_imp.ext',
    '.MSF',
    '.ext',
    '.tab',
    '.chn',
    'par.tab'
  )
){
  runno <- get_model_id(.mod)
  modelfile <- get_model_path(.mod)
  based_on <- .mod$based_on[1]
  message(glue::glue("replacing {based_on} with {runno} in {modelfile}"))
  
  txt <- readLines(modelfile) 
  ## edit text of new model file
  for (.s in .suffixes) {
    txt <- gsub(
      paste0(based_on, .s), 
      paste0(runno, .s), 
      txt, 
      ignore.case = T
    )
  }
  ## write updated model file
  cat(txt, file=modelfile, sep='\n')
  ## return model to make this a pipeable function
  return(.mod)
}


copy_mod <- function(.parent_mod, ...){
  ## grab model directory path
  dir <- get_output_dir(.parent_mod) %>% dirname()
  # dir <- build_path_from_model(.parent_mod) %>% dirname()
  ## find next integer
  child_mod <- get_next_integer(dir, ...)
  ## copy model using default settings
  copy_model_from(.parent_mod=.parent_mod, .new_model=child_mod, 
                  .inherit_tags = T, ...) %>%
    update_run_number()
}


## return next largest integer for CTL files in the model directory
get_next_integer <- function(.dir, .padding = TRUE){
  mod.n <- list.files(.dir, pattern="ctl") %>% 
    str_extract_all("\\d+") %>% 
    unlist %>% 
    as.integer %>% 
    max + 1
  
  if(.padding) mod.n <- pad_zeroes(mod.n)
  
  return(mod.n)
}

pad_zeroes <- function(.int){
  ## currently this pads to 3 digits, not generalized solution
  ## padding with zeroes for values < 100
  if(.int<10) .int <- paste0("00",.int)
  if(.int>=10 && .int<100) .int <- paste0("0", .int)
  return(.int)
}

mod_diff <- function(.x,.y){
  model_diff(
    file.path(MODEL_DIR, .x) %>% read_model,
    file.path(MODEL_DIR, .y) %>% read_model
  )
}

## from Tim W example project Bayes
#' Run Bayes chains
#' 
#' Run multiple chains of a Bayes model after initial estimates have been
#' generated
#'
#' @param .model_dir path to directory containing model
#' assumes template model structure:
#' $EST METHOD=CHAIN ...
#' ;$EST METHOD=BAYES ...
#' ;$TABLE ...
#' @param .run run name
#' @param .bbi_args list of arguments to be passed to `submit_model()`
run_chains <- function(.model_dir, .run, .mode = "sge", .bbi_args) {
  mod <- read_model(file.path(.model_dir, .run))
  ctl <- read_lines(get_model_path(mod))
  
  row_bayes <- str_detect(ctl, "METHOD=BAYES|METHOD=NUTS")
  est_bayes <- ctl[row_bayes]
  est_bayes <- str_replace(est_bayes, "^;", "")
  ctl[row_bayes] <- est_bayes
  
  row_table <- str_detect(ctl, ";\\s*\\$TABLE")
  block_table <- ctl[row_table]
  block_table <- str_replace(block_table, "^;", "")
  # ctl[row_table] <- block_table ## update table names before writing
  
  row_chain <- str_detect(ctl, "METHOD=CHAIN")
  est_chain <- ctl[row_chain]
  n_chain <- as.numeric(str_extract(est_chain, "(?<=NSAMPLE=)[0-9]+"))
  est_chain <- str_replace(est_chain, "NSAMPLE=[0-9]+", "NSAMPLE=0")
  est_chain <- str_replace(est_chain, "FILE=", "FILE=../")
  
  row_data <- str_detect(ctl, "\\$DATA")
  data_record <- ctl[row_data]
  ctl[row_data] <- str_replace(data_record, "\\$DATA\\s+", "$DATA ../")
  
  row_extrasend <- str_detect(ctl, "extrasend")
  ctl[row_extrasend] <- str_replace(ctl[row_extrasend], "extrasend", "../extrasend")
  
  walk(seq_len(n_chain), function(.chain) {

    est_chain_i <- str_replace(est_chain, "ISAMPLE=0", glue::glue("ISAMPLE={.chain}"))

    est_bayes_i <- str_replace(est_bayes, "SEED=[0-9]+", glue::glue("SEED={.chain}")) %>% 
      str_replace(glue::glue("FILE={.run}.ext"), glue::glue("FILE={.run}_{.chain}.ext")) %>% 
      str_replace(glue::glue("FILE={.run}.EXT"), glue::glue("FILE={.run}_{.chain}.EXT"))
    
    block_table_i <- str_replace(block_table, 
                                 glue::glue("FILE={.run}.tab"), 
                                 glue::glue("FILE={.run}_{.chain}.tab")) %>%
      str_replace(glue::glue("FILE={.run}.TAB"), 
                  glue::glue("FILE={.run}_{.chain}.TAB"))
    #cat(est_chain_i, "\n")
    ctl_i <- ctl
    ctl_i[row_chain] <- est_chain_i
    ctl_i[row_bayes] <- est_bayes_i
    ctl_i[row_table] <- block_table_i
    write_lines(ctl_i, file.path(
      .model_dir,
      glue::glue("{.run}/{.run}_{.chain}.ctl"))
    )
    
    mod <- new_model(
      #glue::glue("./{.run}/{.run}.{.chain}.yaml"),
      file.path(.model_dir, .run, glue::glue("{.run}_{.chain}")),
      .description = glue::glue("Chain {.chain}"),
      .overwrite = TRUE
    )
    
    proc <- submit_model(
      mod,
      #.directory = file.path(.model_dir, .run),
      .bbi_args = .bbi_args,
      .mode = .mode,
      .config_path = file.path(.model_dir, "bbi.yaml")
      #.dry_run = FALSE
    )
  })
}


format_tags_diff <- function(.log_df, .keep = FALSE) {
  if (
    !any(str_detect(names(.log_df), "tags_added")) ||
    !any(str_detect(names(.log_df), "tags_removed"))
  ) {
    stop("Must have both `tags_added` and `tags_removed` columns to call `format_tags_diff()`. Use `add_tags_diff()` to create them")
  }
  .log_df <- bbr::suppressSpecificWarning(
    collapse_to_string(.log_df, tags_added, tags_removed),
    .regexpr = "collapse_to_string\\(\\) only works on list columns"
  ) %>%
    mutate(
      tags_diff = paste0(tags_added, "; ~~", tags_removed, "~~") %>% 
        str_replace("; ~~~~","") %>% 
        str_replace("^; ", "")
    )
  if (isFALSE(.keep)) {
    .log_df <- select(.log_df, -tags_added, -tags_removed)
  }
  return(.log_df)
}

#' Arrange tags on a model object
#'
#' Arrange the tags on a model object so that they appear in the same order as
#' in the corresponding tags YAML file.
#'
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param glossary Tags glossary list (e.g., value from
#'   `yaml::read_yaml("tags.yaml")`)
arrange_tags <- function(.mod, glossary = yaml::read_yaml("tags.yaml")) {
  x <- .mod[["tags"]]
  y <- unlist(glossary)
  return(replace_all_tags(.mod, .tags = x[order(match(x, y))]))
}

arrange_dir_tags <- function(.dir, .tags){
  walk(pull(run_log(.dir, .recurse=FALSE), "run"), ~ {
    read_model(file.path(.dir, .x)) %>%
      arrange_tags(glossary=.tags)
  })
}

#' Assign unique note to ONLY ONE model in a directory
#' 
#' Assign a note to a given model and remove it from
#' all other models in the model directory
#' 
#' @param .mod The `bbi_{.model_type}_model` object to modify
#' @param .note string to be uniquely assigned to `.mod`
new_best <- function(.mod, .note="**CURRENT BEST**"){
  ## extract directory from bbr model object
  .dir <- dirname(get_model_path(.mod))
  
  ## list all models in directory
  dirmods <- list.files(path=.dir, pattern="ctl") %>% 
    str_remove(".ctl")
  
  ## don't touch the loaded .mod to avoid error
  dirmods <- dirmods[dirmods != get_model_id(.mod)]
  
  ## remove note from all mods
  walk(dirmods, ~ suppressSpecificWarning({
    file.path(.dir,.x) %>% read_model %>% remove_notes(.note)
  }, "`notes` does not contain any of the following, so they cannot be removed"))
  
  ## assign note to FIRST NOTE of .mod (always displayed)
  notes <- .mod$notes
  return(.mod %>% remove_notes(notes) %>% add_notes(c(.note, notes)))
}

## return run number with "current best" tag
## complement to the "new_best" function
get_best <- function(.dir, .note="**CURRENT BEST**"){
  .clean_string <- str_replace_all(.note, "\\*", "\\\\*")
  yams <- list.files(path=.dir, pattern="[[:digit:]+].yaml")
  
  purrr::map(yams, ~ {
    out <- yaml::read_yaml(file.path(.dir,.x)) 
    out$run <- .x
    return(out)
  }) %>% 
    do.call(bind_rows,.) %>% 
    distinct(run,notes) %>% 
    filter(str_detect(notes, .clean_string)) %>% 
    pull(run) %>% str_extract("[:digit:]+")
}

add_aic <- function(.runlog) mutate(.runlog, aic=2*param_count + ofv)
