#' build_assessment_bookdown
#' @description This function will build a skeleton/template html bookdown summarizing a single model MPD (casal2 -r, -e)
#' @author Craig Marsh
#' @param csl_dir directory path to where the Casal2 model is. Currently this function assumes config_filename and mpd_filename are in this directory
#' @param output_folder_name foldername for bookdown. It will be relative to csl_dir
#' @param mpd_filename the filename for the Casal2 model output
#' @param config_filename the filename for the Casal2 config. Default is 'config.csl2' can be replaced with others if you use the -c argument when running Casal2
#' @param verbose if the function unexpectedly quits errors, set this to T to see where the problem lies
#' @param prompt_user_before_deleting <bool> everytime this function is run it can delete existing bookdowns. Should always be T, unless used in unittests
#' @param model_label string a model label used for headers
#' @return will save a suite of Rmd files in output_dir that can be compiled as a bookdown. It should compile the bookdown in the folder _book
#' @importFrom Casal2 extract.mpd
#' @importFrom bookdown render_book
#' @importFrom knitr kable
#' @examples
#' \dontrun{
#' library(r4Casal2)
#' }
#' @rdname build_assessment_bookdown
#' @export build_assessment_bookdown
#'
# csl_dir = "C:\\Users\\marshc\\OneDrive - NIWA\\22_23\\SNA1\\csl\\ENLD\\Casal2"
# mpd_filename = "estimate.log"
# config_filename = "config.csl2"
# output_folder_name = "BookDown";model_label = NULL; verbose = F
#
build_assessment_bookdown <- function(csl_dir, output_folder_name, mpd_filename, config_filename = "config.csl2", model_label = NULL, verbose = F, prompt_user_before_deleting = T) {
  if (verbose) {
    print("Enter: build_assessment_bookdown")
  }

  full_mpd_file <- file.path(csl_dir, mpd_filename)
  full_config_file <- file.path(csl_dir, config_filename)
  ## check they exist
  if (!file.exists(full_mpd_file)) {
    stop(paste0("Could not find ", mpd_filename, " in 'csl_dir'"))
  }
  if (!file.exists(full_config_file)) {
    stop(paste0("Could not find ", config_filename, " in 'csl_dir'"))
  }
  ## check they can be read without error
  cas2_mpd <- tryCatch(expr = extract.mpd(file = mpd_filename, path = csl_dir), error = function(e) {
    e
  })
  if (is.null(cas2_mpd) | inherits(cas2_mpd, "error")) {
    stop("Could not read in mpd_filename")
  }
  config_summary <- tryCatch(expr = summarise_config(config_file = config_filename, config_dir = csl_dir), error = function(e) {
    e
  })
  if (is.null(config_summary) | inherits(config_summary, "error")) {
    stop("Could not read in config_file")
  }

  ## create output directory

  output_dir <- normalizePath(file.path(csl_dir, output_folder_name), winslash = "/")
  if (dir.exists(output_dir)) {
    if (prompt_user_before_deleting) {
      result <- menu(c("Yes", "No"), title = "Do you want to delete the existing Bookdown located at output_dir?")
      if (result == 2) {
        return(stop("exiting function because you don't want to delete output_dir"))
      }
    }
    unlink(output_dir, recursive = T, force = T)
    if (verbose) {
      print("deleting 'output_dir'")
    }
  }
  dir.create(output_dir)
  if (verbose) {
    print(paste0("creating 'output_dir' ", output_dir))
  }
  ## build a skeleton Bookdown
  bookdown:::bookdown_skeleton(output_dir)
  ## get rid of these skeleton changes. This may need tweaking over time
  files_to_remove <- c("01-intro.Rmd", "02-cross-refs.Rmd", "03-parts.Rmd", "04-citations.Rmd", "05-blocks.Rmd", "06-share.Rmd")
  for (i in 1:length(files_to_remove)) {
    file.remove(file.path(output_dir, files_to_remove[i]))
  }
  #################################################
  ## Create the first page '01-Model Inputs.Rmd'
  ## this page will contain most of the output from summarise_config
  #################################################
  model_input_file <- file.path(output_dir, "01-ModelInputs.Rmd")
  file.create(model_input_file)
  header <- paste0("# Model structure {#inputs}")
  if (is.null(model_label)) {
    header <- paste0("# Model structure for ", model_label, "{#inputs}")
  }
  first_chunk <-
    "
## Read in necessary R libraries

```{r install_packages, results = 'hide', message=FALSE, warning=FALSE}
library(r4Casal2)
library(Casal2)
library(knitr)
library(ggplot2)
library(dplyr)
library(reshape2)
```

"
  write(header, file = model_input_file, append = T)
  write(first_chunk, file = model_input_file, append = T)

  ## hide the path setting so people can't see your directory
  ## when bookdow compiled. this id done by setting echo = F
  write("```{r set_path, eval = T, echo = F}", file = model_input_file, append = T)
  write(paste0('csl_dir = "', normalizePath(csl_dir, winslash = "/"), '"'), file = model_input_file, append = T)
  write("```", file = model_input_file, append = T)

  ## read in model
  write("```{r read_in_info, eval = T, echo = T, results = F}", file = model_input_file, append = T)
  write(paste0("cas2_mpd = extract.mpd(file = '", mpd_filename, "', path = csl_dir)"), file = model_input_file, append = T)
  write(paste0("config_summary = summarise_config(config_file = '", config_filename, "', config_dir = csl_dir)"), file = model_input_file, append = T)
  write("```\n\n\n", file = model_input_file, append = T)

  ## Plot input Catch
  write('## Input catches\n\n```{r plot_input_catches, eval = T, echo = T, results = T,fig.cap = "Catch by fishery over time"}', file = model_input_file, append = T)
  write(
    'ggplot(config_summary$catch_df, aes(x = year, y = catch, col = fishery)) +
  geom_line(size = 1.5) +
  labs(x = "Year", y = "Catch (t)", col = "Fishery")',
    file = model_input_file, append = T
  )
  write("```\n\n\n", file = model_input_file, append = T)

  ## Plot input Observations
  write('## Input Observations\n\n```{r plot_input_observations, eval = T, echo = T, results = T,fig.cap = "Years in which observations are calculated"}', file = model_input_file, append = T)
  write(
    'ggplot(config_summary$obs_year_df, aes(x = year, y = observation, col = observation, size = active)) +
  geom_point() +
  guides(colour = "none", size = "none")',
    file = model_input_file, append = T
  )
  write("```\n\n\n", file = model_input_file, append = T)

  ## Annual cycle table
  write("## Annual cycle\n\n```{r table_annual_cycle, eval = T, echo = T, results = T}", file = model_input_file, append = T)
  cat(paste0("kable(x = config_summary$time_step_df,format = 'html', tab.attr = ", dQuote("style='width:100%;'", q = F), ")\n"), file = model_input_file, append = T)
  write("```\n\n\n", file = model_input_file, append = T)


  ## Category info
  write("## The Partition\n\n```{r table_partition, eval = T, echo = T, results = T}", file = model_input_file, append = T)
  cat(paste0("kable(x = config_summary$full_category_df,format = 'html', tab.attr = ", dQuote("style='width:100%;'", q = F), ")\n"), file = model_input_file, append = T)
  write("```\n\n\n", file = model_input_file, append = T)

  ## Estimated parameters
  write("## Estimated parameters\n\n```{r table_est_pars, eval = T, echo = T, results = T}", file = model_input_file, append = T)
  cat(paste0("kable(x = config_summary$estimate_df,format = 'html', tab.attr = ", dQuote("style='width:100%;'", q = F), ")\n"), file = model_input_file, append = T)
  write("```\n\n\n", file = model_input_file, append = T)


  #################################################
  ## Create the second page '02-ModelFits.Rmd'
  ## this page will contain model fits
  #################################################
  model_fit_file <- file.path(output_dir, "02-ModelFits.Rmd")
  file.create(model_fit_file)
  header <- paste0("# Model fit to observations {#modelfit}")
  if (is.null(model_label)) {
    header <- paste0("# Model fit to observations for ", model_label, "{#modelfit}")
  }
  write(header, file = model_fit_file, append = T)

  ## get abundance observations
  write("## Abundance/Biomass \n\n```{r abundance_fits, eval = T, echo = T}", file = model_fit_file, append = T)
  write("plot_relative_index(cas2_mpd, plot_type = 'classic')", file = model_fit_file, append = T)
  write("plot_relative_index(cas2_mpd, plot_type = 'classic_ribbon')", file = model_fit_file, append = T)
  write("plot_relative_index(cas2_mpd, plot_type = 'residual')", file = model_fit_file, append = T)
  write("``` \n", file = model_fit_file, append = T)

  ## get compositional observations
  write("## Composition \n\n```{r comp_fits, eval = T, echo = T}", file = model_fit_file, append = T)
  write("plot_compositional_observations(cas2_mpd)", file = model_fit_file, append = T)
  write("``` \n", file = model_fit_file, append = T)

  #################################################
  ## Create the third page '03-ModelQuantities.Rmd'
  #################################################
  model_quant_file <- file.path(output_dir, "02-ModelQuantities.Rmd")
  file.create(model_quant_file)
  header <- paste0("# Model quantities {#modelquantities}")
  if (is.null(model_label)) {
    header <- paste0("# Model quantities for ", model_label, "{#modelquantities}")
  }
  write(header, file = model_quant_file, append = T)

  ## SSB
  write("## SSB \n\n```{r ssb, eval = T, echo = T}", file = model_quant_file, append = T)
  write("plot_derived_quantities(cas2_mpd)", file = model_quant_file, append = T)
  write("``` \n", file = model_quant_file, append = T)

  ## Recruitment
  write("## Recruitment \n\n```{r recruit, eval = T, echo = T}", file = model_quant_file, append = T)
  write("plot_recruitment(model = cas2_mpd, quantity = 'recruitment_multipliers') + ylab('Recruitment multipliers')", file = model_quant_file, append = T)
  write("plot_recruitment(model = cas2_mpd, quantity = 'standardised_recruitment_multipliers') + ylab('Standardised recruitment multipliers')", file = model_quant_file, append = T)
  write("plot_recruitment(model = cas2_mpd, quantity = 'recruits') + ylab('Recruits (000)')", file = model_quant_file, append = T)
  write("``` \n", file = model_quant_file, append = T)

  ## Exploitation
  write("## Exploitation \n\n```{r exploitation, eval = T, echo = T}", file = model_quant_file, append = T)
  write("plot_fishery(model = cas2_mpd, quantity = 'fishing_pressure') + ylab('Fishing Pressure')", file = model_quant_file, append = T)
  write("plot_fishery(model = cas2_mpd, quantity = 'exploitation') + ylab('Exploitation')", file = model_quant_file, append = T)
  write("plot_fishery(model = cas2_mpd, quantity = 'actual_catch') + ylab('Actual Catch')", file = model_quant_file, append = T)
  write("``` \n", file = model_quant_file, append = T)

  ## Selectivities
  write("## Selectivities \n\n```{r selectivities, eval = T, echo = T}", file = model_quant_file, append = T)
  write("plot_selectivities(model = cas2_mpd)", file = model_quant_file, append = T)
  write("``` \n", file = model_quant_file, append = T)

  ## render book
  render_book(input = output_dir)
  ## copy a script to help users recompile the bookdown
  recompile_file <- file.path(output_dir, "RecompileBookdown.R")
  file.create(recompile_file)
  write(paste0("# if your R path is == '", output_dir, "'. Then this script should run"), file = recompile_file, append = T)
  write("# We will check anyway", file = recompile_file, append = T)
  write(paste0("if(getwd() != '", output_dir, "') {"), file = recompile_file, append = T)
  write(paste0(" print('You need to change your working directory to ", output_dir, ". to recompile the bookdown.')"), file = recompile_file, append = T)
  write(paste0("  setwd('", output_dir, "')"), file = recompile_file, append = T)
  write("}", file = recompile_file, append = T)
  write("## Now Recompile", file = recompile_file, append = T)
  write("library(bookdown)", file = recompile_file, append = T)
  write("render_book()", file = recompile_file, append = T)


  ## give users a message
  message(
    "Success (hopefully), if you navigate to the directory \n",
    output_dir,
    "\nand go to the folder '_book'. You will see a whole lot of html file
that can be viewed by double clicking the .html files, or opening them in browser.
You can now edit the .Rmd files to add auxiliary information and change plots.
Find the Rscript 'RecompileBookdown.R' for notes on recompiling the bookdown."
  )
}
