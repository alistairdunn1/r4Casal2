#' is_matrix_invertable
#' @description helper function to see if a matrix is invertable
#' @param m an n x n matrix
#' @export
#' @return bool
#'  \itemize{
#'   \item false: not invertable
#'   \item true: is invertable
#' }
#' @examples
#' \dontrun{
#' x <- matrix(rep(1, 25), nc = 5) # singular
#' y <- matrix(1 + 1e-10 * rnorm(25), nc = 5) # very nearly singular matrix
#' z <- 0.001 * diag(1, 5) # non-singular, but very smalll determinant
#' is_matrix_invertable(x)
#' # [1] FALSE
#' is_matrix_invertable(y)
#' # [1] TRUE
#' is_matrix_invertable(z)
#' # [1] TRUE
#' }
is_matrix_invertable <- function(m) {
  any("matrix" %in% class(try(solve(m), silent = TRUE)))
}

#' every_nth
#' @description return a vector of boolean that has every nth value = true
#' @param n every 'n' values will equal TRUE
#' @export
#' @return vector<bool>
every_nth <- function(n) {
  return(function(x) {
    x[c(TRUE, rep(FALSE, n - 1))]
  })
}

## A function to check if the covariance matrix is positive definite
#' is_positive_definite
#' @description helper function to see if a matrix is positive definite
#' @param m an n x n matrix
#' @param tol real value. Eigen values less than this tolerance value will fail the check.
#' @export
#' @return bool
#'  \itemize{
#'   \item false: not a positive definite matrix
#'   \item true: is a positive definite matrix
#' }
is_positive_definite <- function(m, tol = 1e-6) {
  ## check it is symetric
  if (!isSymmetric(m, tol = tol)) {
    message("Matrix 'm' was not symmetric. Derived using 'isSymmetric(m, tol = tol)'")
    return(FALSE)
  }
  ## get eigen values
  eS <- eigen(m, symmetric = TRUE)
  ev <- eS$values
  ## check with tolerance
  n <- nrow(m)
  for (i in 1:n) {
    if (abs(ev[i]) < tol) {
      ev[i] <- 0
    }
  }
  if (any(ev <= 0)) {
    return(FALSE)
  }
  return(TRUE)
}

#' is_constant
#' is a vector constant
#' @param x vector
#' @param tol the magnitude that flags a difference
is_constant <- function(x, tol = .Machine$double.eps) {
  abs(max(x) - min(x)) < tol
}

# should NA be returned by a convergence diagnostic?
should_return_NA <- function(x) {
  anyNA(x) || any(!is.finite(x)) || is_constant(x)
}

#' unpaste
#' @export
#' @return unpasted string
unpaste <- function(string, sep) {
  return(unlist(strsplit(string, split = sep)))
}

#' string.to.vector.of.words
#' @export
#' @return vector of words
string.to.vector.of.words <- function(string) {
  temp <- unpaste(string, sep = " ")
  return(temp[temp != ""])
}

#' @title expand_category_block
#' @description
#' A utility function for expanding short hand syntax in @category blocks in casal2 config files
#' @author Craig Marsh
#' @param categories string of categories
#' @return a vector of strings
#' @rdname expand_category_block
#' @export
#' @examples
#' \dontrun{
#' expand_category_block("stock")
#' expand_category_block("stock.male,female")
#' expand_category_block("stock.male,female.untagged,1990")
#' }
expand_category_block <- function(categories) {
  expanded_labels <- vector()
  groups <- strsplit(categories, split = ".", fixed = TRUE)[[1]]
  category_list <- list()
  for (i in 1:length(groups)) {
    if (grepl(groups[i], pattern = ",")) {
      sub_groups <- strsplit(groups[i], split = ",", fixed = TRUE)[[1]]
      category_list[[i]] <- sub_groups
    } else {
      category_list[[i]] <- groups[i]
    }
  }
  ## now permutate them
  all_perms <- expand.grid(category_list, stringsAsFactors = FALSE)
  new_perms <- NULL
  ## need to re-order based on first factor
  i <- 1
  for (j in 1:length(category_list[[i]])) {
    ndx <- all_perms$Var1 == category_list[[i]][j]
    new_perms <- rbind(new_perms, all_perms[ndx, ])
  }
  for (i in 1:nrow(new_perms)) {
    expanded_labels <- c(expanded_labels, paste0(new_perms[i, ], collapse = "."))
  }
  return(expanded_labels)
}

#' @title expand_category_shorthand
#' @description
#' A utility function for expanding short hand syntax of categories used in subcommands through casal2 config files
#' @author Craig Marsh
#' @param shorthand_categories shorthand values to expand
#' @param reference_categories string of categories expanded from the @category block of a config. derived from expand_category_block
#' @param category_format the format defined in the @categories block
#' @return a vector of strings
#' @rdname expand_category_block
#' @export
#' @examples
#' \dontrun{
#' expand_category_shorthand(shorthand_categories = "format=*.EN.*", reference_categories = c("BOP.EN.male"))
#' expand_category_shorthand(shorthand_categories = "format=*.EN.*", reference_categories = c("BOP.EN.male", "BOP.EN.female"))
#' expand_category_shorthand(shorthand_categories = "format=*.EN.*", reference_categories = c("BOP.EN.male", "BOP.EN.female", "HAGU.EN.male", "HAGU.EN.female"))
#' expand_category_shorthand(shorthand_categories = "format = *.EN.+", reference_categories = c("BOP.EN.male", "BOP.EN.female"))
#' expand_category_shorthand(shorthand_categories = "format = *.EN.+", reference_categories = c("BOP.EN.male", "BOP.EN.female", "HAGU.EN.male", "HAGU.EN.female"))
#' expand_category_shorthand(shorthand_categories = "*", reference_categories = c("BOP.EN.male", "BOP.EN.female", "HAGU.EN.male", "HAGU.EN.female"))
#' expand_category_shorthand(shorthand_categories = "stock=BOP", reference_categories = c("BOP.EN.male", "BOP.EN.female", "HAGU.EN.male", "HAGU.EN.female"), category_format = "stock.area.sex")
#' }
expand_category_shorthand <- function(shorthand_categories, reference_categories, category_format = NULL) {
  expanded_categories <- vector()
  formats <- NULL
  if (!is.null(category_format)) {
    formats <- strsplit(category_format, ".", fixed = T)[[1]]
  }
  broken_ref_vals <- Reduce(rbind, strsplit(reference_categories, ".", fixed = T))
  if (is.null(dim(broken_ref_vals))) {
    broken_ref_vals <- matrix(broken_ref_vals, nrow = 1)
  }
  # strip whitespace
  shorthand_categories_no_space <- gsub(" ", "", shorthand_categories, fixed = TRUE)
  expanded_categories <- shorthand_categories_no_space
  # if format strip that out
  format_found <- grepl(shorthand_categories_no_space, pattern = "format=", fixed = TRUE)
  if (format_found) {
    shorthand_categories_no_space <- gsub("format=", "", shorthand_categories_no_space, fixed = TRUE)
  }
  # if other shorthand
  format_found <- grepl(shorthand_categories_no_space, pattern = "=", fixed = TRUE)
  if (format_found) {
    expanded_categories <- NULL
    temp_formats <- strsplit(shorthand_categories_no_space, "=", fixed = T)[[1]]
    cat_ndx <- which(formats == temp_formats[1])
    ref_ndx <- broken_ref_vals[, cat_ndx] == temp_formats[2]
    these_categories <- broken_ref_vals[ref_ndx, ]
    for (i in 1:nrow(these_categories)) {
      expanded_categories <- c(expanded_categories, paste0(these_categories[i, ], collapse = "."))
    }
  }

  ## now should be only category labels, periods ('.'), asterisk, or +
  rep_syntax <- grepl(shorthand_categories_no_space, pattern = "*", fixed = TRUE)
  category_list <- list()
  if (rep_syntax) {
    if (shorthand_categories_no_space == "*") {
      # return all categories
      expanded_categories <- reference_categories
    } else {
      ## need to repeat some category levels
      broken_vals <- strsplit(shorthand_categories_no_space, ".", fixed = T)[[1]]
      for (i in 1:length(broken_vals)) {
        if (broken_vals[i] == "*") {
          category_list[[i]] <- unique(broken_ref_vals[, i])
        } else {
          category_list[[i]] <- broken_vals[i]
        }
      }
      ## now permutate them
      all_perms <- expand.grid(category_list, stringsAsFactors = FALSE)
      for (i in 1:nrow(all_perms)) {
        expanded_categories <- c(expanded_categories, paste0(all_perms[i, ], collapse = "."))
      }
    }
  }
  return(expanded_categories)
}

#' @title expand_shorthand_syntax
#' @description
#' A utility function for expanding short hand syntax for based on - format=*.EN.*, label*5, and + syntax
#' @author Craig Marsh
#' @param syntax string of the syntax to expand
#' @return a vector of strings
#' @rdname expand_shorthand_syntax
#' @export
#' @examples
#' \dontrun{
#' # currently can handle these shorthand
#' expand_shorthand_syntax(syntax = "age_length")
#' expand_shorthand_syntax(syntax = "age_length * 8")
#' expand_shorthand_syntax(syntax = "age_length*8")
#' expand_shorthand_syntax(syntax = "age_length_BP*4 age_length_EN*4")
#' expand_shorthand_syntax(syntax = "1990:2000")
#' expand_shorthand_syntax(syntax = "age_length_BP*4,age_length_EN*4")
#' expand_shorthand_syntax(syntax = "0.75*3")
#' expand_shorthand_syntax(syntax = "0.75 * 3 ")
#' }
expand_shorthand_syntax <- function(syntax) {
  syntax <- paste(syntax, collapse = "")
  ## strip whitespace out of syntax to make it easier
  syntax_no_space <- gsub(" ", "", syntax, fixed = TRUE)
  # we wont deal with  ',' so strip them out to
  syntax_no_space <- gsub(",", "", syntax_no_space, fixed = TRUE)

  rep_syntax <- grepl(syntax_no_space, pattern = "*", fixed = TRUE)
  colon_syntax <- grepl(syntax_no_space, pattern = ":", fixed = TRUE)

  ## deal with repeat labels
  repeated_values <- vector()
  if (rep_syntax) {
    ## simple expansion
    rep_components <- strsplit(syntax_no_space, split = "*", fixed = TRUE)[[1]]
    convert_vals <- suppressWarnings(as.numeric(rep_components))
    if (all(!is.na(convert_vals))) {
      ## all numbers so must be 0.45 * 2
      #   pairs = length(rep_components) / 2
      if (length(rep_components) != 2) {
        stop("unknown syntax")
      }
      repeated_values <- rep(convert_vals[1], convert_vals[2])
    } else {
      # Numbers and words
      for (i in 1:length(rep_components)) {
        # no numbers
        if (gsub("[^\\d]+", "", rep_components[i], perl = TRUE) == "") {
          ## simple case "label"
          value <- rep_components[i]
        } else if (gsub("[^\\d]+", "", rep_components[i], perl = TRUE) != "" & (nchar(gsub("[^\\d]+", "", rep_components[i], perl = TRUE)) != nchar(rep_components[i]))) {
          ## has numbers and words...
          ## get number
          rep_num <- as.numeric(gsub("[^\\d]+", "", rep_components[i], perl = TRUE))
          repeated_values <- c(repeated_values, rep(value, rep_num))
          ## get the other word
          value <- gsub("[[:digit:]]+", "", rep_components[i])
        } else {
          ## just number repeat value by this number
          repeated_values <- c(repeated_values, rep(value, as.numeric(gsub("[^\\d]+", "", rep_components[i], perl = TRUE))))
        }
      }
    }
  } else if (colon_syntax) {
    ## assumes only numbers lowerval:upperval
    seq_components <- strsplit(syntax_no_space, split = ":", fixed = TRUE)[[1]]
    repeated_values <- as.numeric(seq_components[1]):as.numeric(seq_components[2])
  } else {
    repeated_values <- syntax
  }
  return(repeated_values)
}

#' @title StripComments
#' @description
#' A utility function for stripping out comments that is
#' line start with #
#' or a wrapped in /* */
#' @author Craig Marsh
#' @param file a file read in by scan
#' @return a file that has removed lines starting with # or betwee '/*' '*/'
#' @rdname strip_coms
#' @export
#' @import stats
StripComments <- function(file) {
  file <- file[substring(file, 1, 1) != "#"]
  file <- ifelse(regexpr("#", file) > 0, substring(file, 1, regexpr("#", file) - 1), file)
  file <- file[file != ""]
  ## remove lines that have /* */ partway through them
  ## probably a little inefficient
  for (i in 1:length(file)) {
    index1 <- regexpr(pattern = "//*", file[i])
    first_section <- substring(file[i], first = 1, last = index1 - 1)
    second_section <- substring(file[i], first = index1 + 2)
    index2 <- regexpr(pattern = "*/", second_section)
    if (index2 > 0) { ## otherwise its a multiline comment
      file[i] <- paste0(first_section, substring(second_section, first = index2 + 1))
    }
  }
  ## we have stripped multiline comments that are on the same line
  possible_multiline_comment <- gregexpr("\\*", file)
  in_comment <- FALSE
  for (i in 1:length(file)) {
    if (in_comment) {
      # check line for laggin comment otherwise delete
      if (possible_multiline_comment[[i]][1] > 0) {
        # double check it is */
        if (substring(file[i], first = possible_multiline_comment[[i]], last = possible_multiline_comment[[i]] + 1) == "*/") {
          file[i] <- substring(file[i], first = possible_multiline_comment[[i]] + 2)
          in_comment <- FALSE
        }
      } else {
        # clear line
        file[i] <- ""
      }
    } else {
      if (possible_multiline_comment[[i]][1] > 0) {
        # double check it is /*
        if (substring(file[i], first = possible_multiline_comment[[i]] - 1, last = possible_multiline_comment[[i]]) == "/*") {
          in_comment <- T
          file[i] <- substring(file[i], first = 0, last = possible_multiline_comment[[i]] - 2)
        }
      }
    }
  }
  file <- file[file != ""]
  return(file)
}

#' drop_runs_from_multi_input_mpd
#' @description this function will remove runs from an extract.mpd that is a multiinput paramter run
#' @param model a model that has been run with -i format
#' @param elements_to_drop vector of integers that will be deleted from the model
#' @return model that has been modified
#' @export
drop_runs_from_multi_input_mpd <- function(model, elements_to_drop) {
  reports <- names(model)
  for (i in 1:length(reports)) {
    if (reports[i] == "header") {
      next
    }
    this_report <- model[[i]]
    for (j in elements_to_drop) {
      this_report[[as.character(j)]] <- NULL
    }
    model[[i]] <- this_report
  }
  return(model)
}

#' log_cv Calculate the CV of the lognormal distribution based on \deqn{cv = \sqrt{e^{\sigma^2} - 1}}
#' @param sigma The standard deviation of the lognormal distribution
#' @return The the cv
#' @export
log_cv <- function(sigma) {
  cv <- sqrt(exp(sigma^2) - 1)
  return(cv)
}

#' log_sigma
#' @description Calculate the sigma of the lognormal distribution based on \deqn{\sigma = \sqrt{log(cv^2 + 1)}}
#' @param cv The CV (note this is in proportion not percentage) of the lognormal distribution
#' @return The the sigma
#' @export
log_sigma <- function(cv) {
  sigma <- sqrt(log(cv^2 + 1))
  return(sigma)
}

#' get_block
#' @description get the block type for a parameter
#' @param parameter_label full parameter label 'block[label].parameter'
#' @return the block label
#' @export
get_block <- function(parameter_label) {
  return(sapply(strsplit(parameter_label, split = "\\["), "[", 1))
}

#' get_label
#' @description get the label type for a parameter
#' @param parameter_label full parameter label 'block[label].parameter'
#' @return the block label
#' @export
get_label <- function(parameter_label) {
  labels <- (sapply(strsplit(parameter_label, split = "\\["), "[", 2))
  labels <- (sapply(strsplit(labels, split = "\\]"), "[", 1))
  return(labels)
}
#' get_parameter
#' @description get the parameter label
#' @param parameter_label full parameter label 'block[label].parameter'
#' @return the block label
#' @export
get_parameter <- function(parameter_label) {
  param <- (sapply(strsplit(parameter_label, split = "\\."), "[", 2))
  return(param)
}
