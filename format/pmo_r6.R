# Auto-generated R6 classes from JSON Schema

PMO_NA_STRINGS <- c('N/A','NA','Not Applicable','')
PMO_ID_OFFSET <- 1
PMO_ARRAY_FIELD_NAMES <- c("additional_argument", "alt_annotations", "alternate_identifiers", "associated_protein_variants", "associated_seq_variants", "associations", "bioinformatics_methods_info", "bioinformatics_run_info", "chromosomes", "detected_microhaplotypes", "drug_usage", "library_sample_info", "library_samples", "markers_of_interest", "masking", "methods", "mhaps", "microhaplotypes", "panel_info", "panel_targets", "parasite_density_info", "project_contributors", "project_info", "qpcr_parasite_density_info", "reactions", "read_counts_by_library_sample_by_stage", "read_counts_by_stage", "read_counts_for_targets", "sequencing_info", "specimen_comments", "specimen_info", "specimen_taxon_id", "stages", "target_attributes", "target_info", "target_results", "targeted_genomes", "targets", "taxon_id", "travel_out_six_month", "treatment_status")
PMO_ID_EXTRA_FIELDS <- c("panel_targets")

`%||%` <- function(a, b) if (is.null(a)) b else a

pmo_should_apply_id_offset <- function(field_name) {
  (grepl('_id$', field_name) && !grepl('taxon', field_name, ignore.case = TRUE)) || field_name %in% PMO_ID_EXTRA_FIELDS
}

#' Open a text connection, optionally compressed
#'
#' @param path File path.
#' @param mode Either `"rt"` or `"wt"`.
#' @return A connection object.
#' @keywords internal
open_text_connection <- function(path, mode = c('rt', 'wt')) {
  mode <- match.arg(mode)
  if (grepl('\\.gz$', path, ignore.case = TRUE)) {
    gzfile(path, open = mode)
  } else if (grepl('\\.bz2$', path, ignore.case = TRUE)) {
    bzfile(path, open = mode)
  } else if (grepl('\\.xz$', path, ignore.case = TRUE)) {
    xzfile(path, open = mode)
  } else {
    file(path, open = mode)
  }
}

#' @keywords internal
pmo_apply_id_offset_read <- function(x, field_name, offset = PMO_ID_OFFSET) {
  if (offset == 0 || is.null(x) || !pmo_should_apply_id_offset(field_name)) return(x)
  if (length(x) == 0) return(x)
  if (is.list(x)) return(x)
  ifelse(is.na(x), x, x + offset)
}

#' @keywords internal
pmo_apply_id_offset_write <- function(x, field_name, offset = PMO_ID_OFFSET) {
  if (offset == 0 || is.null(x) || !pmo_should_apply_id_offset(field_name)) return(x)
  if (length(x) == 0) return(x)
  if (is.list(x)) return(x)
  ifelse(is.na(x), x, x - offset)
}

pmo_raw_postprocess <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x)) return(x)
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) {
    out <- x
    for (nm in names(out)) {
      v <- out[[nm]]
      if (is.list(v) && is.null(names(v))) {
        if (length(v) == 0) {
          if (pmo_should_apply_id_offset(nm)) out[[nm]] <- numeric() else out[[nm]] <- list()
        } else if (all(vapply(v, function(.x) is.null(.x) || is.character(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) {
          out[[nm]] <- as.character(unlist(v, use.names = FALSE))
        } else if (all(vapply(v, function(.x) is.null(.x) || is.logical(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) {
          out[[nm]] <- as.logical(unlist(v, use.names = FALSE))
        } else if (all(vapply(v, function(.x) is.null(.x) || is.numeric(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) {
          out[[nm]] <- as.numeric(unlist(v, use.names = FALSE))
        } else {
          out[[nm]] <- lapply(v, pmo_raw_postprocess)
        }
      } else {
        out[[nm]] <- pmo_raw_postprocess(v)
      }
      if (pmo_should_apply_id_offset(nm)) {
        out[[nm]] <- pmo_apply_id_offset_read(out[[nm]], nm)
      }
    }
    return(out)
  }
  if (length(x) == 0) return(list())
  if (all(vapply(x, function(.x) is.null(.x) || is.character(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) return(as.character(unlist(x, use.names = FALSE)))
  if (all(vapply(x, function(.x) is.null(.x) || is.logical(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) return(as.logical(unlist(x, use.names = FALSE)))
  if (all(vapply(x, function(.x) is.null(.x) || is.numeric(.x) || (length(.x) == 1 && is.na(.x)), logical(1)))) return(as.numeric(unlist(x, use.names = FALSE)))
  lapply(x, pmo_raw_postprocess)
}

pmo_raw_prepare_for_json <- function(x, field_name = NULL) {
  if (is.null(x)) return(NULL)
  if (is.atomic(x)) {
    if (!is.null(field_name) && field_name %in% PMO_ARRAY_FIELD_NAMES) return(I(x))
    return(x)
  }
  if (!is.list(x)) return(x)
  if (!is.null(names(x))) {
    out <- x
    for (nm in names(out)) {
      v <- out[[nm]]
      if (pmo_should_apply_id_offset(nm)) {
        v <- pmo_apply_id_offset_write(v, nm)
      }
      out[[nm]] <- pmo_raw_prepare_for_json(v, nm)
    }
    return(out)
  }
  if (!is.null(field_name) && field_name %in% PMO_ARRAY_FIELD_NAMES) {
    return(I(lapply(x, function(.x) pmo_raw_prepare_for_json(.x, NULL))))
  }
  lapply(x, function(.x) pmo_raw_prepare_for_json(.x, NULL))
}

#' BioMethod
#'
#' Bioinformatics methodology description with info on program, version, and arguments different from the default.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field additional_argument Any additional arguments that differ from the default arguments.
#' @field program Name of the program used for this portion of the pipeline.
#' @field program_description A short description of what this method does.
#' @field program_url A url pointing to code base of a program, e.g. a github link.
#' @field program_version The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `additional_argument`: Any additional arguments that differ from the default arguments.
#' * `program`: Name of the program used for this portion of the pipeline.
#' * `program_description`: A short description of what this method does.
#' * `program_url`: A url pointing to code base of a program, e.g. a github link.
#' * `program_version`: The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
BioMethod <- R6::R6Class(
  "BioMethod",
  public = list(
    additional_argument = character(),
    program = NA_character_,
    program_description = NA_character_,
    program_url = NA_character_,
    program_version = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param additional_argument Any additional arguments that differ from the default arguments.
    #' @param program Name of the program used for this portion of the pipeline.
    #' @param program_description A short description of what this method does.
    #' @param program_url A url pointing to code base of a program, e.g. a github link.
    #' @param program_version The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(additional_argument = NULL, program = NA_character_, program_description = NULL, program_url = NULL, program_version = NA_character_, extras = list()) {
      self$additional_argument <- additional_argument
      self$program <- program
      self$program_description <- program_description
      self$program_url <- program_url
      self$program_version <- program_version
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$additional_argument) && !is.character(self$additional_argument)) stop("BioMethod.additional_argument must be a character vector")
      if (!is.null(self$additional_argument) && length(self$additional_argument) > 0 && any(!grepl("^[A-z-._0-9{}\\(\\),\\/\\ ]+$", self$additional_argument, perl = TRUE))) stop("BioMethod.additional_argument contains values that do not match pattern: ^[A-z-._0-9{}\\(\\),\\/\\ ]+$")
      if (!is.null(self$program) && !is.na(self$program) && (!is.character(self$program) || length(self$program) != 1)) stop("BioMethod.program must be a single string")
      if (!is.null(self$program) && !is.na(self$program) && !grepl("^[A-z-._0-9 ]+$", self$program, perl = TRUE)) stop("BioMethod.program does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$program_description) && !is.na(self$program_description) && (!is.character(self$program_description) || length(self$program_description) != 1)) stop("BioMethod.program_description must be a single string")
      if (!is.null(self$program_description) && !is.na(self$program_description) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$program_description, perl = TRUE)) stop("BioMethod.program_description does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$program_url) && !is.na(self$program_url) && (!is.character(self$program_url) || length(self$program_url) != 1)) stop("BioMethod.program_url must be a single string")
      if (!is.null(self$program_url) && !is.na(self$program_url) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$program_url, perl = TRUE)) stop("BioMethod.program_url does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$program_version) && !is.na(self$program_version) && (!is.character(self$program_version) || length(self$program_version) != 1)) stop("BioMethod.program_version must be a single string")
      if (!is.null(self$program_version) && !is.na(self$program_version) && !grepl("^[A-z-._0-9 ]+$", self$program_version, perl = TRUE)) stop("BioMethod.program_version does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$additional_argument)) out$additional_argument <- self$additional_argument
      if (!is.null(self$program)) out$program <- if (is.na(self$program)) "NA" else self$program
      if (!is.null(self$program_description)) out$program_description <- if (is.na(self$program_description)) "NA" else self$program_description
      if (!is.null(self$program_url)) out$program_url <- if (is.na(self$program_url)) "NA" else self$program_url
      if (!is.null(self$program_version)) out$program_version <- if (is.na(self$program_version)) "NA" else self$program_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$additional_argument)) out$additional_argument <- I(self$additional_argument)
      if (!is.null(self$program)) out$program <- if (is.na(self$program)) "NA" else self$program
      if (!is.null(self$program_description)) out$program_description <- if (is.na(self$program_description)) "NA" else self$program_description
      if (!is.null(self$program_url)) out$program_url <- if (is.na(self$program_url)) "NA" else self$program_url
      if (!is.null(self$program_version)) out$program_version <- if (is.na(self$program_version)) "NA" else self$program_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

BioMethod$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("program","program_version")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("BioMethod missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("additional_argument","program","program_description","program_url","program_version")
  extras <- obj[setdiff(names(obj), known)]
  inst <- BioMethod$new(additional_argument = { v <- obj[["additional_argument"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, program = { v <- obj[["program"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, program_description = { v <- obj[["program_description"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, program_url = { v <- obj[["program_url"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, program_version = { v <- obj[["program_version"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' BioinformaticsMethodInfo
#'
#' The targeted amplicon bioinformatics methods used to generate the microhaplotype data in this PMO.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field methods Methodology used to generate the microhaplotype data stored in this PMO, e.g. demultiplexing method, denosing method, or a pipeline method that ties all th steps together.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `methods`: Methodology used to generate the microhaplotype data stored in this PMO, e.g. demultiplexing method, denosing method, or a pipeline method that ties all th steps together.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
BioinformaticsMethodInfo <- R6::R6Class(
  "BioinformaticsMethodInfo",
  public = list(
    methods = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param methods Methodology used to generate the microhaplotype data stored in this PMO, e.g. demultiplexing method, denosing method, or a pipeline method that ties all th steps together.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(methods = list(), extras = list()) {
      self$methods <- methods
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$methods) && !is.list(self$methods)) stop("BioinformaticsMethodInfo.methods must be a list")
      if (!is.null(self$methods)) for (.x in self$methods) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$methods)) out$methods <- lapply(self$methods, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$methods)) out$methods <- I(lapply(self$methods, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

BioinformaticsMethodInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("methods")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("BioinformaticsMethodInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("methods")
  extras <- obj[setdiff(names(obj), known)]
  inst <- BioinformaticsMethodInfo$new(methods = if (!is.null(obj[["methods"]])) lapply(obj[["methods"]], function(.x) BioMethod$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' BioinformaticsRunInfo
#'
#' Information about the pipeline run that generated microhaplotype_detected and reads_by_stage.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field bioinformatics_methods_id The index into the bioinformatics_methods_info list.
#' @field bioinformatics_run_name A name to for this run, needs to be unique to each run.
#' @field run_date The date when the run was done, should be YYYY-MM-DD.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `bioinformatics_methods_id`: The index into the bioinformatics_methods_info list.
#' * `bioinformatics_run_name`: A name to for this run, needs to be unique to each run.
#' * `run_date`: The date when the run was done, should be YYYY-MM-DD.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
BioinformaticsRunInfo <- R6::R6Class(
  "BioinformaticsRunInfo",
  public = list(
    bioinformatics_methods_id = NA_real_,
    bioinformatics_run_name = NA_character_,
    run_date = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param bioinformatics_methods_id The index into the bioinformatics_methods_info list.
    #' @param bioinformatics_run_name A name to for this run, needs to be unique to each run.
    #' @param run_date The date when the run was done, should be YYYY-MM-DD.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(bioinformatics_methods_id = NA_real_, bioinformatics_run_name = NA_character_, run_date = NULL, extras = list()) {
      self$bioinformatics_methods_id <- bioinformatics_methods_id
      self$bioinformatics_run_name <- bioinformatics_run_name
      self$run_date <- run_date
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$bioinformatics_methods_id) && !is.na(self$bioinformatics_methods_id) && (!is.numeric(self$bioinformatics_methods_id) || length(self$bioinformatics_methods_id) != 1)) stop("BioinformaticsRunInfo.bioinformatics_methods_id must be a single numeric value")
      if (!is.null(self$bioinformatics_methods_id) && !is.na(self$bioinformatics_methods_id) && self$bioinformatics_methods_id < 0) stop("BioinformaticsRunInfo.bioinformatics_methods_id < minimum 0")
      if (!is.null(self$bioinformatics_methods_id) && !is.na(self$bioinformatics_methods_id) && !(is.numeric(self$bioinformatics_methods_id) && isTRUE(all.equal(self$bioinformatics_methods_id, as.integer(self$bioinformatics_methods_id))))) stop("BioinformaticsRunInfo.bioinformatics_methods_id must be integer-like")
      if (!is.null(self$bioinformatics_run_name) && !is.na(self$bioinformatics_run_name) && (!is.character(self$bioinformatics_run_name) || length(self$bioinformatics_run_name) != 1)) stop("BioinformaticsRunInfo.bioinformatics_run_name must be a single string")
      if (!is.null(self$bioinformatics_run_name) && !is.na(self$bioinformatics_run_name) && !grepl("^[A-z-._0-9 ]+$", self$bioinformatics_run_name, perl = TRUE)) stop("BioinformaticsRunInfo.bioinformatics_run_name does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$run_date) && !is.na(self$run_date) && (!is.character(self$run_date) || length(self$run_date) != 1)) stop("BioinformaticsRunInfo.run_date must be a single string")
      if (!is.null(self$run_date) && !is.na(self$run_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$run_date, perl = TRUE)) stop("BioinformaticsRunInfo.run_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_methods_id)) out$bioinformatics_methods_id <- self$bioinformatics_methods_id
      if (!is.null(self$bioinformatics_run_name)) out$bioinformatics_run_name <- if (is.na(self$bioinformatics_run_name)) "NA" else self$bioinformatics_run_name
      if (!is.null(self$run_date)) out$run_date <- if (is.na(self$run_date)) "NA" else self$run_date
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_methods_id)) out$bioinformatics_methods_id <- pmo_apply_id_offset_write(self$bioinformatics_methods_id, "bioinformatics_methods_id")
      if (!is.null(self$bioinformatics_run_name)) out$bioinformatics_run_name <- if (is.na(self$bioinformatics_run_name)) "NA" else self$bioinformatics_run_name
      if (!is.null(self$run_date)) out$run_date <- if (is.na(self$run_date)) "NA" else self$run_date
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

BioinformaticsRunInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("bioinformatics_methods_id","bioinformatics_run_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("BioinformaticsRunInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("bioinformatics_methods_id","bioinformatics_run_name","run_date")
  extras <- obj[setdiff(names(obj), known)]
  inst <- BioinformaticsRunInfo$new(bioinformatics_methods_id = pmo_apply_id_offset_read(if (!is.null(obj[["bioinformatics_methods_id"]])) obj[["bioinformatics_methods_id"]] else NA_real_, "bioinformatics_methods_id"), bioinformatics_run_name = { v <- obj[["bioinformatics_run_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, run_date = { v <- obj[["run_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' MicrohaplotypeForTarget
#'
#' Microhaplotype detected for a specific target.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field mhap_id The index for a microhaplotype for a target in the representative_microhaplotypes list, e.g. representative_microhaplotypes\[mhaps_target_id\]\[mhap_id\].
#' @field reads The read count for this microhaplotype.
#' @field umis The unique molecular identifier (umi) count for this microhaplotype.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `mhap_id`: The index for a microhaplotype for a target in the representative_microhaplotypes list, e.g. representative_microhaplotypes\[mhaps_target_id\]\[mhap_id\].
#' * `reads`: The read count for this microhaplotype.
#' * `umis`: The unique molecular identifier (umi) count for this microhaplotype.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
MicrohaplotypeForTarget <- R6::R6Class(
  "MicrohaplotypeForTarget",
  public = list(
    mhap_id = NA_real_,
    reads = NA_real_,
    umis = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param mhap_id The index for a microhaplotype for a target in the representative_microhaplotypes list, e.g. representative_microhaplotypes\[mhaps_target_id\]\[mhap_id\].
    #' @param reads The read count for this microhaplotype.
    #' @param umis The unique molecular identifier (umi) count for this microhaplotype.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(mhap_id = NA_real_, reads = NA_real_, umis = NULL, extras = list()) {
      self$mhap_id <- mhap_id
      self$reads <- reads
      self$umis <- umis
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$mhap_id) && !is.na(self$mhap_id) && (!is.numeric(self$mhap_id) || length(self$mhap_id) != 1)) stop("MicrohaplotypeForTarget.mhap_id must be a single numeric value")
      if (!is.null(self$mhap_id) && !is.na(self$mhap_id) && self$mhap_id < 0) stop("MicrohaplotypeForTarget.mhap_id < minimum 0")
      if (!is.null(self$mhap_id) && !is.na(self$mhap_id) && !(is.numeric(self$mhap_id) && isTRUE(all.equal(self$mhap_id, as.integer(self$mhap_id))))) stop("MicrohaplotypeForTarget.mhap_id must be integer-like")
      if (!is.null(self$reads) && !is.na(self$reads) && (!is.numeric(self$reads) || length(self$reads) != 1)) stop("MicrohaplotypeForTarget.reads must be a single numeric value")
      if (!is.null(self$reads) && !is.na(self$reads) && self$reads < 0) stop("MicrohaplotypeForTarget.reads < minimum 0")
      if (!is.null(self$reads) && !is.na(self$reads) && !(is.numeric(self$reads) && isTRUE(all.equal(self$reads, as.integer(self$reads))))) stop("MicrohaplotypeForTarget.reads must be integer-like")
      if (!is.null(self$umis) && !is.na(self$umis) && (!is.numeric(self$umis) || length(self$umis) != 1)) stop("MicrohaplotypeForTarget.umis must be a single numeric value")
      if (!is.null(self$umis) && !is.na(self$umis) && self$umis < 0) stop("MicrohaplotypeForTarget.umis < minimum 0")
      if (!is.null(self$umis) && !is.na(self$umis) && !(is.numeric(self$umis) && isTRUE(all.equal(self$umis, as.integer(self$umis))))) stop("MicrohaplotypeForTarget.umis must be integer-like")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$mhap_id)) out$mhap_id <- self$mhap_id
      if (!is.null(self$reads)) out$reads <- self$reads
      if (!is.null(self$umis)) out$umis <- self$umis
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$mhap_id)) out$mhap_id <- pmo_apply_id_offset_write(self$mhap_id, "mhap_id")
      if (!is.null(self$reads)) out$reads <- self$reads
      if (!is.null(self$umis)) out$umis <- self$umis
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

MicrohaplotypeForTarget$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("mhap_id","reads")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("MicrohaplotypeForTarget missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("mhap_id","reads","umis")
  extras <- obj[setdiff(names(obj), known)]
  inst <- MicrohaplotypeForTarget$new(mhap_id = pmo_apply_id_offset_read(if (!is.null(obj[["mhap_id"]])) obj[["mhap_id"]] else NA_real_, "mhap_id"), reads = if (!is.null(obj[["reads"]])) obj[["reads"]] else NA_real_, umis = if (!is.null(obj[["umis"]])) obj[["umis"]] else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' DetectedMicrohaplotypesForTarget
#'
#' Microhaplotypes detected for a specific target.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field mhaps A list of the microhaplotypes detected for this target.
#' @field mhaps_target_id The index for a target in the representative_microhaplotypes list.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `mhaps`: A list of the microhaplotypes detected for this target.
#' * `mhaps_target_id`: The index for a target in the representative_microhaplotypes list.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
DetectedMicrohaplotypesForTarget <- R6::R6Class(
  "DetectedMicrohaplotypesForTarget",
  public = list(
    mhaps = list(),
    mhaps_target_id = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param mhaps A list of the microhaplotypes detected for this target.
    #' @param mhaps_target_id The index for a target in the representative_microhaplotypes list.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(mhaps = list(), mhaps_target_id = NA_real_, extras = list()) {
      self$mhaps <- mhaps
      self$mhaps_target_id <- mhaps_target_id
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$mhaps) && !is.list(self$mhaps)) stop("DetectedMicrohaplotypesForTarget.mhaps must be a list")
      if (!is.null(self$mhaps_target_id) && !is.na(self$mhaps_target_id) && (!is.numeric(self$mhaps_target_id) || length(self$mhaps_target_id) != 1)) stop("DetectedMicrohaplotypesForTarget.mhaps_target_id must be a single numeric value")
      if (!is.null(self$mhaps_target_id) && !is.na(self$mhaps_target_id) && self$mhaps_target_id < 0) stop("DetectedMicrohaplotypesForTarget.mhaps_target_id < minimum 0")
      if (!is.null(self$mhaps_target_id) && !is.na(self$mhaps_target_id) && !(is.numeric(self$mhaps_target_id) && isTRUE(all.equal(self$mhaps_target_id, as.integer(self$mhaps_target_id))))) stop("DetectedMicrohaplotypesForTarget.mhaps_target_id must be integer-like")
      if (!is.null(self$mhaps)) for (.x in self$mhaps) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$mhaps)) out$mhaps <- lapply(self$mhaps, function(x) x$to_list())
      if (!is.null(self$mhaps_target_id)) out$mhaps_target_id <- self$mhaps_target_id
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$mhaps)) out$mhaps <- I(lapply(self$mhaps, function(x) x$to_json_list()))
      if (!is.null(self$mhaps_target_id)) out$mhaps_target_id <- pmo_apply_id_offset_write(self$mhaps_target_id, "mhaps_target_id")
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

DetectedMicrohaplotypesForTarget$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("mhaps","mhaps_target_id")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("DetectedMicrohaplotypesForTarget missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("mhaps","mhaps_target_id")
  extras <- obj[setdiff(names(obj), known)]
  inst <- DetectedMicrohaplotypesForTarget$new(mhaps = if (!is.null(obj[["mhaps"]])) lapply(obj[["mhaps"]], function(.x) MicrohaplotypeForTarget$from_json(.x, validate = FALSE)) else NULL, mhaps_target_id = pmo_apply_id_offset_read(if (!is.null(obj[["mhaps_target_id"]])) obj[["mhaps_target_id"]] else NA_real_, "mhaps_target_id"), extras = extras)
  if (validate) inst$validate()
  inst
}

#' DetectedMicrohaplotypesForSample
#'
#' Microhaplotypes detected for a sample for all targets.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field library_sample_id The index into the library_sample_info list.
#' @field target_results A list of the microhaplotypes detected for a list of targets.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `library_sample_id`: The index into the library_sample_info list.
#' * `target_results`: A list of the microhaplotypes detected for a list of targets.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
DetectedMicrohaplotypesForSample <- R6::R6Class(
  "DetectedMicrohaplotypesForSample",
  public = list(
    library_sample_id = NA_real_,
    target_results = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param library_sample_id The index into the library_sample_info list.
    #' @param target_results A list of the microhaplotypes detected for a list of targets.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(library_sample_id = NA_real_, target_results = list(), extras = list()) {
      self$library_sample_id <- library_sample_id
      self$target_results <- target_results
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && (!is.numeric(self$library_sample_id) || length(self$library_sample_id) != 1)) stop("DetectedMicrohaplotypesForSample.library_sample_id must be a single numeric value")
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && self$library_sample_id < 0) stop("DetectedMicrohaplotypesForSample.library_sample_id < minimum 0")
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && !(is.numeric(self$library_sample_id) && isTRUE(all.equal(self$library_sample_id, as.integer(self$library_sample_id))))) stop("DetectedMicrohaplotypesForSample.library_sample_id must be integer-like")
      if (!is.null(self$target_results) && !is.list(self$target_results)) stop("DetectedMicrohaplotypesForSample.target_results must be a list")
      if (!is.null(self$target_results)) for (.x in self$target_results) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$library_sample_id)) out$library_sample_id <- self$library_sample_id
      if (!is.null(self$target_results)) out$target_results <- lapply(self$target_results, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$library_sample_id)) out$library_sample_id <- pmo_apply_id_offset_write(self$library_sample_id, "library_sample_id")
      if (!is.null(self$target_results)) out$target_results <- I(lapply(self$target_results, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

DetectedMicrohaplotypesForSample$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("library_sample_id","target_results")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("DetectedMicrohaplotypesForSample missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("library_sample_id","target_results")
  extras <- obj[setdiff(names(obj), known)]
  inst <- DetectedMicrohaplotypesForSample$new(library_sample_id = pmo_apply_id_offset_read(if (!is.null(obj[["library_sample_id"]])) obj[["library_sample_id"]] else NA_real_, "library_sample_id"), target_results = if (!is.null(obj[["target_results"]])) lapply(obj[["target_results"]], function(.x) DetectedMicrohaplotypesForTarget$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' DetectedMicrohaplotypes
#'
#' The microhaplotypes detected in a targeted amplicon analysis.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field bioinformatics_run_id The index into bioinformatics_run_info list.
#' @field library_samples A list of the microhaplotypes detected for all samples with a list for each target.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `bioinformatics_run_id`: The index into bioinformatics_run_info list.
#' * `library_samples`: A list of the microhaplotypes detected for all samples with a list for each target.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
DetectedMicrohaplotypes <- R6::R6Class(
  "DetectedMicrohaplotypes",
  public = list(
    bioinformatics_run_id = NA_real_,
    library_samples = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param bioinformatics_run_id The index into bioinformatics_run_info list.
    #' @param library_samples A list of the microhaplotypes detected for all samples with a list for each target.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(bioinformatics_run_id = NULL, library_samples = list(), extras = list()) {
      self$bioinformatics_run_id <- bioinformatics_run_id
      self$library_samples <- library_samples
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && (!is.numeric(self$bioinformatics_run_id) || length(self$bioinformatics_run_id) != 1)) stop("DetectedMicrohaplotypes.bioinformatics_run_id must be a single numeric value")
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && self$bioinformatics_run_id < 0) stop("DetectedMicrohaplotypes.bioinformatics_run_id < minimum 0")
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && !(is.numeric(self$bioinformatics_run_id) && isTRUE(all.equal(self$bioinformatics_run_id, as.integer(self$bioinformatics_run_id))))) stop("DetectedMicrohaplotypes.bioinformatics_run_id must be integer-like")
      if (!is.null(self$library_samples) && !is.list(self$library_samples)) stop("DetectedMicrohaplotypes.library_samples must be a list")
      if (!is.null(self$library_samples)) for (.x in self$library_samples) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_run_id)) out$bioinformatics_run_id <- self$bioinformatics_run_id
      if (!is.null(self$library_samples)) out$library_samples <- lapply(self$library_samples, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_run_id)) out$bioinformatics_run_id <- pmo_apply_id_offset_write(self$bioinformatics_run_id, "bioinformatics_run_id")
      if (!is.null(self$library_samples)) out$library_samples <- I(lapply(self$library_samples, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

DetectedMicrohaplotypes$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("library_samples")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("DetectedMicrohaplotypes missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("bioinformatics_run_id","library_samples")
  extras <- obj[setdiff(names(obj), known)]
  inst <- DetectedMicrohaplotypes$new(bioinformatics_run_id = pmo_apply_id_offset_read(if (!is.null(obj[["bioinformatics_run_id"]])) obj[["bioinformatics_run_id"]] else NULL, "bioinformatics_run_id"), library_samples = if (!is.null(obj[["library_samples"]])) lapply(obj[["library_samples"]], function(.x) DetectedMicrohaplotypesForSample$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' GenomeInfo
#'
#' Information on a genome.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field chromosomes A list of the chromosomes/contigs found within this genome.
#' @field genome_version The genome version.
#' @field gff_url A link to the where this genome's annotation file could be downloaded.
#' @field name Name of the genome.
#' @field taxon_id The NCBI taxonomy number, can be a list of values if it's a genome file that has been created by combining gnomes from different species.
#' @field url A link to the where this genome file could be downloaded.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `chromosomes`: A list of the chromosomes/contigs found within this genome.
#' * `genome_version`: The genome version.
#' * `gff_url`: A link to the where this genome's annotation file could be downloaded.
#' * `name`: Name of the genome.
#' * `taxon_id`: The NCBI taxonomy number, can be a list of values if it's a genome file that has been created by combining gnomes from different species.
#' * `url`: A link to the where this genome file could be downloaded.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
GenomeInfo <- R6::R6Class(
  "GenomeInfo",
  public = list(
    chromosomes = character(),
    genome_version = NA_character_,
    gff_url = NA_character_,
    name = NA_character_,
    taxon_id = numeric(),
    url = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param chromosomes A list of the chromosomes/contigs found within this genome.
    #' @param genome_version The genome version.
    #' @param gff_url A link to the where this genome's annotation file could be downloaded.
    #' @param name Name of the genome.
    #' @param taxon_id The NCBI taxonomy number, can be a list of values if it's a genome file that has been created by combining gnomes from different species.
    #' @param url A link to the where this genome file could be downloaded.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(chromosomes = NULL, genome_version = NA_character_, gff_url = NULL, name = NA_character_, taxon_id = numeric(), url = NA_character_, extras = list()) {
      self$chromosomes <- chromosomes
      self$genome_version <- genome_version
      self$gff_url <- gff_url
      self$name <- name
      self$taxon_id <- taxon_id
      self$url <- url
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$chromosomes) && !is.character(self$chromosomes)) stop("GenomeInfo.chromosomes must be a character vector")
      if (!is.null(self$chromosomes) && length(self$chromosomes) > 0 && any(!grepl("^[A-z-._0-9]+$", self$chromosomes, perl = TRUE))) stop("GenomeInfo.chromosomes contains values that do not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$genome_version) && !is.na(self$genome_version) && (!is.character(self$genome_version) || length(self$genome_version) != 1)) stop("GenomeInfo.genome_version must be a single string")
      if (!is.null(self$genome_version) && !is.na(self$genome_version) && !grepl("^[A-z-._0-9]+$", self$genome_version, perl = TRUE)) stop("GenomeInfo.genome_version does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$gff_url) && !is.na(self$gff_url) && (!is.character(self$gff_url) || length(self$gff_url) != 1)) stop("GenomeInfo.gff_url must be a single string")
      if (!is.null(self$gff_url) && !is.na(self$gff_url) && !grepl("^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$", self$gff_url, perl = TRUE)) stop("GenomeInfo.gff_url does not match pattern: ^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$")
      if (!is.null(self$name) && !is.na(self$name) && (!is.character(self$name) || length(self$name) != 1)) stop("GenomeInfo.name must be a single string")
      if (!is.null(self$name) && !is.na(self$name) && !grepl("^[A-z-._0-9]+$", self$name, perl = TRUE)) stop("GenomeInfo.name does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$taxon_id) && !is.numeric(self$taxon_id)) stop("GenomeInfo.taxon_id must be a numeric vector")
      if (!is.null(self$taxon_id) && length(self$taxon_id) > 0 && any(self$taxon_id < 0, na.rm = TRUE)) stop("GenomeInfo.taxon_id contains values < minimum 0")
      if (!is.null(self$taxon_id) && length(self$taxon_id) > 0 && any(!vapply(self$taxon_id, function(.x) is.na(.x) || (is.numeric(.x) && isTRUE(all.equal(.x, as.integer(.x)))), logical(1)))) stop("GenomeInfo.taxon_id must contain integer-like values")
      if (!is.null(self$url) && !is.na(self$url) && (!is.character(self$url) || length(self$url) != 1)) stop("GenomeInfo.url must be a single string")
      if (!is.null(self$url) && !is.na(self$url) && !grepl("^(?:NA|(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*)$", self$url, perl = TRUE)) stop("GenomeInfo.url does not match pattern: ^(?:NA|(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*)$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$chromosomes)) out$chromosomes <- self$chromosomes
      if (!is.null(self$genome_version)) out$genome_version <- if (is.na(self$genome_version)) "NA" else self$genome_version
      if (!is.null(self$gff_url)) out$gff_url <- if (is.na(self$gff_url)) "NA" else self$gff_url
      if (!is.null(self$name)) out$name <- if (is.na(self$name)) "NA" else self$name
      if (!is.null(self$taxon_id)) out$taxon_id <- self$taxon_id
      if (!is.null(self$url)) out$url <- if (is.na(self$url)) "NA" else self$url
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$chromosomes)) out$chromosomes <- I(self$chromosomes)
      if (!is.null(self$genome_version)) out$genome_version <- if (is.na(self$genome_version)) "NA" else self$genome_version
      if (!is.null(self$gff_url)) out$gff_url <- if (is.na(self$gff_url)) "NA" else self$gff_url
      if (!is.null(self$name)) out$name <- if (is.na(self$name)) "NA" else self$name
      if (!is.null(self$taxon_id)) out$taxon_id <- I(self$taxon_id)
      if (!is.null(self$url)) out$url <- if (is.na(self$url)) "NA" else self$url
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

GenomeInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("genome_version","name","taxon_id","url")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("GenomeInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("chromosomes","genome_version","gff_url","name","taxon_id","url")
  extras <- obj[setdiff(names(obj), known)]
  inst <- GenomeInfo$new(chromosomes = { v <- obj[["chromosomes"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, genome_version = { v <- obj[["genome_version"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, gff_url = { v <- obj[["gff_url"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, name = { v <- obj[["name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, taxon_id = { v <- obj[["taxon_id"]]; if (is.null(v)) NULL else if (length(v) == 0) numeric() else as.numeric(unlist(v, use.names = FALSE)) }, url = { v <- obj[["url"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' GenomicLocation
#'
#' Information on the genomic location of specific sequence.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field alt_seq A possible alternative sequence of this genomic location.
#' @field chrom The chromosome name.
#' @field end The end of the location, 0-based positioning.
#' @field genome_id The index to the genome in the targeted_genomes list that this location refers to.
#' @field ref_seq The reference sequence of this genomic location.
#' @field start The start of the location, 0-based positioning.
#' @field strand Which strand the location is, either + for plus strand or - for negative strand.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `alt_seq`: A possible alternative sequence of this genomic location.
#' * `chrom`: The chromosome name.
#' * `end`: The end of the location, 0-based positioning.
#' * `genome_id`: The index to the genome in the targeted_genomes list that this location refers to.
#' * `ref_seq`: The reference sequence of this genomic location.
#' * `start`: The start of the location, 0-based positioning.
#' * `strand`: Which strand the location is, either + for plus strand or - for negative strand.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
GenomicLocation <- R6::R6Class(
  "GenomicLocation",
  public = list(
    alt_seq = NA_character_,
    chrom = NA_character_,
    end = NA_real_,
    genome_id = NA_real_,
    ref_seq = NA_character_,
    start = NA_real_,
    strand = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param alt_seq A possible alternative sequence of this genomic location.
    #' @param chrom The chromosome name.
    #' @param end The end of the location, 0-based positioning.
    #' @param genome_id The index to the genome in the targeted_genomes list that this location refers to.
    #' @param ref_seq The reference sequence of this genomic location.
    #' @param start The start of the location, 0-based positioning.
    #' @param strand Which strand the location is, either + for plus strand or - for negative strand.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(alt_seq = NULL, chrom = NA_character_, end = NA_real_, genome_id = NA_real_, ref_seq = NULL, start = NA_real_, strand = NULL, extras = list()) {
      self$alt_seq <- alt_seq
      self$chrom <- chrom
      self$end <- end
      self$genome_id <- genome_id
      self$ref_seq <- ref_seq
      self$start <- start
      self$strand <- strand
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$alt_seq) && !is.na(self$alt_seq) && (!is.character(self$alt_seq) || length(self$alt_seq) != 1)) stop("GenomicLocation.alt_seq must be a single string")
      if (!is.null(self$alt_seq) && !is.na(self$alt_seq) && !grepl("^[A-z-]+$", self$alt_seq, perl = TRUE)) stop("GenomicLocation.alt_seq does not match pattern: ^[A-z-]+$")
      if (!is.null(self$chrom) && !is.na(self$chrom) && (!is.character(self$chrom) || length(self$chrom) != 1)) stop("GenomicLocation.chrom must be a single string")
      if (!is.null(self$chrom) && !is.na(self$chrom) && !grepl("^[A-z-._0-9]+$", self$chrom, perl = TRUE)) stop("GenomicLocation.chrom does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$end) && !is.na(self$end) && (!is.numeric(self$end) || length(self$end) != 1)) stop("GenomicLocation.end must be a single numeric value")
      if (!is.null(self$end) && !is.na(self$end) && self$end < 0) stop("GenomicLocation.end < minimum 0")
      if (!is.null(self$end) && !is.na(self$end) && !(is.numeric(self$end) && isTRUE(all.equal(self$end, as.integer(self$end))))) stop("GenomicLocation.end must be integer-like")
      if (!is.null(self$genome_id) && !is.na(self$genome_id) && (!is.numeric(self$genome_id) || length(self$genome_id) != 1)) stop("GenomicLocation.genome_id must be a single numeric value")
      if (!is.null(self$genome_id) && !is.na(self$genome_id) && self$genome_id < 0) stop("GenomicLocation.genome_id < minimum 0")
      if (!is.null(self$genome_id) && !is.na(self$genome_id) && !(is.numeric(self$genome_id) && isTRUE(all.equal(self$genome_id, as.integer(self$genome_id))))) stop("GenomicLocation.genome_id must be integer-like")
      if (!is.null(self$ref_seq) && !is.na(self$ref_seq) && (!is.character(self$ref_seq) || length(self$ref_seq) != 1)) stop("GenomicLocation.ref_seq must be a single string")
      if (!is.null(self$ref_seq) && !is.na(self$ref_seq) && !grepl("^[A-z-]+$", self$ref_seq, perl = TRUE)) stop("GenomicLocation.ref_seq does not match pattern: ^[A-z-]+$")
      if (!is.null(self$start) && !is.na(self$start) && (!is.numeric(self$start) || length(self$start) != 1)) stop("GenomicLocation.start must be a single numeric value")
      if (!is.null(self$start) && !is.na(self$start) && self$start < 0) stop("GenomicLocation.start < minimum 0")
      if (!is.null(self$start) && !is.na(self$start) && !(is.numeric(self$start) && isTRUE(all.equal(self$start, as.integer(self$start))))) stop("GenomicLocation.start must be integer-like")
      if (!is.null(self$strand) && !is.na(self$strand) && (!is.character(self$strand) || length(self$strand) != 1)) stop("GenomicLocation.strand must be a single string")
      if (!is.null(self$strand) && !is.na(self$strand) && !grepl("[+-]", self$strand, perl = TRUE)) stop("GenomicLocation.strand does not match pattern: [+-]")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$alt_seq)) out$alt_seq <- if (is.na(self$alt_seq)) "NA" else self$alt_seq
      if (!is.null(self$chrom)) out$chrom <- if (is.na(self$chrom)) "NA" else self$chrom
      if (!is.null(self$end)) out$end <- self$end
      if (!is.null(self$genome_id)) out$genome_id <- self$genome_id
      if (!is.null(self$ref_seq)) out$ref_seq <- if (is.na(self$ref_seq)) "NA" else self$ref_seq
      if (!is.null(self$start)) out$start <- self$start
      if (!is.null(self$strand)) out$strand <- if (is.na(self$strand)) "NA" else self$strand
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$alt_seq)) out$alt_seq <- if (is.na(self$alt_seq)) "NA" else self$alt_seq
      if (!is.null(self$chrom)) out$chrom <- if (is.na(self$chrom)) "NA" else self$chrom
      if (!is.null(self$end)) out$end <- self$end
      if (!is.null(self$genome_id)) out$genome_id <- pmo_apply_id_offset_write(self$genome_id, "genome_id")
      if (!is.null(self$ref_seq)) out$ref_seq <- if (is.na(self$ref_seq)) "NA" else self$ref_seq
      if (!is.null(self$start)) out$start <- self$start
      if (!is.null(self$strand)) out$strand <- if (is.na(self$strand)) "NA" else self$strand
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

GenomicLocation$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("chrom","end","genome_id","start")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("GenomicLocation missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("alt_seq","chrom","end","genome_id","ref_seq","start","strand")
  extras <- obj[setdiff(names(obj), known)]
  inst <- GenomicLocation$new(alt_seq = { v <- obj[["alt_seq"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, chrom = { v <- obj[["chrom"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, end = if (!is.null(obj[["end"]])) obj[["end"]] else NA_real_, genome_id = pmo_apply_id_offset_read(if (!is.null(obj[["genome_id"]])) obj[["genome_id"]] else NA_real_, "genome_id"), ref_seq = { v <- obj[["ref_seq"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, start = if (!is.null(obj[["start"]])) obj[["start"]] else NA_real_, strand = { v <- obj[["strand"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ParasiteDensity
#'
#' Method and value of determined parasite density.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field date_measured The date the qpcr was performed, can be YYYY, YYYY-MM, or YYYY-MM-DD.
#' @field density_method_comments Additional comments about how the density was performed.
#' @field parasite_density The density in microliters.
#' @field parasite_density_method The method of how this density was obtained.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `date_measured`: The date the qpcr was performed, can be YYYY, YYYY-MM, or YYYY-MM-DD.
#' * `density_method_comments`: Additional comments about how the density was performed.
#' * `parasite_density`: The density in microliters.
#' * `parasite_density_method`: The method of how this density was obtained.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ParasiteDensity <- R6::R6Class(
  "ParasiteDensity",
  public = list(
    date_measured = NA_character_,
    density_method_comments = NA_character_,
    parasite_density = NA_real_,
    parasite_density_method = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param date_measured The date the qpcr was performed, can be YYYY, YYYY-MM, or YYYY-MM-DD.
    #' @param density_method_comments Additional comments about how the density was performed.
    #' @param parasite_density The density in microliters.
    #' @param parasite_density_method The method of how this density was obtained.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(date_measured = NULL, density_method_comments = NULL, parasite_density = NA_real_, parasite_density_method = NA_character_, extras = list()) {
      self$date_measured <- date_measured
      self$density_method_comments <- density_method_comments
      self$parasite_density <- parasite_density
      self$parasite_density_method <- parasite_density_method
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$date_measured) && !is.na(self$date_measured) && (!is.character(self$date_measured) || length(self$date_measured) != 1)) stop("ParasiteDensity.date_measured must be a single string")
      if (!is.null(self$date_measured) && !is.na(self$date_measured) && !grepl("(?:\\d{4}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?)?|NA)", self$date_measured, perl = TRUE)) stop("ParasiteDensity.date_measured does not match pattern: (?:\\d{4}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?)?|NA)")
      if (!is.null(self$density_method_comments) && !is.na(self$density_method_comments) && (!is.character(self$density_method_comments) || length(self$density_method_comments) != 1)) stop("ParasiteDensity.density_method_comments must be a single string")
      if (!is.null(self$density_method_comments) && !is.na(self$density_method_comments) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$density_method_comments, perl = TRUE)) stop("ParasiteDensity.density_method_comments does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$parasite_density) && !is.na(self$parasite_density) && (!is.numeric(self$parasite_density) || length(self$parasite_density) != 1)) stop("ParasiteDensity.parasite_density must be a single numeric value")
      if (!is.null(self$parasite_density) && !is.na(self$parasite_density) && self$parasite_density < 0) stop("ParasiteDensity.parasite_density < minimum 0")
      if (!is.null(self$parasite_density_method) && !is.na(self$parasite_density_method) && (!is.character(self$parasite_density_method) || length(self$parasite_density_method) != 1)) stop("ParasiteDensity.parasite_density_method must be a single string")
      if (!is.null(self$parasite_density_method) && !is.na(self$parasite_density_method) && !grepl("^[A-z-._0-9 ]+$", self$parasite_density_method, perl = TRUE)) stop("ParasiteDensity.parasite_density_method does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$date_measured)) out$date_measured <- if (is.na(self$date_measured)) "NA" else self$date_measured
      if (!is.null(self$density_method_comments)) out$density_method_comments <- if (is.na(self$density_method_comments)) "NA" else self$density_method_comments
      if (!is.null(self$parasite_density)) out$parasite_density <- self$parasite_density
      if (!is.null(self$parasite_density_method)) out$parasite_density_method <- if (is.na(self$parasite_density_method)) "NA" else self$parasite_density_method
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$date_measured)) out$date_measured <- if (is.na(self$date_measured)) "NA" else self$date_measured
      if (!is.null(self$density_method_comments)) out$density_method_comments <- if (is.na(self$density_method_comments)) "NA" else self$density_method_comments
      if (!is.null(self$parasite_density)) out$parasite_density <- self$parasite_density
      if (!is.null(self$parasite_density_method)) out$parasite_density_method <- if (is.na(self$parasite_density_method)) "NA" else self$parasite_density_method
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ParasiteDensity$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("parasite_density","parasite_density_method")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ParasiteDensity missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("date_measured","density_method_comments","parasite_density","parasite_density_method")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ParasiteDensity$new(date_measured = { v <- obj[["date_measured"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, density_method_comments = { v <- obj[["density_method_comments"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, parasite_density = if (!is.null(obj[["parasite_density"]])) obj[["parasite_density"]] else NA_real_, parasite_density_method = { v <- obj[["parasite_density_method"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PlateInfo
#'
#' Information about a plate location, e.g. a standard 96 well plate with row having a letter and column having a number.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field plate_col The column position.
#' @field plate_name A name for the plate.
#' @field plate_row The row position.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `plate_col`: The column position.
#' * `plate_name`: A name for the plate.
#' * `plate_row`: The row position.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PlateInfo <- R6::R6Class(
  "PlateInfo",
  public = list(
    plate_col = NA_real_,
    plate_name = NA_character_,
    plate_row = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param plate_col The column position.
    #' @param plate_name A name for the plate.
    #' @param plate_row The row position.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(plate_col = NA_real_, plate_name = NA_character_, plate_row = NA_character_, extras = list()) {
      self$plate_col <- plate_col
      self$plate_name <- plate_name
      self$plate_row <- plate_row
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$plate_col) && !is.na(self$plate_col) && (!is.numeric(self$plate_col) || length(self$plate_col) != 1)) stop("PlateInfo.plate_col must be a single numeric value")
      if (!is.null(self$plate_col) && !is.na(self$plate_col) && self$plate_col < 0) stop("PlateInfo.plate_col < minimum 0")
      if (!is.null(self$plate_col) && !is.na(self$plate_col) && !(is.numeric(self$plate_col) && isTRUE(all.equal(self$plate_col, as.integer(self$plate_col))))) stop("PlateInfo.plate_col must be integer-like")
      if (!is.null(self$plate_name) && !is.na(self$plate_name) && (!is.character(self$plate_name) || length(self$plate_name) != 1)) stop("PlateInfo.plate_name must be a single string")
      if (!is.null(self$plate_name) && !is.na(self$plate_name) && !grepl("^[A-z-._0-9 ]+$", self$plate_name, perl = TRUE)) stop("PlateInfo.plate_name does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$plate_row) && !is.na(self$plate_row) && (!is.character(self$plate_row) || length(self$plate_row) != 1)) stop("PlateInfo.plate_row must be a single string")
      if (!is.null(self$plate_row) && !is.na(self$plate_row) && !grepl("^[A-z]$", self$plate_row, perl = TRUE)) stop("PlateInfo.plate_row does not match pattern: ^[A-z]$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$plate_col)) out$plate_col <- self$plate_col
      if (!is.null(self$plate_name)) out$plate_name <- if (is.na(self$plate_name)) "NA" else self$plate_name
      if (!is.null(self$plate_row)) out$plate_row <- if (is.na(self$plate_row)) "NA" else self$plate_row
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$plate_col)) out$plate_col <- self$plate_col
      if (!is.null(self$plate_name)) out$plate_name <- if (is.na(self$plate_name)) "NA" else self$plate_name
      if (!is.null(self$plate_row)) out$plate_row <- if (is.na(self$plate_row)) "NA" else self$plate_row
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PlateInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("plate_col","plate_name","plate_row")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PlateInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("plate_col","plate_name","plate_row")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PlateInfo$new(plate_col = if (!is.null(obj[["plate_col"]])) obj[["plate_col"]] else NA_real_, plate_name = { v <- obj[["plate_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, plate_row = { v <- obj[["plate_row"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' LibrarySampleInfo
#'
#' Information about a specific amplification and sequencing of a specimen.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field alternate_identifiers A list of alternative names.
#' @field experiment_accession ERA/SRA experiment accession number for the sample if it was submitted.
#' @field fastqs_loc The location (url or filename path) of the fastqs for a library run.
#' @field library_prep_plate_info Plate location of where library was prepared for sequencing.
#' @field library_sample_name A unique identifier for this sequencing/amplification run.
#' @field panel_id The index into the panel_info list.
#' @field qpcr_parasite_density_info Qpcr parasite density measurement for this extracted sample.
#' @field run_accession ERA/SRA run accession number for the sample if it was submitted.
#' @field sequencing_info_id The index into the sequencing_info list.
#' @field specimen_id The index into the specimen_info list.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `alternate_identifiers`: A list of alternative names.
#' * `experiment_accession`: ERA/SRA experiment accession number for the sample if it was submitted.
#' * `fastqs_loc`: The location (url or filename path) of the fastqs for a library run.
#' * `library_prep_plate_info`: Plate location of where library was prepared for sequencing.
#' * `library_sample_name`: A unique identifier for this sequencing/amplification run.
#' * `panel_id`: The index into the panel_info list.
#' * `qpcr_parasite_density_info`: Qpcr parasite density measurement for this extracted sample.
#' * `run_accession`: ERA/SRA run accession number for the sample if it was submitted.
#' * `sequencing_info_id`: The index into the sequencing_info list.
#' * `specimen_id`: The index into the specimen_info list.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
LibrarySampleInfo <- R6::R6Class(
  "LibrarySampleInfo",
  public = list(
    alternate_identifiers = character(),
    experiment_accession = NA_character_,
    fastqs_loc = NA_character_,
    library_prep_plate_info = NULL,
    library_sample_name = NA_character_,
    panel_id = NA_real_,
    qpcr_parasite_density_info = list(),
    run_accession = NA_character_,
    sequencing_info_id = NA_real_,
    specimen_id = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param alternate_identifiers A list of alternative names.
    #' @param experiment_accession ERA/SRA experiment accession number for the sample if it was submitted.
    #' @param fastqs_loc The location (url or filename path) of the fastqs for a library run.
    #' @param library_prep_plate_info Plate location of where library was prepared for sequencing.
    #' @param library_sample_name A unique identifier for this sequencing/amplification run.
    #' @param panel_id The index into the panel_info list.
    #' @param qpcr_parasite_density_info Qpcr parasite density measurement for this extracted sample.
    #' @param run_accession ERA/SRA run accession number for the sample if it was submitted.
    #' @param sequencing_info_id The index into the sequencing_info list.
    #' @param specimen_id The index into the specimen_info list.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(alternate_identifiers = NULL, experiment_accession = NULL, fastqs_loc = NULL, library_prep_plate_info = NULL, library_sample_name = NA_character_, panel_id = NA_real_, qpcr_parasite_density_info = NULL, run_accession = NULL, sequencing_info_id = NULL, specimen_id = NA_real_, extras = list()) {
      self$alternate_identifiers <- alternate_identifiers
      self$experiment_accession <- experiment_accession
      self$fastqs_loc <- fastqs_loc
      self$library_prep_plate_info <- library_prep_plate_info
      self$library_sample_name <- library_sample_name
      self$panel_id <- panel_id
      self$qpcr_parasite_density_info <- qpcr_parasite_density_info
      self$run_accession <- run_accession
      self$sequencing_info_id <- sequencing_info_id
      self$specimen_id <- specimen_id
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$alternate_identifiers) && !is.character(self$alternate_identifiers)) stop("LibrarySampleInfo.alternate_identifiers must be a character vector")
      if (!is.null(self$alternate_identifiers) && length(self$alternate_identifiers) > 0 && any(!grepl("^[A-z-._0-9 ]+$", self$alternate_identifiers, perl = TRUE))) stop("LibrarySampleInfo.alternate_identifiers contains values that do not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$experiment_accession) && !is.na(self$experiment_accession) && (!is.character(self$experiment_accession) || length(self$experiment_accession) != 1)) stop("LibrarySampleInfo.experiment_accession must be a single string")
      if (!is.null(self$experiment_accession) && !is.na(self$experiment_accession) && !grepl("^[A-z-._0-9]+$", self$experiment_accession, perl = TRUE)) stop("LibrarySampleInfo.experiment_accession does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$fastqs_loc) && !is.na(self$fastqs_loc) && (!is.character(self$fastqs_loc) || length(self$fastqs_loc) != 1)) stop("LibrarySampleInfo.fastqs_loc must be a single string")
      if (!is.null(self$fastqs_loc) && !is.na(self$fastqs_loc) && !grepl("^[A-z-.;,_0-9\\(\\),\\/\\ ]+$", self$fastqs_loc, perl = TRUE)) stop("LibrarySampleInfo.fastqs_loc does not match pattern: ^[A-z-.;,_0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$library_sample_name) && !is.na(self$library_sample_name) && (!is.character(self$library_sample_name) || length(self$library_sample_name) != 1)) stop("LibrarySampleInfo.library_sample_name must be a single string")
      if (!is.null(self$library_sample_name) && !is.na(self$library_sample_name) && !grepl("^[A-z-._0-9 ]+$", self$library_sample_name, perl = TRUE)) stop("LibrarySampleInfo.library_sample_name does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$panel_id) && !is.na(self$panel_id) && (!is.numeric(self$panel_id) || length(self$panel_id) != 1)) stop("LibrarySampleInfo.panel_id must be a single numeric value")
      if (!is.null(self$panel_id) && !is.na(self$panel_id) && self$panel_id < 0) stop("LibrarySampleInfo.panel_id < minimum 0")
      if (!is.null(self$panel_id) && !is.na(self$panel_id) && !(is.numeric(self$panel_id) && isTRUE(all.equal(self$panel_id, as.integer(self$panel_id))))) stop("LibrarySampleInfo.panel_id must be integer-like")
      if (!is.null(self$qpcr_parasite_density_info) && !is.list(self$qpcr_parasite_density_info)) stop("LibrarySampleInfo.qpcr_parasite_density_info must be a list")
      if (!is.null(self$run_accession) && !is.na(self$run_accession) && (!is.character(self$run_accession) || length(self$run_accession) != 1)) stop("LibrarySampleInfo.run_accession must be a single string")
      if (!is.null(self$run_accession) && !is.na(self$run_accession) && !grepl("^[A-z-._0-9]+$", self$run_accession, perl = TRUE)) stop("LibrarySampleInfo.run_accession does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$sequencing_info_id) && !is.na(self$sequencing_info_id) && (!is.numeric(self$sequencing_info_id) || length(self$sequencing_info_id) != 1)) stop("LibrarySampleInfo.sequencing_info_id must be a single numeric value")
      if (!is.null(self$sequencing_info_id) && !is.na(self$sequencing_info_id) && self$sequencing_info_id < 0) stop("LibrarySampleInfo.sequencing_info_id < minimum 0")
      if (!is.null(self$sequencing_info_id) && !is.na(self$sequencing_info_id) && !(is.numeric(self$sequencing_info_id) && isTRUE(all.equal(self$sequencing_info_id, as.integer(self$sequencing_info_id))))) stop("LibrarySampleInfo.sequencing_info_id must be integer-like")
      if (!is.null(self$specimen_id) && !is.na(self$specimen_id) && (!is.numeric(self$specimen_id) || length(self$specimen_id) != 1)) stop("LibrarySampleInfo.specimen_id must be a single numeric value")
      if (!is.null(self$specimen_id) && !is.na(self$specimen_id) && self$specimen_id < 0) stop("LibrarySampleInfo.specimen_id < minimum 0")
      if (!is.null(self$specimen_id) && !is.na(self$specimen_id) && !(is.numeric(self$specimen_id) && isTRUE(all.equal(self$specimen_id, as.integer(self$specimen_id))))) stop("LibrarySampleInfo.specimen_id must be integer-like")
      if (!is.null(self$qpcr_parasite_density_info)) for (.x in self$qpcr_parasite_density_info) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$alternate_identifiers)) out$alternate_identifiers <- self$alternate_identifiers
      if (!is.null(self$experiment_accession)) out$experiment_accession <- if (is.na(self$experiment_accession)) "NA" else self$experiment_accession
      if (!is.null(self$fastqs_loc)) out$fastqs_loc <- if (is.na(self$fastqs_loc)) "NA" else self$fastqs_loc
      if (!is.null(self$library_prep_plate_info)) out$library_prep_plate_info <- self$library_prep_plate_info
      if (!is.null(self$library_sample_name)) out$library_sample_name <- if (is.na(self$library_sample_name)) "NA" else self$library_sample_name
      if (!is.null(self$panel_id)) out$panel_id <- self$panel_id
      if (!is.null(self$qpcr_parasite_density_info)) out$qpcr_parasite_density_info <- lapply(self$qpcr_parasite_density_info, function(x) x$to_list())
      if (!is.null(self$run_accession)) out$run_accession <- if (is.na(self$run_accession)) "NA" else self$run_accession
      if (!is.null(self$sequencing_info_id)) out$sequencing_info_id <- self$sequencing_info_id
      if (!is.null(self$specimen_id)) out$specimen_id <- self$specimen_id
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$alternate_identifiers)) out$alternate_identifiers <- I(self$alternate_identifiers)
      if (!is.null(self$experiment_accession)) out$experiment_accession <- if (is.na(self$experiment_accession)) "NA" else self$experiment_accession
      if (!is.null(self$fastqs_loc)) out$fastqs_loc <- if (is.na(self$fastqs_loc)) "NA" else self$fastqs_loc
      if (!is.null(self$library_prep_plate_info)) out$library_prep_plate_info <- self$library_prep_plate_info
      if (!is.null(self$library_sample_name)) out$library_sample_name <- if (is.na(self$library_sample_name)) "NA" else self$library_sample_name
      if (!is.null(self$panel_id)) out$panel_id <- pmo_apply_id_offset_write(self$panel_id, "panel_id")
      if (!is.null(self$qpcr_parasite_density_info)) out$qpcr_parasite_density_info <- I(lapply(self$qpcr_parasite_density_info, function(x) x$to_json_list()))
      if (!is.null(self$run_accession)) out$run_accession <- if (is.na(self$run_accession)) "NA" else self$run_accession
      if (!is.null(self$sequencing_info_id)) out$sequencing_info_id <- pmo_apply_id_offset_write(self$sequencing_info_id, "sequencing_info_id")
      if (!is.null(self$specimen_id)) out$specimen_id <- pmo_apply_id_offset_write(self$specimen_id, "specimen_id")
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

LibrarySampleInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("library_sample_name","panel_id","specimen_id")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("LibrarySampleInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("alternate_identifiers","experiment_accession","fastqs_loc","library_prep_plate_info","library_sample_name","panel_id","qpcr_parasite_density_info","run_accession","sequencing_info_id","specimen_id")
  extras <- obj[setdiff(names(obj), known)]
  inst <- LibrarySampleInfo$new(alternate_identifiers = { v <- obj[["alternate_identifiers"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, experiment_accession = { v <- obj[["experiment_accession"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, fastqs_loc = { v <- obj[["fastqs_loc"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_prep_plate_info = if (!is.null(obj[["library_prep_plate_info"]])) obj[["library_prep_plate_info"]] else NULL, library_sample_name = { v <- obj[["library_sample_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, panel_id = pmo_apply_id_offset_read(if (!is.null(obj[["panel_id"]])) obj[["panel_id"]] else NA_real_, "panel_id"), qpcr_parasite_density_info = if (!is.null(obj[["qpcr_parasite_density_info"]])) lapply(obj[["qpcr_parasite_density_info"]], function(.x) ParasiteDensity$from_json(.x, validate = FALSE)) else NULL, run_accession = { v <- obj[["run_accession"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, sequencing_info_id = pmo_apply_id_offset_read(if (!is.null(obj[["sequencing_info_id"]])) obj[["sequencing_info_id"]] else NULL, "sequencing_info_id"), specimen_id = pmo_apply_id_offset_read(if (!is.null(obj[["specimen_id"]])) obj[["specimen_id"]] else NA_real_, "specimen_id"), extras = extras)
  if (validate) inst$validate()
  inst
}

#' MarkerOfInterest
#'
#' A specific genomic location of interest, e.g. drug resistance, or other phenotypical marker.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field associations A list of associations with this marker, e.g. SP resistance, etc.
#' @field marker_location The genomic location.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `associations`: A list of associations with this marker, e.g. SP resistance, etc.
#' * `marker_location`: The genomic location.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
MarkerOfInterest <- R6::R6Class(
  "MarkerOfInterest",
  public = list(
    associations = character(),
    marker_location = NULL,
    extras = list(),

    #' @description Create a new instance.
    #' @param associations A list of associations with this marker, e.g. SP resistance, etc.
    #' @param marker_location The genomic location.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(associations = NULL, marker_location = NULL, extras = list()) {
      self$associations <- associations
      self$marker_location <- marker_location
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$associations) && !is.character(self$associations)) stop("MarkerOfInterest.associations must be a character vector")
      if (!is.null(self$associations) && length(self$associations) > 0 && any(!grepl("^[A-z-._0-9]+$", self$associations, perl = TRUE))) stop("MarkerOfInterest.associations contains values that do not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$marker_location)) self$marker_location$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$associations)) out$associations <- self$associations
      if (!is.null(self$marker_location)) out$marker_location <- self$marker_location$to_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$associations)) out$associations <- I(self$associations)
      if (!is.null(self$marker_location)) out$marker_location <- self$marker_location$to_json_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

MarkerOfInterest$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("marker_location")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("MarkerOfInterest missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("associations","marker_location")
  extras <- obj[setdiff(names(obj), known)]
  inst <- MarkerOfInterest$new(associations = { v <- obj[["associations"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, marker_location = if (!is.null(obj[["marker_location"]])) GenomicLocation$from_json(obj[["marker_location"]], validate = FALSE) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' MaskingInfo
#'
#' Information about a subsegment of the sequence that should be masked.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field masking_generation_description A description of how the masking information was generated.
#' @field replacement_size The size of replacement mask.
#' @field seq_segment_size The size of the masking.
#' @field seq_start The start of the masking.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `masking_generation_description`: A description of how the masking information was generated.
#' * `replacement_size`: The size of replacement mask.
#' * `seq_segment_size`: The size of the masking.
#' * `seq_start`: The start of the masking.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
MaskingInfo <- R6::R6Class(
  "MaskingInfo",
  public = list(
    masking_generation_description = NA_character_,
    replacement_size = NA_real_,
    seq_segment_size = NA_real_,
    seq_start = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param masking_generation_description A description of how the masking information was generated.
    #' @param replacement_size The size of replacement mask.
    #' @param seq_segment_size The size of the masking.
    #' @param seq_start The start of the masking.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(masking_generation_description = NULL, replacement_size = NA_real_, seq_segment_size = NA_real_, seq_start = NA_real_, extras = list()) {
      self$masking_generation_description <- masking_generation_description
      self$replacement_size <- replacement_size
      self$seq_segment_size <- seq_segment_size
      self$seq_start <- seq_start
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$masking_generation_description) && !is.na(self$masking_generation_description) && (!is.character(self$masking_generation_description) || length(self$masking_generation_description) != 1)) stop("MaskingInfo.masking_generation_description must be a single string")
      if (!is.null(self$masking_generation_description) && !is.na(self$masking_generation_description) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$masking_generation_description, perl = TRUE)) stop("MaskingInfo.masking_generation_description does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$replacement_size) && !is.na(self$replacement_size) && (!is.numeric(self$replacement_size) || length(self$replacement_size) != 1)) stop("MaskingInfo.replacement_size must be a single numeric value")
      if (!is.null(self$replacement_size) && !is.na(self$replacement_size) && self$replacement_size < 0) stop("MaskingInfo.replacement_size < minimum 0")
      if (!is.null(self$replacement_size) && !is.na(self$replacement_size) && !(is.numeric(self$replacement_size) && isTRUE(all.equal(self$replacement_size, as.integer(self$replacement_size))))) stop("MaskingInfo.replacement_size must be integer-like")
      if (!is.null(self$seq_segment_size) && !is.na(self$seq_segment_size) && (!is.numeric(self$seq_segment_size) || length(self$seq_segment_size) != 1)) stop("MaskingInfo.seq_segment_size must be a single numeric value")
      if (!is.null(self$seq_segment_size) && !is.na(self$seq_segment_size) && self$seq_segment_size < 0) stop("MaskingInfo.seq_segment_size < minimum 0")
      if (!is.null(self$seq_segment_size) && !is.na(self$seq_segment_size) && !(is.numeric(self$seq_segment_size) && isTRUE(all.equal(self$seq_segment_size, as.integer(self$seq_segment_size))))) stop("MaskingInfo.seq_segment_size must be integer-like")
      if (!is.null(self$seq_start) && !is.na(self$seq_start) && (!is.numeric(self$seq_start) || length(self$seq_start) != 1)) stop("MaskingInfo.seq_start must be a single numeric value")
      if (!is.null(self$seq_start) && !is.na(self$seq_start) && self$seq_start < 0) stop("MaskingInfo.seq_start < minimum 0")
      if (!is.null(self$seq_start) && !is.na(self$seq_start) && !(is.numeric(self$seq_start) && isTRUE(all.equal(self$seq_start, as.integer(self$seq_start))))) stop("MaskingInfo.seq_start must be integer-like")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$masking_generation_description)) out$masking_generation_description <- if (is.na(self$masking_generation_description)) "NA" else self$masking_generation_description
      if (!is.null(self$replacement_size)) out$replacement_size <- self$replacement_size
      if (!is.null(self$seq_segment_size)) out$seq_segment_size <- self$seq_segment_size
      if (!is.null(self$seq_start)) out$seq_start <- self$seq_start
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$masking_generation_description)) out$masking_generation_description <- if (is.na(self$masking_generation_description)) "NA" else self$masking_generation_description
      if (!is.null(self$replacement_size)) out$replacement_size <- self$replacement_size
      if (!is.null(self$seq_segment_size)) out$seq_segment_size <- self$seq_segment_size
      if (!is.null(self$seq_start)) out$seq_start <- self$seq_start
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

MaskingInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("replacement_size","seq_segment_size","seq_start")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("MaskingInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("masking_generation_description","replacement_size","seq_segment_size","seq_start")
  extras <- obj[setdiff(names(obj), known)]
  inst <- MaskingInfo$new(masking_generation_description = { v <- obj[["masking_generation_description"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, replacement_size = if (!is.null(obj[["replacement_size"]])) obj[["replacement_size"]] else NA_real_, seq_segment_size = if (!is.null(obj[["seq_segment_size"]])) obj[["seq_segment_size"]] else NA_real_, seq_start = if (!is.null(obj[["seq_start"]])) obj[["seq_start"]] else NA_real_, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ReactionInfo
#'
#' Information on a panel of targeted amplicon primer pairs.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field panel_targets A list of the target indexes in the target_info list.
#' @field reaction_name A name for this reaction.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `panel_targets`: A list of the target indexes in the target_info list.
#' * `reaction_name`: A name for this reaction.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ReactionInfo <- R6::R6Class(
  "ReactionInfo",
  public = list(
    panel_targets = numeric(),
    reaction_name = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param panel_targets A list of the target indexes in the target_info list.
    #' @param reaction_name A name for this reaction.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(panel_targets = numeric(), reaction_name = NA_character_, extras = list()) {
      self$panel_targets <- panel_targets
      self$reaction_name <- reaction_name
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$panel_targets) && !is.numeric(self$panel_targets)) stop("ReactionInfo.panel_targets must be a numeric vector")
      if (!is.null(self$panel_targets) && length(self$panel_targets) > 0 && any(self$panel_targets < 0, na.rm = TRUE)) stop("ReactionInfo.panel_targets contains values < minimum 0")
      if (!is.null(self$panel_targets) && length(self$panel_targets) > 0 && any(!vapply(self$panel_targets, function(.x) is.na(.x) || (is.numeric(.x) && isTRUE(all.equal(.x, as.integer(.x)))), logical(1)))) stop("ReactionInfo.panel_targets must contain integer-like values")
      if (!is.null(self$reaction_name) && !is.na(self$reaction_name) && (!is.character(self$reaction_name) || length(self$reaction_name) != 1)) stop("ReactionInfo.reaction_name must be a single string")
      if (!is.null(self$reaction_name) && !is.na(self$reaction_name) && !grepl("^[A-z-._0-9]+$", self$reaction_name, perl = TRUE)) stop("ReactionInfo.reaction_name does not match pattern: ^[A-z-._0-9]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$panel_targets)) out$panel_targets <- self$panel_targets
      if (!is.null(self$reaction_name)) out$reaction_name <- if (is.na(self$reaction_name)) "NA" else self$reaction_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$panel_targets)) out$panel_targets <- I(pmo_apply_id_offset_write(self$panel_targets, "panel_targets"))
      if (!is.null(self$reaction_name)) out$reaction_name <- if (is.na(self$reaction_name)) "NA" else self$reaction_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ReactionInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("panel_targets","reaction_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ReactionInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("panel_targets","reaction_name")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ReactionInfo$new(panel_targets = { v <- obj[["panel_targets"]]; if (is.null(v)) NULL else if (length(v) == 0) numeric() else as.numeric(unlist(v, use.names = FALSE)) }, reaction_name = { v <- obj[["reaction_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PanelInfo
#'
#' Information on a panel of targeted amplicon primer pairs.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field panel_name A name for the panel.
#' @field reactions A list of 1 or more reactions that this panel contains, each reactions list the targets that were amplified in that reaction, e.g. pool1, pool2.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `panel_name`: A name for the panel.
#' * `reactions`: A list of 1 or more reactions that this panel contains, each reactions list the targets that were amplified in that reaction, e.g. pool1, pool2.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PanelInfo <- R6::R6Class(
  "PanelInfo",
  public = list(
    panel_name = NA_character_,
    reactions = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param panel_name A name for the panel.
    #' @param reactions A list of 1 or more reactions that this panel contains, each reactions list the targets that were amplified in that reaction, e.g. pool1, pool2.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(panel_name = NA_character_, reactions = list(), extras = list()) {
      self$panel_name <- panel_name
      self$reactions <- reactions
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$panel_name) && !is.na(self$panel_name) && (!is.character(self$panel_name) || length(self$panel_name) != 1)) stop("PanelInfo.panel_name must be a single string")
      if (!is.null(self$panel_name) && !is.na(self$panel_name) && !grepl("^[A-z-._0-9]+$", self$panel_name, perl = TRUE)) stop("PanelInfo.panel_name does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$reactions) && !is.list(self$reactions)) stop("PanelInfo.reactions must be a list")
      if (!is.null(self$reactions)) for (.x in self$reactions) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$panel_name)) out$panel_name <- if (is.na(self$panel_name)) "NA" else self$panel_name
      if (!is.null(self$reactions)) out$reactions <- lapply(self$reactions, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$panel_name)) out$panel_name <- if (is.na(self$panel_name)) "NA" else self$panel_name
      if (!is.null(self$reactions)) out$reactions <- I(lapply(self$reactions, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PanelInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("panel_name","reactions")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PanelInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("panel_name","reactions")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PanelInfo$new(panel_name = { v <- obj[["panel_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, reactions = if (!is.null(obj[["reactions"]])) lapply(obj[["reactions"]], function(.x) ReactionInfo$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PmoGenerationMethod
#'
#' Information about how a PMO was generated.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field program_name The name of the program.
#' @field program_version The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `program_name`: The name of the program.
#' * `program_version`: The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PmoGenerationMethod <- R6::R6Class(
  "PmoGenerationMethod",
  public = list(
    program_name = NA_character_,
    program_version = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param program_name The name of the program.
    #' @param program_version The version of program, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(program_name = NA_character_, program_version = NA_character_, extras = list()) {
      self$program_name <- program_name
      self$program_version <- program_version
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$program_name) && !is.na(self$program_name) && (!is.character(self$program_name) || length(self$program_name) != 1)) stop("PmoGenerationMethod.program_name must be a single string")
      if (!is.null(self$program_name) && !is.na(self$program_name) && !grepl("^[A-z-._0-9 ]+$", self$program_name, perl = TRUE)) stop("PmoGenerationMethod.program_name does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$program_version) && !is.na(self$program_version) && (!is.character(self$program_version) || length(self$program_version) != 1)) stop("PmoGenerationMethod.program_version must be a single string")
      if (!is.null(self$program_version) && !is.na(self$program_version) && !grepl("^[A-z-._0-9 ]+$", self$program_version, perl = TRUE)) stop("PmoGenerationMethod.program_version does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$program_name)) out$program_name <- if (is.na(self$program_name)) "NA" else self$program_name
      if (!is.null(self$program_version)) out$program_version <- if (is.na(self$program_version)) "NA" else self$program_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$program_name)) out$program_name <- if (is.na(self$program_name)) "NA" else self$program_name
      if (!is.null(self$program_version)) out$program_version <- if (is.na(self$program_version)) "NA" else self$program_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PmoGenerationMethod$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("program_name","program_version")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PmoGenerationMethod missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("program_name","program_version")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PmoGenerationMethod$new(program_name = { v <- obj[["program_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, program_version = { v <- obj[["program_version"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PmoHeader
#'
#' Information on the PMO file itself.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field creation_date The date of when the PMO file was created or modified, should be YYYY-MM-DD.
#' @field generation_method The generation method to create this PMO.
#' @field pmo_version The version of the PMO file, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `creation_date`: The date of when the PMO file was created or modified, should be YYYY-MM-DD.
#' * `generation_method`: The generation method to create this PMO.
#' * `pmo_version`: The version of the PMO file, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PmoHeader <- R6::R6Class(
  "PmoHeader",
  public = list(
    creation_date = NA_character_,
    generation_method = NULL,
    pmo_version = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param creation_date The date of when the PMO file was created or modified, should be YYYY-MM-DD.
    #' @param generation_method The generation method to create this PMO.
    #' @param pmo_version The version of the PMO file, should be in the format of v\[MAJOR\].\[MINOR\].\[PATCH\].
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(creation_date = NULL, generation_method = NULL, pmo_version = NA_character_, extras = list()) {
      self$creation_date <- creation_date
      self$generation_method <- generation_method
      self$pmo_version <- pmo_version
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$creation_date) && !is.na(self$creation_date) && (!is.character(self$creation_date) || length(self$creation_date) != 1)) stop("PmoHeader.creation_date must be a single string")
      if (!is.null(self$creation_date) && !is.na(self$creation_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$creation_date, perl = TRUE)) stop("PmoHeader.creation_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      if (!is.null(self$pmo_version) && !is.na(self$pmo_version) && (!is.character(self$pmo_version) || length(self$pmo_version) != 1)) stop("PmoHeader.pmo_version must be a single string")
      if (!is.null(self$pmo_version) && !is.na(self$pmo_version) && !grepl("^[A-z-._0-9 ]+$", self$pmo_version, perl = TRUE)) stop("PmoHeader.pmo_version does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$creation_date)) out$creation_date <- if (is.na(self$creation_date)) "NA" else self$creation_date
      if (!is.null(self$generation_method)) out$generation_method <- self$generation_method
      if (!is.null(self$pmo_version)) out$pmo_version <- if (is.na(self$pmo_version)) "NA" else self$pmo_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$creation_date)) out$creation_date <- if (is.na(self$creation_date)) "NA" else self$creation_date
      if (!is.null(self$generation_method)) out$generation_method <- self$generation_method
      if (!is.null(self$pmo_version)) out$pmo_version <- if (is.na(self$pmo_version)) "NA" else self$pmo_version
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PmoHeader$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("pmo_version")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PmoHeader missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("creation_date","generation_method","pmo_version")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PmoHeader$new(creation_date = { v <- obj[["creation_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, generation_method = if (!is.null(obj[["generation_method"]])) obj[["generation_method"]] else NULL, pmo_version = { v <- obj[["pmo_version"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ProjectInfo
#'
#' Information on a project underwhich a collection of specimens belong to.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field BioProject_accession An SRA bioproject accession e.g. PRJNA33823.
#' @field project_collector_chief_scientist Can be collection of names separated by a semicolon if multiple people involved or can just be the name of the primary person managing the specimen.
#' @field project_contributors A list of collaborators who contributed to this project.
#' @field project_description A short description of the project.
#' @field project_name A name for the project, should be unique if multiple projects listed.
#' @field project_type The type of project conducted, e.g. TES vs surveillance vs transmission.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `BioProject_accession`: An SRA bioproject accession e.g. PRJNA33823.
#' * `project_collector_chief_scientist`: Can be collection of names separated by a semicolon if multiple people involved or can just be the name of the primary person managing the specimen.
#' * `project_contributors`: A list of collaborators who contributed to this project.
#' * `project_description`: A short description of the project.
#' * `project_name`: A name for the project, should be unique if multiple projects listed.
#' * `project_type`: The type of project conducted, e.g. TES vs surveillance vs transmission.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ProjectInfo <- R6::R6Class(
  "ProjectInfo",
  public = list(
    BioProject_accession = NA_character_,
    project_collector_chief_scientist = NA_character_,
    project_contributors = character(),
    project_description = NA_character_,
    project_name = NA_character_,
    project_type = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param BioProject_accession An SRA bioproject accession e.g. PRJNA33823.
    #' @param project_collector_chief_scientist Can be collection of names separated by a semicolon if multiple people involved or can just be the name of the primary person managing the specimen.
    #' @param project_contributors A list of collaborators who contributed to this project.
    #' @param project_description A short description of the project.
    #' @param project_name A name for the project, should be unique if multiple projects listed.
    #' @param project_type The type of project conducted, e.g. TES vs surveillance vs transmission.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(BioProject_accession = NULL, project_collector_chief_scientist = NULL, project_contributors = NULL, project_description = NA_character_, project_name = NA_character_, project_type = NULL, extras = list()) {
      self$BioProject_accession <- BioProject_accession
      self$project_collector_chief_scientist <- project_collector_chief_scientist
      self$project_contributors <- project_contributors
      self$project_description <- project_description
      self$project_name <- project_name
      self$project_type <- project_type
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$BioProject_accession) && !is.na(self$BioProject_accession) && (!is.character(self$BioProject_accession) || length(self$BioProject_accession) != 1)) stop("ProjectInfo.BioProject_accession must be a single string")
      if (!is.null(self$BioProject_accession) && !is.na(self$BioProject_accession) && !grepl("^[A-z-._0-9 ]+$", self$BioProject_accession, perl = TRUE)) stop("ProjectInfo.BioProject_accession does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$project_collector_chief_scientist) && !is.na(self$project_collector_chief_scientist) && (!is.character(self$project_collector_chief_scientist) || length(self$project_collector_chief_scientist) != 1)) stop("ProjectInfo.project_collector_chief_scientist must be a single string")
      if (!is.null(self$project_collector_chief_scientist) && !is.na(self$project_collector_chief_scientist) && !grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$project_collector_chief_scientist, perl = TRUE)) stop("ProjectInfo.project_collector_chief_scientist does not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$project_contributors) && !is.character(self$project_contributors)) stop("ProjectInfo.project_contributors must be a character vector")
      if (!is.null(self$project_contributors) && length(self$project_contributors) > 0 && any(!grepl("^[A-z-._0-9 ]+$", self$project_contributors, perl = TRUE))) stop("ProjectInfo.project_contributors contains values that do not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$project_description) && !is.na(self$project_description) && (!is.character(self$project_description) || length(self$project_description) != 1)) stop("ProjectInfo.project_description must be a single string")
      if (!is.null(self$project_name) && !is.na(self$project_name) && (!is.character(self$project_name) || length(self$project_name) != 1)) stop("ProjectInfo.project_name must be a single string")
      if (!is.null(self$project_name) && !is.na(self$project_name) && !grepl("^[A-z-._0-9 ]+$", self$project_name, perl = TRUE)) stop("ProjectInfo.project_name does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$project_type) && !is.na(self$project_type) && (!is.character(self$project_type) || length(self$project_type) != 1)) stop("ProjectInfo.project_type must be a single string")
      if (!is.null(self$project_type) && !is.na(self$project_type) && !grepl("^[A-z-._0-9 ]+$", self$project_type, perl = TRUE)) stop("ProjectInfo.project_type does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$BioProject_accession)) out$BioProject_accession <- if (is.na(self$BioProject_accession)) "NA" else self$BioProject_accession
      if (!is.null(self$project_collector_chief_scientist)) out$project_collector_chief_scientist <- if (is.na(self$project_collector_chief_scientist)) "NA" else self$project_collector_chief_scientist
      if (!is.null(self$project_contributors)) out$project_contributors <- self$project_contributors
      if (!is.null(self$project_description)) out$project_description <- if (is.na(self$project_description)) "NA" else self$project_description
      if (!is.null(self$project_name)) out$project_name <- if (is.na(self$project_name)) "NA" else self$project_name
      if (!is.null(self$project_type)) out$project_type <- if (is.na(self$project_type)) "NA" else self$project_type
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$BioProject_accession)) out$BioProject_accession <- if (is.na(self$BioProject_accession)) "NA" else self$BioProject_accession
      if (!is.null(self$project_collector_chief_scientist)) out$project_collector_chief_scientist <- if (is.na(self$project_collector_chief_scientist)) "NA" else self$project_collector_chief_scientist
      if (!is.null(self$project_contributors)) out$project_contributors <- I(self$project_contributors)
      if (!is.null(self$project_description)) out$project_description <- if (is.na(self$project_description)) "NA" else self$project_description
      if (!is.null(self$project_name)) out$project_name <- if (is.na(self$project_name)) "NA" else self$project_name
      if (!is.null(self$project_type)) out$project_type <- if (is.na(self$project_type)) "NA" else self$project_type
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ProjectInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("project_description","project_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ProjectInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("BioProject_accession","project_collector_chief_scientist","project_contributors","project_description","project_name","project_type")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ProjectInfo$new(BioProject_accession = { v <- obj[["BioProject_accession"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, project_collector_chief_scientist = { v <- obj[["project_collector_chief_scientist"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, project_contributors = { v <- obj[["project_contributors"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, project_description = { v <- obj[["project_description"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, project_name = { v <- obj[["project_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, project_type = { v <- obj[["project_type"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' StageReadCounts
#'
#' Information on the reads counts at several stages.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field reads The read counts for this stage.
#' @field stage The stage of the pipeline, e.g. demultiplexed, denoised, etc.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `reads`: The read counts for this stage.
#' * `stage`: The stage of the pipeline, e.g. demultiplexed, denoised, etc.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
StageReadCounts <- R6::R6Class(
  "StageReadCounts",
  public = list(
    reads = NA_real_,
    stage = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param reads The read counts for this stage.
    #' @param stage The stage of the pipeline, e.g. demultiplexed, denoised, etc.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(reads = NA_real_, stage = NA_character_, extras = list()) {
      self$reads <- reads
      self$stage <- stage
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$reads) && !is.na(self$reads) && (!is.numeric(self$reads) || length(self$reads) != 1)) stop("StageReadCounts.reads must be a single numeric value")
      if (!is.null(self$reads) && !is.na(self$reads) && self$reads < 0) stop("StageReadCounts.reads < minimum 0")
      if (!is.null(self$reads) && !is.na(self$reads) && !(is.numeric(self$reads) && isTRUE(all.equal(self$reads, as.integer(self$reads))))) stop("StageReadCounts.reads must be integer-like")
      if (!is.null(self$stage) && !is.na(self$stage) && (!is.character(self$stage) || length(self$stage) != 1)) stop("StageReadCounts.stage must be a single string")
      if (!is.null(self$stage) && !is.na(self$stage) && !grepl("^[A-z-._0-9 ]+$", self$stage, perl = TRUE)) stop("StageReadCounts.stage does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$reads)) out$reads <- self$reads
      if (!is.null(self$stage)) out$stage <- if (is.na(self$stage)) "NA" else self$stage
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$reads)) out$reads <- self$reads
      if (!is.null(self$stage)) out$stage <- if (is.na(self$stage)) "NA" else self$stage
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

StageReadCounts$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("reads","stage")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("StageReadCounts missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("reads","stage")
  extras <- obj[setdiff(names(obj), known)]
  inst <- StageReadCounts$new(reads = if (!is.null(obj[["reads"]])) obj[["reads"]] else NA_real_, stage = { v <- obj[["stage"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ReadCountsByStageForTarget
#'
#' Information on the reads counts at several stages of a pipeline for a target.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field stages The read counts by each stage.
#' @field target_id The index into the target_info list.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `stages`: The read counts by each stage.
#' * `target_id`: The index into the target_info list.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ReadCountsByStageForTarget <- R6::R6Class(
  "ReadCountsByStageForTarget",
  public = list(
    stages = list(),
    target_id = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param stages The read counts by each stage.
    #' @param target_id The index into the target_info list.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(stages = list(), target_id = NA_real_, extras = list()) {
      self$stages <- stages
      self$target_id <- target_id
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$stages) && !is.list(self$stages)) stop("ReadCountsByStageForTarget.stages must be a list")
      if (!is.null(self$target_id) && !is.na(self$target_id) && (!is.numeric(self$target_id) || length(self$target_id) != 1)) stop("ReadCountsByStageForTarget.target_id must be a single numeric value")
      if (!is.null(self$target_id) && !is.na(self$target_id) && self$target_id < 0) stop("ReadCountsByStageForTarget.target_id < minimum 0")
      if (!is.null(self$target_id) && !is.na(self$target_id) && !(is.numeric(self$target_id) && isTRUE(all.equal(self$target_id, as.integer(self$target_id))))) stop("ReadCountsByStageForTarget.target_id must be integer-like")
      if (!is.null(self$stages)) for (.x in self$stages) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$stages)) out$stages <- lapply(self$stages, function(x) x$to_list())
      if (!is.null(self$target_id)) out$target_id <- self$target_id
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$stages)) out$stages <- I(lapply(self$stages, function(x) x$to_json_list()))
      if (!is.null(self$target_id)) out$target_id <- pmo_apply_id_offset_write(self$target_id, "target_id")
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ReadCountsByStageForTarget$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("stages","target_id")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ReadCountsByStageForTarget missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("stages","target_id")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ReadCountsByStageForTarget$new(stages = if (!is.null(obj[["stages"]])) lapply(obj[["stages"]], function(.x) StageReadCounts$from_json(.x, validate = FALSE)) else NULL, target_id = pmo_apply_id_offset_read(if (!is.null(obj[["target_id"]])) obj[["target_id"]] else NA_real_, "target_id"), extras = extras)
  if (validate) inst$validate()
  inst
}

#' ReadCountsByStageForLibrarySample
#'
#' Information on the reads counts at several stages of a pipeline for a library_sample.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field library_sample_id The index into the library_sample_info list.
#' @field read_counts_for_targets A list of counts by stage for a target.
#' @field total_raw_count The raw counts off the sequencing machine that a sample began with.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `library_sample_id`: The index into the library_sample_info list.
#' * `read_counts_for_targets`: A list of counts by stage for a target.
#' * `total_raw_count`: The raw counts off the sequencing machine that a sample began with.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ReadCountsByStageForLibrarySample <- R6::R6Class(
  "ReadCountsByStageForLibrarySample",
  public = list(
    library_sample_id = NA_real_,
    read_counts_for_targets = list(),
    total_raw_count = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param library_sample_id The index into the library_sample_info list.
    #' @param read_counts_for_targets A list of counts by stage for a target.
    #' @param total_raw_count The raw counts off the sequencing machine that a sample began with.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(library_sample_id = NA_real_, read_counts_for_targets = NULL, total_raw_count = NA_real_, extras = list()) {
      self$library_sample_id <- library_sample_id
      self$read_counts_for_targets <- read_counts_for_targets
      self$total_raw_count <- total_raw_count
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && (!is.numeric(self$library_sample_id) || length(self$library_sample_id) != 1)) stop("ReadCountsByStageForLibrarySample.library_sample_id must be a single numeric value")
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && self$library_sample_id < 0) stop("ReadCountsByStageForLibrarySample.library_sample_id < minimum 0")
      if (!is.null(self$library_sample_id) && !is.na(self$library_sample_id) && !(is.numeric(self$library_sample_id) && isTRUE(all.equal(self$library_sample_id, as.integer(self$library_sample_id))))) stop("ReadCountsByStageForLibrarySample.library_sample_id must be integer-like")
      if (!is.null(self$read_counts_for_targets) && !is.list(self$read_counts_for_targets)) stop("ReadCountsByStageForLibrarySample.read_counts_for_targets must be a list")
      if (!is.null(self$total_raw_count) && !is.na(self$total_raw_count) && (!is.numeric(self$total_raw_count) || length(self$total_raw_count) != 1)) stop("ReadCountsByStageForLibrarySample.total_raw_count must be a single numeric value")
      if (!is.null(self$total_raw_count) && !is.na(self$total_raw_count) && self$total_raw_count < 0) stop("ReadCountsByStageForLibrarySample.total_raw_count < minimum 0")
      if (!is.null(self$total_raw_count) && !is.na(self$total_raw_count) && !(is.numeric(self$total_raw_count) && isTRUE(all.equal(self$total_raw_count, as.integer(self$total_raw_count))))) stop("ReadCountsByStageForLibrarySample.total_raw_count must be integer-like")
      if (!is.null(self$read_counts_for_targets)) for (.x in self$read_counts_for_targets) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$library_sample_id)) out$library_sample_id <- self$library_sample_id
      if (!is.null(self$read_counts_for_targets)) out$read_counts_for_targets <- lapply(self$read_counts_for_targets, function(x) x$to_list())
      if (!is.null(self$total_raw_count)) out$total_raw_count <- self$total_raw_count
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$library_sample_id)) out$library_sample_id <- pmo_apply_id_offset_write(self$library_sample_id, "library_sample_id")
      if (!is.null(self$read_counts_for_targets)) out$read_counts_for_targets <- I(lapply(self$read_counts_for_targets, function(x) x$to_json_list()))
      if (!is.null(self$total_raw_count)) out$total_raw_count <- self$total_raw_count
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ReadCountsByStageForLibrarySample$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("library_sample_id","total_raw_count")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ReadCountsByStageForLibrarySample missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("library_sample_id","read_counts_for_targets","total_raw_count")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ReadCountsByStageForLibrarySample$new(library_sample_id = pmo_apply_id_offset_read(if (!is.null(obj[["library_sample_id"]])) obj[["library_sample_id"]] else NA_real_, "library_sample_id"), read_counts_for_targets = if (!is.null(obj[["read_counts_for_targets"]])) lapply(obj[["read_counts_for_targets"]], function(.x) ReadCountsByStageForTarget$from_json(.x, validate = FALSE)) else NULL, total_raw_count = if (!is.null(obj[["total_raw_count"]])) obj[["total_raw_count"]] else NA_real_, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ReadCountsByStage
#'
#' Information on the reads counts at several stages of a pipeline.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field bioinformatics_run_id The index into bioinformatics_run_info list.
#' @field read_counts_by_library_sample_by_stage A list by library_sample for the counts at each stage.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `bioinformatics_run_id`: The index into bioinformatics_run_info list.
#' * `read_counts_by_library_sample_by_stage`: A list by library_sample for the counts at each stage.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ReadCountsByStage <- R6::R6Class(
  "ReadCountsByStage",
  public = list(
    bioinformatics_run_id = NA_real_,
    read_counts_by_library_sample_by_stage = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param bioinformatics_run_id The index into bioinformatics_run_info list.
    #' @param read_counts_by_library_sample_by_stage A list by library_sample for the counts at each stage.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(bioinformatics_run_id = NULL, read_counts_by_library_sample_by_stage = list(), extras = list()) {
      self$bioinformatics_run_id <- bioinformatics_run_id
      self$read_counts_by_library_sample_by_stage <- read_counts_by_library_sample_by_stage
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && (!is.numeric(self$bioinformatics_run_id) || length(self$bioinformatics_run_id) != 1)) stop("ReadCountsByStage.bioinformatics_run_id must be a single numeric value")
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && self$bioinformatics_run_id < 0) stop("ReadCountsByStage.bioinformatics_run_id < minimum 0")
      if (!is.null(self$bioinformatics_run_id) && !is.na(self$bioinformatics_run_id) && !(is.numeric(self$bioinformatics_run_id) && isTRUE(all.equal(self$bioinformatics_run_id, as.integer(self$bioinformatics_run_id))))) stop("ReadCountsByStage.bioinformatics_run_id must be integer-like")
      if (!is.null(self$read_counts_by_library_sample_by_stage) && !is.list(self$read_counts_by_library_sample_by_stage)) stop("ReadCountsByStage.read_counts_by_library_sample_by_stage must be a list")
      if (!is.null(self$read_counts_by_library_sample_by_stage)) for (.x in self$read_counts_by_library_sample_by_stage) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_run_id)) out$bioinformatics_run_id <- self$bioinformatics_run_id
      if (!is.null(self$read_counts_by_library_sample_by_stage)) out$read_counts_by_library_sample_by_stage <- lapply(self$read_counts_by_library_sample_by_stage, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_run_id)) out$bioinformatics_run_id <- pmo_apply_id_offset_write(self$bioinformatics_run_id, "bioinformatics_run_id")
      if (!is.null(self$read_counts_by_library_sample_by_stage)) out$read_counts_by_library_sample_by_stage <- I(lapply(self$read_counts_by_library_sample_by_stage, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ReadCountsByStage$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("read_counts_by_library_sample_by_stage")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ReadCountsByStage missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("bioinformatics_run_id","read_counts_by_library_sample_by_stage")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ReadCountsByStage$new(bioinformatics_run_id = pmo_apply_id_offset_read(if (!is.null(obj[["bioinformatics_run_id"]])) obj[["bioinformatics_run_id"]] else NULL, "bioinformatics_run_id"), read_counts_by_library_sample_by_stage = if (!is.null(obj[["read_counts_by_library_sample_by_stage"]])) lapply(obj[["read_counts_by_library_sample_by_stage"]], function(.x) ReadCountsByStageForLibrarySample$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' ProteinVariant
#'
#' Information on a variant in protein sequence.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field alternative_gene_name An alternative gene name.
#' @field codon_genomic_location The position within the genomic sequence of the codon.
#' @field gene_name An identifier of the gene, if any, is being covered with this targeted.
#' @field protein_location The position within the protein, the chromosome in this case would be the transcript name.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `alternative_gene_name`: An alternative gene name.
#' * `codon_genomic_location`: The position within the genomic sequence of the codon.
#' * `gene_name`: An identifier of the gene, if any, is being covered with this targeted.
#' * `protein_location`: The position within the protein, the chromosome in this case would be the transcript name.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
ProteinVariant <- R6::R6Class(
  "ProteinVariant",
  public = list(
    alternative_gene_name = NA_character_,
    codon_genomic_location = NULL,
    gene_name = NA_character_,
    protein_location = NULL,
    extras = list(),

    #' @description Create a new instance.
    #' @param alternative_gene_name An alternative gene name.
    #' @param codon_genomic_location The position within the genomic sequence of the codon.
    #' @param gene_name An identifier of the gene, if any, is being covered with this targeted.
    #' @param protein_location The position within the protein, the chromosome in this case would be the transcript name.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(alternative_gene_name = NULL, codon_genomic_location = NULL, gene_name = NULL, protein_location = NULL, extras = list()) {
      self$alternative_gene_name <- alternative_gene_name
      self$codon_genomic_location <- codon_genomic_location
      self$gene_name <- gene_name
      self$protein_location <- protein_location
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$alternative_gene_name) && !is.na(self$alternative_gene_name) && (!is.character(self$alternative_gene_name) || length(self$alternative_gene_name) != 1)) stop("ProteinVariant.alternative_gene_name must be a single string")
      if (!is.null(self$alternative_gene_name) && !is.na(self$alternative_gene_name) && !grepl("^[A-z-._0-9]+$", self$alternative_gene_name, perl = TRUE)) stop("ProteinVariant.alternative_gene_name does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$gene_name) && !is.na(self$gene_name) && (!is.character(self$gene_name) || length(self$gene_name) != 1)) stop("ProteinVariant.gene_name must be a single string")
      if (!is.null(self$gene_name) && !is.na(self$gene_name) && !grepl("^[A-z-._0-9:]+$", self$gene_name, perl = TRUE)) stop("ProteinVariant.gene_name does not match pattern: ^[A-z-._0-9:]+$")
      if (!is.null(self$protein_location)) self$protein_location$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$alternative_gene_name)) out$alternative_gene_name <- if (is.na(self$alternative_gene_name)) "NA" else self$alternative_gene_name
      if (!is.null(self$codon_genomic_location)) out$codon_genomic_location <- self$codon_genomic_location
      if (!is.null(self$gene_name)) out$gene_name <- if (is.na(self$gene_name)) "NA" else self$gene_name
      if (!is.null(self$protein_location)) out$protein_location <- self$protein_location$to_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$alternative_gene_name)) out$alternative_gene_name <- if (is.na(self$alternative_gene_name)) "NA" else self$alternative_gene_name
      if (!is.null(self$codon_genomic_location)) out$codon_genomic_location <- self$codon_genomic_location
      if (!is.null(self$gene_name)) out$gene_name <- if (is.na(self$gene_name)) "NA" else self$gene_name
      if (!is.null(self$protein_location)) out$protein_location <- self$protein_location$to_json_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

ProteinVariant$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("protein_location")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("ProteinVariant missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("alternative_gene_name","codon_genomic_location","gene_name","protein_location")
  extras <- obj[setdiff(names(obj), known)]
  inst <- ProteinVariant$new(alternative_gene_name = { v <- obj[["alternative_gene_name"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, codon_genomic_location = if (!is.null(obj[["codon_genomic_location"]])) obj[["codon_genomic_location"]] else NULL, gene_name = { v <- obj[["gene_name"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, protein_location = if (!is.null(obj[["protein_location"]])) GenomicLocation$from_json(obj[["protein_location"]], validate = FALSE) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' Pseudocigar
#'
#' Information on pseudocigar for a sequence.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field pseudocigar_generation_description A description of how the pseudocigar information was generated.
#' @field pseudocigar_seq The pseudocigar itself.
#' @field ref_loc The genomic location the pseudocigar is in reference to.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `pseudocigar_generation_description`: A description of how the pseudocigar information was generated.
#' * `pseudocigar_seq`: The pseudocigar itself.
#' * `ref_loc`: The genomic location the pseudocigar is in reference to.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
Pseudocigar <- R6::R6Class(
  "Pseudocigar",
  public = list(
    pseudocigar_generation_description = NA_character_,
    pseudocigar_seq = NA_character_,
    ref_loc = NULL,
    extras = list(),

    #' @description Create a new instance.
    #' @param pseudocigar_generation_description A description of how the pseudocigar information was generated.
    #' @param pseudocigar_seq The pseudocigar itself.
    #' @param ref_loc The genomic location the pseudocigar is in reference to.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(pseudocigar_generation_description = NULL, pseudocigar_seq = NA_character_, ref_loc = NULL, extras = list()) {
      self$pseudocigar_generation_description <- pseudocigar_generation_description
      self$pseudocigar_seq <- pseudocigar_seq
      self$ref_loc <- ref_loc
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$pseudocigar_generation_description) && !is.na(self$pseudocigar_generation_description) && (!is.character(self$pseudocigar_generation_description) || length(self$pseudocigar_generation_description) != 1)) stop("Pseudocigar.pseudocigar_generation_description must be a single string")
      if (!is.null(self$pseudocigar_generation_description) && !is.na(self$pseudocigar_generation_description) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$pseudocigar_generation_description, perl = TRUE)) stop("Pseudocigar.pseudocigar_generation_description does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$pseudocigar_seq) && !is.na(self$pseudocigar_seq) && (!is.character(self$pseudocigar_seq) || length(self$pseudocigar_seq) != 1)) stop("Pseudocigar.pseudocigar_seq must be a single string")
      if (!is.null(self$pseudocigar_seq) && !is.na(self$pseudocigar_seq) && !grepl("^[a-zA-Z0-9+=.]+$", self$pseudocigar_seq, perl = TRUE)) stop("Pseudocigar.pseudocigar_seq does not match pattern: ^[a-zA-Z0-9+=.]+$")
      if (!is.null(self$ref_loc)) self$ref_loc$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$pseudocigar_generation_description)) out$pseudocigar_generation_description <- if (is.na(self$pseudocigar_generation_description)) "NA" else self$pseudocigar_generation_description
      if (!is.null(self$pseudocigar_seq)) out$pseudocigar_seq <- if (is.na(self$pseudocigar_seq)) "NA" else self$pseudocigar_seq
      if (!is.null(self$ref_loc)) out$ref_loc <- self$ref_loc$to_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$pseudocigar_generation_description)) out$pseudocigar_generation_description <- if (is.na(self$pseudocigar_generation_description)) "NA" else self$pseudocigar_generation_description
      if (!is.null(self$pseudocigar_seq)) out$pseudocigar_seq <- if (is.na(self$pseudocigar_seq)) "NA" else self$pseudocigar_seq
      if (!is.null(self$ref_loc)) out$ref_loc <- self$ref_loc$to_json_list()
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

Pseudocigar$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("pseudocigar_seq","ref_loc")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("Pseudocigar missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("pseudocigar_generation_description","pseudocigar_seq","ref_loc")
  extras <- obj[setdiff(names(obj), known)]
  inst <- Pseudocigar$new(pseudocigar_generation_description = { v <- obj[["pseudocigar_generation_description"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, pseudocigar_seq = { v <- obj[["pseudocigar_seq"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, ref_loc = if (!is.null(obj[["ref_loc"]])) GenomicLocation$from_json(obj[["ref_loc"]], validate = FALSE) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' RepresentativeMicrohaplotype
#'
#' The representative sequence for a microhaplotype.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field alt_annotations A list of additional annotations associated with this microhaplotype, e.g. wildtype.
#' @field associated_protein_variants A list of protein variants for this haplotype, e.g. amino acid changes/INDELS.
#' @field associated_seq_variants A list of sequence variants for this haplotype, e.g. SNPS, indels.
#' @field masking Masking info for the sequence.
#' @field microhaplotype_name An optional name for this microhaplotype.
#' @field pseudocigar The pseudocigar of the haplotype.
#' @field quality The ASCII fastq per base quality score for this sequence, this is optional, must be same length as the sequence.
#' @field seq The sequence.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `alt_annotations`: A list of additional annotations associated with this microhaplotype, e.g. wildtype.
#' * `associated_protein_variants`: A list of protein variants for this haplotype, e.g. amino acid changes/INDELS.
#' * `associated_seq_variants`: A list of sequence variants for this haplotype, e.g. SNPS, indels.
#' * `masking`: Masking info for the sequence.
#' * `microhaplotype_name`: An optional name for this microhaplotype.
#' * `pseudocigar`: The pseudocigar of the haplotype.
#' * `quality`: The ASCII fastq per base quality score for this sequence, this is optional, must be same length as the sequence.
#' * `seq`: The sequence.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
RepresentativeMicrohaplotype <- R6::R6Class(
  "RepresentativeMicrohaplotype",
  public = list(
    alt_annotations = character(),
    associated_protein_variants = list(),
    associated_seq_variants = list(),
    masking = list(),
    microhaplotype_name = NA_character_,
    pseudocigar = NULL,
    quality = NA_character_,
    seq = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param alt_annotations A list of additional annotations associated with this microhaplotype, e.g. wildtype.
    #' @param associated_protein_variants A list of protein variants for this haplotype, e.g. amino acid changes/INDELS.
    #' @param associated_seq_variants A list of sequence variants for this haplotype, e.g. SNPS, indels.
    #' @param masking Masking info for the sequence.
    #' @param microhaplotype_name An optional name for this microhaplotype.
    #' @param pseudocigar The pseudocigar of the haplotype.
    #' @param quality The ASCII fastq per base quality score for this sequence, this is optional, must be same length as the sequence.
    #' @param seq The sequence.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(alt_annotations = NULL, associated_protein_variants = NULL, associated_seq_variants = NULL, masking = NULL, microhaplotype_name = NULL, pseudocigar = NULL, quality = NULL, seq = NA_character_, extras = list()) {
      self$alt_annotations <- alt_annotations
      self$associated_protein_variants <- associated_protein_variants
      self$associated_seq_variants <- associated_seq_variants
      self$masking <- masking
      self$microhaplotype_name <- microhaplotype_name
      self$pseudocigar <- pseudocigar
      self$quality <- quality
      self$seq <- seq
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$alt_annotations) && !is.character(self$alt_annotations)) stop("RepresentativeMicrohaplotype.alt_annotations must be a character vector")
      if (!is.null(self$associated_protein_variants) && !is.list(self$associated_protein_variants)) stop("RepresentativeMicrohaplotype.associated_protein_variants must be a list")
      if (!is.null(self$associated_seq_variants) && !is.list(self$associated_seq_variants)) stop("RepresentativeMicrohaplotype.associated_seq_variants must be a list")
      if (!is.null(self$masking) && !is.list(self$masking)) stop("RepresentativeMicrohaplotype.masking must be a list")
      if (!is.null(self$microhaplotype_name) && !is.na(self$microhaplotype_name) && (!is.character(self$microhaplotype_name) || length(self$microhaplotype_name) != 1)) stop("RepresentativeMicrohaplotype.microhaplotype_name must be a single string")
      if (!is.null(self$microhaplotype_name) && !is.na(self$microhaplotype_name) && !grepl("^[A-z-._0-9]+$", self$microhaplotype_name, perl = TRUE)) stop("RepresentativeMicrohaplotype.microhaplotype_name does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$quality) && !is.na(self$quality) && (!is.character(self$quality) || length(self$quality) != 1)) stop("RepresentativeMicrohaplotype.quality must be a single string")
      if (!is.null(self$quality) && !is.na(self$quality) && !grepl("^[A-z-._0-9]+$", self$quality, perl = TRUE)) stop("RepresentativeMicrohaplotype.quality does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$seq) && !is.na(self$seq) && (!is.character(self$seq) || length(self$seq) != 1)) stop("RepresentativeMicrohaplotype.seq must be a single string")
      if (!is.null(self$seq) && !is.na(self$seq) && !grepl("^[A-z]+$", self$seq, perl = TRUE)) stop("RepresentativeMicrohaplotype.seq does not match pattern: ^[A-z]+$")
      if (!is.null(self$associated_protein_variants)) for (.x in self$associated_protein_variants) .x$validate()
      if (!is.null(self$associated_seq_variants)) for (.x in self$associated_seq_variants) .x$validate()
      if (!is.null(self$masking)) for (.x in self$masking) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$alt_annotations)) out$alt_annotations <- self$alt_annotations
      if (!is.null(self$associated_protein_variants)) out$associated_protein_variants <- lapply(self$associated_protein_variants, function(x) x$to_list())
      if (!is.null(self$associated_seq_variants)) out$associated_seq_variants <- lapply(self$associated_seq_variants, function(x) x$to_list())
      if (!is.null(self$masking)) out$masking <- lapply(self$masking, function(x) x$to_list())
      if (!is.null(self$microhaplotype_name)) out$microhaplotype_name <- if (is.na(self$microhaplotype_name)) "NA" else self$microhaplotype_name
      if (!is.null(self$pseudocigar)) out$pseudocigar <- self$pseudocigar
      if (!is.null(self$quality)) out$quality <- if (is.na(self$quality)) "NA" else self$quality
      if (!is.null(self$seq)) out$seq <- if (is.na(self$seq)) "NA" else self$seq
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$alt_annotations)) out$alt_annotations <- I(self$alt_annotations)
      if (!is.null(self$associated_protein_variants)) out$associated_protein_variants <- I(lapply(self$associated_protein_variants, function(x) x$to_json_list()))
      if (!is.null(self$associated_seq_variants)) out$associated_seq_variants <- I(lapply(self$associated_seq_variants, function(x) x$to_json_list()))
      if (!is.null(self$masking)) out$masking <- I(lapply(self$masking, function(x) x$to_json_list()))
      if (!is.null(self$microhaplotype_name)) out$microhaplotype_name <- if (is.na(self$microhaplotype_name)) "NA" else self$microhaplotype_name
      if (!is.null(self$pseudocigar)) out$pseudocigar <- self$pseudocigar
      if (!is.null(self$quality)) out$quality <- if (is.na(self$quality)) "NA" else self$quality
      if (!is.null(self$seq)) out$seq <- if (is.na(self$seq)) "NA" else self$seq
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

RepresentativeMicrohaplotype$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("seq")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("RepresentativeMicrohaplotype missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("alt_annotations","associated_protein_variants","associated_seq_variants","masking","microhaplotype_name","pseudocigar","quality","seq")
  extras <- obj[setdiff(names(obj), known)]
  inst <- RepresentativeMicrohaplotype$new(alt_annotations = { v <- obj[["alt_annotations"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, associated_protein_variants = if (!is.null(obj[["associated_protein_variants"]])) lapply(obj[["associated_protein_variants"]], function(.x) ProteinVariant$from_json(.x, validate = FALSE)) else NULL, associated_seq_variants = if (!is.null(obj[["associated_seq_variants"]])) lapply(obj[["associated_seq_variants"]], function(.x) GenomicLocation$from_json(.x, validate = FALSE)) else NULL, masking = if (!is.null(obj[["masking"]])) lapply(obj[["masking"]], function(.x) MaskingInfo$from_json(.x, validate = FALSE)) else NULL, microhaplotype_name = { v <- obj[["microhaplotype_name"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, pseudocigar = if (!is.null(obj[["pseudocigar"]])) obj[["pseudocigar"]] else NULL, quality = { v <- obj[["quality"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, seq = { v <- obj[["seq"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' RepresentativeMicrohaplotypesForTarget
#'
#' A list of the representative sequence for the microhaplotypes for a target.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field mhap_location A genomic location that was analyzed for this target info, this allows listing location that may be different from the full target location (e.g 1 trimmed off the full length).
#' @field microhaplotypes A list of all the microhaplotypes for a target.
#' @field target_id The index into the target_info list.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `mhap_location`: A genomic location that was analyzed for this target info, this allows listing location that may be different from the full target location (e.g 1 trimmed off the full length).
#' * `microhaplotypes`: A list of all the microhaplotypes for a target.
#' * `target_id`: The index into the target_info list.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
RepresentativeMicrohaplotypesForTarget <- R6::R6Class(
  "RepresentativeMicrohaplotypesForTarget",
  public = list(
    mhap_location = NULL,
    microhaplotypes = list(),
    target_id = NA_real_,
    extras = list(),

    #' @description Create a new instance.
    #' @param mhap_location A genomic location that was analyzed for this target info, this allows listing location that may be different from the full target location (e.g 1 trimmed off the full length).
    #' @param microhaplotypes A list of all the microhaplotypes for a target.
    #' @param target_id The index into the target_info list.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(mhap_location = NULL, microhaplotypes = list(), target_id = NA_real_, extras = list()) {
      self$mhap_location <- mhap_location
      self$microhaplotypes <- microhaplotypes
      self$target_id <- target_id
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$microhaplotypes) && !is.list(self$microhaplotypes)) stop("RepresentativeMicrohaplotypesForTarget.microhaplotypes must be a list")
      if (!is.null(self$target_id) && !is.na(self$target_id) && (!is.numeric(self$target_id) || length(self$target_id) != 1)) stop("RepresentativeMicrohaplotypesForTarget.target_id must be a single numeric value")
      if (!is.null(self$target_id) && !is.na(self$target_id) && self$target_id < 0) stop("RepresentativeMicrohaplotypesForTarget.target_id < minimum 0")
      if (!is.null(self$target_id) && !is.na(self$target_id) && !(is.numeric(self$target_id) && isTRUE(all.equal(self$target_id, as.integer(self$target_id))))) stop("RepresentativeMicrohaplotypesForTarget.target_id must be integer-like")
      if (!is.null(self$microhaplotypes)) for (.x in self$microhaplotypes) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$mhap_location)) out$mhap_location <- self$mhap_location
      if (!is.null(self$microhaplotypes)) out$microhaplotypes <- lapply(self$microhaplotypes, function(x) x$to_list())
      if (!is.null(self$target_id)) out$target_id <- self$target_id
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$mhap_location)) out$mhap_location <- self$mhap_location
      if (!is.null(self$microhaplotypes)) out$microhaplotypes <- I(lapply(self$microhaplotypes, function(x) x$to_json_list()))
      if (!is.null(self$target_id)) out$target_id <- pmo_apply_id_offset_write(self$target_id, "target_id")
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

RepresentativeMicrohaplotypesForTarget$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("microhaplotypes","target_id")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("RepresentativeMicrohaplotypesForTarget missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("mhap_location","microhaplotypes","target_id")
  extras <- obj[setdiff(names(obj), known)]
  inst <- RepresentativeMicrohaplotypesForTarget$new(mhap_location = if (!is.null(obj[["mhap_location"]])) obj[["mhap_location"]] else NULL, microhaplotypes = if (!is.null(obj[["microhaplotypes"]])) lapply(obj[["microhaplotypes"]], function(.x) RepresentativeMicrohaplotype$from_json(.x, validate = FALSE)) else NULL, target_id = pmo_apply_id_offset_read(if (!is.null(obj[["target_id"]])) obj[["target_id"]] else NA_real_, "target_id"), extras = extras)
  if (validate) inst$validate()
  inst
}

#' RepresentativeMicrohaplotypes
#'
#' A collection of representative sequences for microhaplotypes for all targets.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field targets A list of the microhaplotypes for each targets.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `targets`: A list of the microhaplotypes for each targets.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
RepresentativeMicrohaplotypes <- R6::R6Class(
  "RepresentativeMicrohaplotypes",
  public = list(
    targets = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param targets A list of the microhaplotypes for each targets.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(targets = list(), extras = list()) {
      self$targets <- targets
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$targets) && !is.list(self$targets)) stop("RepresentativeMicrohaplotypes.targets must be a list")
      if (!is.null(self$targets)) for (.x in self$targets) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$targets)) out$targets <- lapply(self$targets, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$targets)) out$targets <- I(lapply(self$targets, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

RepresentativeMicrohaplotypes$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("targets")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("RepresentativeMicrohaplotypes missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("targets")
  extras <- obj[setdiff(names(obj), known)]
  inst <- RepresentativeMicrohaplotypes$new(targets = if (!is.null(obj[["targets"]])) lapply(obj[["targets"]], function(.x) RepresentativeMicrohaplotypesForTarget$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' SequencingInfo
#'
#' Information on sequencing info.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field library_kit Name, version, and applicable cell or cycle numbers for the kit used to prepare libraries and load cells or chips for sequencing. If possible, include a part number, e.g. MiSeq Reagent Kit v3 (150-cycle), MS-102-3001.
#' @field library_layout Specify the configuration of reads, e.g. paired-end, single.
#' @field library_screen Describe enrichment, screening, or normalization methods applied during amplification or library preparation, e.g. size selection 390bp, diluted to 1 ng DNA/sample.
#' @field library_selection How amplification was done (common are PCR=Source material was selected by designed primers, RANDOM =Random selection by shearing or other method).
#' @field library_source Source of amplification material e.g. was it DNA (GENOMIC) or RNA (TRANSCRIPTOMIC) (common names GENOMIC, TRANSCRIPTOMIC).
#' @field library_strategy What the nuceloacid sequencing/amplification strategy was (common names are AMPLICON, WGS).
#' @field nucl_acid_amp Link to a reference or kit that describes the enzymatic amplification of nucleic acids.
#' @field nucl_acid_amp_date The date of the nucleoacid amplification.
#' @field nucl_acid_ext Link to a reference or kit that describes the recovery of nucleic acids from the sample.
#' @field nucl_acid_ext_date The date of the nucleoacid extraction.
#' @field pcr_cond The method/conditions for PCR, List PCR cycles used to amplify the target.
#' @field seq_center Name of facility where sequencing was performed (lab, core facility, or company).
#' @field seq_date The date of sequencing, should be YYYY-MM or YYYY-MM-DD.
#' @field seq_instrument_model The sequencing instrument model used to sequence the run, e.g. NextSeq 2000, MinION, Revio.
#' @field seq_platform The sequencing technology used to sequence the run, e.g. ILLUMINA, NANOPORE, PACBIO.
#' @field sequencing_info_name A name for a specific sequencing run, e.g. batch1.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `library_kit`: Name, version, and applicable cell or cycle numbers for the kit used to prepare libraries and load cells or chips for sequencing. If possible, include a part number, e.g. MiSeq Reagent Kit v3 (150-cycle), MS-102-3001.
#' * `library_layout`: Specify the configuration of reads, e.g. paired-end, single.
#' * `library_screen`: Describe enrichment, screening, or normalization methods applied during amplification or library preparation, e.g. size selection 390bp, diluted to 1 ng DNA/sample.
#' * `library_selection`: How amplification was done (common are PCR=Source material was selected by designed primers, RANDOM =Random selection by shearing or other method).
#' * `library_source`: Source of amplification material e.g. was it DNA (GENOMIC) or RNA (TRANSCRIPTOMIC) (common names GENOMIC, TRANSCRIPTOMIC).
#' * `library_strategy`: What the nuceloacid sequencing/amplification strategy was (common names are AMPLICON, WGS).
#' * `nucl_acid_amp`: Link to a reference or kit that describes the enzymatic amplification of nucleic acids.
#' * `nucl_acid_amp_date`: The date of the nucleoacid amplification.
#' * `nucl_acid_ext`: Link to a reference or kit that describes the recovery of nucleic acids from the sample.
#' * `nucl_acid_ext_date`: The date of the nucleoacid extraction.
#' * `pcr_cond`: The method/conditions for PCR, List PCR cycles used to amplify the target.
#' * `seq_center`: Name of facility where sequencing was performed (lab, core facility, or company).
#' * `seq_date`: The date of sequencing, should be YYYY-MM or YYYY-MM-DD.
#' * `seq_instrument_model`: The sequencing instrument model used to sequence the run, e.g. NextSeq 2000, MinION, Revio.
#' * `seq_platform`: The sequencing technology used to sequence the run, e.g. ILLUMINA, NANOPORE, PACBIO.
#' * `sequencing_info_name`: A name for a specific sequencing run, e.g. batch1.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
SequencingInfo <- R6::R6Class(
  "SequencingInfo",
  public = list(
    library_kit = NA_character_,
    library_layout = NA_character_,
    library_screen = NA_character_,
    library_selection = NA_character_,
    library_source = NA_character_,
    library_strategy = NA_character_,
    nucl_acid_amp = NA_character_,
    nucl_acid_amp_date = NA_character_,
    nucl_acid_ext = NA_character_,
    nucl_acid_ext_date = NA_character_,
    pcr_cond = NA_character_,
    seq_center = NA_character_,
    seq_date = NA_character_,
    seq_instrument_model = NA_character_,
    seq_platform = NA_character_,
    sequencing_info_name = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param library_kit Name, version, and applicable cell or cycle numbers for the kit used to prepare libraries and load cells or chips for sequencing. If possible, include a part number, e.g. MiSeq Reagent Kit v3 (150-cycle), MS-102-3001.
    #' @param library_layout Specify the configuration of reads, e.g. paired-end, single.
    #' @param library_screen Describe enrichment, screening, or normalization methods applied during amplification or library preparation, e.g. size selection 390bp, diluted to 1 ng DNA/sample.
    #' @param library_selection How amplification was done (common are PCR=Source material was selected by designed primers, RANDOM =Random selection by shearing or other method).
    #' @param library_source Source of amplification material e.g. was it DNA (GENOMIC) or RNA (TRANSCRIPTOMIC) (common names GENOMIC, TRANSCRIPTOMIC).
    #' @param library_strategy What the nuceloacid sequencing/amplification strategy was (common names are AMPLICON, WGS).
    #' @param nucl_acid_amp Link to a reference or kit that describes the enzymatic amplification of nucleic acids.
    #' @param nucl_acid_amp_date The date of the nucleoacid amplification.
    #' @param nucl_acid_ext Link to a reference or kit that describes the recovery of nucleic acids from the sample.
    #' @param nucl_acid_ext_date The date of the nucleoacid extraction.
    #' @param pcr_cond The method/conditions for PCR, List PCR cycles used to amplify the target.
    #' @param seq_center Name of facility where sequencing was performed (lab, core facility, or company).
    #' @param seq_date The date of sequencing, should be YYYY-MM or YYYY-MM-DD.
    #' @param seq_instrument_model The sequencing instrument model used to sequence the run, e.g. NextSeq 2000, MinION, Revio.
    #' @param seq_platform The sequencing technology used to sequence the run, e.g. ILLUMINA, NANOPORE, PACBIO.
    #' @param sequencing_info_name A name for a specific sequencing run, e.g. batch1.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(library_kit = NULL, library_layout = NA_character_, library_screen = NULL, library_selection = NA_character_, library_source = NA_character_, library_strategy = NA_character_, nucl_acid_amp = NULL, nucl_acid_amp_date = NULL, nucl_acid_ext = NULL, nucl_acid_ext_date = NULL, pcr_cond = NULL, seq_center = NULL, seq_date = NULL, seq_instrument_model = NA_character_, seq_platform = NA_character_, sequencing_info_name = NA_character_, extras = list()) {
      self$library_kit <- library_kit
      self$library_layout <- library_layout
      self$library_screen <- library_screen
      self$library_selection <- library_selection
      self$library_source <- library_source
      self$library_strategy <- library_strategy
      self$nucl_acid_amp <- nucl_acid_amp
      self$nucl_acid_amp_date <- nucl_acid_amp_date
      self$nucl_acid_ext <- nucl_acid_ext
      self$nucl_acid_ext_date <- nucl_acid_ext_date
      self$pcr_cond <- pcr_cond
      self$seq_center <- seq_center
      self$seq_date <- seq_date
      self$seq_instrument_model <- seq_instrument_model
      self$seq_platform <- seq_platform
      self$sequencing_info_name <- sequencing_info_name
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$library_kit) && !is.na(self$library_kit) && (!is.character(self$library_kit) || length(self$library_kit) != 1)) stop("SequencingInfo.library_kit must be a single string")
      if (!is.null(self$library_kit) && !is.na(self$library_kit) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$library_kit, perl = TRUE)) stop("SequencingInfo.library_kit does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$library_layout) && !is.na(self$library_layout) && (!is.character(self$library_layout) || length(self$library_layout) != 1)) stop("SequencingInfo.library_layout must be a single string")
      if (!is.null(self$library_layout) && !is.na(self$library_layout) && !grepl("^[A-z-._0-9 ]+$", self$library_layout, perl = TRUE)) stop("SequencingInfo.library_layout does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$library_screen) && !is.na(self$library_screen) && (!is.character(self$library_screen) || length(self$library_screen) != 1)) stop("SequencingInfo.library_screen must be a single string")
      if (!is.null(self$library_screen) && !is.na(self$library_screen) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$library_screen, perl = TRUE)) stop("SequencingInfo.library_screen does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$library_selection) && !is.na(self$library_selection) && (!is.character(self$library_selection) || length(self$library_selection) != 1)) stop("SequencingInfo.library_selection must be a single string")
      if (!is.null(self$library_selection) && !is.na(self$library_selection) && !grepl("^[A-z-._0-9 ]+$", self$library_selection, perl = TRUE)) stop("SequencingInfo.library_selection does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$library_source) && !is.na(self$library_source) && (!is.character(self$library_source) || length(self$library_source) != 1)) stop("SequencingInfo.library_source must be a single string")
      if (!is.null(self$library_source) && !is.na(self$library_source) && !grepl("^[A-z-._0-9 ]+$", self$library_source, perl = TRUE)) stop("SequencingInfo.library_source does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$library_strategy) && !is.na(self$library_strategy) && (!is.character(self$library_strategy) || length(self$library_strategy) != 1)) stop("SequencingInfo.library_strategy must be a single string")
      if (!is.null(self$library_strategy) && !is.na(self$library_strategy) && !grepl("^[A-z-._0-9 ]+$", self$library_strategy, perl = TRUE)) stop("SequencingInfo.library_strategy does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$nucl_acid_amp) && !is.na(self$nucl_acid_amp) && (!is.character(self$nucl_acid_amp) || length(self$nucl_acid_amp) != 1)) stop("SequencingInfo.nucl_acid_amp must be a single string")
      if (!is.null(self$nucl_acid_amp) && !is.na(self$nucl_acid_amp) && !grepl("^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$", self$nucl_acid_amp, perl = TRUE)) stop("SequencingInfo.nucl_acid_amp does not match pattern: ^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$")
      if (!is.null(self$nucl_acid_amp_date) && !is.na(self$nucl_acid_amp_date) && (!is.character(self$nucl_acid_amp_date) || length(self$nucl_acid_amp_date) != 1)) stop("SequencingInfo.nucl_acid_amp_date must be a single string")
      if (!is.null(self$nucl_acid_amp_date) && !is.na(self$nucl_acid_amp_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$nucl_acid_amp_date, perl = TRUE)) stop("SequencingInfo.nucl_acid_amp_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      if (!is.null(self$nucl_acid_ext) && !is.na(self$nucl_acid_ext) && (!is.character(self$nucl_acid_ext) || length(self$nucl_acid_ext) != 1)) stop("SequencingInfo.nucl_acid_ext must be a single string")
      if (!is.null(self$nucl_acid_ext) && !is.na(self$nucl_acid_ext) && !grepl("^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$", self$nucl_acid_ext, perl = TRUE)) stop("SequencingInfo.nucl_acid_ext does not match pattern: ^(https?|ftp):\\/\\/[^\\s/$.?#].[^\\s]*$")
      if (!is.null(self$nucl_acid_ext_date) && !is.na(self$nucl_acid_ext_date) && (!is.character(self$nucl_acid_ext_date) || length(self$nucl_acid_ext_date) != 1)) stop("SequencingInfo.nucl_acid_ext_date must be a single string")
      if (!is.null(self$nucl_acid_ext_date) && !is.na(self$nucl_acid_ext_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$nucl_acid_ext_date, perl = TRUE)) stop("SequencingInfo.nucl_acid_ext_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      if (!is.null(self$pcr_cond) && !is.na(self$pcr_cond) && (!is.character(self$pcr_cond) || length(self$pcr_cond) != 1)) stop("SequencingInfo.pcr_cond must be a single string")
      if (!is.null(self$pcr_cond) && !is.na(self$pcr_cond) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$pcr_cond, perl = TRUE)) stop("SequencingInfo.pcr_cond does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$seq_center) && !is.na(self$seq_center) && (!is.character(self$seq_center) || length(self$seq_center) != 1)) stop("SequencingInfo.seq_center must be a single string")
      if (!is.null(self$seq_center) && !is.na(self$seq_center) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$seq_center, perl = TRUE)) stop("SequencingInfo.seq_center does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$seq_date) && !is.na(self$seq_date) && (!is.character(self$seq_date) || length(self$seq_date) != 1)) stop("SequencingInfo.seq_date must be a single string")
      if (!is.null(self$seq_date) && !is.na(self$seq_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$seq_date, perl = TRUE)) stop("SequencingInfo.seq_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      if (!is.null(self$seq_instrument_model) && !is.na(self$seq_instrument_model) && (!is.character(self$seq_instrument_model) || length(self$seq_instrument_model) != 1)) stop("SequencingInfo.seq_instrument_model must be a single string")
      if (!is.null(self$seq_instrument_model) && !is.na(self$seq_instrument_model) && !grepl("^[A-z-._0-9 ]+$", self$seq_instrument_model, perl = TRUE)) stop("SequencingInfo.seq_instrument_model does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$seq_platform) && !is.na(self$seq_platform) && (!is.character(self$seq_platform) || length(self$seq_platform) != 1)) stop("SequencingInfo.seq_platform must be a single string")
      if (!is.null(self$seq_platform) && !is.na(self$seq_platform) && !grepl("^[A-z-._0-9 ]+$", self$seq_platform, perl = TRUE)) stop("SequencingInfo.seq_platform does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$sequencing_info_name) && !is.na(self$sequencing_info_name) && (!is.character(self$sequencing_info_name) || length(self$sequencing_info_name) != 1)) stop("SequencingInfo.sequencing_info_name must be a single string")
      if (!is.null(self$sequencing_info_name) && !is.na(self$sequencing_info_name) && !grepl("^[A-z-._0-9 ]+$", self$sequencing_info_name, perl = TRUE)) stop("SequencingInfo.sequencing_info_name does not match pattern: ^[A-z-._0-9 ]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$library_kit)) out$library_kit <- if (is.na(self$library_kit)) "NA" else self$library_kit
      if (!is.null(self$library_layout)) out$library_layout <- if (is.na(self$library_layout)) "NA" else self$library_layout
      if (!is.null(self$library_screen)) out$library_screen <- if (is.na(self$library_screen)) "NA" else self$library_screen
      if (!is.null(self$library_selection)) out$library_selection <- if (is.na(self$library_selection)) "NA" else self$library_selection
      if (!is.null(self$library_source)) out$library_source <- if (is.na(self$library_source)) "NA" else self$library_source
      if (!is.null(self$library_strategy)) out$library_strategy <- if (is.na(self$library_strategy)) "NA" else self$library_strategy
      if (!is.null(self$nucl_acid_amp)) out$nucl_acid_amp <- if (is.na(self$nucl_acid_amp)) "NA" else self$nucl_acid_amp
      if (!is.null(self$nucl_acid_amp_date)) out$nucl_acid_amp_date <- if (is.na(self$nucl_acid_amp_date)) "NA" else self$nucl_acid_amp_date
      if (!is.null(self$nucl_acid_ext)) out$nucl_acid_ext <- if (is.na(self$nucl_acid_ext)) "NA" else self$nucl_acid_ext
      if (!is.null(self$nucl_acid_ext_date)) out$nucl_acid_ext_date <- if (is.na(self$nucl_acid_ext_date)) "NA" else self$nucl_acid_ext_date
      if (!is.null(self$pcr_cond)) out$pcr_cond <- if (is.na(self$pcr_cond)) "NA" else self$pcr_cond
      if (!is.null(self$seq_center)) out$seq_center <- if (is.na(self$seq_center)) "NA" else self$seq_center
      if (!is.null(self$seq_date)) out$seq_date <- if (is.na(self$seq_date)) "NA" else self$seq_date
      if (!is.null(self$seq_instrument_model)) out$seq_instrument_model <- if (is.na(self$seq_instrument_model)) "NA" else self$seq_instrument_model
      if (!is.null(self$seq_platform)) out$seq_platform <- if (is.na(self$seq_platform)) "NA" else self$seq_platform
      if (!is.null(self$sequencing_info_name)) out$sequencing_info_name <- if (is.na(self$sequencing_info_name)) "NA" else self$sequencing_info_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$library_kit)) out$library_kit <- if (is.na(self$library_kit)) "NA" else self$library_kit
      if (!is.null(self$library_layout)) out$library_layout <- if (is.na(self$library_layout)) "NA" else self$library_layout
      if (!is.null(self$library_screen)) out$library_screen <- if (is.na(self$library_screen)) "NA" else self$library_screen
      if (!is.null(self$library_selection)) out$library_selection <- if (is.na(self$library_selection)) "NA" else self$library_selection
      if (!is.null(self$library_source)) out$library_source <- if (is.na(self$library_source)) "NA" else self$library_source
      if (!is.null(self$library_strategy)) out$library_strategy <- if (is.na(self$library_strategy)) "NA" else self$library_strategy
      if (!is.null(self$nucl_acid_amp)) out$nucl_acid_amp <- if (is.na(self$nucl_acid_amp)) "NA" else self$nucl_acid_amp
      if (!is.null(self$nucl_acid_amp_date)) out$nucl_acid_amp_date <- if (is.na(self$nucl_acid_amp_date)) "NA" else self$nucl_acid_amp_date
      if (!is.null(self$nucl_acid_ext)) out$nucl_acid_ext <- if (is.na(self$nucl_acid_ext)) "NA" else self$nucl_acid_ext
      if (!is.null(self$nucl_acid_ext_date)) out$nucl_acid_ext_date <- if (is.na(self$nucl_acid_ext_date)) "NA" else self$nucl_acid_ext_date
      if (!is.null(self$pcr_cond)) out$pcr_cond <- if (is.na(self$pcr_cond)) "NA" else self$pcr_cond
      if (!is.null(self$seq_center)) out$seq_center <- if (is.na(self$seq_center)) "NA" else self$seq_center
      if (!is.null(self$seq_date)) out$seq_date <- if (is.na(self$seq_date)) "NA" else self$seq_date
      if (!is.null(self$seq_instrument_model)) out$seq_instrument_model <- if (is.na(self$seq_instrument_model)) "NA" else self$seq_instrument_model
      if (!is.null(self$seq_platform)) out$seq_platform <- if (is.na(self$seq_platform)) "NA" else self$seq_platform
      if (!is.null(self$sequencing_info_name)) out$sequencing_info_name <- if (is.na(self$sequencing_info_name)) "NA" else self$sequencing_info_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

SequencingInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("library_layout","library_selection","library_source","library_strategy","seq_instrument_model","seq_platform","sequencing_info_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("SequencingInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("library_kit","library_layout","library_screen","library_selection","library_source","library_strategy","nucl_acid_amp","nucl_acid_amp_date","nucl_acid_ext","nucl_acid_ext_date","pcr_cond","seq_center","seq_date","seq_instrument_model","seq_platform","sequencing_info_name")
  extras <- obj[setdiff(names(obj), known)]
  inst <- SequencingInfo$new(library_kit = { v <- obj[["library_kit"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_layout = { v <- obj[["library_layout"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_screen = { v <- obj[["library_screen"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_selection = { v <- obj[["library_selection"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_source = { v <- obj[["library_source"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, library_strategy = { v <- obj[["library_strategy"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, nucl_acid_amp = { v <- obj[["nucl_acid_amp"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, nucl_acid_amp_date = { v <- obj[["nucl_acid_amp_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, nucl_acid_ext = { v <- obj[["nucl_acid_ext"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, nucl_acid_ext_date = { v <- obj[["nucl_acid_ext_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, pcr_cond = { v <- obj[["pcr_cond"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, seq_center = { v <- obj[["seq_center"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, seq_date = { v <- obj[["seq_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, seq_instrument_model = { v <- obj[["seq_instrument_model"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, seq_platform = { v <- obj[["seq_platform"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, sequencing_info_name = { v <- obj[["sequencing_info_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' TravelInfo
#'
#' Information on travel info.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field bed_net_usage Approximate usage of bed net while traveling, 1 = 100% nights with bed net, 0 = 0% no bed net usage.
#' @field geo_admin1 Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
#' @field geo_admin2 Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
#' @field geo_admin3 Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
#' @field lat_lon The latitude and longitude of a specific site.
#' @field travel_country The name of country, would be the same as admin level 0.
#' @field travel_end_date The date of the end of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
#' @field travel_start_date The date of the start of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `bed_net_usage`: Approximate usage of bed net while traveling, 1 = 100% nights with bed net, 0 = 0% no bed net usage.
#' * `geo_admin1`: Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
#' * `geo_admin2`: Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
#' * `geo_admin3`: Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
#' * `lat_lon`: The latitude and longitude of a specific site.
#' * `travel_country`: The name of country, would be the same as admin level 0.
#' * `travel_end_date`: The date of the end of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
#' * `travel_start_date`: The date of the start of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
TravelInfo <- R6::R6Class(
  "TravelInfo",
  public = list(
    bed_net_usage = NA_real_,
    geo_admin1 = NA_character_,
    geo_admin2 = NA_character_,
    geo_admin3 = NA_character_,
    lat_lon = NA_character_,
    travel_country = NA_character_,
    travel_end_date = NA_character_,
    travel_start_date = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param bed_net_usage Approximate usage of bed net while traveling, 1 = 100% nights with bed net, 0 = 0% no bed net usage.
    #' @param geo_admin1 Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
    #' @param geo_admin2 Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
    #' @param geo_admin3 Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
    #' @param lat_lon The latitude and longitude of a specific site.
    #' @param travel_country The name of country, would be the same as admin level 0.
    #' @param travel_end_date The date of the end of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
    #' @param travel_start_date The date of the start of travel, can be approximate, should be YYYY-MM or YYYY-MM-DD (preferred).
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(bed_net_usage = NULL, geo_admin1 = NULL, geo_admin2 = NULL, geo_admin3 = NULL, lat_lon = NULL, travel_country = NA_character_, travel_end_date = NA_character_, travel_start_date = NA_character_, extras = list()) {
      self$bed_net_usage <- bed_net_usage
      self$geo_admin1 <- geo_admin1
      self$geo_admin2 <- geo_admin2
      self$geo_admin3 <- geo_admin3
      self$lat_lon <- lat_lon
      self$travel_country <- travel_country
      self$travel_end_date <- travel_end_date
      self$travel_start_date <- travel_start_date
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$bed_net_usage) && !is.na(self$bed_net_usage) && (!is.numeric(self$bed_net_usage) || length(self$bed_net_usage) != 1)) stop("TravelInfo.bed_net_usage must be a single numeric value")
      if (!is.null(self$bed_net_usage) && !is.na(self$bed_net_usage) && self$bed_net_usage < 0) stop("TravelInfo.bed_net_usage < minimum 0")
      if (!is.null(self$geo_admin1) && !is.na(self$geo_admin1) && (!is.character(self$geo_admin1) || length(self$geo_admin1) != 1)) stop("TravelInfo.geo_admin1 must be a single string")
      if (!is.null(self$geo_admin2) && !is.na(self$geo_admin2) && (!is.character(self$geo_admin2) || length(self$geo_admin2) != 1)) stop("TravelInfo.geo_admin2 must be a single string")
      if (!is.null(self$geo_admin3) && !is.na(self$geo_admin3) && (!is.character(self$geo_admin3) || length(self$geo_admin3) != 1)) stop("TravelInfo.geo_admin3 must be a single string")
      if (!is.null(self$lat_lon) && !is.na(self$lat_lon) && (!is.character(self$lat_lon) || length(self$lat_lon) != 1)) stop("TravelInfo.lat_lon must be a single string")
      if (!is.null(self$lat_lon) && !is.na(self$lat_lon) && !grepl("^[-+]?\\d{1,2}(?:\\.\\d+)?,[-+]?\\d{1,3}(?:\\.\\d+)?$", self$lat_lon, perl = TRUE)) stop("TravelInfo.lat_lon does not match pattern: ^[-+]?\\d{1,2}(?:\\.\\d+)?,[-+]?\\d{1,3}(?:\\.\\d+)?$")
      if (!is.null(self$travel_country) && !is.na(self$travel_country) && (!is.character(self$travel_country) || length(self$travel_country) != 1)) stop("TravelInfo.travel_country must be a single string")
      if (!is.null(self$travel_country) && !is.na(self$travel_country) && !grepl("^[\\w ,._:'–-]+$", self$travel_country, perl = TRUE)) stop("TravelInfo.travel_country does not match pattern: ^[\\w ,._:'–-]+$")
      if (!is.null(self$travel_end_date) && !is.na(self$travel_end_date) && (!is.character(self$travel_end_date) || length(self$travel_end_date) != 1)) stop("TravelInfo.travel_end_date must be a single string")
      if (!is.null(self$travel_end_date) && !is.na(self$travel_end_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$travel_end_date, perl = TRUE)) stop("TravelInfo.travel_end_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      if (!is.null(self$travel_start_date) && !is.na(self$travel_start_date) && (!is.character(self$travel_start_date) || length(self$travel_start_date) != 1)) stop("TravelInfo.travel_start_date must be a single string")
      if (!is.null(self$travel_start_date) && !is.na(self$travel_start_date) && !grepl("\\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?", self$travel_start_date, perl = TRUE)) stop("TravelInfo.travel_start_date does not match pattern: \\d{4}-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$bed_net_usage)) out$bed_net_usage <- self$bed_net_usage
      if (!is.null(self$geo_admin1)) out$geo_admin1 <- if (is.na(self$geo_admin1)) "NA" else self$geo_admin1
      if (!is.null(self$geo_admin2)) out$geo_admin2 <- if (is.na(self$geo_admin2)) "NA" else self$geo_admin2
      if (!is.null(self$geo_admin3)) out$geo_admin3 <- if (is.na(self$geo_admin3)) "NA" else self$geo_admin3
      if (!is.null(self$lat_lon)) out$lat_lon <- if (is.na(self$lat_lon)) "NA" else self$lat_lon
      if (!is.null(self$travel_country)) out$travel_country <- if (is.na(self$travel_country)) "NA" else self$travel_country
      if (!is.null(self$travel_end_date)) out$travel_end_date <- if (is.na(self$travel_end_date)) "NA" else self$travel_end_date
      if (!is.null(self$travel_start_date)) out$travel_start_date <- if (is.na(self$travel_start_date)) "NA" else self$travel_start_date
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$bed_net_usage)) out$bed_net_usage <- self$bed_net_usage
      if (!is.null(self$geo_admin1)) out$geo_admin1 <- if (is.na(self$geo_admin1)) "NA" else self$geo_admin1
      if (!is.null(self$geo_admin2)) out$geo_admin2 <- if (is.na(self$geo_admin2)) "NA" else self$geo_admin2
      if (!is.null(self$geo_admin3)) out$geo_admin3 <- if (is.na(self$geo_admin3)) "NA" else self$geo_admin3
      if (!is.null(self$lat_lon)) out$lat_lon <- if (is.na(self$lat_lon)) "NA" else self$lat_lon
      if (!is.null(self$travel_country)) out$travel_country <- if (is.na(self$travel_country)) "NA" else self$travel_country
      if (!is.null(self$travel_end_date)) out$travel_end_date <- if (is.na(self$travel_end_date)) "NA" else self$travel_end_date
      if (!is.null(self$travel_start_date)) out$travel_start_date <- if (is.na(self$travel_start_date)) "NA" else self$travel_start_date
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

TravelInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("travel_country","travel_end_date","travel_start_date")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("TravelInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("bed_net_usage","geo_admin1","geo_admin2","geo_admin3","lat_lon","travel_country","travel_end_date","travel_start_date")
  extras <- obj[setdiff(names(obj), known)]
  inst <- TravelInfo$new(bed_net_usage = if (!is.null(obj[["bed_net_usage"]])) obj[["bed_net_usage"]] else NULL, geo_admin1 = { v <- obj[["geo_admin1"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, geo_admin2 = { v <- obj[["geo_admin2"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, geo_admin3 = { v <- obj[["geo_admin3"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, lat_lon = { v <- obj[["lat_lon"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, travel_country = { v <- obj[["travel_country"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, travel_end_date = { v <- obj[["travel_end_date"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, travel_start_date = { v <- obj[["travel_start_date"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' SpecimenInfo
#'
#' Information on specimen info.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field alternate_identifiers A list of alternative names.
#' @field blood_meal Whether host specimen has had a recent blood meal.
#' @field collection_country The name of country collected in, would be the same as admin level 0.
#' @field collection_date The date of the specimen collection, can be YYYY, YYYY-MM, or YYYY-MM-DD.
#' @field drug_usage Any drug used by subject and the frequency of usage; can include multiple drugs used.
#' @field env_broad_scale The broad environment from which the specimen was collected, e.g. highlands, lowlands, mountainous region.
#' @field env_local_scale The local environment from which the specimen was collected, e.g. jungle, urban, rural.
#' @field env_medium The environment medium from which the specimen was collected from.
#' @field geo_admin1 Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
#' @field geo_admin2 Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
#' @field geo_admin3 Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
#' @field gravid Whether host specimen is currently pregnant.
#' @field gravidity The gravidity of the specimen host (number of previous pregnancies).
#' @field has_travel_out_six_month Has travelled out from local region in the last six months.
#' @field host_age If specimen is from a person, the age in years of the person, can be float value so for 3 month old put 0.25.
#' @field host_sex If specimen is collected from a host with a sex, the sex listed for that host.
#' @field host_subject_name An identifier for the individual/person/patient a specimen was collected from.
#' @field host_taxon_id The NCBI taxonomy number of the host that the specimen was collected from.
#' @field lat_lon The latitude and longitude of a specific site.
#' @field parasite_density_info One or more parasite densities in microliters for this specimen.
#' @field project_id The index into the project_info list.
#' @field specimen_accession If specimen is deposited in a database, what accession is it associated with.
#' @field specimen_collect_device The way the specimen was collected, e.g. whole blood, dried blood spot.
#' @field specimen_comments Any additional comments about the specimen.
#' @field specimen_name An identifier for the specimen, should be unique within this sample set.
#' @field specimen_store_loc The specimen store site, address or facility name.
#' @field specimen_taxon_id The NCBI taxonomy number of the organism(s) in the specimen, can list multiple if a mixed sample.
#' @field specimen_type What type of specimen this is, e.g. negative_control, positive_control, field_sample.
#' @field storage_plate_info Plate location of where specimen is stored if stored in a plate.
#' @field travel_out_six_month Specification of the countries travelled in the last six months; can include multiple travels.
#' @field treatment_status If person has been treated with drugs, what was the treatment outcome.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `alternate_identifiers`: A list of alternative names.
#' * `blood_meal`: Whether host specimen has had a recent blood meal.
#' * `collection_country`: The name of country collected in, would be the same as admin level 0.
#' * `collection_date`: The date of the specimen collection, can be YYYY, YYYY-MM, or YYYY-MM-DD.
#' * `drug_usage`: Any drug used by subject and the frequency of usage; can include multiple drugs used.
#' * `env_broad_scale`: The broad environment from which the specimen was collected, e.g. highlands, lowlands, mountainous region.
#' * `env_local_scale`: The local environment from which the specimen was collected, e.g. jungle, urban, rural.
#' * `env_medium`: The environment medium from which the specimen was collected from.
#' * `geo_admin1`: Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
#' * `geo_admin2`: Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
#' * `geo_admin3`: Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
#' * `gravid`: Whether host specimen is currently pregnant.
#' * `gravidity`: The gravidity of the specimen host (number of previous pregnancies).
#' * `has_travel_out_six_month`: Has travelled out from local region in the last six months.
#' * `host_age`: If specimen is from a person, the age in years of the person, can be float value so for 3 month old put 0.25.
#' * `host_sex`: If specimen is collected from a host with a sex, the sex listed for that host.
#' * `host_subject_name`: An identifier for the individual/person/patient a specimen was collected from.
#' * `host_taxon_id`: The NCBI taxonomy number of the host that the specimen was collected from.
#' * `lat_lon`: The latitude and longitude of a specific site.
#' * `parasite_density_info`: One or more parasite densities in microliters for this specimen.
#' * `project_id`: The index into the project_info list.
#' * `specimen_accession`: If specimen is deposited in a database, what accession is it associated with.
#' * `specimen_collect_device`: The way the specimen was collected, e.g. whole blood, dried blood spot.
#' * `specimen_comments`: Any additional comments about the specimen.
#' * `specimen_name`: An identifier for the specimen, should be unique within this sample set.
#' * `specimen_store_loc`: The specimen store site, address or facility name.
#' * `specimen_taxon_id`: The NCBI taxonomy number of the organism(s) in the specimen, can list multiple if a mixed sample.
#' * `specimen_type`: What type of specimen this is, e.g. negative_control, positive_control, field_sample.
#' * `storage_plate_info`: Plate location of where specimen is stored if stored in a plate.
#' * `travel_out_six_month`: Specification of the countries travelled in the last six months; can include multiple travels.
#' * `treatment_status`: If person has been treated with drugs, what was the treatment outcome.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
SpecimenInfo <- R6::R6Class(
  "SpecimenInfo",
  public = list(
    alternate_identifiers = character(),
    blood_meal = NA,
    collection_country = NA_character_,
    collection_date = NA_character_,
    drug_usage = character(),
    env_broad_scale = NA_character_,
    env_local_scale = NA_character_,
    env_medium = NA_character_,
    geo_admin1 = NA_character_,
    geo_admin2 = NA_character_,
    geo_admin3 = NA_character_,
    gravid = NA,
    gravidity = NA_real_,
    has_travel_out_six_month = NA,
    host_age = NA_real_,
    host_sex = NA_character_,
    host_subject_name = NA_character_,
    host_taxon_id = NA_real_,
    lat_lon = NA_character_,
    parasite_density_info = list(),
    project_id = NA_real_,
    specimen_accession = NA_character_,
    specimen_collect_device = NA_character_,
    specimen_comments = character(),
    specimen_name = NA_character_,
    specimen_store_loc = NA_character_,
    specimen_taxon_id = numeric(),
    specimen_type = NA_character_,
    storage_plate_info = NULL,
    travel_out_six_month = list(),
    treatment_status = character(),
    extras = list(),

    #' @description Create a new instance.
    #' @param alternate_identifiers A list of alternative names.
    #' @param blood_meal Whether host specimen has had a recent blood meal.
    #' @param collection_country The name of country collected in, would be the same as admin level 0.
    #' @param collection_date The date of the specimen collection, can be YYYY, YYYY-MM, or YYYY-MM-DD.
    #' @param drug_usage Any drug used by subject and the frequency of usage; can include multiple drugs used.
    #' @param env_broad_scale The broad environment from which the specimen was collected, e.g. highlands, lowlands, mountainous region.
    #' @param env_local_scale The local environment from which the specimen was collected, e.g. jungle, urban, rural.
    #' @param env_medium The environment medium from which the specimen was collected from.
    #' @param geo_admin1 Geographical admin level 1, the secondary large demarcation of a nation (nation = admin level 0).
    #' @param geo_admin2 Geographical admin level 2, the third large demarcation of a nation (nation = admin level 0).
    #' @param geo_admin3 Geographical admin level 3, the third large demarcation of a nation (nation = admin level 0).
    #' @param gravid Whether host specimen is currently pregnant.
    #' @param gravidity The gravidity of the specimen host (number of previous pregnancies).
    #' @param has_travel_out_six_month Has travelled out from local region in the last six months.
    #' @param host_age If specimen is from a person, the age in years of the person, can be float value so for 3 month old put 0.25.
    #' @param host_sex If specimen is collected from a host with a sex, the sex listed for that host.
    #' @param host_subject_name An identifier for the individual/person/patient a specimen was collected from.
    #' @param host_taxon_id The NCBI taxonomy number of the host that the specimen was collected from.
    #' @param lat_lon The latitude and longitude of a specific site.
    #' @param parasite_density_info One or more parasite densities in microliters for this specimen.
    #' @param project_id The index into the project_info list.
    #' @param specimen_accession If specimen is deposited in a database, what accession is it associated with.
    #' @param specimen_collect_device The way the specimen was collected, e.g. whole blood, dried blood spot.
    #' @param specimen_comments Any additional comments about the specimen.
    #' @param specimen_name An identifier for the specimen, should be unique within this sample set.
    #' @param specimen_store_loc The specimen store site, address or facility name.
    #' @param specimen_taxon_id The NCBI taxonomy number of the organism(s) in the specimen, can list multiple if a mixed sample.
    #' @param specimen_type What type of specimen this is, e.g. negative_control, positive_control, field_sample.
    #' @param storage_plate_info Plate location of where specimen is stored if stored in a plate.
    #' @param travel_out_six_month Specification of the countries travelled in the last six months; can include multiple travels.
    #' @param treatment_status If person has been treated with drugs, what was the treatment outcome.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(alternate_identifiers = NULL, blood_meal = NULL, collection_country = NULL, collection_date = NULL, drug_usage = NULL, env_broad_scale = NULL, env_local_scale = NULL, env_medium = NULL, geo_admin1 = NULL, geo_admin2 = NULL, geo_admin3 = NULL, gravid = NULL, gravidity = NULL, has_travel_out_six_month = NULL, host_age = NULL, host_sex = NULL, host_subject_name = NULL, host_taxon_id = NULL, lat_lon = NULL, parasite_density_info = NULL, project_id = NULL, specimen_accession = NULL, specimen_collect_device = NULL, specimen_comments = NULL, specimen_name = NA_character_, specimen_store_loc = NULL, specimen_taxon_id = NULL, specimen_type = NULL, storage_plate_info = NULL, travel_out_six_month = NULL, treatment_status = NULL, extras = list()) {
      self$alternate_identifiers <- alternate_identifiers
      self$blood_meal <- blood_meal
      self$collection_country <- collection_country
      self$collection_date <- collection_date
      self$drug_usage <- drug_usage
      self$env_broad_scale <- env_broad_scale
      self$env_local_scale <- env_local_scale
      self$env_medium <- env_medium
      self$geo_admin1 <- geo_admin1
      self$geo_admin2 <- geo_admin2
      self$geo_admin3 <- geo_admin3
      self$gravid <- gravid
      self$gravidity <- gravidity
      self$has_travel_out_six_month <- has_travel_out_six_month
      self$host_age <- host_age
      self$host_sex <- host_sex
      self$host_subject_name <- host_subject_name
      self$host_taxon_id <- host_taxon_id
      self$lat_lon <- lat_lon
      self$parasite_density_info <- parasite_density_info
      self$project_id <- project_id
      self$specimen_accession <- specimen_accession
      self$specimen_collect_device <- specimen_collect_device
      self$specimen_comments <- specimen_comments
      self$specimen_name <- specimen_name
      self$specimen_store_loc <- specimen_store_loc
      self$specimen_taxon_id <- specimen_taxon_id
      self$specimen_type <- specimen_type
      self$storage_plate_info <- storage_plate_info
      self$travel_out_six_month <- travel_out_six_month
      self$treatment_status <- treatment_status
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$alternate_identifiers) && !is.character(self$alternate_identifiers)) stop("SpecimenInfo.alternate_identifiers must be a character vector")
      if (!is.null(self$alternate_identifiers) && length(self$alternate_identifiers) > 0 && any(!grepl("^[A-z-._0-9 ]+$", self$alternate_identifiers, perl = TRUE))) stop("SpecimenInfo.alternate_identifiers contains values that do not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$blood_meal) && !is.na(self$blood_meal) && (!is.logical(self$blood_meal) || length(self$blood_meal) != 1)) stop("SpecimenInfo.blood_meal must be a single logical value")
      if (!is.null(self$collection_country) && !is.na(self$collection_country) && (!is.character(self$collection_country) || length(self$collection_country) != 1)) stop("SpecimenInfo.collection_country must be a single string")
      if (!is.null(self$collection_country) && !is.na(self$collection_country) && !grepl("^[\\w ,._:'–-]+$", self$collection_country, perl = TRUE)) stop("SpecimenInfo.collection_country does not match pattern: ^[\\w ,._:'–-]+$")
      if (!is.null(self$collection_date) && !is.na(self$collection_date) && (!is.character(self$collection_date) || length(self$collection_date) != 1)) stop("SpecimenInfo.collection_date must be a single string")
      if (!is.null(self$collection_date) && !is.na(self$collection_date) && !grepl("(?:\\d{4}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?)?|NA)", self$collection_date, perl = TRUE)) stop("SpecimenInfo.collection_date does not match pattern: (?:\\d{4}(?:-(?:0[1-9]|1[0-2])(?:-(?:0[1-9]|[12][0-9]|3[01]))?)?|NA)")
      if (!is.null(self$drug_usage) && !is.character(self$drug_usage)) stop("SpecimenInfo.drug_usage must be a character vector")
      if (!is.null(self$drug_usage) && length(self$drug_usage) > 0 && any(!grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$drug_usage, perl = TRUE))) stop("SpecimenInfo.drug_usage contains values that do not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$env_broad_scale) && !is.na(self$env_broad_scale) && (!is.character(self$env_broad_scale) || length(self$env_broad_scale) != 1)) stop("SpecimenInfo.env_broad_scale must be a single string")
      if (!is.null(self$env_broad_scale) && !is.na(self$env_broad_scale) && !grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$env_broad_scale, perl = TRUE)) stop("SpecimenInfo.env_broad_scale does not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$env_local_scale) && !is.na(self$env_local_scale) && (!is.character(self$env_local_scale) || length(self$env_local_scale) != 1)) stop("SpecimenInfo.env_local_scale must be a single string")
      if (!is.null(self$env_local_scale) && !is.na(self$env_local_scale) && !grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$env_local_scale, perl = TRUE)) stop("SpecimenInfo.env_local_scale does not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$env_medium) && !is.na(self$env_medium) && (!is.character(self$env_medium) || length(self$env_medium) != 1)) stop("SpecimenInfo.env_medium must be a single string")
      if (!is.null(self$env_medium) && !is.na(self$env_medium) && !grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$env_medium, perl = TRUE)) stop("SpecimenInfo.env_medium does not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$geo_admin1) && !is.na(self$geo_admin1) && (!is.character(self$geo_admin1) || length(self$geo_admin1) != 1)) stop("SpecimenInfo.geo_admin1 must be a single string")
      if (!is.null(self$geo_admin2) && !is.na(self$geo_admin2) && (!is.character(self$geo_admin2) || length(self$geo_admin2) != 1)) stop("SpecimenInfo.geo_admin2 must be a single string")
      if (!is.null(self$geo_admin3) && !is.na(self$geo_admin3) && (!is.character(self$geo_admin3) || length(self$geo_admin3) != 1)) stop("SpecimenInfo.geo_admin3 must be a single string")
      if (!is.null(self$gravid) && !is.na(self$gravid) && (!is.logical(self$gravid) || length(self$gravid) != 1)) stop("SpecimenInfo.gravid must be a single logical value")
      if (!is.null(self$gravidity) && !is.na(self$gravidity) && (!is.numeric(self$gravidity) || length(self$gravidity) != 1)) stop("SpecimenInfo.gravidity must be a single numeric value")
      if (!is.null(self$gravidity) && !is.na(self$gravidity) && self$gravidity < 0) stop("SpecimenInfo.gravidity < minimum 0")
      if (!is.null(self$gravidity) && !is.na(self$gravidity) && !(is.numeric(self$gravidity) && isTRUE(all.equal(self$gravidity, as.integer(self$gravidity))))) stop("SpecimenInfo.gravidity must be integer-like")
      if (!is.null(self$has_travel_out_six_month) && !is.na(self$has_travel_out_six_month) && (!is.logical(self$has_travel_out_six_month) || length(self$has_travel_out_six_month) != 1)) stop("SpecimenInfo.has_travel_out_six_month must be a single logical value")
      if (!is.null(self$host_age) && !is.na(self$host_age) && (!is.numeric(self$host_age) || length(self$host_age) != 1)) stop("SpecimenInfo.host_age must be a single numeric value")
      if (!is.null(self$host_age) && !is.na(self$host_age) && self$host_age < 0) stop("SpecimenInfo.host_age < minimum 0")
      if (!is.null(self$host_sex) && !is.na(self$host_sex) && (!is.character(self$host_sex) || length(self$host_sex) != 1)) stop("SpecimenInfo.host_sex must be a single string")
      if (!is.null(self$host_sex) && !is.na(self$host_sex) && !grepl("^[A-z-._0-9 ]+$", self$host_sex, perl = TRUE)) stop("SpecimenInfo.host_sex does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$host_subject_name) && !is.na(self$host_subject_name) && (!is.character(self$host_subject_name) || length(self$host_subject_name) != 1)) stop("SpecimenInfo.host_subject_name must be a single string")
      if (!is.null(self$host_taxon_id) && !is.na(self$host_taxon_id) && (!is.numeric(self$host_taxon_id) || length(self$host_taxon_id) != 1)) stop("SpecimenInfo.host_taxon_id must be a single numeric value")
      if (!is.null(self$host_taxon_id) && !is.na(self$host_taxon_id) && self$host_taxon_id < 0) stop("SpecimenInfo.host_taxon_id < minimum 0")
      if (!is.null(self$host_taxon_id) && !is.na(self$host_taxon_id) && !(is.numeric(self$host_taxon_id) && isTRUE(all.equal(self$host_taxon_id, as.integer(self$host_taxon_id))))) stop("SpecimenInfo.host_taxon_id must be integer-like")
      if (!is.null(self$lat_lon) && !is.na(self$lat_lon) && (!is.character(self$lat_lon) || length(self$lat_lon) != 1)) stop("SpecimenInfo.lat_lon must be a single string")
      if (!is.null(self$lat_lon) && !is.na(self$lat_lon) && !grepl("^[-+]?\\d{1,2}(?:\\.\\d+)?,[-+]?\\d{1,3}(?:\\.\\d+)?$", self$lat_lon, perl = TRUE)) stop("SpecimenInfo.lat_lon does not match pattern: ^[-+]?\\d{1,2}(?:\\.\\d+)?,[-+]?\\d{1,3}(?:\\.\\d+)?$")
      if (!is.null(self$parasite_density_info) && !is.list(self$parasite_density_info)) stop("SpecimenInfo.parasite_density_info must be a list")
      if (!is.null(self$project_id) && !is.na(self$project_id) && (!is.numeric(self$project_id) || length(self$project_id) != 1)) stop("SpecimenInfo.project_id must be a single numeric value")
      if (!is.null(self$project_id) && !is.na(self$project_id) && self$project_id < 0) stop("SpecimenInfo.project_id < minimum 0")
      if (!is.null(self$project_id) && !is.na(self$project_id) && !(is.numeric(self$project_id) && isTRUE(all.equal(self$project_id, as.integer(self$project_id))))) stop("SpecimenInfo.project_id must be integer-like")
      if (!is.null(self$specimen_accession) && !is.na(self$specimen_accession) && (!is.character(self$specimen_accession) || length(self$specimen_accession) != 1)) stop("SpecimenInfo.specimen_accession must be a single string")
      if (!is.null(self$specimen_accession) && !is.na(self$specimen_accession) && !grepl("^[A-z-._0-9 ]+$", self$specimen_accession, perl = TRUE)) stop("SpecimenInfo.specimen_accession does not match pattern: ^[A-z-._0-9 ]+$")
      if (!is.null(self$specimen_collect_device) && !is.na(self$specimen_collect_device) && (!is.character(self$specimen_collect_device) || length(self$specimen_collect_device) != 1)) stop("SpecimenInfo.specimen_collect_device must be a single string")
      if (!is.null(self$specimen_collect_device) && !is.na(self$specimen_collect_device) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$specimen_collect_device, perl = TRUE)) stop("SpecimenInfo.specimen_collect_device does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$specimen_comments) && !is.character(self$specimen_comments)) stop("SpecimenInfo.specimen_comments must be a character vector")
      if (!is.null(self$specimen_comments) && length(self$specimen_comments) > 0 && any(!grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$specimen_comments, perl = TRUE))) stop("SpecimenInfo.specimen_comments contains values that do not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$specimen_name) && !is.na(self$specimen_name) && (!is.character(self$specimen_name) || length(self$specimen_name) != 1)) stop("SpecimenInfo.specimen_name must be a single string")
      if (!is.null(self$specimen_name) && !is.na(self$specimen_name) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$specimen_name, perl = TRUE)) stop("SpecimenInfo.specimen_name does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$specimen_store_loc) && !is.na(self$specimen_store_loc) && (!is.character(self$specimen_store_loc) || length(self$specimen_store_loc) != 1)) stop("SpecimenInfo.specimen_store_loc must be a single string")
      if (!is.null(self$specimen_store_loc) && !is.na(self$specimen_store_loc) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$specimen_store_loc, perl = TRUE)) stop("SpecimenInfo.specimen_store_loc does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$specimen_taxon_id) && !is.numeric(self$specimen_taxon_id)) stop("SpecimenInfo.specimen_taxon_id must be a numeric vector")
      if (!is.null(self$specimen_taxon_id) && length(self$specimen_taxon_id) > 0 && any(self$specimen_taxon_id < 0, na.rm = TRUE)) stop("SpecimenInfo.specimen_taxon_id contains values < minimum 0")
      if (!is.null(self$specimen_taxon_id) && length(self$specimen_taxon_id) > 0 && any(!vapply(self$specimen_taxon_id, function(.x) is.na(.x) || (is.numeric(.x) && isTRUE(all.equal(.x, as.integer(.x)))), logical(1)))) stop("SpecimenInfo.specimen_taxon_id must contain integer-like values")
      if (!is.null(self$specimen_type) && !is.na(self$specimen_type) && (!is.character(self$specimen_type) || length(self$specimen_type) != 1)) stop("SpecimenInfo.specimen_type must be a single string")
      if (!is.null(self$specimen_type) && !is.na(self$specimen_type) && !grepl("^[A-z-._0-9\\(\\),\\/\\ ]+$", self$specimen_type, perl = TRUE)) stop("SpecimenInfo.specimen_type does not match pattern: ^[A-z-._0-9\\(\\),\\/\\ ]+$")
      if (!is.null(self$travel_out_six_month) && !is.list(self$travel_out_six_month)) stop("SpecimenInfo.travel_out_six_month must be a list")
      if (!is.null(self$treatment_status) && !is.character(self$treatment_status)) stop("SpecimenInfo.treatment_status must be a character vector")
      if (!is.null(self$treatment_status) && length(self$treatment_status) > 0 && any(!grepl("^[A-z-._0-9;|\\(\\),\\/\\ ]+$", self$treatment_status, perl = TRUE))) stop("SpecimenInfo.treatment_status contains values that do not match pattern: ^[A-z-._0-9;|\\(\\),\\/\\ ]+$")
      if (!is.null(self$parasite_density_info)) for (.x in self$parasite_density_info) .x$validate()
      if (!is.null(self$travel_out_six_month)) for (.x in self$travel_out_six_month) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$alternate_identifiers)) out$alternate_identifiers <- self$alternate_identifiers
      if (!is.null(self$blood_meal)) out$blood_meal <- self$blood_meal
      if (!is.null(self$collection_country)) out$collection_country <- if (is.na(self$collection_country)) "NA" else self$collection_country
      if (!is.null(self$collection_date)) out$collection_date <- if (is.na(self$collection_date)) "NA" else self$collection_date
      if (!is.null(self$drug_usage)) out$drug_usage <- self$drug_usage
      if (!is.null(self$env_broad_scale)) out$env_broad_scale <- if (is.na(self$env_broad_scale)) "NA" else self$env_broad_scale
      if (!is.null(self$env_local_scale)) out$env_local_scale <- if (is.na(self$env_local_scale)) "NA" else self$env_local_scale
      if (!is.null(self$env_medium)) out$env_medium <- if (is.na(self$env_medium)) "NA" else self$env_medium
      if (!is.null(self$geo_admin1)) out$geo_admin1 <- if (is.na(self$geo_admin1)) "NA" else self$geo_admin1
      if (!is.null(self$geo_admin2)) out$geo_admin2 <- if (is.na(self$geo_admin2)) "NA" else self$geo_admin2
      if (!is.null(self$geo_admin3)) out$geo_admin3 <- if (is.na(self$geo_admin3)) "NA" else self$geo_admin3
      if (!is.null(self$gravid)) out$gravid <- self$gravid
      if (!is.null(self$gravidity)) out$gravidity <- self$gravidity
      if (!is.null(self$has_travel_out_six_month)) out$has_travel_out_six_month <- self$has_travel_out_six_month
      if (!is.null(self$host_age)) out$host_age <- self$host_age
      if (!is.null(self$host_sex)) out$host_sex <- if (is.na(self$host_sex)) "NA" else self$host_sex
      if (!is.null(self$host_subject_name)) out$host_subject_name <- if (is.na(self$host_subject_name)) "NA" else self$host_subject_name
      if (!is.null(self$host_taxon_id)) out$host_taxon_id <- self$host_taxon_id
      if (!is.null(self$lat_lon)) out$lat_lon <- if (is.na(self$lat_lon)) "NA" else self$lat_lon
      if (!is.null(self$parasite_density_info)) out$parasite_density_info <- lapply(self$parasite_density_info, function(x) x$to_list())
      if (!is.null(self$project_id)) out$project_id <- self$project_id
      if (!is.null(self$specimen_accession)) out$specimen_accession <- if (is.na(self$specimen_accession)) "NA" else self$specimen_accession
      if (!is.null(self$specimen_collect_device)) out$specimen_collect_device <- if (is.na(self$specimen_collect_device)) "NA" else self$specimen_collect_device
      if (!is.null(self$specimen_comments)) out$specimen_comments <- self$specimen_comments
      if (!is.null(self$specimen_name)) out$specimen_name <- if (is.na(self$specimen_name)) "NA" else self$specimen_name
      if (!is.null(self$specimen_store_loc)) out$specimen_store_loc <- if (is.na(self$specimen_store_loc)) "NA" else self$specimen_store_loc
      if (!is.null(self$specimen_taxon_id)) out$specimen_taxon_id <- self$specimen_taxon_id
      if (!is.null(self$specimen_type)) out$specimen_type <- if (is.na(self$specimen_type)) "NA" else self$specimen_type
      if (!is.null(self$storage_plate_info)) out$storage_plate_info <- self$storage_plate_info
      if (!is.null(self$travel_out_six_month)) out$travel_out_six_month <- lapply(self$travel_out_six_month, function(x) x$to_list())
      if (!is.null(self$treatment_status)) out$treatment_status <- self$treatment_status
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$alternate_identifiers)) out$alternate_identifiers <- I(self$alternate_identifiers)
      if (!is.null(self$blood_meal)) out$blood_meal <- self$blood_meal
      if (!is.null(self$collection_country)) out$collection_country <- if (is.na(self$collection_country)) "NA" else self$collection_country
      if (!is.null(self$collection_date)) out$collection_date <- if (is.na(self$collection_date)) "NA" else self$collection_date
      if (!is.null(self$drug_usage)) out$drug_usage <- I(self$drug_usage)
      if (!is.null(self$env_broad_scale)) out$env_broad_scale <- if (is.na(self$env_broad_scale)) "NA" else self$env_broad_scale
      if (!is.null(self$env_local_scale)) out$env_local_scale <- if (is.na(self$env_local_scale)) "NA" else self$env_local_scale
      if (!is.null(self$env_medium)) out$env_medium <- if (is.na(self$env_medium)) "NA" else self$env_medium
      if (!is.null(self$geo_admin1)) out$geo_admin1 <- if (is.na(self$geo_admin1)) "NA" else self$geo_admin1
      if (!is.null(self$geo_admin2)) out$geo_admin2 <- if (is.na(self$geo_admin2)) "NA" else self$geo_admin2
      if (!is.null(self$geo_admin3)) out$geo_admin3 <- if (is.na(self$geo_admin3)) "NA" else self$geo_admin3
      if (!is.null(self$gravid)) out$gravid <- self$gravid
      if (!is.null(self$gravidity)) out$gravidity <- self$gravidity
      if (!is.null(self$has_travel_out_six_month)) out$has_travel_out_six_month <- self$has_travel_out_six_month
      if (!is.null(self$host_age)) out$host_age <- self$host_age
      if (!is.null(self$host_sex)) out$host_sex <- if (is.na(self$host_sex)) "NA" else self$host_sex
      if (!is.null(self$host_subject_name)) out$host_subject_name <- if (is.na(self$host_subject_name)) "NA" else self$host_subject_name
      if (!is.null(self$host_taxon_id)) out$host_taxon_id <- self$host_taxon_id
      if (!is.null(self$lat_lon)) out$lat_lon <- if (is.na(self$lat_lon)) "NA" else self$lat_lon
      if (!is.null(self$parasite_density_info)) out$parasite_density_info <- I(lapply(self$parasite_density_info, function(x) x$to_json_list()))
      if (!is.null(self$project_id)) out$project_id <- pmo_apply_id_offset_write(self$project_id, "project_id")
      if (!is.null(self$specimen_accession)) out$specimen_accession <- if (is.na(self$specimen_accession)) "NA" else self$specimen_accession
      if (!is.null(self$specimen_collect_device)) out$specimen_collect_device <- if (is.na(self$specimen_collect_device)) "NA" else self$specimen_collect_device
      if (!is.null(self$specimen_comments)) out$specimen_comments <- I(self$specimen_comments)
      if (!is.null(self$specimen_name)) out$specimen_name <- if (is.na(self$specimen_name)) "NA" else self$specimen_name
      if (!is.null(self$specimen_store_loc)) out$specimen_store_loc <- if (is.na(self$specimen_store_loc)) "NA" else self$specimen_store_loc
      if (!is.null(self$specimen_taxon_id)) out$specimen_taxon_id <- I(self$specimen_taxon_id)
      if (!is.null(self$specimen_type)) out$specimen_type <- if (is.na(self$specimen_type)) "NA" else self$specimen_type
      if (!is.null(self$storage_plate_info)) out$storage_plate_info <- self$storage_plate_info
      if (!is.null(self$travel_out_six_month)) out$travel_out_six_month <- I(lapply(self$travel_out_six_month, function(x) x$to_json_list()))
      if (!is.null(self$treatment_status)) out$treatment_status <- I(self$treatment_status)
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

SpecimenInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("specimen_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("SpecimenInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("alternate_identifiers","blood_meal","collection_country","collection_date","drug_usage","env_broad_scale","env_local_scale","env_medium","geo_admin1","geo_admin2","geo_admin3","gravid","gravidity","has_travel_out_six_month","host_age","host_sex","host_subject_name","host_taxon_id","lat_lon","parasite_density_info","project_id","specimen_accession","specimen_collect_device","specimen_comments","specimen_name","specimen_store_loc","specimen_taxon_id","specimen_type","storage_plate_info","travel_out_six_month","treatment_status")
  extras <- obj[setdiff(names(obj), known)]
  inst <- SpecimenInfo$new(alternate_identifiers = { v <- obj[["alternate_identifiers"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, blood_meal = if (!is.null(obj[["blood_meal"]])) obj[["blood_meal"]] else NULL, collection_country = { v <- obj[["collection_country"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, collection_date = { v <- obj[["collection_date"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, drug_usage = { v <- obj[["drug_usage"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, env_broad_scale = { v <- obj[["env_broad_scale"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, env_local_scale = { v <- obj[["env_local_scale"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, env_medium = { v <- obj[["env_medium"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, geo_admin1 = { v <- obj[["geo_admin1"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, geo_admin2 = { v <- obj[["geo_admin2"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, geo_admin3 = { v <- obj[["geo_admin3"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, gravid = if (!is.null(obj[["gravid"]])) obj[["gravid"]] else NULL, gravidity = if (!is.null(obj[["gravidity"]])) obj[["gravidity"]] else NULL, has_travel_out_six_month = if (!is.null(obj[["has_travel_out_six_month"]])) obj[["has_travel_out_six_month"]] else NULL, host_age = if (!is.null(obj[["host_age"]])) obj[["host_age"]] else NULL, host_sex = { v <- obj[["host_sex"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, host_subject_name = { v <- obj[["host_subject_name"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, host_taxon_id = if (!is.null(obj[["host_taxon_id"]])) obj[["host_taxon_id"]] else NULL, lat_lon = { v <- obj[["lat_lon"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, parasite_density_info = if (!is.null(obj[["parasite_density_info"]])) lapply(obj[["parasite_density_info"]], function(.x) ParasiteDensity$from_json(.x, validate = FALSE)) else NULL, project_id = pmo_apply_id_offset_read(if (!is.null(obj[["project_id"]])) obj[["project_id"]] else NULL, "project_id"), specimen_accession = { v <- obj[["specimen_accession"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, specimen_collect_device = { v <- obj[["specimen_collect_device"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, specimen_comments = { v <- obj[["specimen_comments"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, specimen_name = { v <- obj[["specimen_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, specimen_store_loc = { v <- obj[["specimen_store_loc"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, specimen_taxon_id = { v <- obj[["specimen_taxon_id"]]; if (is.null(v)) NULL else if (length(v) == 0) numeric() else as.numeric(unlist(v, use.names = FALSE)) }, specimen_type = { v <- obj[["specimen_type"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, storage_plate_info = if (!is.null(obj[["storage_plate_info"]])) obj[["storage_plate_info"]] else NULL, travel_out_six_month = if (!is.null(obj[["travel_out_six_month"]])) lapply(obj[["travel_out_six_month"]], function(.x) TravelInfo$from_json(.x, validate = FALSE)) else NULL, treatment_status = { v <- obj[["treatment_status"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PrimerInfo
#'
#' Information on a primer sequence.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field location What the intended genomic location of the primer is.
#' @field seq The sequence.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `location`: What the intended genomic location of the primer is.
#' * `seq`: The sequence.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PrimerInfo <- R6::R6Class(
  "PrimerInfo",
  public = list(
    location = NULL,
    seq = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param location What the intended genomic location of the primer is.
    #' @param seq The sequence.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(location = NULL, seq = NA_character_, extras = list()) {
      self$location <- location
      self$seq <- seq
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$seq) && !is.na(self$seq) && (!is.character(self$seq) || length(self$seq) != 1)) stop("PrimerInfo.seq must be a single string")
      if (!is.null(self$seq) && !is.na(self$seq) && !grepl("^[A-z]+$", self$seq, perl = TRUE)) stop("PrimerInfo.seq does not match pattern: ^[A-z]+$")
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$location)) out$location <- self$location
      if (!is.null(self$seq)) out$seq <- if (is.na(self$seq)) "NA" else self$seq
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$location)) out$location <- self$location
      if (!is.null(self$seq)) out$seq <- if (is.na(self$seq)) "NA" else self$seq
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PrimerInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("seq")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PrimerInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("location","seq")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PrimerInfo$new(location = if (!is.null(obj[["location"]])) obj[["location"]] else NULL, seq = { v <- obj[["seq"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' TargetInfo
#'
#' Information about a specific targeted microhaplotype.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field forward_primer The forward primer for this target.
#' @field gene_name An identifier of the gene, if any, is being covered with this targeted.
#' @field insert_location The intended genomic location of the insert of the amplicon (the location between the end of the forward primer and the beginning of the reverse primer).
#' @field markers_of_interest A list of markers of interest that are covered by this target.
#' @field reverse_primer The reverse primer for this target.
#' @field target_attributes A list of classification types for this target.
#' @field target_name A name for this target.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `forward_primer`: The forward primer for this target.
#' * `gene_name`: An identifier of the gene, if any, is being covered with this targeted.
#' * `insert_location`: The intended genomic location of the insert of the amplicon (the location between the end of the forward primer and the beginning of the reverse primer).
#' * `markers_of_interest`: A list of markers of interest that are covered by this target.
#' * `reverse_primer`: The reverse primer for this target.
#' * `target_attributes`: A list of classification types for this target.
#' * `target_name`: A name for this target.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
TargetInfo <- R6::R6Class(
  "TargetInfo",
  public = list(
    forward_primer = NULL,
    gene_name = NA_character_,
    insert_location = NULL,
    markers_of_interest = list(),
    reverse_primer = NULL,
    target_attributes = character(),
    target_name = NA_character_,
    extras = list(),

    #' @description Create a new instance.
    #' @param forward_primer The forward primer for this target.
    #' @param gene_name An identifier of the gene, if any, is being covered with this targeted.
    #' @param insert_location The intended genomic location of the insert of the amplicon (the location between the end of the forward primer and the beginning of the reverse primer).
    #' @param markers_of_interest A list of markers of interest that are covered by this target.
    #' @param reverse_primer The reverse primer for this target.
    #' @param target_attributes A list of classification types for this target.
    #' @param target_name A name for this target.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(forward_primer = NULL, gene_name = NULL, insert_location = NULL, markers_of_interest = NULL, reverse_primer = NULL, target_attributes = NULL, target_name = NA_character_, extras = list()) {
      self$forward_primer <- forward_primer
      self$gene_name <- gene_name
      self$insert_location <- insert_location
      self$markers_of_interest <- markers_of_interest
      self$reverse_primer <- reverse_primer
      self$target_attributes <- target_attributes
      self$target_name <- target_name
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$gene_name) && !is.na(self$gene_name) && (!is.character(self$gene_name) || length(self$gene_name) != 1)) stop("TargetInfo.gene_name must be a single string")
      if (!is.null(self$gene_name) && !is.na(self$gene_name) && !grepl("^[A-z-._0-9:]+$", self$gene_name, perl = TRUE)) stop("TargetInfo.gene_name does not match pattern: ^[A-z-._0-9:]+$")
      if (!is.null(self$markers_of_interest) && !is.list(self$markers_of_interest)) stop("TargetInfo.markers_of_interest must be a list")
      if (!is.null(self$target_attributes) && !is.character(self$target_attributes)) stop("TargetInfo.target_attributes must be a character vector")
      if (!is.null(self$target_name) && !is.na(self$target_name) && (!is.character(self$target_name) || length(self$target_name) != 1)) stop("TargetInfo.target_name must be a single string")
      if (!is.null(self$target_name) && !is.na(self$target_name) && !grepl("^[A-z-._0-9]+$", self$target_name, perl = TRUE)) stop("TargetInfo.target_name does not match pattern: ^[A-z-._0-9]+$")
      if (!is.null(self$forward_primer)) self$forward_primer$validate()
      if (!is.null(self$markers_of_interest)) for (.x in self$markers_of_interest) .x$validate()
      if (!is.null(self$reverse_primer)) self$reverse_primer$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$forward_primer)) out$forward_primer <- self$forward_primer$to_list()
      if (!is.null(self$gene_name)) out$gene_name <- if (is.na(self$gene_name)) "NA" else self$gene_name
      if (!is.null(self$insert_location)) out$insert_location <- self$insert_location
      if (!is.null(self$markers_of_interest)) out$markers_of_interest <- lapply(self$markers_of_interest, function(x) x$to_list())
      if (!is.null(self$reverse_primer)) out$reverse_primer <- self$reverse_primer$to_list()
      if (!is.null(self$target_attributes)) out$target_attributes <- self$target_attributes
      if (!is.null(self$target_name)) out$target_name <- if (is.na(self$target_name)) "NA" else self$target_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$forward_primer)) out$forward_primer <- self$forward_primer$to_json_list()
      if (!is.null(self$gene_name)) out$gene_name <- if (is.na(self$gene_name)) "NA" else self$gene_name
      if (!is.null(self$insert_location)) out$insert_location <- self$insert_location
      if (!is.null(self$markers_of_interest)) out$markers_of_interest <- I(lapply(self$markers_of_interest, function(x) x$to_json_list()))
      if (!is.null(self$reverse_primer)) out$reverse_primer <- self$reverse_primer$to_json_list()
      if (!is.null(self$target_attributes)) out$target_attributes <- I(self$target_attributes)
      if (!is.null(self$target_name)) out$target_name <- if (is.na(self$target_name)) "NA" else self$target_name
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

TargetInfo$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("forward_primer","reverse_primer","target_name")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("TargetInfo missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("forward_primer","gene_name","insert_location","markers_of_interest","reverse_primer","target_attributes","target_name")
  extras <- obj[setdiff(names(obj), known)]
  inst <- TargetInfo$new(forward_primer = if (!is.null(obj[["forward_primer"]])) PrimerInfo$from_json(obj[["forward_primer"]], validate = FALSE) else NULL, gene_name = { v <- obj[["gene_name"]]; if (is.null(v)) NULL else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, insert_location = if (!is.null(obj[["insert_location"]])) obj[["insert_location"]] else NULL, markers_of_interest = if (!is.null(obj[["markers_of_interest"]])) lapply(obj[["markers_of_interest"]], function(.x) MarkerOfInterest$from_json(.x, validate = FALSE)) else NULL, reverse_primer = if (!is.null(obj[["reverse_primer"]])) PrimerInfo$from_json(obj[["reverse_primer"]], validate = FALSE) else NULL, target_attributes = { v <- obj[["target_attributes"]]; if (is.null(v)) NULL else if (length(v) == 0) character() else as.character(unlist(v, use.names = FALSE)) }, target_name = { v <- obj[["target_name"]]; if (is.null(v)) NA_character_ else if (is.character(v) && length(v)==1 && v %in% PMO_NA_STRINGS) NA_character_ else v }, extras = extras)
  if (validate) inst$validate()
  inst
}

#' PortableMicrohaplotypeObject
#'
#' Information on final microhaplotype results from a targeted amplicon analysis with associated meta data.
#'
#' Auto-generated R6 class from JSON Schema.
#'
#' @field bioinformatics_methods_info The bioinformatics pipeline/methods used to generated the microhaplotype analysis for this project.
#' @field bioinformatics_run_info The runtime info for the bioinformatics pipeline used to generated the microhaplotypes analysis for this project.
#' @field detected_microhaplotypes The microhaplotypes detected in this projects.
#' @field library_sample_info A list of libraries of all the seq/amp of the specimens within this PMO file.
#' @field panel_info A list of info on the panels.
#' @field pmo_header The PMO information for this file including version etc.
#' @field project_info The information about the projects stored in this PMO.
#' @field read_counts_by_stage The read counts for library_samples for different stages of the pipeline.
#' @field representative_microhaplotypes A list of the information on the representative microhaplotypes.
#' @field sequencing_info A list of sequencing infos for this PMO file.
#' @field specimen_info A list of all the specimens within this PMO file.
#' @field target_info A list of info on the targets.
#' @field targeted_genomes A list of genomes that any genomic location information refers to.
#' @field extras Additional properties not explicitly defined in the schema.
#'
#' @section Constructor:
#' `new(...)` supports the following arguments.
#' * `bioinformatics_methods_info`: The bioinformatics pipeline/methods used to generated the microhaplotype analysis for this project.
#' * `bioinformatics_run_info`: The runtime info for the bioinformatics pipeline used to generated the microhaplotypes analysis for this project.
#' * `detected_microhaplotypes`: The microhaplotypes detected in this projects.
#' * `library_sample_info`: A list of libraries of all the seq/amp of the specimens within this PMO file.
#' * `panel_info`: A list of info on the panels.
#' * `pmo_header`: The PMO information for this file including version etc.
#' * `project_info`: The information about the projects stored in this PMO.
#' * `read_counts_by_stage`: The read counts for library_samples for different stages of the pipeline.
#' * `representative_microhaplotypes`: A list of the information on the representative microhaplotypes.
#' * `sequencing_info`: A list of sequencing infos for this PMO file.
#' * `specimen_info`: A list of all the specimens within this PMO file.
#' * `target_info`: A list of info on the targets.
#' * `targeted_genomes`: A list of genomes that any genomic location information refers to.
#' * `extras`: Additional properties not explicitly defined in the schema.
#'
#' @section Methods:
#' See inline method documentation for `initialize()`, `validate()`, `to_list()`, `to_json_list()`, and `to_json()`.
#'
#' @format An [R6::R6Class()] generator object.
#' @export
PortableMicrohaplotypeObject <- R6::R6Class(
  "PortableMicrohaplotypeObject",
  public = list(
    bioinformatics_methods_info = list(),
    bioinformatics_run_info = list(),
    detected_microhaplotypes = list(),
    library_sample_info = list(),
    panel_info = list(),
    pmo_header = NULL,
    project_info = list(),
    read_counts_by_stage = list(),
    representative_microhaplotypes = NULL,
    sequencing_info = list(),
    specimen_info = list(),
    target_info = list(),
    targeted_genomes = list(),
    extras = list(),

    #' @description Create a new instance.
    #' @param bioinformatics_methods_info The bioinformatics pipeline/methods used to generated the microhaplotype analysis for this project.
    #' @param bioinformatics_run_info The runtime info for the bioinformatics pipeline used to generated the microhaplotypes analysis for this project.
    #' @param detected_microhaplotypes The microhaplotypes detected in this projects.
    #' @param library_sample_info A list of libraries of all the seq/amp of the specimens within this PMO file.
    #' @param panel_info A list of info on the panels.
    #' @param pmo_header The PMO information for this file including version etc.
    #' @param project_info The information about the projects stored in this PMO.
    #' @param read_counts_by_stage The read counts for library_samples for different stages of the pipeline.
    #' @param representative_microhaplotypes A list of the information on the representative microhaplotypes.
    #' @param sequencing_info A list of sequencing infos for this PMO file.
    #' @param specimen_info A list of all the specimens within this PMO file.
    #' @param target_info A list of info on the targets.
    #' @param targeted_genomes A list of genomes that any genomic location information refers to.
    #' @param extras Additional properties not explicitly defined in the schema.
    initialize = function(bioinformatics_methods_info = NULL, bioinformatics_run_info = NULL, detected_microhaplotypes = list(), library_sample_info = list(), panel_info = list(), pmo_header = NULL, project_info = NULL, read_counts_by_stage = NULL, representative_microhaplotypes = NULL, sequencing_info = NULL, specimen_info = list(), target_info = list(), targeted_genomes = NULL, extras = list()) {
      self$bioinformatics_methods_info <- bioinformatics_methods_info
      self$bioinformatics_run_info <- bioinformatics_run_info
      self$detected_microhaplotypes <- detected_microhaplotypes
      self$library_sample_info <- library_sample_info
      self$panel_info <- panel_info
      self$pmo_header <- pmo_header
      self$project_info <- project_info
      self$read_counts_by_stage <- read_counts_by_stage
      self$representative_microhaplotypes <- representative_microhaplotypes
      self$sequencing_info <- sequencing_info
      self$specimen_info <- specimen_info
      self$target_info <- target_info
      self$targeted_genomes <- targeted_genomes
      self$extras <- extras
    },

    #' @description Validate the current instance against schema-derived constraints.
    validate = function() {
      if (!is.null(self$bioinformatics_methods_info) && !is.list(self$bioinformatics_methods_info)) stop("PortableMicrohaplotypeObject.bioinformatics_methods_info must be a list")
      if (!is.null(self$bioinformatics_run_info) && !is.list(self$bioinformatics_run_info)) stop("PortableMicrohaplotypeObject.bioinformatics_run_info must be a list")
      if (!is.null(self$detected_microhaplotypes) && !is.list(self$detected_microhaplotypes)) stop("PortableMicrohaplotypeObject.detected_microhaplotypes must be a list")
      if (!is.null(self$library_sample_info) && !is.list(self$library_sample_info)) stop("PortableMicrohaplotypeObject.library_sample_info must be a list")
      if (!is.null(self$panel_info) && !is.list(self$panel_info)) stop("PortableMicrohaplotypeObject.panel_info must be a list")
      if (!is.null(self$project_info) && !is.list(self$project_info)) stop("PortableMicrohaplotypeObject.project_info must be a list")
      if (!is.null(self$read_counts_by_stage) && !is.list(self$read_counts_by_stage)) stop("PortableMicrohaplotypeObject.read_counts_by_stage must be a list")
      if (!is.null(self$sequencing_info) && !is.list(self$sequencing_info)) stop("PortableMicrohaplotypeObject.sequencing_info must be a list")
      if (!is.null(self$specimen_info) && !is.list(self$specimen_info)) stop("PortableMicrohaplotypeObject.specimen_info must be a list")
      if (!is.null(self$target_info) && !is.list(self$target_info)) stop("PortableMicrohaplotypeObject.target_info must be a list")
      if (!is.null(self$targeted_genomes) && !is.list(self$targeted_genomes)) stop("PortableMicrohaplotypeObject.targeted_genomes must be a list")
      if (!is.null(self$bioinformatics_methods_info)) for (.x in self$bioinformatics_methods_info) .x$validate()
      if (!is.null(self$bioinformatics_run_info)) for (.x in self$bioinformatics_run_info) .x$validate()
      if (!is.null(self$detected_microhaplotypes)) for (.x in self$detected_microhaplotypes) .x$validate()
      if (!is.null(self$library_sample_info)) for (.x in self$library_sample_info) .x$validate()
      if (!is.null(self$panel_info)) for (.x in self$panel_info) .x$validate()
      if (!is.null(self$pmo_header)) self$pmo_header$validate()
      if (!is.null(self$project_info)) for (.x in self$project_info) .x$validate()
      if (!is.null(self$read_counts_by_stage)) for (.x in self$read_counts_by_stage) .x$validate()
      if (!is.null(self$representative_microhaplotypes)) self$representative_microhaplotypes$validate()
      if (!is.null(self$sequencing_info)) for (.x in self$sequencing_info) .x$validate()
      if (!is.null(self$specimen_info)) for (.x in self$specimen_info) .x$validate()
      if (!is.null(self$target_info)) for (.x in self$target_info) .x$validate()
      if (!is.null(self$targeted_genomes)) for (.x in self$targeted_genomes) .x$validate()
      invisible(TRUE)
    },

    #' @description Convert the object to a plain R list using in-memory values.
    to_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_methods_info)) out$bioinformatics_methods_info <- lapply(self$bioinformatics_methods_info, function(x) x$to_list())
      if (!is.null(self$bioinformatics_run_info)) out$bioinformatics_run_info <- lapply(self$bioinformatics_run_info, function(x) x$to_list())
      if (!is.null(self$detected_microhaplotypes)) out$detected_microhaplotypes <- lapply(self$detected_microhaplotypes, function(x) x$to_list())
      if (!is.null(self$library_sample_info)) out$library_sample_info <- lapply(self$library_sample_info, function(x) x$to_list())
      if (!is.null(self$panel_info)) out$panel_info <- lapply(self$panel_info, function(x) x$to_list())
      if (!is.null(self$pmo_header)) out$pmo_header <- self$pmo_header$to_list()
      if (!is.null(self$project_info)) out$project_info <- lapply(self$project_info, function(x) x$to_list())
      if (!is.null(self$read_counts_by_stage)) out$read_counts_by_stage <- lapply(self$read_counts_by_stage, function(x) x$to_list())
      if (!is.null(self$representative_microhaplotypes)) out$representative_microhaplotypes <- self$representative_microhaplotypes$to_list()
      if (!is.null(self$sequencing_info)) out$sequencing_info <- lapply(self$sequencing_info, function(x) x$to_list())
      if (!is.null(self$specimen_info)) out$specimen_info <- lapply(self$specimen_info, function(x) x$to_list())
      if (!is.null(self$target_info)) out$target_info <- lapply(self$target_info, function(x) x$to_list())
      if (!is.null(self$targeted_genomes)) out$targeted_genomes <- lapply(self$targeted_genomes, function(x) x$to_list())
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON-ready R list.
    to_json_list = function() {
      out <- list()
      if (!is.null(self$bioinformatics_methods_info)) out$bioinformatics_methods_info <- I(lapply(self$bioinformatics_methods_info, function(x) x$to_json_list()))
      if (!is.null(self$bioinformatics_run_info)) out$bioinformatics_run_info <- I(lapply(self$bioinformatics_run_info, function(x) x$to_json_list()))
      if (!is.null(self$detected_microhaplotypes)) out$detected_microhaplotypes <- I(lapply(self$detected_microhaplotypes, function(x) x$to_json_list()))
      if (!is.null(self$library_sample_info)) out$library_sample_info <- I(lapply(self$library_sample_info, function(x) x$to_json_list()))
      if (!is.null(self$panel_info)) out$panel_info <- I(lapply(self$panel_info, function(x) x$to_json_list()))
      if (!is.null(self$pmo_header)) out$pmo_header <- self$pmo_header$to_json_list()
      if (!is.null(self$project_info)) out$project_info <- I(lapply(self$project_info, function(x) x$to_json_list()))
      if (!is.null(self$read_counts_by_stage)) out$read_counts_by_stage <- I(lapply(self$read_counts_by_stage, function(x) x$to_json_list()))
      if (!is.null(self$representative_microhaplotypes)) out$representative_microhaplotypes <- self$representative_microhaplotypes$to_json_list()
      if (!is.null(self$sequencing_info)) out$sequencing_info <- I(lapply(self$sequencing_info, function(x) x$to_json_list()))
      if (!is.null(self$specimen_info)) out$specimen_info <- I(lapply(self$specimen_info, function(x) x$to_json_list()))
      if (!is.null(self$target_info)) out$target_info <- I(lapply(self$target_info, function(x) x$to_json_list()))
      if (!is.null(self$targeted_genomes)) out$targeted_genomes <- I(lapply(self$targeted_genomes, function(x) x$to_json_list()))
      for (nm in names(self$extras)) out[[nm]] <- self$extras[[nm]]
      out
    },

    #' @description Convert the object to a JSON string.
    #' @param pretty Logical; pretty-print the JSON.
    #' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
    #' @param ... Additional arguments passed to [jsonlite::toJSON()].
    to_json = function(pretty = FALSE, auto_unbox = TRUE, ...) {
      jsonlite::toJSON(self$to_json_list(), pretty = pretty, auto_unbox = auto_unbox, na = "string", ...)
    }
  )
)

PortableMicrohaplotypeObject$from_json <- function(x, validate = TRUE) {
  obj <- if (is.character(x)) jsonlite::fromJSON(x, simplifyVector = FALSE) else x
  if (!is.list(obj)) stop("from_json expects a JSON object or list")
  required_fields <- c("detected_microhaplotypes","library_sample_info","panel_info","pmo_header","representative_microhaplotypes","specimen_info","target_info")
  missing_required <- setdiff(required_fields, names(obj))
  if (length(missing_required) > 0) stop("PortableMicrohaplotypeObject missing required field(s): ", paste(missing_required, collapse = ", "))
  known <- c("bioinformatics_methods_info","bioinformatics_run_info","detected_microhaplotypes","library_sample_info","panel_info","pmo_header","project_info","read_counts_by_stage","representative_microhaplotypes","sequencing_info","specimen_info","target_info","targeted_genomes")
  extras <- obj[setdiff(names(obj), known)]
  inst <- PortableMicrohaplotypeObject$new(bioinformatics_methods_info = if (!is.null(obj[["bioinformatics_methods_info"]])) lapply(obj[["bioinformatics_methods_info"]], function(.x) BioinformaticsMethodInfo$from_json(.x, validate = FALSE)) else NULL, bioinformatics_run_info = if (!is.null(obj[["bioinformatics_run_info"]])) lapply(obj[["bioinformatics_run_info"]], function(.x) BioinformaticsRunInfo$from_json(.x, validate = FALSE)) else NULL, detected_microhaplotypes = if (!is.null(obj[["detected_microhaplotypes"]])) lapply(obj[["detected_microhaplotypes"]], function(.x) DetectedMicrohaplotypes$from_json(.x, validate = FALSE)) else NULL, library_sample_info = if (!is.null(obj[["library_sample_info"]])) lapply(obj[["library_sample_info"]], function(.x) LibrarySampleInfo$from_json(.x, validate = FALSE)) else NULL, panel_info = if (!is.null(obj[["panel_info"]])) lapply(obj[["panel_info"]], function(.x) PanelInfo$from_json(.x, validate = FALSE)) else NULL, pmo_header = if (!is.null(obj[["pmo_header"]])) PmoHeader$from_json(obj[["pmo_header"]], validate = FALSE) else NULL, project_info = if (!is.null(obj[["project_info"]])) lapply(obj[["project_info"]], function(.x) ProjectInfo$from_json(.x, validate = FALSE)) else NULL, read_counts_by_stage = if (!is.null(obj[["read_counts_by_stage"]])) lapply(obj[["read_counts_by_stage"]], function(.x) ReadCountsByStage$from_json(.x, validate = FALSE)) else NULL, representative_microhaplotypes = if (!is.null(obj[["representative_microhaplotypes"]])) RepresentativeMicrohaplotypes$from_json(obj[["representative_microhaplotypes"]], validate = FALSE) else NULL, sequencing_info = if (!is.null(obj[["sequencing_info"]])) lapply(obj[["sequencing_info"]], function(.x) SequencingInfo$from_json(.x, validate = FALSE)) else NULL, specimen_info = if (!is.null(obj[["specimen_info"]])) lapply(obj[["specimen_info"]], function(.x) SpecimenInfo$from_json(.x, validate = FALSE)) else NULL, target_info = if (!is.null(obj[["target_info"]])) lapply(obj[["target_info"]], function(.x) TargetInfo$from_json(.x, validate = FALSE)) else NULL, targeted_genomes = if (!is.null(obj[["targeted_genomes"]])) lapply(obj[["targeted_genomes"]], function(.x) GenomeInfo$from_json(.x, validate = FALSE)) else NULL, extras = extras)
  if (validate) inst$validate()
  inst
}

#' Read a PMO object from a file
#'
#' Information on final microhaplotype results from a targeted amplicon analysis with associated meta data.
#'
#' Reads a PMO JSON file from disk, including compressed files ending in
#' `.gz`, `.bz2`, or `.xz`, and parses it into a PMO R6 instance.
#'
#' @param path Path to the PMO JSON file.
#' @param validate Logical; if `TRUE`, validate the parsed object.
#'
#' @return A `PortableMicrohaplotypeObject` instance.
#' @export
read_pmo <- function(path, validate = TRUE) {
  obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  PortableMicrohaplotypeObject$from_json(obj, validate = validate)
}

#' Read a PMO file as a raw nested list
#'
#' Reads a PMO JSON file directly into nested R lists without wrapping
#' it into R6 classes. This is useful for performance comparisons or
#' workflows that do not need the object-oriented API.
#'
#' @param path Path to the PMO JSON file.
#'
#' @return A nested list parsed from JSON.
#' @export
read_pmo_raw <- function(path) {
  pmo_raw_postprocess(jsonlite::fromJSON(path, simplifyVector = FALSE))
}

#' Write a raw PMO nested list to a file
#'
#' Writes a PMO represented as nested R lists back to JSON, reapplying
#' JSON-facing conventions such as zero-based `_id` fields.
#'
#' @param x A nested list as returned by `read_pmo_raw()`.
#' @param path Output path.
#' @param pretty Logical; pretty-print the JSON.
#' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
#' @return Invisibly returns `path`.
#' @export
write_pmo_raw <- function(x, path, pretty = FALSE, auto_unbox = TRUE) {
  json_ready <- pmo_raw_prepare_for_json(x)
  con <- open_text_connection(path, 'wt')
  on.exit(close(con), add = TRUE)
  writeLines(jsonlite::toJSON(json_ready, pretty = pretty, auto_unbox = auto_unbox, na = 'string'), con = con)
  invisible(path)
}

#' Write a PMO object to a file
#'
#' Information on final microhaplotype results from a targeted amplicon analysis with associated meta data.
#'
#' Writes a PMO object to JSON, with optional compression inferred
#' from the file extension.
#'
#' @param pmo A `PortableMicrohaplotypeObject` instance.
#' @param path Output path.
#' @param pretty Logical; pretty-print the JSON.
#' @param auto_unbox Logical; passed to [jsonlite::toJSON()].
#' @param validate Logical; if `TRUE`, validate before writing.
#' @param ... Additional arguments passed through to `to_json()`.
#'
#' @return Invisibly returns `path`.
#' @export
write_pmo <- function(pmo, path, pretty = FALSE, auto_unbox = TRUE, validate = TRUE, ...) {
  if (validate) pmo$validate()
  con <- open_text_connection(path, 'wt')
  on.exit(close(con), add = TRUE)
  writeLines(pmo$to_json(pretty = pretty, auto_unbox = auto_unbox, ...), con = con)
  invisible(path)
}

PortableMicrohaplotypeObject$from_file <- read_pmo

PortableMicrohaplotypeObject$set('public', 'to_file', function(path, pretty = FALSE, auto_unbox = TRUE, validate = TRUE, ...) {
  write_pmo(self, path = path, pretty = pretty, auto_unbox = auto_unbox, validate = validate, ...)
})
