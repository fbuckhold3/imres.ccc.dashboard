# =============================================================================
# GLOBAL.R - RDM 2.0 VERSION
# =============================================================================
# This file uses the gmed package for RDM 2.0 data loading
# Modernized for the new REDCap system

# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(gmed)  # GMED package with REDCap functions and updated graphs
library(bslib)
library(httr)
library(gganimate)
library(stringr)
library(xml2)
library(fontawesome)
library(tidyr)
library(reactable)
library(htmltools)
library(data.table)
library(purrr)

# ---------- SOURCE HELPER FUNCTIONS ----------
source("R/helpers.R")
source("R/redcap_submission.R")
source("R/modules.R")

# ---------- ACCESS CODE ----------
stored_access_code <- Sys.getenv("ACCESS_CODE", "default123")

# ---------- INITIALIZE APP CONFIG ----------
initialize_app_config <- function() {
  # Set up REDCap API URL for RDM 2.0
  url <- "https://redcapsurvey.slu.edu/api/"

  # Debug information
  message("=== Initializing App Config ===")
  message("REDCap URL: ", url)

  # Load RDM token from environment
  rdm_token <- Sys.getenv("RDM_TOKEN")

  if (rdm_token == "" || rdm_token == "YOUR_RDM2_TOKEN_HERE") {
    stop("RDM_TOKEN not set! Please add your RDM 2.0 token to .Renviron")
  }

  message("RDM_TOKEN length: ", nchar(rdm_token))

  # Optional: Load other tokens if needed (for assessments, faculty, etc.)
  eval_token <- Sys.getenv("EVAL_TOKEN", "")
  fac_token <- Sys.getenv("FAC_TOKEN", "")

  # Return configuration
  list(
    url = url,
    rdm_token = rdm_token,
    eval_token = eval_token,
    fac_token = fac_token
  )
}

# ---------- LOAD RDM 2.0 DATA ----------
load_imres_data <- function(config) {
  message("=== LOADING RDM 2.0 DATA ===")

  # Load all RDM data using gmed package
  message("Calling gmed::load_rdm_complete()...")
  rdm_data <- tryCatch({
    gmed::load_rdm_complete(
      redcap_url = config$url,
      rdm_token = config$rdm_token,
      raw_or_label = "raw"  # CRITICAL: Always use raw
    )
  }, error = function(e) {
    message("ERROR loading RDM data: ", e$message)
    message("Stack trace: ", paste(e$call, collapse = "\n"))
    stop("Failed to load RDM 2.0 data. Please check your RDM_TOKEN and network connection.")
  })

  message("✓ RDM data loaded successfully")
  message("Available components: ", paste(names(rdm_data), collapse = ", "))

  # --- Extract Components from gmed Structure ---

  # Data dictionary
  rdm_dict <- rdm_data$data_dict %||% NULL
  message("Data dictionary: ", if(is.null(rdm_dict)) "NULL" else paste(nrow(rdm_dict), "fields"))

  # Resident data (main demographic/roster data)
  resident_data <- rdm_data$residents %||% NULL
  message("Resident data: ", if(is.null(resident_data)) "NULL" else paste(nrow(resident_data), "residents"))

  # All forms (repeating instruments)
  all_forms <- rdm_data$all_forms %||% list()
  message("All forms: ", paste(names(all_forms), collapse = ", "))

  # Extract specific forms
  ccc_review_data <- all_forms$ccc_review %||% NULL
  message("CCC review data: ", if(is.null(ccc_review_data)) "NULL" else paste(nrow(ccc_review_data), "reviews"))

  milestone_entry_data <- all_forms$milestone_entry %||% NULL
  message("Milestone entry data: ", if(is.null(milestone_entry_data)) "NULL" else paste(nrow(milestone_entry_data), "entries"))

  milestone_self_data <- all_forms$milestone_selfevaluation_c33c %||% NULL
  message("Self milestone data: ", if(is.null(milestone_self_data)) "NULL" else paste(nrow(milestone_self_data), "entries"))

  ilp_data <- all_forms$ilp %||% NULL
  message("ILP data: ", if(is.null(ilp_data)) "NULL" else paste(nrow(ilp_data), "entries"))

  s_eval_data <- all_forms$s_eval %||% NULL
  message("Self eval data: ", if(is.null(s_eval_data)) "NULL" else paste(nrow(s_eval_data), "entries"))

  schol_data <- all_forms$scholarship %||% NULL
  message("Scholarship data: ", if(is.null(schol_data)) "NULL" else paste(nrow(schol_data), "entries"))

  # === AUTO-DETECT PERIOD FOR EACH RESIDENT ===
  if (!is.null(resident_data) && nrow(resident_data) > 0) {
    # CRITICAL: Translate grad_yr and type from REDCap codes to actual values
    if (!is.null(rdm_dict)) {

      # Translate type field (1 = Preliminary, 2 = Categorical, etc.)
      type_choices <- rdm_dict %>%
        filter(field_name == "type") %>%
        pull(select_choices_or_calculations)

      if (length(type_choices) > 0 && !is.na(type_choices[1])) {
        choice_pairs <- strsplit(type_choices[1], "\\|")[[1]]
        type_map <- list()
        for (pair in choice_pairs) {
          parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
          if (length(parts) >= 2) {
            code <- trimws(parts[1])
            label <- trimws(paste(parts[-1], collapse = ","))
            type_map[[code]] <- label
          }
        }
        type_map <- unlist(type_map)

        resident_data <- resident_data %>%
          mutate(
            type = if_else(!is.na(type) & type %in% names(type_map),
                          type_map[type], type)
          )
      }

      # Translate grad_yr field (1 = 2025, 2 = 2026, etc.)
      grad_yr_choices <- rdm_dict %>%
        filter(field_name == "grad_yr") %>%
        pull(select_choices_or_calculations)

      if (length(grad_yr_choices) > 0 && !is.na(grad_yr_choices[1])) {
        choice_pairs <- strsplit(grad_yr_choices[1], "\\|")[[1]]
        grad_yr_map <- list()
        for (pair in choice_pairs) {
          parts <- strsplit(trimws(pair), ",", fixed = TRUE)[[1]]
          if (length(parts) >= 2) {
            code <- trimws(parts[1])
            year <- trimws(paste(parts[-1], collapse = ","))
            grad_yr_map[[code]] <- year
          }
        }
        grad_yr_map <- unlist(grad_yr_map)

        resident_data <- resident_data %>%
          mutate(
            grad_yr = if_else(!is.na(grad_yr) & grad_yr %in% names(grad_yr_map),
                             grad_yr_map[grad_yr], grad_yr)
          )
      }
    }

    # Now calculate periods with translated values
    resident_data <- resident_data %>%
      rowwise() %>%
      mutate(
        current_period = {
          tryCatch({
            # Get graduation year and type
            grad_year <- if ("grad_yr" %in% names(pick(everything()))) {
              suppressWarnings(as.numeric(grad_yr))
            } else {
              NA
            }

            res_type <- if ("type" %in% names(pick(everything()))) {
              type
            } else {
              NA
            }

            # Only calculate if we have required data
            if (!is.na(grad_year) && !is.na(res_type)) {
              # Convert type to numeric if needed (2 = categorical, 1 = prelim)
              type_code <- if (is.character(res_type)) {
                res_type_lower <- tolower(trimws(res_type))
                if (res_type_lower == "categorical") {
                  2
                } else if (res_type_lower %in% c("preliminary", "prelim")) {
                  1
                } else {
                  NA
                }
              } else {
                as.numeric(res_type)
              }

              if (!is.na(type_code) && type_code %in% c(1, 2)) {
                # Use the calculate_pgy_and_period function from helpers.R
                period_calc <- calculate_pgy_and_period(
                  grad_yr = grad_year,
                  type = type_code,
                  current_date = Sys.Date()
                )

                if (!is.null(period_calc$period_name) &&
                    !is.na(period_calc$period_name) &&
                    period_calc$is_valid) {
                  period_calc$period_name
                } else {
                  "Mid Intern"  # Fallback
                }
              } else {
                "Mid Intern"  # Fallback
              }
            } else {
              "Mid Intern"  # Fallback if missing data
            }
          }, error = function(e) {
            message("Error calculating period for resident: ", e$message)
            "Mid Intern"  # Fallback on error
          })
        },
        current_period_num = {
          # Map period names to numbers
          period_labels <- c(
            "Entering Residency" = 7,
            "Mid Intern" = 1,
            "End Intern" = 2,
            "Mid PGY2" = 3,
            "End PGY2" = 4,
            "Mid PGY3" = 5,
            "Graduating" = 6
          )
          period_labels[current_period] %||% 1
        }
      ) %>%
      ungroup()

    # Add Level column based on grad_yr and type using helper function
    resident_data <- calculate_resident_level(resident_data)
  }

  # --- Process Milestones (if processing functions are available) ---
  p_miles <- NULL
  s_miles <- NULL

  # Try to process program milestones if processing functions exist
  if (!is.null(milestone_entry_data) && exists("process_milestones")) {
    p_miles <- tryCatch({
      message("Processing program milestones...")
      process_milestones(milestone_entry_data, type = "program")
    }, error = function(e) {
      message("Could not process program milestones: ", e$message)
      milestone_entry_data  # Fall back to raw data
    })
  } else {
    p_miles <- milestone_entry_data
  }

  # Try to process self milestones if processing functions exist
  if (!is.null(milestone_self_data) && exists("process_milestones")) {
    s_miles <- tryCatch({
      message("Processing self milestones...")
      process_milestones(milestone_self_data, type = "self")
    }, error = function(e) {
      message("Could not process self milestones: ", e$message)
      milestone_self_data  # Fall back to raw data
    })
  } else {
    s_miles <- milestone_self_data
  }

  # --- Create Result List ---
  # This maintains backward compatibility with the rest of the app
  result_list <- list(
    # Data dictionaries
    rdm_dict = rdm_dict,

    # Resident information
    resident_data = resident_data,

    # CCC review data
    ccc_review = ccc_review_data,

    # Milestone data
    miles = milestone_entry_data,  # Raw milestone data
    p_miles = p_miles,             # Processed program milestones
    s_miles = s_miles,             # Processed self milestones
    p_miles_descriptions = p_miles,  # For now, same as p_miles
    s_miles_descriptions = s_miles,  # For now, same as s_miles

    # Other forms
    ilp = ilp_data,
    s_eval = s_eval_data,
    schol_data = schol_data,

    # Config info
    url = config$url,
    rdm_token = config$rdm_token,
    eval_token = config$eval_token,
    fac_token = config$fac_token
  )

  message("=== DATA LOADING COMPLETE ===")
  message("Returning data with components: ", paste(names(result_list), collapse = ", "))

  return(result_list)
}

# ---------- PERIOD HELPER FUNCTIONS ----------

#' Map period name to REDCap code (1-7)
period_to_code <- c(
  "Mid Intern" = "1",
  "End Intern" = "2",
  "Mid PGY2" = "3",
  "End PGY2" = "4",
  "Mid PGY3" = "5",
  "Graduation" = "6",
  "Graduating" = "6",
  "Entering Residency" = "7"
)

#' Map REDCap code to period name
code_to_period <- setNames(names(period_to_code), period_to_code)

#' Get REDCap period code from period name
get_period_code <- function(period_name) {
  period_to_code[[period_name]] %||% "1"
}

#' Get period name from REDCap code
get_period_name_from_code <- function(period_code) {
  code_to_period[[as.character(period_code)]] %||% "Mid Intern"
}

# ---------- GLOBAL APP DATA MANAGEMENT ----------
app_data_store <- NULL

ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    message("Initializing app data...")
    config <- initialize_app_config()
    app_data_store <<- load_imres_data(config)
    message("App data initialized")
  }
  return(app_data_store)
}

# ---------- MILESTONE IMAGES SETUP ----------
is_posit_connect <- function() {
  return(
    Sys.getenv("CONNECT_SERVER") != "" ||
      Sys.getenv("SHINY_PORT") != "" ||
      Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" ||
      Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server"
  )
}

setup_milestone_images <- function() {
  message("=== Setting up milestone images ===")

  local_available <- dir.exists("www/milestones")

  if (local_available) {
    message("✓ Found local milestones directory")
    shiny::addResourcePath("milestones", "www/milestones")
    available_images <- list.files("www/milestones", pattern = "\\.png$")
    message("Available images: ", length(available_images))

    return(list(
      local = TRUE,
      count = length(available_images),
      environment = if(is_posit_connect()) "posit_connect" else "local"
    ))
  } else {
    message("❌ Local milestones directory not found")
    message("Will use GitHub images when needed")

    return(list(
      local = FALSE,
      count = 0,
      environment = if(is_posit_connect()) "posit_connect" else "local",
      github_url = "https://raw.githubusercontent.com/fbuckhold3/imres.ccc.dashboard/main/www/milestones/"
    ))
  }
}

milestone_images_config <- setup_milestone_images()

message("✓ Global.R loaded successfully (RDM 2.0 version)")
