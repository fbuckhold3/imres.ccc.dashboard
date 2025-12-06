# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(gmed)  # NEW: RDM 2.0 data loading package
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
library(ggradar)

# ---------- SOURCE HELPER FUNCTIONS ----------
# Uncomment and adjust paths as needed
source("R/helpers.R")
source("R/redcap_submission.R")
source("R/modules.R")

# Grab ACCESS_CODE from the environment, default locally to "default123"
stored_access_code <- Sys.getenv("ACCESS_CODE", "default123")
# ---------- INITIALIZE APP CONFIG ----------
initialize_app_config <- function() {
  # Set up REDCap API URL for RDM 2.0
  url <- "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/"
  
  # Debug information about environment variables
  message("Available environment variables (first 10):")
  env_vars <- names(Sys.getenv())
  print(head(env_vars, 10))
  message("EVAL_TOKEN exists:", "EVAL_TOKEN" %in% names(Sys.getenv()))
  message("RDM_TOKEN exists:", "RDM_TOKEN" %in% names(Sys.getenv()))
  message("FAC_TOKEN exists:", "FAC_TOKEN" %in% names(Sys.getenv()))
  
  # Identify whether we are in a hosted environment
  is_hosted <- Sys.getenv("EVAL_TOKEN") != ""
  
  # Load tokens from environment variables or config file
  if (is_hosted) {
    eval_token <- Sys.getenv("EVAL_TOKEN")
    rdm_token <- Sys.getenv("RDM_TOKEN")
    fac_token <- Sys.getenv("FAC_TOKEN")
    
    # Check if tokens are empty strings even though they exist
    if (nchar(eval_token) == 0 || nchar(rdm_token) == 0 || nchar(fac_token) == 0) {
      message("WARNING: One or more required tokens are empty in environment!")
      message("Using config file as fallback.")
      # Load from config file as fallback
      conf <- tryCatch({
        config::get(file = "config.yml")
      }, error = function(e) {
        message("Error loading config file: ", e$message)
        list(
          eval_token = "",
          rdm_token = "",
          fac_token = ""
        )
      })
      
      # Use config values if environment variables are empty
      if (nchar(eval_token) == 0) eval_token <- conf$eval_token
      if (nchar(rdm_token) == 0) rdm_token <- conf$rdm_token
      if (nchar(fac_token) == 0) fac_token <- conf$fac_token
    }
    
    # Disable SSL verification in the hosted environment (NOT recommended for production)
    httr::set_config(httr::config(ssl_verifypeer = FALSE))
  } else {
    # Use config file for local development
    conf <- tryCatch({
      config::get(file = "config.yml")
    }, error = function(e) {
      message("Error loading config file: ", e$message)
      list(
        eval_token = "",
        rdm_token = "",
        fac_token = ""
      )
    })
    eval_token <- conf$eval_token
    rdm_token <- conf$rdm_token
    fac_token <- conf$fac_token
  }
  
  # Print token values (length only for security)
  message("EVAL_TOKEN length:", nchar(eval_token))
  message("RDM_TOKEN length:", nchar(rdm_token))
  message("FAC_TOKEN length:", nchar(fac_token))
  
  # Return the environment with the tokens and URL
  list(
    url = url,
    eval_token = eval_token,
    rdm_token = rdm_token,
    fac_token = fac_token
  )
}

get_access_code <- function() {
  # First try environment variable (for Posit Connect deployment)
  env_code <- Sys.getenv("ACCESS_CODE")
  
  if (env_code != "") {
    message("Using ACCESS_CODE from environment variable")
    return(env_code)
  }
  
  # Second try config file (for local development)
  tryCatch({
    conf <- config::get(file = "config.yml")
    if (!is.null(conf$access_code) && conf$access_code != "") {
      message("Using access_code from config.yml")
      return(conf$access_code)
    }
  }, error = function(e) {
    message("No config.yml found or access_code not specified")
  })
  
  # Default fallback for testing
  message("Using default access code for testing")
  return("default123")
}

# Store the access code globally
stored_access_code <- get_access_code()


# ---------- LOAD IMRES DATA ----------
# RDM 2.0 VERSION - Uses gmed package for data loading
# This replaces the old imres-based data loading with modern gmed approach

load_imres_data <- function(config) {
  message("=== STARTING load_imres_data FUNCTION (RDM 2.0) ===")

  # Load all RDM data using gmed package
  message("Loading RDM 2.0 data using gmed::load_rdm_complete()...")
  rdm_data <- tryCatch({
    gmed::load_rdm_complete(
      redcap_url = config$url,
      rdm_token = config$rdm_token,
      raw_or_label = "raw"  # CRITICAL: Always use raw for proper data handling
    )
  }, error = function(e) {
    message("ERROR loading RDM data: ", e$message)
    stop("Failed to load RDM data. Check your RDM_TOKEN and network connection.")
  })

  message("RDM data loaded successfully")
  message("Available components: ", paste(names(rdm_data), collapse = ", "))

  # --- Extract Data Dictionary ---
  rdm_dict <- tryCatch({
    message("Extracting data dictionary...")
    rdm_data$data_dict
  }, error = function(e) {
    message("Error getting rdm_dict: ", e$message)
    NULL
  })

  # --- Extract Resident Data ---
  resident_data <- tryCatch({
    message("Extracting resident data from RDM 2.0...")
    rdm_data$residents
  }, error = function(e) {
    message("Error getting resident data: ", e$message)
    NULL
  })

  # --- Extract Form Data from RDM 2.0 ---
  message("Extracting all forms from gmed data structure...")

  all_forms <- tryCatch({
    rdm_data$all_forms
  }, error = function(e) {
    message("Error getting all_forms: ", e$message)
    list()
  })
    
    message("Forms data pulled. Structure of rdm_dat:")
    message("rdm_dat is of class: ", paste(class(result), collapse=", "))
    message("rdm_dat contains these keys: ", paste(names(result), collapse=", "))
    
    # If rdm_dat is a data frame, check if it has the redcap_repeat_instrument column
    if (is.data.frame(result)) {
      message("rdm_dat is a data frame with ", nrow(result), " rows and ", ncol(result), " columns")
      if ("redcap_repeat_instrument" %in% names(result)) {
        message("redcap_repeat_instrument values: ", paste(unique(result$redcap_repeat_instrument), collapse=", "))
      } else {
        message("WARNING: redcap_repeat_instrument column not found in rdm_dat")
      }
    }
    
    result
  }, error = function(e) {
    message("Error pulling forms data: ", e$message)
    NULL
  })
  
  # --- Extract ILP data ---
  ilp_data <- tryCatch({
    message("Starting ILP data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract ILP data")
      NULL
    } else {
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract ILP data
      if ("ilp" %in% names(rdm_dat)) {
        message("Found ilp data (lowercase) in rdm_dat")
        ilp_data <- rdm_dat$ilp
      } else if ("ILP" %in% names(rdm_dat)) {
        message("Found ILP data (capitalized) in rdm_dat")
        ilp_data <- rdm_dat$ILP
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for ILP in repeating instruments, case-insensitive
        message("Looking for ilp data in repeating instruments")
        
        # Print all unique values in redcap_repeat_instrument
        message("Unique redcap_repeat_instrument values: ", 
                paste(unique(rdm_dat$redcap_repeat_instrument), collapse=", "))
        
        ilp_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "ilp")
        
        if (nrow(ilp_data) > 0) {
          message("Extracted ", nrow(ilp_data), " rows of ILP data from main dataframe")
        } else {
          message("No rows found with redcap_repeat_instrument='ilp' (case insensitive)")
          
          # Try one more approach - check if there's a column that has 'ilp' in its name
          ilp_columns <- names(rdm_dat)[grepl("ilp", tolower(names(rdm_dat)))]
          if (length(ilp_columns) > 0) {
            message("Found columns with 'ilp' in their name: ", paste(ilp_columns, collapse=", "))
          }
          
          NULL
        }
      } else {
        message("No ILP data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("ilp_data") && !is.null(ilp_data)) {
        message("ILP data class: ", paste(class(ilp_data), collapse=", "))
        if (is.data.frame(ilp_data)) {
          message("ILP data has ", nrow(ilp_data), " rows and ", ncol(ilp_data), " columns")
          message("ILP data column names: ", paste(names(ilp_data), collapse=", "))
        }
        ilp_data
      } else {
        message("ilp_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting ILP data: ", e$message)
    NULL
  })
  
  # --- Extract s_eval data ---
  s_eval_data <- tryCatch({
    message("Starting s_eval data extraction")
    
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract s_eval data")
      NULL
    } else {
      # Try multiple approaches to extract s_eval data
      
      # Approach 1: Check if s_eval is a direct component
      if ("s_eval" %in% names(rdm_dat)) {
        message("Found s_eval data as a direct component in rdm_dat")
        rdm_dat$s_eval
      } else if ("S_eval" %in% names(rdm_dat)) {
        message("Found S_eval data (capitalized) in rdm_dat")
        rdm_dat$S_eval
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        message("Looking for s_eval data in repeating instruments")
        
        # Try case-insensitive match
        s_eval_rows <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) %in% c("s_eval", "self evaluation", "self_evaluation"))
        
        if (nrow(s_eval_rows) > 0) {
          message("Extracted ", nrow(s_eval_rows), " rows of s_eval data from main dataframe")
          s_eval_rows
        } else {
          message("No rows found with matching redcap_repeat_instrument values")
          NULL
        }
      } else {
        message("No s_eval data found using any extraction method")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting s_eval data: ", e$message)
    NULL
  })
  
  # --- Extract scholarship data ---
  schol_data <- tryCatch({
    message("Starting scholarship data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract scholarship data")
      NULL
    } else {
      message("rdm_dat is not NULL")
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract scholarship data
      if ("scholarship" %in% names(rdm_dat)) {
        message("Found scholarship data (lowercase) in rdm_dat")
        schol_data <- rdm_dat$scholarship
      } else if ("Scholarship" %in% names(rdm_dat)) {
        message("Found Scholarship data (capitalized) in rdm_dat")
        schol_data <- rdm_dat$Scholarship
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for scholarship in repeating instruments, case-insensitive
        message("Looking for scholarship data in repeating instruments")
        
        schol_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "scholarship")
        
        if (nrow(schol_data) > 0) {
          message("Extracted ", nrow(schol_data), " rows of scholarship data from main dataframe")
        } else {
          message("No scholarship data found in main dataframe")
          NULL
        }
      } else {
        message("No scholarship data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("schol_data") && !is.null(schol_data)) {
        message("scholarship data class: ", paste(class(schol_data), collapse=", "))
        if (is.data.frame(schol_data)) {
          message("scholarship data has ", nrow(schol_data), " rows and ", ncol(schol_data), " columns")
        }
        schol_data
      } else {
        message("schol_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting scholarship data: ", e$message)
    NULL
  })
  
  # --- Get resident data ---
  resident_data <- tryCatch({
    message("Attempting to pull assessment data...")
    ass_dat <- full_api_pull(config$eval_token, config$url)
    message("Successfully pulled assessment data")
    
    message("Wrangling assessment data...")
    ass_dat <- wrangle_assessment_data(ass_dat)
    message("Assessment data wrangled")
    
    message("Creating resident data...")
    result <- create_res_data(ass_dat, rdm_dat)
    message("Resident data created with ", nrow(result), " rows")
    
    result
  }, error = function(e) {
    message("Error in resident data API pull: ", e$message)
    NULL
  })
  
  # Debug check for resident_data
  if (exists("resident_data")) {
    message("resident_data exists and is ", if(is.null(resident_data)) "NULL" else "not NULL")
  } else {
    message("WARNING: resident_data variable doesn't exist!")
    # Initialize it to prevent errors
    resident_data <- NULL
  }
  
  # --- Get milestone data ---
  miles <- tryCatch({
    message("Getting all milestones...")
    result <- get_all_milestones(config$rdm_token, config$url)
    message("All milestones retrieved")
    
    message("Filling missing resident data in milestones...")
    result <- fill_missing_resident_data(result)
    message("Resident data filled in milestones")
    
    result
  }, error = function(e) {
    message("Error loading milestones: ", e$message)
    NULL
  })
  
  # --- Process milestones ---
  p_miles <- NULL
  s_miles <- NULL
  if (!is.null(miles)) {
    p_miles <- tryCatch({
      message("Processing program milestones...")
      result <- process_milestones(miles, type = "program")
      message("Program milestones processed")
      result
    }, error = function(e) {
      message("Error processing program milestones: ", e$message)
      NULL
    })
    
    s_miles <- tryCatch({
      message("Processing self milestones...")
      result <- process_milestones(miles, type = "self")
      message("Self milestones processed")
      result
    }, error = function(e) {
      message("Error processing self milestones: ", e$message)
      NULL
    })
  }
  
  # ============================================================================
  # ENHANCE MILESTONE DATA WITH DESCRIPTIONS
  # ============================================================================
  
  enhanced_s_miles <- NULL
  enhanced_p_miles <- NULL
  
  tryCatch({
    message("=== ENHANCING MILESTONE DATA WITH DESCRIPTIONS ===")
    
    # Get raw self milestone data
    raw_self_milestones <- NULL
    if (!is.null(rdm_dat) && "milestone_selfevaluation_c33c" %in% names(rdm_dat)) {
      raw_self_milestones <- rdm_dat$milestone_selfevaluation_c33c
      message("Found raw self milestone data: ", nrow(raw_self_milestones), " rows")
    } else {
      # Pull fresh if not in rdm_dat
      message("Pulling fresh self milestone data...")
      raw_data <- forms_api_pull(config$rdm_token, config$url, 'milestone_selfevaluation_c33c')
      if (is.list(raw_data) && "milestone_selfevaluation_c33c" %in% names(raw_data)) {
        raw_self_milestones <- raw_data$milestone_selfevaluation_c33c
      } else if (is.data.frame(raw_data)) {
        raw_self_milestones <- raw_data
      }
      message("Pulled self milestone data: ", nrow(raw_self_milestones), " rows")
    }
    
    # Get raw program milestone data
    raw_program_milestones <- NULL
    if (!is.null(rdm_dat) && "milestone_entry" %in% names(rdm_dat)) {
      raw_program_milestones <- rdm_dat$milestone_entry
      message("Found raw program milestone data: ", nrow(raw_program_milestones), " rows")
    } else {
      # Pull fresh if not in rdm_dat
      message("Pulling fresh program milestone data...")
      raw_data <- forms_api_pull(config$rdm_token, config$url, 'milestone_entry')
      if (is.list(raw_data) && "milestone_entry" %in% names(raw_data)) {
        raw_program_milestones <- raw_data$milestone_entry
      } else if (is.data.frame(raw_data)) {
        raw_program_milestones <- raw_data
      }
      message("Pulled program milestone data: ", nrow(raw_program_milestones), " rows")
    }
    
    # Create record_id to name mapping from resident_data
    record_name_map <- NULL
    if (!is.null(resident_data) && "record_id" %in% names(resident_data) && "name" %in% names(resident_data)) {
      record_name_map <- resident_data %>%
        select(record_id, name) %>%
        distinct()
      message("Created record_id to name mapping: ", nrow(record_name_map), " entries")
    }
    
    # FIXED: Create separate milestone descriptions dataframes instead of modifying the original
    s_miles_descriptions <- NULL
    p_miles_descriptions <- NULL
    
    # Create self milestone descriptions table
    if (!is.null(raw_self_milestones) && !is.null(s_miles) && !is.null(record_name_map)) {
      message("Creating self milestone descriptions table...")
      
      # Create a copy of s_miles for descriptions
      s_miles_descriptions <- s_miles
      
      # Add description fields
      desc_fields <- names(raw_self_milestones)[grepl("_desc", names(raw_self_milestones))]
      message("Found ", length(desc_fields), " description fields in raw self data: ", paste(head(desc_fields, 5), collapse = ", "))
      
      # Initialize description columns
      for (desc_field in desc_fields) {
        s_miles_descriptions[[desc_field]] <- NA_character_
      }
      
      # Match rows based on period and milestone scores
      for (i in 1:nrow(s_miles_descriptions)) {
        proc_row <- s_miles_descriptions[i, ]
        proc_period <- proc_row$period
        proc_name <- proc_row$name
        
        # Find matching raw rows by period
        if ("prog_mile_period_self" %in% names(raw_self_milestones)) {
          raw_period_rows <- raw_self_milestones[
            !is.na(raw_self_milestones$prog_mile_period_self) & 
              raw_self_milestones$prog_mile_period_self == proc_period, ]
          
          if (nrow(raw_period_rows) > 0) {
            # Try to match by milestone scores
            best_match_idx <- NULL
            best_match_score <- 0
            
            for (j in 1:nrow(raw_period_rows)) {
              raw_row <- raw_period_rows[j, ]
              match_count <- 0
              
              # Compare milestone scores
              milestone_pairs <- list(
                c("PC1", "rep_pc1_self"),
                c("PC2", "rep_pc2_self"),
                c("PC3", "rep_pc3_self"),
                c("PC6", "rep_pc6_self"),
                c("SBP1", "rep_sbp1_self"),
                c("SBP2", "rep_sbp2_self"),
                c("SBP3", "rep_sbp3_self"),
                c("PBL1", "rep_pbl1_self"),
                c("PBL2", "rep_pbl2_self"),
                c("PROF1", "rep_prof1_self"),
                c("PROF2", "rep_prof2_self"),
                c("PROF3", "rep_prof3_self"),
                c("PROF4", "rep_prof4_self"),
                c("ICS1", "rep_ics1_self"),
                c("ICS2", "rep_ics2_self"),
                c("ICS3", "rep_ics3_self")
              )
              
              for (pair in milestone_pairs) {
                proc_field <- pair[1]
                raw_field <- pair[2]
                
                if (proc_field %in% names(proc_row) && raw_field %in% names(raw_row)) {
                  proc_val <- proc_row[[proc_field]]
                  raw_val <- raw_row[[raw_field]]
                  
                  if (!is.na(proc_val) && !is.na(raw_val) && proc_val == raw_val) {
                    match_count <- match_count + 1
                  }
                }
              }
              
              if (match_count > best_match_score) {
                best_match_score <- match_count
                best_match_idx <- j
              }
            }
            
            # If we found a good match, copy the description fields
            if (!is.null(best_match_idx) && best_match_score > 3) {
              best_raw_row <- raw_period_rows[best_match_idx, ]
              
              for (desc_field in desc_fields) {
                if (desc_field %in% names(best_raw_row)) {
                  s_miles_descriptions[i, desc_field] <- best_raw_row[[desc_field]]
                }
              }
              
              message("Matched self milestone row for ", proc_name, " period ", proc_period, " with ", best_match_score, " matching scores")
            }
          }
        }
      }
      
      message("Self milestone descriptions table created")
    }
    
    # Create program milestone descriptions table
    if (!is.null(raw_program_milestones) && !is.null(p_miles) && !is.null(record_name_map)) {
      message("Creating program milestone descriptions table...")
      
      # Create a copy of p_miles for descriptions
      p_miles_descriptions <- p_miles
      
      # Add description fields
      desc_fields <- names(raw_program_milestones)[grepl("_desc", names(raw_program_milestones))]
      message("Found ", length(desc_fields), " description fields in raw program data: ", paste(head(desc_fields, 5), collapse = ", "))
      
      # Initialize description columns
      for (desc_field in desc_fields) {
        p_miles_descriptions[[desc_field]] <- NA_character_
      }
      
      # Match rows based on period and scores
      for (i in 1:nrow(p_miles_descriptions)) {
        proc_row <- p_miles_descriptions[i, ]
        proc_period <- proc_row$period
        proc_name <- proc_row$name
        
        # Find matching raw rows by period
        if ("prog_mile_period" %in% names(raw_program_milestones)) {
          raw_period_rows <- raw_program_milestones[
            !is.na(raw_program_milestones$prog_mile_period) & 
              raw_program_milestones$prog_mile_period == proc_period, ]
          
          if (nrow(raw_period_rows) > 0) {
            # Match by milestone scores
            best_match_idx <- NULL
            best_match_score <- 0
            
            for (j in 1:nrow(raw_period_rows)) {
              raw_row <- raw_period_rows[j, ]
              match_count <- 0
              
              # Compare milestone scores
              milestone_pairs <- list(
                c("PC1", "rep_pc1"),
                c("PC2", "rep_pc2"),
                c("PC3", "rep_pc3"),
                c("PC4", "rep_pc4"),
                c("PC5", "rep_pc5"),
                c("PC6", "rep_pc6"),
                c("MK1", "rep_mk1"),
                c("MK2", "rep_mk2"),
                c("MK3", "rep_mk3"),
                c("SBP1", "rep_sbp1"),
                c("SBP2", "rep_sbp2"),
                c("SBP3", "rep_sbp3"),
                c("PBL1", "rep_pbl1"),
                c("PBL2", "rep_pbl2"),
                c("PROF1", "rep_prof1"),
                c("PROF2", "rep_prof2"),
                c("PROF3", "rep_prof3"),
                c("PROF4", "rep_prof4"),
                c("ICS1", "rep_ics1"),
                c("ICS2", "rep_ics2"),
                c("ICS3", "rep_ics3")
              )
              
              for (pair in milestone_pairs) {
                proc_field <- pair[1]
                raw_field <- pair[2]
                
                if (proc_field %in% names(proc_row) && raw_field %in% names(raw_row)) {
                  proc_val <- proc_row[[proc_field]]
                  raw_val <- raw_row[[raw_field]]
                  
                  if (!is.na(proc_val) && !is.na(raw_val) && proc_val == raw_val) {
                    match_count <- match_count + 1
                  }
                }
              }
              
              if (match_count > best_match_score) {
                best_match_score <- match_count
                best_match_idx <- j
              }
            }
            
            # Copy description fields from best match
            if (!is.null(best_match_idx) && best_match_score > 5) {
              best_raw_row <- raw_period_rows[best_match_idx, ]
              
              for (desc_field in desc_fields) {
                if (desc_field %in% names(best_raw_row)) {
                  p_miles_descriptions[i, desc_field] <- best_raw_row[[desc_field]]
                }
              }
              
              message("Matched program milestone row for ", proc_name, " period ", proc_period, " with ", best_match_score, " matching scores")
            }
          }
        }
      }
      
      message("Program milestone descriptions table created")
    }
    
  }, error = function(e) {
    message("Error enhancing milestone data: ", e$message)
    # Use original data if enhancement fails
    s_miles_descriptions <- s_miles
    p_miles_descriptions <- p_miles
  })
  
  # FIXED: Keep original milestone data for plots, add descriptions as separate objects
  message("Keeping original milestone data for plots, adding descriptions separately")
  message("s_miles columns (for plots): ", paste(names(s_miles), collapse = ", "))
  message("p_miles columns (for plots): ", paste(names(p_miles), collapse = ", "))
  
  if (!is.null(s_miles_descriptions)) {
    message("s_miles_descriptions available with ", ncol(s_miles_descriptions), " columns")
  }
  if (!is.null(p_miles_descriptions)) {
    message("p_miles_descriptions available with ", ncol(p_miles_descriptions), " columns")
  }
  
  # --- Extract CCC review data ---
  ccc_review_data <- tryCatch({
    message("Starting CCC review data extraction")
    if (is.null(rdm_dat)) {
      message("rdm_dat is NULL, cannot extract CCC review data")
      NULL
    } else {
      message("rdm_dat contains these keys: ", paste(names(rdm_dat), collapse=", "))
      
      # Try multiple approaches to extract CCC review data
      if ("ccc_review" %in% names(rdm_dat)) {
        message("Found ccc_review data in rdm_dat")
        ccc_review_data <- rdm_dat$ccc_review
      } else if (is.data.frame(rdm_dat) && "redcap_repeat_instrument" %in% names(rdm_dat)) {
        # Check for ccc_review in repeating instruments
        message("Looking for ccc_review data in repeating instruments")
        
        ccc_review_data <- rdm_dat %>%
          filter(tolower(redcap_repeat_instrument) == "ccc_review")
        
        if (nrow(ccc_review_data) > 0) {
          message("Extracted ", nrow(ccc_review_data), " rows of CCC review data from main dataframe")
        } else {
          message("No rows found with redcap_repeat_instrument='ccc_review'")
          NULL
        }
      } else {
        message("No CCC review data found using any extraction method")
        NULL
      }
      
      # Debug the extracted data
      if (exists("ccc_review_data") && !is.null(ccc_review_data)) {
        message("CCC review data class: ", paste(class(ccc_review_data), collapse=", "))
        if (is.data.frame(ccc_review_data)) {
          message("CCC review data has ", nrow(ccc_review_data), " rows and ", ncol(ccc_review_data), " columns")
          message("CCC review data column names: ", paste(names(ccc_review_data), collapse=", "))
        }
        ccc_review_data
      } else {
        message("ccc_review_data is NULL after extraction attempts")
        NULL
      }
    }
  }, error = function(e) {
    message("Error extracting CCC review data: ", e$message)
    NULL
  })
  
  # --- Create result list ---
  message("Preparing return value with all data components")
  
  # Make sure all variables exist before creating the result list
  # If any variable doesn't exist, initialize it to NULL
  for (var_name in c("rdm_dict", "ass_dict", "resident_data", "miles", 
                     "ilp_data", "s_eval_data", "schol_data", "p_miles", "s_miles")) {
    if (!exists(var_name)) {
      message(paste("WARNING:", var_name, "variable doesn't exist, initializing to NULL"))
      assign(var_name, NULL)
    }
  }
  
  result_list <- list(
    rdm_dict = rdm_dict,
    ass_dict = ass_dict,
    resident_data = resident_data,
    miles = miles,
    ilp = ilp_data,
    s_eval = s_eval_data,
    schol_data = schol_data,
    p_miles = p_miles,                          # CLEAN milestone data for plots (no _desc fields)
    s_miles = s_miles,                          # CLEAN milestone data for plots (no _desc fields)
    p_miles_descriptions = p_miles_descriptions, # Milestone data WITH descriptions for tables
    s_miles_descriptions = s_miles_descriptions, # Milestone data WITH descriptions for tables
    ccc_review = ccc_review_data,
    url = config$url,
    eval_token = config$eval_token,
    rdm_token = config$rdm_token,
    fac_token = config$fac_token
  )
  
  # Final debugging check of the return value
  message("Return list contains these keys: ", paste(names(result_list), collapse=", "))
  message("ILP data in return list is NULL? ", is.null(result_list$ilp))
  
  message("=== FINISHED load_imres_data FUNCTION ===")
  return(result_list)
}

# ---------- GLOBAL APP DATA MANAGEMENT ----------
# Define a variable to hold app data
app_data_store <- NULL

# Updated ensure_data_loaded function with tryCatch for s_eval enhancement
ensure_data_loaded <- function() {
  if (is.null(app_data_store)) {
    # Only initialize data when needed
    message("Starting data load process...")
    config <- initialize_app_config()
    message("Config initialized")
    app_data_store <<- load_imres_data(config)
    message("Data loaded")
    
    # Remove the enhanced s_eval extraction section completely
    # The filters will handle this dynamically when needed
    
    # Add final check after data is loaded
    message("FINAL CHECK: app_data_store contains these keys:", paste(names(app_data_store), collapse=", "))
  }
  return(app_data_store)
}

# Add this to your global.R file after your existing setup

# ---------- MILESTONE IMAGES SETUP ----------

# Function to detect if running on Posit Connect
is_posit_connect <- function() {
  return(
    Sys.getenv("CONNECT_SERVER") != "" || 
      Sys.getenv("SHINY_PORT") != "" ||
      Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect" ||
      Sys.getenv("RSTUDIO_PROGRAM_MODE") == "server"
  )
}

# Setup milestone images (replaces your current setup_local_milestones function)
setup_milestone_images <- function() {
  message("=== Setting up milestone images ===")
  
  # Always check local directory first
  local_available <- dir.exists("www/milestones")
  
  if (local_available) {
    message("✓ Found local milestones directory: www/milestones")
    
    # Tell Shiny to serve files from www/milestones as /milestones
    shiny::addResourcePath("milestones", "www/milestones")
    
    # List available images
    available_images <- list.files("www/milestones", pattern = "\\.png$")
    message("Available local milestone images: ", length(available_images), " files")
    message("Sample images: ", paste(head(available_images, 3), collapse = ", "))
    
    # If we're on Posit Connect but have local images, that's ideal
    if (is_posit_connect()) {
      message("ℹ️  Running on Posit Connect with local images available")
    } else {
      message("ℹ️  Running locally with local images")
    }
    
    return(list(
      local = TRUE, 
      count = length(available_images),
      environment = if(is_posit_connect()) "posit_connect" else "local"
    ))
  } else {
    message("❌ Local milestones directory not found: www/milestones")
    
    if (is_posit_connect()) {
      message("ℹ️  Running on Posit Connect - will use GitHub images")
      message("GitHub base URL: https://raw.githubusercontent.com/fbuckhold3/imres.ccc.dashboard/main/www/milestones/")
    } else {
      message("⚠️  Running locally without local images - will use GitHub images")
    }
    
    return(list(
      local = FALSE, 
      count = 0,
      environment = if(is_posit_connect()) "posit_connect" else "local",
      github_url = "https://raw.githubusercontent.com/fbuckhold3/imres.ccc.dashboard/main/www/milestones/"
    ))
  }
}

# Call this function and store the result
milestone_images_config <- setup_milestone_images()