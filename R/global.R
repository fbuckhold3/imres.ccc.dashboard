# ---------- LIBRARY IMPORTS ----------
library(shiny)
library(shinyjs)
library(redcapAPI)
library(REDCapR)
library(ggplot2)
library(DT)
library(dplyr)
library(config)
library(imres)  # Make sure this package is installed
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
  # Set up REDCap API URL
  url <- "https://redcapsurvey.slu.edu/api/"
  
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
load_imres_data <- function(config) {
  message("=== STARTING load_imres_data FUNCTION ===")
  
  # --- RDM Dictionary ---
  rdm_dict <- tryCatch({
    message("Attempting to get rdm_dict data dictionary...")
    result <- get_data_dict(config$rdm_token, config$url)
    message("Successfully retrieved rdm_dict with ", nrow(result), " rows")
    result
  }, error = function(e) {
    message("Error getting rdm_dict: ", e$message)
    NULL
  })
  
  # --- Assessment Dictionary ---
  ass_dict <- tryCatch({
    message("Attempting to get ass_dict data dictionary...")
    result <- get_data_dict(config$eval_token, config$url)
    message("Successfully retrieved ass_dict with ", nrow(result), " rows")
    result
  }, error = function(e) {
    message("Error getting ass_dict: ", e$message)
    NULL
  })
  
  # --- Pull all forms data ---
  message("About to call forms_api_pull with these arguments:")
  message("- rdm_token length: ", nchar(config$rdm_token))
  message("- url: ", config$url)
  message("- forms: resident_data, faculty_evaluation, ilp, s_eval, scholarship")
  
  rdm_dat <- tryCatch({
    message("Pulling forms data...")
    result <- forms_api_pull(config$rdm_token, config$url, 'resident_data', 'faculty_evaluation', 'ilp', 's_eval', 'scholarship', 'ccc_review', 'coach_rev', 'second_review')
    
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
  # Additional debugging for career data rendering
  # Add this to your server.R file in the renderUI function for career_data_ui
  
  # Enhanced s_eval data extraction function with tryCatch
  # Add this to your global.R or helpers.R file
  
  get_s_eval_data <- function(rdm_data) {
    tryCatch({
      message("Starting enhanced s_eval data extraction")
      
      if (is.null(rdm_data)) {
        message("rdm_data is NULL, cannot extract s_eval data")
        return(NULL)
      }
      
      # Try multiple approaches to extract s_eval data
      
      # Approach 1: Check if s_eval is a direct component
      if ("s_eval" %in% names(rdm_data)) {
        message("Found s_eval data as a direct component in rdm_data")
        return(rdm_data$s_eval)
      }
      
      # Approach 2: Check if it's a capitalized key
      if ("S_eval" %in% names(rdm_data)) {
        message("Found S_eval data (capitalized) in rdm_data")
        return(rdm_data$S_eval)
      }
      
      # Approach 3: If rdm_data is a data frame with repeating instruments
      if (is.data.frame(rdm_data) && "redcap_repeat_instrument" %in% names(rdm_data)) {
        message("Looking for s_eval data in repeating instruments")
        
        # Print all unique values in redcap_repeat_instrument for debugging
        message("Unique redcap_repeat_instrument values: ", 
                paste(unique(rdm_data$redcap_repeat_instrument), collapse=", "))
        
        # Try case-insensitive match
        s_eval_rows <- rdm_data %>%
          filter(tolower(redcap_repeat_instrument) %in% c("s_eval", "self evaluation", "self_evaluation"))
        
        if (nrow(s_eval_rows) > 0) {
          message("Extracted ", nrow(s_eval_rows), " rows of s_eval data from main dataframe")
          return(s_eval_rows)
        }
        
        message("No rows found with matching redcap_repeat_instrument values")
      }
      
      # Approach 4: Look for s_eval in field names
      message("Searching for s_eval fields in rdm_data...")
      if (is.data.frame(rdm_data)) {
        s_eval_cols <- names(rdm_data)[grepl("s_e_", names(rdm_data), fixed = TRUE)]
        
        if (length(s_eval_cols) > 0) {
          message("Found ", length(s_eval_cols), " columns with 's_e_' prefix")
          message("Sample column names: ", paste(head(s_eval_cols, 5), collapse=", "))
          
          # If we found columns, return the dataframe with at least name column and all s_eval columns
          select_cols <- c("name", s_eval_cols)
          select_cols <- select_cols[select_cols %in% names(rdm_data)]
          
          if ("name" %in% select_cols) {
            s_eval_data <- rdm_data[, select_cols]
            message("Created s_eval dataframe with ", nrow(s_eval_data), " rows and ", ncol(s_eval_data), " columns")
            return(s_eval_data)
          } else {
            message("WARNING: Found s_eval columns but no 'name' column")
          }
        }
      }
      
      # Approach 5: Look for objects with career paths/fellowship fields
      message("Searching for career fields in rdm_data components...")
      
      if (is.list(rdm_data) && !is.data.frame(rdm_data)) {
        for (component_name in names(rdm_data)) {
          component <- rdm_data[[component_name]]
          
          if (is.data.frame(component)) {
            career_cols <- names(component)[grepl("career|fellow|track", names(component), ignore.case = TRUE)]
            
            if (length(career_cols) > 0) {
              message("Found career fields in component '", component_name, "': ", 
                      paste(head(career_cols, 5), collapse=", "))
              return(component)
            }
          }
        }
      }
      
      message("No s_eval data found using any extraction method")
      return(NULL)
    }, 
    error = function(e) {
      message("ERROR in get_s_eval_data: ", e$message)
      # Print stack trace for debugging
      message("Stack trace:")
      print(sys.calls())
      # Return NULL on error
      return(NULL)
    },
    warning = function(w) {
      message("WARNING in get_s_eval_data: ", w$message)
      # Continue execution
      NULL
    },
    finally = {
      message("Completed s_eval data extraction attempt")
    })
  }
  
  # Updated ensure_data_loaded function with tryCatch for s_eval enhancement
  ensure_data_loaded <- function() {
    if (is.null(app_data_store)) {
      # Only initialize data when needed
      message("Starting data load process...")
      config <- initialize_app_config()
      message("Config initialized")
      app_data_store <<- load_imres_data(config)
      message("Data loaded")
      
      # Add enhanced s_eval extraction if it's not present
      if (is.null(app_data_store$s_eval)) {
        message("s_eval data is NULL, trying enhanced extraction...")
        
        # Use tryCatch for the enhancement attempt
        tryCatch({
          app_data_store$s_eval <<- get_s_eval_data(app_data_store)
          message("Enhanced s_eval extraction complete. s_eval is now ", 
                  ifelse(is.null(app_data_store$s_eval), "still NULL", "available"))
        },
        error = function(e) {
          message("ERROR during enhanced s_eval extraction: ", e$message)
          # Don't modify app_data_store on error
        })
      }
      
      # Add final check after data is loaded
      message("FINAL CHECK: app_data_store contains these keys:", paste(names(app_data_store), collapse=", "))
      message("FINAL CHECK: s_eval data is NULL?", is.null(app_data_store$s_eval))
    }
    return(app_data_store)
  }
  
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
    p_miles = p_miles,
    s_miles = s_miles,
    ccc_review = ccc_review_data,  # Add this line
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

setup_imres_resources <- function() {
  # Set up resource paths for imres package images
  tryCatch({
    # Check if imres package is available
    if (requireNamespace("imres", quietly = TRUE)) {
      img_path <- system.file("www", package = "imres")
      
      if (dir.exists(img_path)) {
        message("Found imres www directory at: ", img_path)
        
        # Use shiny namespace to call addResourcePath
        shiny::addResourcePath("imres-images", img_path)
        message("Successfully set up imres-images resource path")
        
        # Check if milestones subdirectory exists
        milestone_path <- file.path(img_path, "milestones")
        if (dir.exists(milestone_path)) {
          milestone_files <- list.files(milestone_path, pattern = "\\.png$")
          message("Found ", length(milestone_files), " milestone images")
          if (length(milestone_files) > 0) {
            message("Sample milestone images: ", paste(head(milestone_files, 3), collapse = ", "))
          }
        } else {
          message("No milestones subdirectory found in imres package")
        }
      } else {
        warning("Could not find imres www directory")
      }
    } else {
      warning("imres package not available")
    }
  }, error = function(e) {
    warning("Error setting up imres resource path: ", e$message)
  })
}

# Call this function when the app starts
setup_imres_resources()

