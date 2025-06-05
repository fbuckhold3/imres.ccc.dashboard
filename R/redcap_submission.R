# ============================================================================
# CONSOLIDATED REDCap INSTANCE AND PERIOD MAPPING FUNCTIONS
# ============================================================================
# Replace ALL the overlapping functions in R/redcap_submission.R with these

#' Get REDCap Instance Number - CONSOLIDATED VERSION
#' 
#' This is the ONLY function you should use for getting REDCap instances
#' Handles both standard periods (1-7) and interim reviews (9+)
#'
#' @param level Resident level (Intern, PGY2, PGY3)
#' @param period Period name (can be app format, milestone format, or direct)
#' @param review_type Type of review ("scheduled" or "interim")
#' @param redcap_url REDCap API URL (only needed for interim reviews)
#' @param redcap_token REDCap token (only needed for interim reviews)
#' @param record_id Record ID (only needed for interim reviews)
#' @return Numeric instance number
get_redcap_instance <- function(level, period, review_type = "scheduled", 
                                redcap_url = NULL, redcap_token = NULL, record_id = NULL) {
  
  message("get_redcap_instance called with level: ", level, ", period: ", period, ", review_type: ", review_type)
  
  # Handle interim reviews differently
  if (review_type == "interim") {
    if (is.null(redcap_url) || is.null(redcap_token) || is.null(record_id)) {
      warning("Interim review requires redcap_url, redcap_token, and record_id")
      return(9)  # Default to first interim instance
    }
    
    # Get next available interim instance (9+)
    return(get_next_interim_instance(redcap_url, redcap_token, record_id, "ccc_review"))
  }
  
  # STANDARD SCHEDULED REVIEWS - Direct mapping table
  instance_map <- list(
    "Intern" = list(
      # App format periods
      "Intern Intro" = 7,
      "Mid Review" = 1,
      "End Review" = 2,
      # Direct milestone format periods
      "Mid Intern" = 1,
      "End Intern" = 2
    ),
    "PGY2" = list(
      # App format periods  
      "Intern Intro" = 8,  # Fallback for PGY2 in intro period
      "Mid Review" = 3,
      "End Review" = 4,
      # Direct milestone format periods
      "Mid PGY2" = 3,
      "End PGY2" = 4
    ),
    "PGY3" = list(
      # App format periods
      "Intern Intro" = 8,  # Fallback for PGY3 in intro period
      "Mid Review" = 5,
      "End Review" = 6,
      # Direct milestone format periods
      "Mid PGY3" = 5,
      "Graduation" = 6,
      "Graduating" = 6
    )
  )
  
  # First try direct mapping
  if (level %in% names(instance_map) && period %in% names(instance_map[[level]])) {
    instance <- instance_map[[level]][[period]]
    message("Direct mapping found: ", level, " + ", period, " -> instance ", instance)
    return(instance)
  }
  
  # Fallback to numeric period codes (CCC session values)
  period_codes <- c(
    "1" = 1,  # Mid Intern
    "2" = 2,  # End Intern
    "3" = 3,  # Mid PGY2
    "4" = 4,  # End PGY2
    "5" = 5,  # Mid PGY3
    "6" = 6,  # Graduation
    "7" = 7   # Intern Intro
  )
  
  if (period %in% names(period_codes)) {
    instance <- period_codes[[period]]
    message("Period code mapping found: ", period, " -> instance ", instance)
    return(instance)
  }
  
  # Final fallback
  warning("Could not map level ", level, " and period ", period, " to instance. Using default 8.")
  return(8)
}

#' Get Next Available Interim Instance
#' 
#' Finds the next available instance number for interim reviews (starts at 9)
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Record ID
#' @param form_name REDCap form name (default: "ccc_review")
#' @return Numeric instance number (9 or higher)
get_next_interim_instance <- function(redcap_url, redcap_token, record_id, form_name = "ccc_review") {
  
  message("Finding next interim instance for record ", record_id, " in form ", form_name)
  
  # Get existing instances for this record and form
  response <- tryCatch({
    httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        forms = form_name,
        fields = "record_id,redcap_repeat_instance",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
  }, error = function(e) {
    message("Error checking existing instances: ", e$message)
    return(NULL)
  })
  
  if (!is.null(response) && httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    
    tryCatch({
      data <- jsonlite::fromJSON(content)
      
      if (is.data.frame(data) && nrow(data) > 0) {
        # Get all existing instances for this form
        existing_instances <- as.numeric(data$redcap_repeat_instance)
        existing_instances <- existing_instances[!is.na(existing_instances)]
        
        if (length(existing_instances) > 0) {
          # Return the next available instance (starting from 9 for interim reviews)
          max_instance <- max(existing_instances)
          next_instance <- max(max_instance + 1, 9)  # Ensure it's at least 9
          message("Found existing instances: ", paste(existing_instances, collapse = ", "))
          message("Using next available instance: ", next_instance)
          return(next_instance)
        }
      }
    }, error = function(e) {
      message("Error parsing instance data: ", e$message)
    })
  }
  
  # Default to instance 9 for first interim review
  message("No existing instances found, using default instance 9 for interim review")
  return(9)
}

# ============================================================================
# REMOVE/REPLACE THESE DUPLICATE FUNCTIONS
# ============================================================================
# The following functions should be REMOVED from redcap_submission.R:
# 1. get_redcap_instance() (if it exists elsewhere)
# 2. map_inputs_to_coach_rev() - the instance mapping part
# 3. Any other instance mapping functions

# ============================================================================
# UPDATED SUBMISSION FUNCTIONS
# ============================================================================

#' Submit milestone data to REDCap - UPDATED VERSION
submit_milestone_data <- function(redcap_url, redcap_token, record_id, selected_period, 
                                  resident_level, milestone_scores, milestone_desc = list()) {
  
  # Validate record_id first
  if (is.null(record_id) || length(record_id) == 0) {
    error_msg <- "ERROR: Cannot submit milestone data - record_id is NULL or empty"
    message(error_msg)
    return(list(
      success = FALSE,
      outcome_message = error_msg
    ))
  }
  
  # Convert record_id to character and ensure it's a single value
  record_id <- as.character(record_id)[1]
  
  # SIMPLIFIED: Use the consolidated instance function
  instance_number <- get_redcap_instance(
    level = resident_level,
    period = selected_period,
    review_type = "scheduled"  # Milestones are always scheduled reviews
  )
  
  message("Using instance number: ", instance_number, " for milestone submission")
  
  if (is.na(instance_number) || is.null(instance_number)) {
    return(list(
      success = FALSE,
      outcome_message = paste("Could not map period", selected_period, 
                              "with level", resident_level, "to a REDCap instance.")
    ))
  }
  
  # Format today's date properly for REDCap as YYYY-MM-DD
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Build fields list with all milestone data
  fields <- list(
    prog_mile_date = today_date,
    prog_mile_period = as.character(instance_number)  # Use same as instance for simplicity
  )
  
  # Add milestone scores
  if (!is.null(milestone_scores) && length(milestone_scores) > 0) {
    for(key in names(milestone_scores)) {
      fields[[key]] <- as.character(milestone_scores[[key]])
      
      # Add description if it exists
      if (!is.null(milestone_desc) && length(milestone_desc) > 0 && 
          key %in% names(milestone_desc) && 
          !is.null(milestone_desc[[key]]) &&
          nzchar(trimws(milestone_desc[[key]]))) {
        desc_field <- paste0(key, "_desc")
        fields[[desc_field]] <- as.character(milestone_desc[[key]])
      }
    }
  }
  
  # Get existing milestone data to preserve unchanged values
  message("Retrieving existing milestone data to preserve unchanged values...")
  
  existing_response <- tryCatch({
    httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "export",
        format = "json",
        type = "flat",
        records = as.character(record_id),
        forms = "milestone_entry",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
  }, error = function(e) {
    message("Error retrieving existing data: ", e$message)
    return(NULL)
  })
  
  # Parse existing data and merge with new data
  if (!is.null(existing_response) && httr::status_code(existing_response) == 200) {
    existing_content <- httr::content(existing_response, "text", encoding = "UTF-8")
    
    tryCatch({
      existing_data <- jsonlite::fromJSON(existing_content)
      
      if (is.data.frame(existing_data) && nrow(existing_data) > 0) {
        # Find matching records with safer filtering
        matching_rows <- existing_data[
          !is.na(existing_data$record_id) & existing_data$record_id == record_id & 
            !is.na(existing_data$redcap_repeat_instrument) & existing_data$redcap_repeat_instrument == "milestone_entry" &
            !is.na(existing_data$redcap_repeat_instance) & existing_data$redcap_repeat_instance == instance_number, 
        ]
        
        if (nrow(matching_rows) > 0) {
          matching_row <- matching_rows[1, ]
          message("Found existing milestone data for this instance, preserving unchanged values")
          
          # All milestone field names
          all_milestone_fields <- c(
            "rep_pc1", "rep_pc2", "rep_pc3", "rep_pc4", "rep_pc5", "rep_pc6",
            "rep_mk1", "rep_mk2", "rep_mk3", "rep_sbp1", "rep_sbp2", "rep_sbp3",
            "rep_pbl1", "rep_pbl2", "rep_prof1", "rep_prof2", "rep_prof3", 
            "rep_prof4", "rep_ics1", "rep_ics2", "rep_ics3"
          )
          
          # Preserve existing values for fields we're not updating
          for (field in all_milestone_fields) {
            # Check if this field is being updated
            field_being_updated <- FALSE
            if (!is.null(milestone_scores) && is.list(milestone_scores)) {
              field_being_updated <- field %in% names(milestone_scores)
            }
            
            # If not being updated and exists in existing data, preserve it
            if (!field_being_updated && field %in% names(matching_row)) {
              existing_value <- matching_row[[field]][1]
              if (!is.null(existing_value) && !is.na(existing_value) && existing_value != "") {
                fields[[field]] <- as.character(existing_value)
                message("Preserving existing value for ", field, ": ", existing_value)
              }
            }
          }
        }
      }
    }, error = function(e) {
      message("Error parsing existing data, proceeding with new data only: ", e$message)
    })
  }
  
  # Build JSON for submission
  data_str <- '['
  data_str <- paste0(data_str, '{"record_id":"', escape_json_string(as.character(record_id)), '"')
  data_str <- paste0(data_str, ',"redcap_repeat_instrument":"milestone_entry"')
  data_str <- paste0(data_str, ',"redcap_repeat_instance":"', escape_json_string(as.character(instance_number)), '"')
  
  # Add all fields
  for (field in names(fields)) {
    if (!is.null(fields[[field]]) && !is.na(fields[[field]])) {
      value <- escape_json_string(as.character(fields[[field]]))
      data_str <- paste0(data_str, ',"', field, '":"', value, '"')
    }
  }
  
  data_str <- paste0(data_str, "}]")
  
  message("Submitting milestone data (first 150 chars): ", substr(data_str, 1, 150))
  
  # Submit to REDCap
  response <- tryCatch({
    httr::POST(
      url = redcap_url,
      body = list(
        token = redcap_token,
        content = "record",
        action = "import",
        format = "json",
        type = "flat",
        overwriteBehavior = "overwrite",
        forceAutoNumber = "false",
        data = data_str,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form",
      httr::timeout(30)
    )
  }, error = function(e) {
    return(list(status_code = 0, error = e$message))
  })
  
  # Process response
  if ("error" %in% names(response)) {
    return(list(
      success = FALSE,
      outcome_message = paste("HTTP error:", response$error)
    ))
  }
  
  status_code <- httr::status_code(response)
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  message("REDCap API response status: ", status_code)
  message("REDCap API response: ", response_content)
  
  if (status_code == 200) {
    tryCatch({
      if (grepl("^\\{", response_content)) {
        response_json <- jsonlite::fromJSON(response_content)
        records_updated <- as.numeric(response_json$count)
      } else {
        records_updated <- as.numeric(response_content)
      }
      
      if (!is.na(records_updated) && records_updated > 0) {
        return(list(
          success = TRUE,
          outcome_message = paste("Milestone data successfully submitted. Records updated:", records_updated)
        ))
      } else {
        return(list(
          success = FALSE,
          outcome_message = "No records were updated in REDCap"
        ))
      }
    }, error = function(e) {
      return(list(
        success = TRUE,
        outcome_message = "Milestone data submitted (response parsing failed but HTTP 200 received)"
      ))
    })
  } else {
    if (grepl("Form Status field", response_content)) {
      return(list(
        success = TRUE,
        outcome_message = "Milestone data saved (form status field warning ignored)"
      ))
    } else {
      return(list(
        success = FALSE,
        outcome_message = paste("REDCap error (", status_code, "):", response_content)
      ))
    }
  }
}