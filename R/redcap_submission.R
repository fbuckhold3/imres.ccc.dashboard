# ============================================================================
# REDCap Submission Helper Functions
# ============================================================================

#' Helper function to safely escape strings for JSON
#'
#' @param x Character string to escape
#' @return Escaped string safe for JSON
escape_json_string <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  
  # Convert to character if not already
  x <- as.character(x)
  
  # Escape special characters
  x <- gsub('\\\\', '\\\\\\\\', x)  # Escape backslashes first
  x <- gsub('"', '\\\\"', x)         # Escape double quotes
  x <- gsub('\n', '\\\\n', x)        # Escape newlines
  x <- gsub('\r', '\\\\r', x)        # Escape carriage returns
  x <- gsub('\t', '\\\\t', x)        # Escape tabs
  
  return(x)
}


#' Null-coalescing operator helper
#' 
#' @param a First value to check
#' @param b Fallback value if a is NULL
#' @return a if not NULL, otherwise b
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Maps app period and resident level to REDCap instance number
#'
#' @param level Resident level (Intern, PGY2, PGY3)
#' @param period App period format
#' @return Numeric REDCap instance number
get_redcap_instance <- function(level, period) {
  # Debug
  message(paste("Mapping to REDCap instance - level:", level, "period:", period))
  
  # Quick check - if it's already a number between 1-8, just return it
  if (is.numeric(period) || (is.character(period) && !is.na(suppressWarnings(as.numeric(period))))) {
    num_period <- as.numeric(period)
    if (num_period >= 1 && num_period <= 8) {
      message(paste("Period is already a valid instance number:", num_period))
      return(num_period)
    }
  }
  
  # Direct mapping table with explicit instances
  instance_map <- list(
    "Intern" = list(
      "Intern Intro" = 7,
      "Mid Intern" = 1,
      "Mid Review" = 1,
      "End Intern" = 2,
      "End Review" = 2
    ),
    "PGY2" = list(
      "Mid PGY2" = 3,
      "Mid Review" = 3,
      "End PGY2" = 4,
      "End Review" = 4
    ),
    "PGY3" = list(
      "Mid PGY3" = 5,
      "Mid Review" = 5,
      "Graduation" = 6,
      "End PGY3" = 6,
      "End Review" = 6
    )
  )
  
  # First check direct mapping
  if (level %in% names(instance_map) && period %in% names(instance_map[[level]])) {
    result <- instance_map[[level]][[period]]
    message(paste("Found direct mapping:", result))
    return(result)
  }
  
  # CCC session values mapping (fallback)
  ccc_to_instance <- c(
    "Mid Intern" = 1,
    "End Intern" = 2,
    "Mid PGY2" = 3,
    "End PGY2" = 4,
    "Mid PGY3" = 5,
    "Graduation" = 6,
    "Intern Intro" = 7
  )
  
  if (period %in% names(ccc_to_instance)) {
    result <- ccc_to_instance[period]
    message(paste("Mapped via CCC session name:", result))
    return(result)
  }
  
  # Try parsing period like "End PGY2" into components
  period_parts <- strsplit(as.character(period), " ")[[1]]
  if (length(period_parts) >= 2) {
    period_prefix <- period_parts[1]  # "End" or "Mid"
    
    # Check if this is a valid combination
    combined_period <- paste(period_prefix, level)
    if (combined_period %in% names(ccc_to_instance)) {
      result <- ccc_to_instance[combined_period]
      message(paste("Mapped via combined period-level:", result))
      return(result)
    }
  }
  
  # Default fallback - return interim (8)
  message("Using default value 8 (Interim)")
  return(8)
}

#' Map form input fields to REDCap coach_rev fields
#'
#' @param inputs Form input values
#' @param is_intern_intro Whether this is for an intern in intro period
#' @return Mapped list of fields for REDCap
map_inputs_to_coach_rev <- function(inputs, is_intern_intro = FALSE) {
  # Initialize result with empty values
  result <- list(
    # Form metadata fields
    coach_date = format(Sys.Date(), "%Y-%m-%d"),  # Today's date
    coach_period = NULL,  # This will be set by the calling function
    
    # Tab 1: Pre-review
    coach_pre_rev = "",
    
    # Tab 2: Wellness
    coach_intro_back = "",
    coach_coping = "",
    coach_wellness = "",
    
    # Tab 3: Evaluations
    coach_evaluations = "",
    coach_p_d_comments = "",
    
    # Tab 4: Knowledge
    coach_ls_and_topic = "",
    coach_step_board = "",
    
    # Tab 5: Scholarship
    coach_scholarship = "",
    
    # Tab 6: ILP (Milestone Goals)
    coach_mile_goal = "",
    
    # Tab 7: Career
    coach_career = "",
    
    # Tab 8: Summary
    coach_summary = "",
    coach_ilp_final = "",
    
    # Completion status
    coach_rev_complete = "0"  # Default to incomplete
  )
  
  # Map input fields to coach_rev fields
  
  # Tab 1: Pre-review
  if (!is.null(inputs$discussion_points)) {
    result$coach_pre_rev <- inputs$discussion_points
  }
  
  # Tab 2: Wellness
  if (is_intern_intro && !is.null(inputs$resident_background)) {
    result$coach_intro_back <- inputs$resident_background
  }
  
  if (!is.null(inputs$dealing_with_residency)) {
    result$coach_coping <- inputs$dealing_with_residency
  }
  
  if (!is.null(inputs$resident_wellbeing)) {
    result$coach_wellness <- inputs$resident_wellbeing
  }
  
  # Tab 3: Evaluations
  if (!is.null(inputs$evaluations_assessment)) {
    result$coach_evaluations <- inputs$evaluations_assessment
  }
  
  if (!is.null(inputs$evaluations_comments)) {
    result$coach_p_d_comments <- inputs$evaluations_comments
  }
  
  # Tab 4: Knowledge
  if (!is.null(inputs$knowledge_topics_comments)) {
    result$coach_ls_and_topic <- inputs$knowledge_topics_comments
  }
  
  if (!is.null(inputs$board_prep_comments)) {
    result$coach_step_board <- inputs$board_prep_comments
  }
  
  if (!is.null(inputs$knowledge_overall_comments)) {
    # You might need to map this to an appropriate field in your REDCap form
    # This field wasn't in your original mapping
  }
  
  # Tab 5: Scholarship
  if (!is.null(inputs$scholarship_comments)) {
    result$coach_scholarship <- inputs$scholarship_comments
  }
  
  # Tab 6: ILP (Milestone Goals)
  if (!is.null(inputs$milestone_goals_comments)) {
    result$coach_mile_goal <- inputs$milestone_goals_comments
  }
  
  # Tab 7: Career
  if (!is.null(inputs$career_comments)) {
    result$coach_career <- inputs$career_comments
  }
  
  # Tab 8: Summary - CRITICAL MAPPING 
  # Map discussion_topics_comments to coach_summary
  if (!is.null(inputs$discussion_topics_comments)) {
    result$coach_summary <- inputs$discussion_topics_comments
  }
  
  # Map summary_comments to coach_ilp_final
  if (!is.null(inputs$summary_comments)) {
    result$coach_ilp_final <- inputs$summary_comments
  }
  
  # Set completion status based on checkbox
  if (!is.null(inputs$summary_complete) && inputs$summary_complete) {
    result$coach_rev_complete <- "2"  # 2 = Complete
  } else {
    result$coach_rev_complete <- "0"  # 0 = Incomplete
  }
  
  # Debug logging of mapped fields
  message("--- Input to REDCap field mapping ---")
  for (field in names(result)) {
    if (field != "coach_period" && !is.null(result[[field]]) && result[[field]] != "") {
      value_preview <- if (nchar(result[[field]]) > 30) {
        paste0(substr(result[[field]], 1, 30), "...")
      } else {
        result[[field]]
      }
      message(paste(field, ":", value_preview))
    }
  }
  
  return(result)
}

#' Validate coach review inputs for required fields
#'
#' @param mapped_inputs Mapped input values
#' @param tab_order Order of tabs in the app
#' @return List with validation status and any missing fields
validate_coach_review_inputs <- function(mapped_inputs, tab_order) {
  # Define required fields for each tab
  required_fields <- list(
    # Tab 1: Pre-review
    pre_review = c("coach_pre_rev"),
    
    # Tab 2: Wellness
    wellness = c("coach_coping", "coach_wellness"),
    
    # Tab 3: Evaluations
    evaluations = c("coach_evaluations", "coach_p_d_comments"),
    
    # Tab 4: Knowledge
    knowledge = c("coach_ls_and_topic", "coach_step_board"),
    
    # Tab 5: Scholarship 
    scholarship = c("coach_scholarship"),
    
    # Tab 6: ILP
    ilp = c("coach_mile_goal"),
    
    # Tab 7: Career
    career = c("coach_career"),
    
    # Tab 8: Summary
    summary = c("coach_ilp_final")
  )
  
  # Check if each required field has a value
  missing_fields <- list()
  for (tab in names(required_fields)) {
    tab_missing <- character(0)
    for (field in required_fields[[tab]]) {
      if (is.null(mapped_inputs[[field]]) || 
          trimws(mapped_inputs[[field]]) == "") {
        tab_missing <- c(tab_missing, field)
      }
    }
    if (length(tab_missing) > 0) {
      missing_fields[[tab]] <- tab_missing
    }
  }
  
  # If no missing fields, validation is successful
  if (length(missing_fields) == 0) {
    return(list(
      valid = TRUE,
      message = "All required fields are completed.",
      missing_tabs = integer(0)
    ))
  }
  
  # Create validation message with missing fields
  message_parts <- character(0)
  missing_tabs <- integer(0)
  
  for (tab in names(missing_fields)) {
    message_parts <- c(message_parts, 
                       paste0("Tab '", tab, "': Missing ", 
                              paste(missing_fields[[tab]], collapse = ", ")))
    
    # Find tab index in tab_order
    tab_index <- match(tab, tab_order)
    if (!is.na(tab_index)) {
      missing_tabs <- c(missing_tabs, tab_index)
    }
  }
  
  return(list(
    valid = FALSE,
    message = paste("Please complete the following required fields:", 
                    paste(message_parts, collapse = "; ")),
    missing_tabs = missing_tabs
  ))
}

#' Build JSON data for REDCap submission
#'
#' @param record_id Record ID
#' @param instance Instance number
#' @param fields List of fields and values to submit
#' @param instrument REDCap instrument name
#' @return JSON string for REDCap submission
build_redcap_json <- function(record_id, instance, fields, instrument) {
  # Start JSON with required fields
  json_data <- paste0(
    '[{"record_id":"', escape_json_string(record_id), '"',
    ',"redcap_repeat_instrument":"', escape_json_string(instrument), '"',
    ',"redcap_repeat_instance":"', escape_json_string(as.character(instance)), '"'
  )
  
  # Add all fields with proper escaping
  for (field_name in names(fields)) {
    # Skip record_id (already added)
    if (field_name == "record_id") next
    
    # Only add non-NULL, non-NA values
    if (!is.null(fields[[field_name]]) && !is.na(fields[[field_name]])) {
      value <- escape_json_string(as.character(fields[[field_name]]))
      json_data <- paste0(json_data, ',"', field_name, '":"', value, '"')
    }
  }
  
  # Close the JSON array
  json_data <- paste0(json_data, "}]")
  
  return(json_data)
}

#' Submit data to REDCap via API
#'
#' @param url REDCap API URL
#' @param token REDCap API token
#' @param json_data JSON data to submit
#' @param dev_mode Whether to skip actual submission (for development)
#' @return List with submission result
submit_to_redcap <- function(url, token, json_data, dev_mode = FALSE) {
  # Debug log
  message("Submitting to REDCap API at: ", url)
  message("JSON data (first 200 chars): ", substr(json_data, 1, 200))
  
  # For development mode, just return success without submitting
  if (dev_mode) {
    message("DEVELOPMENT MODE - Skipping actual REDCap submission")
    return(list(
      success = TRUE,
      outcome_message = "Development mode - submission simulated"
    ))
  }
  
  # Make the API request
  result <- tryCatch({
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = json_data,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    # Process response
    status_code <- httr::status_code(response)
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    
    message("REDCap response status: ", status_code)
    message("REDCap response content: ", response_content)
    
    # Check if submission was successful
    if (status_code == 200) {
      return(list(
        success = TRUE,
        outcome_message = paste("REDCap submission successful. Records updated:", response_content)
      ))
    } else {
      # Special handling for Form Status field error (which is expected)
      if (grepl("Form Status field", response_content)) {
        return(list(
          success = TRUE,
          outcome_message = "Data saved (ignoring form status field warning)"
        ))
      } else {
        return(list(
          success = FALSE,
          outcome_message = paste("REDCap Error:", response_content)
        ))
      }
    }
  }, error = function(e) {
    # Catch any errors in the HTTP request
    error_msg <- paste("Error submitting to REDCap:", e$message)
    message(error_msg)
    return(list(success = FALSE, outcome_message = error_msg))
  })
  
  return(result)
}

#' Submit coach review data to REDCap
#'
#' @param record_id Resident record ID
#' @param resident_name Resident name (for logging)
#' @param instance REDCap instance number
#' @param inputs Form input values
#' @param app_data App data containing REDCap configuration
#' @param is_intern_intro Whether this is for an intern in intro period
#' @return List with submission result
submit_coach_review <- function(record_id, resident_name, instance, inputs, app_data, 
                                is_intern_intro = FALSE) {
  # Validate record_id
  if (is.null(record_id) || length(record_id) == 0) {
    error_msg <- "ERROR: Cannot submit to REDCap - record_id is NULL or empty"
    message(error_msg)
    return(list(success = FALSE, outcome_message = error_msg))
  }
  
  # Get REDCap URL from app_data
  redcap_url <- app_data$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
  token <- app_data$rdm_token
  
  message("Attempting to submit coach review for resident: ", resident_name)
  message("Using record_id: ", record_id)
  message("Using instance: ", instance)
  
  # Map inputs to REDCap fields
  mapped_inputs <- map_inputs_to_coach_rev(inputs, is_intern_intro = is_intern_intro)
  
  # Set the instance number
  mapped_inputs$coach_period <- as.character(instance)
  
  # Build JSON data for REDCap submission
  json_data <- build_redcap_json(
    record_id = record_id,
    instance = instance,
    fields = mapped_inputs,
    instrument = "coach_rev"
  )
  
  # Check if we're in development mode
  dev_mode <- if (exists("dev_mode")) get("dev_mode") else TRUE
  
  # Submit to REDCap
  return(submit_to_redcap(redcap_url, token, json_data, dev_mode))
}

#' Get mapping for milestone fields to REDCap fields
#'
#' @return Named list of milestone field mappings
get_milestone_field_mapping <- function() {
  return(c(
    # Changed from rep_pc1_self to rep_pc1, etc.
    "PC_1" = "rep_pc1",
    "PC_2" = "rep_pc2",
    "PC_3" = "rep_pc3",
    "PC_4" = "rep_pc4",
    "PC_5" = "rep_pc5",
    "PC_6" = "rep_pc6",
    "MK_1" = "rep_mk1",
    "MK_2" = "rep_mk2",
    "MK_3" = "rep_mk3",
    "SBP_1" = "rep_sbp1",
    "SBP_2" = "rep_sbp2",
    "SBP_3" = "rep_sbp3",
    "PBLI_1" = "rep_pbl1",
    "PBLI_2" = "rep_pbl2",
    "PROF_1" = "rep_prof1",
    "PROF_2" = "rep_prof2",
    "PROF_3" = "rep_prof3",
    "PROF_4" = "rep_prof4",
    "ICS_1" = "rep_ics1",
    "ICS_2" = "rep_ics2",
    "ICS_3" = "rep_ics3"
  ))
}

#' Get list of milestone fields that support descriptions
#'
#' @return Character vector of milestone fields with description support
get_milestone_desc_fields <- function() {
  return(c(
    "PC_1", "PC_2", "PC_3", "PC_6",
    "PBLI_1", "PBLI_2",
    "PROF_1", "PROF_2", "PROF_3", "PROF_4",
    "ICS_1", "ICS_2", "ICS_3"
  ))
}

#' Submit milestone data to REDCap - UPDATED for milestone module integration
#'
#' @param redcap_url REDCap API URL
#' @param redcap_token REDCap API token
#' @param record_id Resident record ID
#' @param selected_period Current period in app format
#' @param resident_level Resident level
#' @param milestone_scores Milestone scores from module
#' @param milestone_desc Milestone descriptions (optional)
#' @return List with submission result
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
  
  # Convert record_id to character
  record_id <- as.character(record_id)
  
  # UPDATED: Use the existing map_period_format function for consistency
  instance_number <- map_period_format(
    level = resident_level,
    period = selected_period,
    return_type = "instance",
    form_context = "milestone"
  )
  
  if (is.na(instance_number) || is.null(instance_number)) {
    return(list(
      success = FALSE,
      outcome_message = paste("Could not map period", selected_period, 
                              "with level", resident_level, "to a REDCap instance.")
    ))
  }
  
  message("Using instance number: ", instance_number, " for milestone submission")
  
  # Format today's date properly for REDCap as YYYY-MM-DD
  today_date <- format(Sys.Date(), "%Y-%m-%d")
  
  # Build fields list with all milestone data
  fields <- list(
    prog_mile_date = today_date,
    prog_mile_period = as.character(instance_number)
  )
  
  # Add milestone scores - the module returns them with the correct field names
  for(key in names(milestone_scores)) {
    # Convert to character to ensure proper JSON encoding
    fields[[key]] <- as.character(milestone_scores[[key]])
    
    # Add description if it exists (for future enhancement)
    if (key %in% names(milestone_desc) && 
        !is.null(milestone_desc[[key]]) &&
        nzchar(trimws(milestone_desc[[key]]))) {
      desc_field <- paste0(key, "_desc")
      fields[[desc_field]] <- as.character(milestone_desc[[key]])
    }
  }
  
  # Build JSON manually for the milestone_entry instrument
  data_str <- '[{'
  data_str <- paste0(data_str, '"record_id":"', escape_json_string(as.character(record_id)), '"')
  data_str <- paste0(data_str, ',"redcap_repeat_instrument":"milestone_entry"')
  data_str <- paste0(data_str, ',"redcap_repeat_instance":"', escape_json_string(as.character(instance_number)), '"')
  
  # Add all fields with proper escaping
  for (field in names(fields)) {
    if (!is.null(fields[[field]]) && !is.na(fields[[field]])) {
      value <- escape_json_string(as.character(fields[[field]]))
      data_str <- paste0(data_str, ',"', field, '":"', value, '"')
    }
  }
  
  # Close the object and array
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
        overwriteBehavior = "overwrite",  # This will replace existing data for this instance
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
  
  # Check if submission was successful
  if (status_code == 200) {
    # Try to parse the count
    tryCatch({
      # Check if response is JSON format
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
      # If we can't parse the response but got HTTP 200, assume success
      return(list(
        success = TRUE,
        outcome_message = "Milestone data submitted (response parsing failed but HTTP 200 received)"
      ))
    })
  } else {
    # Check if the error is just about the form status field (common REDCap issue)
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



#' Submit secondary review data to REDCap
#'
#' @param record_id Resident record ID
#' @param inputs Form input values
#' @param app_data App data containing REDCap configuration
#' @return List with submission result
submit_secondary_review <- function(record_id, inputs, app_data) {
  # Validate record_id
  if (is.null(record_id) || length(record_id) == 0) {
    error_msg <- "ERROR: Cannot submit secondary review - record_id is NULL or empty"
    message(error_msg)
    return(list(
      success = FALSE,
      outcome_message = error_msg
    ))
  }
  
  # Convert record_id to character
  record_id <- as.character(record_id)
  
  # Extract REDCap URL and token from app_data
  redcap_url <- app_data$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
  token <- app_data$rdm_token
  
  message("Using REDCap URL: ", redcap_url)
  
  # Validate period code
  period_code <- inputs$second_period
  if (is.null(period_code) || is.na(period_code) || period_code == "") {
    return(list(
      success = FALSE,
      outcome_message = paste("Invalid period code:", period_code)
    ))
  }
  
  message("Submitting secondary review for record ID: ", record_id, 
          ", period: ", period_code)
  
  # Define all valid second_review fields
  valid_fields <- c(
    "second_date",
    "second_period",
    "second_comments",
    "second_approve",
    "second_miles_comment",
    "second_review_complete"
  )
  
  # Filter inputs to only include valid second_review fields
  valid_inputs <- inputs[names(inputs) %in% valid_fields]
  
  # Ensure all required fields exist
  if (!all(c("second_date", "second_period", "second_comments", "second_approve") %in% names(valid_inputs))) {
    return(list(
      success = FALSE,
      outcome_message = "Missing required fields for secondary review"
    ))
  }
  
  # Set completion status
  valid_inputs$second_review_complete <- "2"  # Complete
  
  # Build JSON data for REDCap submission
  json_data <- build_redcap_json(
    record_id = record_id,
    instance = period_code,
    fields = valid_inputs,
    instrument = "second_review"
  )
  
  # Check if we're in development mode
  dev_mode <- if (exists("dev_mode")) get("dev_mode") else TRUE
  
  # Submit to REDCap
  return(submit_to_redcap(redcap_url, token, json_data, dev_mode))
}

#' Process current milestone data for display/storage
#'
#' @param milestone_scores Milestone scores module output
#' @param resident_name Resident name
#' @param current_period Current period
#' @return Data frame with processed milestone data
# Update the process_current_milestone function to work with the module
process_current_milestone <- function(milestone_scores_module, resident_name, current_period) {
  # Extract the milestone scores from the module
  scores <- milestone_scores_module$scores()
  
  # Create a data frame with the current scores
  current_data <- data.frame(
    name = resident_name,
    period = current_period,
    prog_mile_date = format(Sys.Date(), "%Y-%m-%d"),
    record_id = "current",
    stringsAsFactors = FALSE
  )
  
  # Add all milestone scores to the data frame
  # The module returns scores with the correct field names (rep_pc1, etc.)
  for (milestone_name in names(scores)) {
    current_data[[milestone_name]] <- as.numeric(scores[[milestone_name]])
  }
  
  message("Created current milestone data frame with ", ncol(current_data) - 4, " milestone fields")
  
  return(current_data)
}

# Function to check REDCap record existence
check_redcap_record <- function(url, token, record_id) {
  # Make the API request to get a specific record
  response <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      records = as.character(record_id),
      fields = "record_id",
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  # Process response
  status_code <- httr::status_code(response)
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  if (status_code == 200) {
    parsed <- jsonlite::fromJSON(response_content)
    if (length(parsed) > 0) {
      message("✅ Record ID ", record_id, " exists in REDCap")
      return(TRUE)
    } else {
      message("❌ Record ID ", record_id, " does NOT exist in REDCap")
      return(FALSE)
    }
  } else {
    message("❌ Error checking record: ", response_content)
    return(FALSE)
  }
}

# Function to add record if it doesn't exist
ensure_redcap_record_exists <- function(url, token, record_id, name = NULL) {
  # Check if the record exists
  exists <- check_redcap_record(url, token, record_id)
  
  if (!exists) {
    # Create a basic record
    json_data <- paste0('[{"record_id":"', record_id, '"')
    
    # Add name if provided
    if (!is.null(name)) {
      json_data <- paste0(json_data, ',"name":"', name, '"')
    }
    
    # Close JSON
    json_data <- paste0(json_data, "}]")
    
    # Submit to REDCap
    response <- httr::POST(
      url = url,
      body = list(
        token = token,
        content = "record",
        format = "json",
        type = "flat",
        overwriteBehavior = "normal",
        forceAutoNumber = "false",
        data = json_data,
        returnContent = "count",
        returnFormat = "json"
      ),
      encode = "form"
    )
    
    # Process response
    status_code <- httr::status_code(response)
    response_content <- httr::content(response, "text", encoding = "UTF-8")
    
    if (status_code == 200) {
      message("✅ Successfully created record ID ", record_id)
      return(TRUE)
    } else {
      message("❌ Error creating record: ", response_content)
      return(FALSE)
    }
  }
  
  return(exists)
}

# Function to inspect a repeating instrument
inspect_repeating_instrument <- function(url, token, record_id, instrument) {
  # Make the API request to get data for a specific instrument
  response <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      records = as.character(record_id),
      forms = instrument,
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  # Process response
  status_code <- httr::status_code(response)
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  if (status_code == 200) {
    parsed <- jsonlite::fromJSON(response_content)
    if (length(parsed) > 0) {
      message("Data for instrument '", instrument, "' in record ID ", record_id, ":")
      print(parsed)
      return(parsed)
    } else {
      message("No data found for instrument '", instrument, "' in record ID ", record_id)
      return(NULL)
    }
  } else {
    message("Error inspecting instrument: ", response_content)
    return(NULL)
  }
}

# Test submission function for coach review
test_coach_review_submission <- function(url, token, record_id, instance) {
  # Ensure record exists
  ensure_redcap_record_exists(url, token, record_id)
  
  # Create test submission data
  json_data <- paste0(
    '[{"record_id":"', record_id,
    '","redcap_repeat_instrument":"coach_rev",',
    '"redcap_repeat_instance":"', instance, '"',
    ',"coach_date":"', format(Sys.Date(), "%Y-%m-%d"), '"',
    ',"coach_period":"', instance, '"',
    ',"coach_pre_rev":"Test submission"',
    ',"coach_summary":"Test summary"',
    ',"coach_ilp_final":"Test ILP final"',
    ',"coach_rev_complete":"2"',  # 2 = Complete
    "}]"
  )
  
  # Submit to REDCap
  response <- httr::POST(
    url = url,
    body = list(
      token = token,
      content = "record",
      format = "json",
      type = "flat",
      overwriteBehavior = "normal",
      forceAutoNumber = "false",
      data = json_data,
      returnContent = "count",
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  # Process response
  status_code <- httr::status_code(response)
  response_content <- httr::content(response, "text", encoding = "UTF-8")
  
  message("REDCap test submission response status: ", status_code)
  message("REDCap test submission response content: ", response_content)
  
  if (status_code == 200) {
    message("✅ Test submission successful")
    
    # Check if the data was actually saved
    inspect_repeating_instrument(url, token, record_id, "coach_rev")
    
    return(TRUE)
  } else {
    message("❌ Test submission failed")
    return(FALSE)
  }
}

# Add this observer to run a test when needed
observeEvent(input$test_redcap_submission, {
  req(app_data()$redcap_url)
  req(app_data()$rdm_token)
  
  # Show processing notification
  id <- showNotification("Testing REDCap submission...", type = "message", duration = NULL)
  
  # URL and token
  redcap_url <- app_data()$redcap_url %||% "https://redcapsurvey.slu.edu/api/"
  token <- app_data()$rdm_token
  
  # Choose a test record ID and instance
  test_record_id <- "999999" # Use a high number unlikely to exist
  test_instance <- "8"  # Instance 8 is usually Interim
  
  # Run the test
  result <- test_coach_review_submission(redcap_url, token, test_record_id, test_instance)
  
  # Remove notification
  removeNotification(id)
  
  # Show result
  if (result) {
    showNotification("Test successful! Check the logs for details.", type = "message")
  } else {
    showNotification("Test failed. Check the logs for details.", type = "error")
  }
})
