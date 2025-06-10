server <- function(input, output, session) {
  # ============================================================================
  # CONFIGURATION AND INITIALIZATION
  # ============================================================================
  
  # Reactive values to store session state
  values <- reactiveValues(
    is_authenticated = FALSE,
    selected_navigation = NULL,
    selected_coach = NULL,
    selected_resident = NULL,
    current_tab = "pre_review",
    review_type = NULL,
    primary_review_data = NULL,
    current_period = NULL,
    redcap_period = NULL,
    redcap_prev_period = NULL,
    tab_order = c("pre_review", "wellness", "evaluations", "knowledge", 
                  "scholarship", "ilp", "career", "summary", "milestones"),
    current_filter = "all"  # For CCC filtering
  )
  
  # Show loading notification
  showNotification("Loading data... please wait", type = "message", duration = NULL, id = "loading")
  
  # Load app data reactively
  app_data <- reactive({
    data <- ensure_data_loaded()
    
    # Remove loading notification once data is loaded
    removeNotification("loading")
    
    return(data)
  })
  
  # Setup milestone images
  setup_milestone_images <- function() {
    # Check if milestone images directory exists in imres package
    milestones_path <- system.file("www", "milestones", package = "imres")
    
    if (dir.exists(milestones_path)) {
      message("Milestone images directory found - images will be available")
      return(TRUE)
    } else {
      message("Milestone images directory not found - using placeholder content")
      return(FALSE)
    }
  }
  
  # Call this in your server initialization
  milestone_images_available <- setup_milestone_images()
  
  # Helper function to check if milestones exist
  check_milestones_exist <- function(milestone_data, resident_name, period) {
    if (is.null(milestone_data) || is.null(resident_name) || is.null(period)) {
      return(FALSE)
    }
    
    # Check if there's any milestone data for this resident and period
    has_data <- any(
      milestone_data$name == resident_name & 
        milestone_data$period == period, 
      na.rm = TRUE
    )
    
    message("Checking milestones for ", resident_name, " in period ", period, ": ", has_data)
    return(has_data)
  }
  
  milestones_exist <- reactive({
    req(values$selected_resident)
    req(values$redcap_period)
    
    data <- app_data()
    
    if (is.null(data$p_miles)) {
      message("No program milestone data available")
      return(FALSE)
    }
    
    # Use the helper function to check if milestones exist
    exists <- check_milestones_exist(
      milestone_data = data$p_miles,
      resident_name = values$selected_resident$name,
      period = values$redcap_period
    )
    
    message("Milestones exist for ", values$selected_resident$name, " in period ", values$redcap_period, ": ", exists)
    return(exists)
  })
  
  # Track milestone entry state
  milestone_entry_state <- reactiveValues(
    should_enter = FALSE,  # Whether user wants to enter milestones
    entry_complete = FALSE  # Whether milestone entry has been completed
  )
  
  # Function to clear form inputs
  clear_ccc_form_inputs <- function(session) {
    # Clear all text inputs
    updateTextAreaInput(session, "ccc_interim", value = "")
    updateTextAreaInput(session, "ccc_ilp", value = "")
    updateTextAreaInput(session, "ccc_issues_follow_up", value = "")
    updateTextAreaInput(session, "ccc_comments", value = "")
    updateTextAreaInput(session, "ccc_mile_concerns", value = "")
    updateTextAreaInput(session, "ccc_concern_notes", value = "")  # ADD THIS LINE
    updateTextInput(session, "ccc_fu_resp", value = "")
    
    # Clear radio buttons
    updateRadioButtons(session, "ccc_rev_type", selected = character(0))
    updateRadioButtons(session, "ccc_concern", selected = character(0))
    updateRadioButtons(session, "ccc_mile", selected = character(0))
    updateRadioButtons(session, "ccc_enter_milestones", selected = character(0))
    
    # Clear select inputs
    updateSelectInput(session, "ccc_session", selected = "")
    
    # Clear checkbox inputs
    updateCheckboxGroupInput(session, "ccc_action", selected = character(0))
    updateCheckboxGroupInput(session, "ccc_action_status", selected = character(0))
    updateCheckboxGroupInput(session, "ccc_competency", selected = character(0))
    updateCheckboxInput(session, "has_action_items", value = FALSE)
    
    message("CCC form inputs cleared")
  }
  
  # ============================================================================
  # ACCESS CODE AUTHENTICATION
  # ============================================================================
  
  observeEvent(input$submit_access, {
    # Compare the input with the stored access code (from global.R)
    if (input$access_code == stored_access_code) {
      values$is_authenticated <- TRUE
      shinyjs::hide("login-page")
      shinyjs::show("navigation-page")
      
      # Clear the access code input for security
      updateTextInput(session, "access_code", value = "")
      
      showNotification("Access granted! Welcome to the IMSLU Dashboard.", 
                       type = "message", duration = 3)
    } else {
      shinyjs::show("access_error")
      
      # Clear the incorrect input
      updateTextInput(session, "access_code", value = "")
    }
  })
  
  # Back to login button
  observeEvent(input$back_to_login, {
    values$is_authenticated <- FALSE
    values$selected_navigation <- NULL
    
    shinyjs::hide("navigation-page")
    shinyjs::hide("ccc-pages")
    shinyjs::hide("coaching-pages")
    shinyjs::show("login-page")
  })
  
  # ============================================================================
  # NAVIGATION SELECTION HANDLERS
  # ============================================================================
  
  # Handle "ILP and Milestone Review" selection
  observeEvent(input$select_coaching, {
    values$selected_navigation <- "coaching"
    shinyjs::hide("navigation-page")
    shinyjs::show("ccc-pages")  # Show the CCC pages (which contains your working table)
    shinyjs::show("ccc-dashboard-page")  # Show the dashboard specifically
    
    showNotification("Welcome to the ILP and Milestone Review dashboard!", 
                     type = "message", duration = 3)
  })
  
  # Handle "Follow-up List" selection - show under construction
  observeEvent(input$select_ccc, {
    values$selected_navigation <- "ccc_construction"
    shinyjs::hide("navigation-page")
    shinyjs::show("ccc-pages")
    shinyjs::hide("ccc-dashboard-page")  # Hide the working dashboard
    
    showNotification("Follow-up List feature coming soon!", 
                     type = "message", duration = 3)
  })
  
  # Back to navigation from CCC
  observeEvent(input$ccc_back_to_nav, {
    values$selected_navigation <- NULL
    
    shinyjs::hide("ccc-pages")
    shinyjs::hide("ccc-dashboard-page")
    shinyjs::show("navigation-page")
  })
  
  # Navigate from under-construction CCC to working coaching dashboard
  observeEvent(input$ccc_to_coaching, {
    values$selected_navigation <- "coaching"
    # Don't hide ccc-pages, just show the dashboard
    shinyjs::show("ccc-dashboard-page")
    
    showNotification("Switched to ILP and Milestone Review dashboard", 
                     type = "message", duration = 3)
  })
  
  # Back to navigation from done page
  observeEvent(input$back_to_navigation, {
    values$selected_navigation <- NULL
    values$selected_coach <- NULL
    values$selected_resident <- NULL
    
    shinyjs::hide("ccc-pages")
    shinyjs::hide("coaching-pages")
    shinyjs::show("navigation-page")
  })
  
  # ============================================================================
  # PROCESSED RESIDENT DATA
  # ============================================================================
  
  processed_resident_data <- reactive({
    data <- app_data()
    
    if (!is.null(data$resident_data)) {
      # Calculate and add resident levels using your exact working function
      processed_data <- calculate_resident_level(data$resident_data)
      
      message("Processed resident data summary:")
      message("  Total rows: ", nrow(processed_data))
      message("  Unique residents: ", length(unique(processed_data$name)))
      message("  Level distribution: ", paste(table(processed_data$Level), collapse = ", "))
      
      return(processed_data)
    } else {
      message("No resident_data found in app_data")
      return(NULL)
    }
  })
  
  # ============================================================================
  # DATATABLE FUNCTION
  # ============================================================================
  
  datatable_with_click_and_single_search <- function(data, caption = NULL) {
    
    message("Creating enhanced datatable with ", nrow(data), " rows and ", ncol(data), " columns")
    if (nrow(data) > 0) {
      message("First row data: ", paste(as.character(data[1, 1:min(6, ncol(data))]), collapse = " | "))
    }
    
    dt <- DT::datatable(
      data,
      escape = FALSE,
      options = list(
        pageLength = 25,
        dom = 'rtip',  # REMOVED 'f' to hide default search, keeping r=processing, t=table, i=info, p=pagination
        scrollX = FALSE,
        autoWidth = TRUE,
        columnDefs = list(
          # Enhanced status columns - center align and optimize for bigger dots
          list(width = "120px", targets = c(6, 7, 8, 9), className = "text-center"),
          # Name column
          list(width = "180px", targets = 0),
          # Other text columns
          list(width = "120px", targets = c(1, 2, 3, 4, 5)),
          list(
            targets = "_all",
            render = DT::JS(
              "function(data, type, row) {
              if (data === null || data === '') {
                  return '<span style=\"color: #999; font-style: italic;\">Not provided</span>';
              }
              return data;
            }"
            )
          )
        ),
        responsive = TRUE,
        search = list(
          regex = FALSE,
          caseInsensitive = TRUE
        ),
        # Enhanced styling
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({",
          "    'background-color': '#f8f9fa',",
          "    'font-weight': 'bold',",
          "    'border-bottom': '2px solid #dee2e6'",
          "  });",
          "}"
        )
      ),
      caption = tags$div(
        style = "caption-side: top; text-align: center; font-size: 18px; font-weight: bold; margin-bottom: 15px; color: #495057;",
        caption
      ),
      rownames = FALSE,
      class = 'table table-striped table-hover table-bordered table-sm',
      selection = 'single',
      callback = JS("
      console.log('Enhanced DataTable callback initialized');
      
      // Connect our custom search to the DataTable
      $('#global_search').on('keyup', function() {
        table.search(this.value).draw();
      });
      
      table.on('click', 'tbody tr', function() {
          console.log('Row clicked!');
          
          // Enhanced row selection with visual feedback
          table.$('tr.selected').removeClass('selected');
          $(this).addClass('selected');
          $(this).css('background-color', '#e3f2fd');
          
          var rowData = table.row(this).data();
          console.log('Row data:', rowData);
          
          var residentName = rowData && rowData[0] ? rowData[0] : '';
          var residentLevel = rowData && rowData[1] ? rowData[1] : '';
          var accessCode = rowData && rowData[2] ? rowData[2] : '';
          var primaryCoach = rowData && rowData[3] ? rowData[3] : '';
          var secondReviewer = rowData && rowData[4] ? rowData[4] : '';
          var reviewPeriod = rowData && rowData[5] ? rowData[5] : '';
          
          console.log('Parsed data - Name:', residentName, 'Level:', residentLevel, 'Access:', accessCode);
          
          if (residentName && residentLevel && accessCode) {
            console.log('Sending data to Shiny...');
            
            Shiny.setInputValue('selected_resident_in_ccc_table', 
                {
                    name: residentName, 
                    level: residentLevel,
                    access_code: accessCode, 
                    primary_coach: primaryCoach,
                    second_reviewer: secondReviewer,
                    review_period: reviewPeriod,
                    timestamp: new Date().getTime()
                }, 
                {priority: 'event'});
          } else {
            console.log('Missing required data, not sending to Shiny');
          }
      });
    ")
    ) %>%
      DT::formatStyle(
        columns = 1:6,
        backgroundColor = '#ffffff',
        borderColor = '#dee2e5'
      ) %>%
      # Enhanced status column styling for bigger dots
      DT::formatStyle(
        columns = 7:10,
        textAlign = 'center',
        verticalAlign = 'middle'
      ) %>%
      # Enhanced hover effects
      DT::formatStyle(
        columns = 1:10,
        cursor = 'pointer'
      )
    
    return(dt)
  }
  
  # ============================================================================
  # STATUS CHECKING FUNCTIONS
  # ============================================================================
  
  # Check self-evaluation completeness
  check_self_eval_complete_status <- function(resident_data, resident_name, period, app_data = NULL) {
    # Hard-code beta test only to have complete self-evaluations (for testing)
    if (resident_name == "beta test") {
      return(TRUE)
    }
    
    # Method 1: Check s_eval_complete indicator
    if ("s_eval_complete" %in% names(resident_data)) {
      self_eval_rows <- resident_data[resident_data$name == resident_name & 
                                        !is.na(resident_data$s_eval_complete) & 
                                        resident_data$s_eval_complete == "Complete", ]
      
      if (nrow(self_eval_rows) > 0) {
        return(TRUE)
      }
    }
    
    # Method 2: Check for matching period with content
    if ("s_e_period" %in% names(resident_data)) {
      period_rows <- resident_data[resident_data$name == resident_name & 
                                     !is.na(resident_data$s_e_period) &
                                     resident_data$s_e_period == period, ]
      
      if (nrow(period_rows) > 0) {
        # Check key fields for content
        for (field in c("s_e_plus", "s_e_delta")) {
          if (field %in% names(period_rows)) {
            for (row_idx in 1:nrow(period_rows)) {
              if (!is.na(period_rows[[field]][row_idx]) && 
                  period_rows[[field]][row_idx] != "") {
                return(TRUE)
              }
            }
          }
        }
      }
    }
    
    # Method 3: Direct check for milestone data
    if (!is.null(app_data) && !is.null(app_data$s_miles)) {
      s_mile_rows <- app_data$s_miles[app_data$s_miles$name == resident_name & 
                                        !is.na(app_data$s_miles$period) &
                                        app_data$s_miles$period == period, ]
      
      if (nrow(s_mile_rows) > 0) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  # Check coach review completeness
  check_coach_review_complete_status <- function(resident_data, resident_name, coach_period) {
    # Method 1: Check coach_rev_complete status for exact period match
    if ("coach_rev_complete" %in% names(resident_data) && "coach_period" %in% names(resident_data)) {
      complete_rows <- resident_data[resident_data$name == resident_name & 
                                       !is.na(resident_data$coach_period) &
                                       resident_data$coach_period == as.character(coach_period) &
                                       !is.na(resident_data$coach_rev_complete) &
                                       resident_data$coach_rev_complete == "2", ]  # 2 = Complete in REDCap
      
      if (nrow(complete_rows) > 0) {
        return(TRUE)
      }
    }
    
    # Method 2: Check for content in key coach_rev fields for exact period match
    if ("coach_period" %in% names(resident_data)) {
      period_rows <- resident_data[resident_data$name == resident_name & 
                                     !is.na(resident_data$coach_period) &
                                     resident_data$coach_period == as.character(coach_period), ]
      
      if (nrow(period_rows) > 0) {
        key_fields <- c("coach_ilp_final", "coach_summary", "coach_pre_rev")
        for (field in key_fields) {
          if (field %in% names(period_rows)) {
            for (row_idx in 1:nrow(period_rows)) {
              if (!is.na(period_rows[[field]][row_idx]) && 
                  period_rows[[field]][row_idx] != "") {
                return(TRUE)
              }
            }
          }
        }
      }
    }
    
    # Method 3: Check for any coach_rev content (regardless of period)
    any_rows <- resident_data[resident_data$name == resident_name, ]
    
    if (nrow(any_rows) > 0) {
      key_fields <- c("coach_ilp_final", "coach_summary", "coach_pre_rev")
      for (field in key_fields) {
        if (field %in% names(any_rows)) {
          for (row_idx in 1:nrow(any_rows)) {
            if (!is.na(any_rows[[field]][row_idx]) && 
                any_rows[[field]][row_idx] != "") {
              return(TRUE)
            }
          }
        }
      }
    }
    
    return(FALSE)
  }
  
  # Check secondary review completeness
  check_second_review_complete_status <- function(resident_data, resident_name, coach_period) {
    # Method 1: Check second_review_complete status for exact period match
    if ("second_review_complete" %in% names(resident_data) && "second_period" %in% names(resident_data)) {
      complete_rows <- resident_data[resident_data$name == resident_name & 
                                       !is.na(resident_data$second_period) &
                                       resident_data$second_period == as.character(coach_period) &
                                       !is.na(resident_data$second_review_complete) &
                                       resident_data$second_review_complete == "2", ]  # 2 = Complete in REDCap
      
      if (nrow(complete_rows) > 0) {
        return(TRUE)
      }
    }
    
    # Method 2: Check for content in key second_review fields for exact period match
    if ("second_period" %in% names(resident_data)) {
      period_rows <- resident_data[resident_data$name == resident_name & 
                                     !is.na(resident_data$second_period) &
                                     resident_data$second_period == as.character(coach_period), ]
      
      if (nrow(period_rows) > 0) {
        key_fields <- c("second_comments", "second_approve", "second_miles_comment")
        for (field in key_fields) {
          if (field %in% names(period_rows)) {
            for (row_idx in 1:nrow(period_rows)) {
              if (!is.na(period_rows[[field]][row_idx]) && 
                  period_rows[[field]][row_idx] != "") {
                return(TRUE)
              }
            }
          }
        }
      }
    }
    
    # Method 3: Check for any second_review content (regardless of period)
    any_rows <- resident_data[resident_data$name == resident_name, ]
    
    if (nrow(any_rows) > 0) {
      key_fields <- c("second_comments", "second_approve", "second_miles_comment")
      for (field in key_fields) {
        if (field %in% names(any_rows)) {
          for (row_idx in 1:nrow(any_rows)) {
            if (!is.na(any_rows[[field]][row_idx]) && 
                any_rows[[field]][row_idx] != "") {
              return(TRUE)
            }
          }
        }
      }
    }
    
    return(FALSE)
  }
  
  # Check CCC review completeness
  check_ccc_review_complete_status <- function(resident_data, resident_name, period, level, current_period) {
    # Map period to CCC session number with better handling
    ccc_session_map <- c(
      "Mid Intern" = "1",
      "End Intern" = "2", 
      "Mid PGY2" = "3",
      "End PGY2" = "4",
      "Mid PGY3" = "5",
      "Graduation" = "6",
      "Graduating" = "6",  # Handle both forms
      "Intern Intro" = "7"
    )
    
    expected_session <- ccc_session_map[period]
    if (is.na(expected_session)) {
      # Try to map from current_period if period mapping failed
      if (current_period == "Mid Review") {
        if (level == "Intern") expected_session <- "1"
        else if (level == "PGY2") expected_session <- "3"  
        else if (level == "PGY3") expected_session <- "5"
      } else if (current_period == "End Review") {
        if (level == "Intern") expected_session <- "2"
        else if (level == "PGY2") expected_session <- "4"
        else if (level == "PGY3") expected_session <- "6"
      } else if (current_period == "Intern Intro") {
        expected_session <- "7"
      }
      
      if (is.na(expected_session)) {
        return(FALSE)
      }
    }
    
    # Filter for this resident
    resident_rows <- resident_data[resident_data$name == resident_name, ]
    
    if (nrow(resident_rows) == 0) {
      return(FALSE)
    }
    
    # Check for CCC review data with the required criteria
    # Method 1: Strict check - ccc_rev_type = "1" (Scheduled), ccc_session matches, ccc_mile = "1" (Yes)
    if (all(c("ccc_rev_type", "ccc_session", "ccc_mile") %in% names(resident_rows))) {
      ccc_complete_rows <- resident_rows[
        !is.na(resident_rows$ccc_rev_type) & resident_rows$ccc_rev_type == "1" &
          !is.na(resident_rows$ccc_session) & resident_rows$ccc_session == expected_session &
          !is.na(resident_rows$ccc_mile) & resident_rows$ccc_mile == "1", ]
      
      if (nrow(ccc_complete_rows) > 0) {
        return(TRUE)
      }
    }
    
    # Method 2: Looser check - any CCC content for the expected session
    if ("ccc_session" %in% names(resident_rows)) {
      session_rows <- resident_rows[!is.na(resident_rows$ccc_session) & 
                                      resident_rows$ccc_session == expected_session, ]
      
      if (nrow(session_rows) > 0) {
        # Check if there's any meaningful content
        content_fields <- c("ccc_date", "ccc_comments", "ccc_ilp", "ccc_mile_notes", "ccc_issues_follow_up")
        existing_content_fields <- intersect(content_fields, names(session_rows))
        
        for (field in existing_content_fields) {
          content_rows <- session_rows[!is.na(session_rows[[field]]) & session_rows[[field]] != "", ]
          if (nrow(content_rows) > 0) {
            return(TRUE)
          }
        }
      }
    }
    
    # Method 3: Any CCC review content at all (fallback)
    ccc_fields <- c("ccc_date", "ccc_rev_type", "ccc_session", "ccc_interim", "ccc_concern", 
                    "ccc_action", "ccc_competency", "ccc_ilp", "ccc_mile", "ccc_mile_notes", 
                    "ccc_issues_follow_up", "ccc_comments")
    
    existing_ccc_fields <- intersect(ccc_fields, names(resident_rows))
    
    for (field in existing_ccc_fields) {
      content_rows <- resident_rows[!is.na(resident_rows[[field]]) & resident_rows[[field]] != "", ]
      if (nrow(content_rows) > 0) {
        return(TRUE)
      }
    }
    
    return(FALSE)
  }
  
  # ============================================================================
  # FILTER BUTTON OBSERVERS
  # ============================================================================
  
  # Filter by Level
  observeEvent(input$filter_by_level, {
    values$current_filter <- "level"
    showNotification("Sorting by Level", type = "message")
  })
  
  # Filter by Fully Complete
  observeEvent(input$filter_fully_complete, {
    values$current_filter <- "fully_complete"
    showNotification("Showing only fully complete residents", type = "message")
  })
  
  # Filter by Self-Eval Done, Coach/Second Pending
  observeEvent(input$filter_self_done_others_pending, {
    values$current_filter <- "self_done_others_pending"
    showNotification("Showing residents with self-eval done but coach/second reviews pending", type = "message")
  })
  
  # Filter by Coach Done, Second Pending
  observeEvent(input$filter_coach_done_second_pending, {
    values$current_filter <- "coach_done_second_pending"
    showNotification("Showing residents with coach review done but second review pending", type = "message")
  })
  
  # Filter by Coach/Second Done, CCC Pending
  observeEvent(input$filter_reviews_done_ccc_pending, {
    values$current_filter <- "reviews_done_ccc_pending"
    showNotification("Showing residents with both reviews done but CCC review pending", type = "message")
  })
  
  # Clear all filters
  observeEvent(input$clear_filters, {
    values$current_filter <- "all"
    showNotification("All filters cleared", type = "message")
  })
  
  # ============================================================================
  # MAIN CCC RESIDENTS TABLE WITH FILTERING
  # ============================================================================
  
  output$ccc_residents_table <- DT::renderDataTable({
    
    # Get resident data using your exact working approach
    resident_data <- processed_resident_data()
    
    # Calculate current period using your existing function
    current_period <- get_current_period()
    message("Current period: ", current_period)
    
    # Check if we have resident data with the required columns
    if (!is.null(resident_data) && is.data.frame(resident_data) && 
        all(c("name", "access_code", "coach", "second_rev", "Level") %in% names(resident_data))) {
      
      # Use the exact same approach as your working coaching dashboard
      all_residents <- resident_data %>%
        filter(!is.na(name) & name != "", 
               !is.na(Level) & Level != "",
               !is.na(access_code) & access_code != "",
               !is.na(coach) & coach != "") %>%
        group_by(name) %>%
        slice(1) %>%
        ungroup() %>%
        distinct(name, .keep_all = TRUE) %>%
        arrange(Level, name)
      
      message("Found ", nrow(all_residents), " unique residents with assigned coaches for CCC review")
      
      # Create empty vectors to store status indicators
      redcap_periods <- character(nrow(all_residents))
      self_eval_statuses <- character(nrow(all_residents))
      coach_review_statuses <- character(nrow(all_residents))
      second_review_statuses <- character(nrow(all_residents))
      ccc_review_statuses <- character(nrow(all_residents))
      
      # Store completion booleans for filtering
      has_self_evals <- logical(nrow(all_residents))
      has_coach_reviews <- logical(nrow(all_residents))
      has_second_reviews <- logical(nrow(all_residents))
      has_ccc_reviews <- logical(nrow(all_residents))
      
      # Create vectors to store the ACTUAL data for the table
      table_names <- character(nrow(all_residents))
      table_levels <- character(nrow(all_residents))
      table_access_codes <- character(nrow(all_residents))
      table_coaches <- character(nrow(all_residents))
      table_second_revs <- character(nrow(all_residents))
      
      # Process each resident to determine completion statuses
      for (i in 1:nrow(all_residents)) {
        res_name <- all_residents$name[i]
        res_level <- all_residents$Level[i]
        res_access_code <- all_residents$access_code[i]
        res_coach <- all_residents$coach[i]
        res_second_rev <- all_residents$second_rev[i]
        
        # Store the actual data in our vectors
        table_names[i] <- ifelse(is.na(res_name), "", as.character(res_name))
        table_levels[i] <- ifelse(is.na(res_level), "Unknown", as.character(res_level))
        table_access_codes[i] <- ifelse(is.na(res_access_code), "", as.character(res_access_code))
        table_coaches[i] <- ifelse(is.na(res_coach), "Not assigned", as.character(res_coach))
        table_second_revs[i] <- ifelse(is.na(res_second_rev), "Not assigned", as.character(res_second_rev))
        
        # Map the period to REDCap format using your existing functions
        redcap_period <- map_to_milestone_period(res_level, current_period)
        redcap_periods[i] <- ifelse(is.na(redcap_period), current_period, as.character(redcap_period))
        
        # Convert period to REDCap instance
        coach_period <- map_period_format(
          level = res_level,
          period = current_period,
          return_type = "instance"
        )
        
        # Check completion statuses using the fixed functions
        has_self_eval <- check_self_eval_complete_status(resident_data, res_name, redcap_period, app_data())
        has_coach_review <- check_coach_review_complete_status(resident_data, res_name, coach_period)
        has_second_review <- check_second_review_complete_status(resident_data, res_name, coach_period)
        has_ccc_review <- check_ccc_review_complete_status(resident_data, res_name, redcap_period, res_level, current_period)
        
        # Store boolean values for filtering
        has_self_evals[i] <- has_self_eval
        has_coach_reviews[i] <- has_coach_review
        has_second_reviews[i] <- has_second_review
        has_ccc_reviews[i] <- has_ccc_review
        
        # Create status dots
        self_eval_statuses[i] <- if(has_self_eval) 
          '<div class="status-dot status-complete"></div>' else 
            '<div class="status-dot status-incomplete"></div>'
        
        coach_review_statuses[i] <- if(has_coach_review) 
          '<div class="status-dot status-complete"></div>' else 
            '<div class="status-dot status-incomplete"></div>'
        
        second_review_statuses[i] <- if(has_second_review) 
          '<div class="status-dot status-complete"></div>' else 
            '<div class="status-dot status-incomplete"></div>'
        
        ccc_review_statuses[i] <- if(has_ccc_review) 
          '<div class="status-dot status-complete"></div>' else 
            '<div class="status-dot status-incomplete"></div>'
        
      } # End of the for loop that processes each resident
      
      # Create the final dataframe
      final_residents_table <- data.frame(
        `Resident Name` = table_names,
        `Level` = table_levels,
        `Access Code` = table_access_codes,
        `Primary Coach` = table_coaches,
        `Second Reviewer` = table_second_revs,
        `Review Period` = redcap_periods,
        `Self Evaluation Complete?` = self_eval_statuses,
        `Coach Review Complete?` = coach_review_statuses,
        `Second Review Complete?` = second_review_statuses,
        `CCC Review Complete?` = ccc_review_statuses,
        stringsAsFactors = FALSE
      )
      
      # Apply filters based on current filter selection
      filter_indices <- switch(values$current_filter,
                               "all" = 1:nrow(final_residents_table),
                               # For level sorting, just return all indices (we'll sort differently)
                               "level" = 1:nrow(final_residents_table),  
                               "fully_complete" = which(has_self_evals & has_coach_reviews & has_second_reviews & has_ccc_reviews),
                               "self_done_others_pending" = which(has_self_evals & (!has_coach_reviews | !has_second_reviews)),
                               "coach_done_second_pending" = which(has_coach_reviews & !has_second_reviews),
                               "reviews_done_ccc_pending" = which(has_coach_reviews & has_second_reviews & !has_ccc_reviews),
                               1:nrow(final_residents_table)  # Default to all
      )
      
      # Apply the filter
      if (values$current_filter == "level") {
        # Get the data first
        temp_table <- final_residents_table[filter_indices, ]
        
        # Sort using explicit column references
        level_col <- temp_table[[which(names(temp_table) == "Level")]]
        name_col <- temp_table[[which(names(temp_table) == "Resident.Name")]]
        
        # Create sort order
        sort_order <- order(level_col, name_col)
        filtered_table <- temp_table[sort_order, ]
        
      } else {
        # For other filters, subset the rows
        filtered_table <- final_residents_table[filter_indices, ]
      }
      
      # Update the caption based on filter
      caption_text <- switch(values$current_filter,
                             "all" = "All Residents - CCC Review Status",
                             "level" = "All Residents - Sorted by Level",
                             "fully_complete" = paste0("Fully Complete Residents (", length(filter_indices), " of ", nrow(final_residents_table), ")"),
                             "self_done_others_pending" = paste0("Self-Eval Done, Others Pending (", length(filter_indices), " of ", nrow(final_residents_table), ")"),
                             "coach_done_second_pending" = paste0("Coach Done, Second Pending (", length(filter_indices), " of ", nrow(final_residents_table), ")"),
                             "reviews_done_ccc_pending" = paste0("Reviews Done, CCC Pending (", length(filter_indices), " of ", nrow(final_residents_table), ")"),
                             "All Residents - CCC Review Status"
      )
      
      if (nrow(filtered_table) > 0) {
        # Create table with click handling and return it
        return(datatable_with_click_and_single_search(
          filtered_table, 
          caption = caption_text
        ))
      }
    } else {
      # Error handling
      if (is.null(resident_data)) {
        message("resident_data is NULL")
      } else if (!is.data.frame(resident_data)) {
        message("resident_data is not a dataframe")
      } else {
        message("Missing required columns in resident_data")
      }
    }
    
    # Return empty datatable if no data found
    return(datatable_with_click_and_single_search(
      data.frame(
        `Resident Name` = character(0),
        `Level` = character(0),
        `Access Code` = character(0),
        `Primary Coach` = character(0),
        `Second Reviewer` = character(0), 
        `Review Period` = character(0),
        `Self Evaluation Complete?` = character(0),
        `Coach Review Complete?` = character(0),
        `Second Review Complete?` = character(0),
        `CCC Review Complete?` = character(0)
      ),
      caption = "No residents found"
    ))
  })
  
  # ============================================================================
  # RESIDENT SELECTION AND DETAIL VIEW
  # ============================================================================
  
  # Handle resident selection in the CCC residents table
  observeEvent(input$selected_resident_in_ccc_table, {
    
    # Debug logging
    message("=== RESIDENT SELECTION EVENT TRIGGERED ===")
    message("Input received: ", deparse(input$selected_resident_in_ccc_table))
    
    req(input$selected_resident_in_ccc_table)
    
    # Get selected resident info
    resident_info <- input$selected_resident_in_ccc_table
    message("Resident info extracted:")
    message("  Name: ", resident_info$name)
    message("  Level: ", resident_info$level) 
    message("  Access Code: ", resident_info$access_code)
    
    # Find the full resident data
    resident_data <- processed_resident_data()
    message("Resident data is null: ", is.null(resident_data))
    
    if (!is.null(resident_data)) {
      message("Searching for resident in data...")
      message("Available residents: ", paste(head(unique(resident_data$name), 5), collapse = ", "))
      
      selected_resident <- resident_data %>%
        filter(name == resident_info$name, access_code == resident_info$access_code) %>%
        select(name, access_code, year, coach, second_rev, Level) %>%
        distinct()
      
      message("Found ", nrow(selected_resident), " matching residents")
      
      if (nrow(selected_resident) > 0) {
        # Store selected resident in reactiveValues
        values$selected_resident <- selected_resident[1, ]
        values$current_period <- resident_info$review_period
        
        message("Stored resident: ", values$selected_resident$name)
        message("Stored period: ", values$current_period)
        
        # Map the current period
        current_app_period <- get_current_period()
        values$redcap_period <- map_to_milestone_period(
          values$selected_resident$Level, 
          current_app_period
        )
        
        message("Mapped period: ", values$redcap_period)
        
        # Clear form inputs when new resident is selected
        clear_ccc_form_inputs(session)
        
        # Navigate to detail view
        message("Attempting to navigate to detail view...")
        
        # Hide dashboard and show review pages
        shinyjs::hide("ccc-dashboard-page")
        message("Hidden dashboard page")
        
        # Small delay then show review pages
        shinyjs::delay(100, {
          shinyjs::show("ccc-review-pages")
          message("Shown review pages")
          
          # Force a scroll to top
          shinyjs::runjs("window.scrollTo(0, 0);")
        })
        
        # Show notification
        showNotification(paste0("Loading details for ", 
                                values$selected_resident$name, " (", 
                                values$selected_resident$Level, " - ", 
                                values$current_period, ")"), 
                         type = "message", duration = 3)
        
      } else {
        message("ERROR: No matching resident found")
        showNotification("Resident data not found. Please try again.", type = "error")
      }
    } else {
      message("ERROR: Resident data is null")
      showNotification("Resident data not available. Please try again.", type = "error")
    }
    
    message("=== END RESIDENT SELECTION EVENT ===")
  })
  
  # Auto-populate CCC form when resident is selected
  observe({
    req(values$selected_resident)
    req(values$current_period)
    
    # Direct mapping from what's displayed in the table
    session_mapping <- c(
      "Mid Intern" = "1",
      "End Intern" = "2", 
      "Mid PGY2" = "3",
      "End PGY2" = "4",
      "Mid PGY3" = "5",
      "Graduation" = "6",
      "Graduating" = "6",
      "Intern Intro" = "7"
    )
    
    # Use the period from the table (values$current_period)
    session_value <- session_mapping[values$current_period]
    
    if (!is.na(session_value)) {
      # Auto-select the session and set to Scheduled review
      updateSelectInput(session, "ccc_session", selected = session_value)
      updateRadioButtons(session, "ccc_rev_type", selected = "1")
      
      message("Auto-populated CCC session: ", values$current_period, " -> ", session_value)
    }
  })
  
  # Back to dashboard button
  observeEvent(input$back_to_dashboard, {
    message("Back to dashboard button clicked")
    
    # Hide review pages
    shinyjs::hide("ccc-review-pages")
    shinyjs::hide("milestone-review-section")
    shinyjs::show("scheduled-step-2")
    
    # Small delay then show dashboard
    shinyjs::delay(100, {
      shinyjs::show("ccc-dashboard-page")
      message("Returned to dashboard")
    })
    
    # Reset resident selection
    values$selected_resident <- NULL
    values$current_period <- NULL
    values$redcap_period <- NULL
    
    # Clear form inputs
    clear_ccc_form_inputs(session)
    
    showNotification("Returned to CCC dashboard", type = "message")
  })
  
  # ============================================================================
  # RESIDENT DETAIL VIEW OUTPUTS
  # ============================================================================
  
  # Display resident info outputs
  output$display_resident_name <- renderText({
    req(values$selected_resident)
    values$selected_resident$name
  })
  
  output$display_resident_level <- renderText({
    req(values$selected_resident)
    values$selected_resident$Level
  })
  
  output$display_access_code <- renderText({
    req(values$selected_resident)
    values$selected_resident$access_code
  })
  
  output$display_primary_coach <- renderText({
    req(values$selected_resident)
    values$selected_resident$coach %||% "Not specified"
  })
  
  output$display_second_reviewer <- renderText({
    req(values$selected_resident)
    values$selected_resident$second_rev %||% "Not specified"
  })
  
  output$display_current_period <- renderText({
    req(values$current_period)
    values$current_period
  })
  
  # ============================================================================
  # CCC REVIEW WORKFLOW NAVIGATION
  # ============================================================================
  
  observeEvent(input$proceed_to_milestones, {
    req(input$ccc_rev_type == "1")  # Only for scheduled reviews
    req(input$ccc_session)
    
    # Validate Step 2 is complete
    validation_errors <- character(0)
    
    # Check required fields
    if (is.null(input$ccc_concern) || input$ccc_concern == "") {
      validation_errors <- c(validation_errors, "Please indicate if there are concerns")
    }
    
    if (!is.null(input$ccc_concern) && input$ccc_concern == "1") {
      if (is.null(input$ccc_action) || length(input$ccc_action) == 0) {
        validation_errors <- c(validation_errors, "Please select at least one CCC action")
      }
      if (is.null(input$ccc_competency) || length(input$ccc_competency) == 0) {
        validation_errors <- c(validation_errors, "Please select at least one competency area")
      }
    }
    
    if (length(validation_errors) > 0) {
      showNotification(
        paste("Please complete the following before proceeding:", 
              paste(validation_errors, collapse = "; ")),
        type = "error",
        duration = 8
      )
      return()
    }
    
    # Hide Step 2 and show Step 3
    shinyjs::hide("scheduled-step-2")
    shinyjs::show("milestone-review-section")
    
    # Scroll to top
    shinyjs::runjs("window.scrollTo(0, 0);")
    
    showNotification("Proceeding to milestone review", type = "message")
  })
  
  observeEvent(input$back_to_step2, {
    shinyjs::hide("milestone-review-section")
    shinyjs::show("scheduled-step-2")
    
    # Scroll to top
    shinyjs::runjs("window.scrollTo(0, 0);")
    
    showNotification("Returned to review details", type = "message")
  })
  
  # Open resident dashboard in new tab
  observeEvent(input$open_resident_dashboard, {
    req(values$selected_resident)
    
    access_code <- values$selected_resident$access_code
    dashboard_url <- "https://fbuckhold3-imsluresidentdashboard.share.connect.posit.cloud"
    
    # JavaScript to open in new tab and try to auto-fill access code
    js_code <- sprintf(
      "
    var newWindow = window.open('%s', '_blank');
    setTimeout(function() {
      if (newWindow && !newWindow.closed) {
        try {
          // Try to auto-fill the access code
          var accessInput = newWindow.document.getElementById('access_code');
          if (accessInput) {
            accessInput.value = '%s';
            // Try to submit the form
            var submitBtn = newWindow.document.getElementById('submit_access');
            if (submitBtn) {
              submitBtn.click();
            }
          }
        } catch(e) {
          console.log('Cross-origin access prevented - user will need to enter code manually');
        }
      }
    }, 2000);
    ",
      dashboard_url,
      access_code
    )
    
    shinyjs::runjs(js_code)
    
    showNotification(
      paste("Opening resident dashboard. Access code:", access_code),
      type = "message",
      duration = 8
    )
  })
  
  # ============================================================================
  # MILESTONE PLOTS
  # ============================================================================
  
  # Current Self-Assessment Milestone Plot
  output$self_milestones_plot <- renderPlot({
    req(values$selected_resident)
    req(values$redcap_period)
    
    data <- app_data()
    
    if (!is.null(data$s_miles) && !is.na(values$redcap_period)) {
      tryCatch({
        message(paste("Rendering self milestone plot for period:", values$redcap_period))
        
        # Debug: check if there's any data for this resident and period
        has_data <- any(data$s_miles$name == values$selected_resident$name & 
                          data$s_miles$period == values$redcap_period, na.rm = TRUE)
        message("Self milestone data exists for this resident and period: ", has_data)
        
        if (has_data) {
          # Call your existing miles_plot function
          miles_plot(data$s_miles, values$selected_resident$name, values$redcap_period)
        } else {
          ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = paste("No self-assessment data available for", 
                                   values$selected_resident$name, "in period", values$redcap_period),
                     color = "darkgray", size = 5) +
            theme_void()
        }
      }, error = function(e) {
        message(paste("Error rendering self milestone plot:", e$message))
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error rendering milestone plot:", e$message),
                   color = "red", size = 4) +
          theme_void()
      })
    } else {
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = "No self-assessment milestone data available",
                 color = "darkgray", size = 5) +
        theme_void()
    }
  })
  
  # Program Milestone Plot (showing current period data)
  output$program_milestones_plot <- renderPlot({
    req(values$selected_resident)
    req(values$redcap_period)
    
    data <- app_data()
    
    # Use the CURRENT period instead of previous period
    current_app_period <- get_current_period()
    current_mile_period <- map_to_milestone_period(
      values$selected_resident$Level, 
      current_app_period,
      form_context = "milestone"
    )
    
    message("Current app period: ", current_app_period)
    message("Current milestone period: ", ifelse(is.na(current_mile_period), "NA", current_mile_period))
    
    # Attempt to render if current period exists and should have program data
    if (!is.na(current_mile_period) && !is.null(data$p_miles)) {
      tryCatch({
        message(paste("Attempting to render program milestone plot for CURRENT period:", current_mile_period))
        
        # Debug: check if there's any data for this resident and current period
        if (!is.null(data$p_miles)) {
          has_data <- any(data$p_miles$name == values$selected_resident$name & 
                            data$p_miles$period == current_mile_period, na.rm = TRUE)
          message("Program milestone data exists for this resident and CURRENT period: ", has_data)
          
          if (has_data) {
            # Call your existing miles_plot function with CURRENT period
            p <- miles_plot(data$p_miles, values$selected_resident$name, current_mile_period)
            return(p)
          }
        }
        
        # If we get here, no data was found for current period
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("No program milestone data found for", values$selected_resident$name, 
                                 "in current period", current_mile_period),
                   color = "darkgray", size = 5) +
          theme_void()
        
      }, error = function(e) {
        message(paste("Error rendering current program milestone plot:", e$message))
        ggplot() +
          annotate("text", x = 0.5, y = 0.5,
                   label = paste("Error rendering milestone plot:", e$message),
                   color = "red", size = 4) +
          theme_void()
      })
    } else {
      # Create an empty plot when no current program data exists
      reason <- if(is.na(current_mile_period)) {
        "No current assessment period mapped"
      } else if (is.null(data$p_miles)) {
        "No program milestone data available"
      } else {
        "No program assessment for the current period"
      }
      
      ggplot() +
        annotate("text", x = 0.5, y = 0.5,
                 label = reason,
                 color = "darkgray", size = 5) +
        theme_void()
    }
  })
  
  # UPDATED: Self-Assessment Milestone Descriptions Table with debugging
  output$self_milestone_descriptions <- DT::renderDataTable({
    req(values$selected_resident)
    req(values$redcap_period)
    
    data <- app_data()
    
    message("=== RENDERING SELF MILESTONE DESCRIPTIONS ===")
    message("Resident: ", values$selected_resident$name)
    message("Period: ", values$redcap_period)
    
    # Debug the data structure
    debug_milestone_data(data, values$selected_resident$name, values$redcap_period)
    
    # Check if we have milestone data
    if (!is.null(data$s_miles)) {
      message("s_miles data available, creating table...")
      create_milestone_description_table(
        resident_name = values$selected_resident$name,
        period = values$redcap_period,
        source_type = "self",
        app_data = data
      )
    } else {
      message("No s_miles data available")
      create_empty_milestone_table("self")
    }
  })
  
  # UPDATED: Program Milestone Descriptions Table with debugging
  output$program_milestone_descriptions <- DT::renderDataTable({
    req(values$selected_resident)
    req(values$redcap_period)
    
    data <- app_data()
    
    message("=== RENDERING PROGRAM MILESTONE DESCRIPTIONS ===")
    message("Resident: ", values$selected_resident$name)
    message("Period: ", values$redcap_period)
    
    # Get current period for program milestones
    current_app_period <- get_current_period()
    current_mile_period <- map_to_milestone_period(
      values$selected_resident$Level, 
      current_app_period,
      form_context = "milestone"
    )
    
    message("Current app period: ", current_app_period)
    message("Current mile period: ", ifelse(is.na(current_mile_period), "NA", current_mile_period))
    
    # Debug the data structure
    debug_milestone_data(data, values$selected_resident$name, current_mile_period)
    
    # Check if we have milestone data
    if (!is.null(data$p_miles) && !is.na(current_mile_period)) {
      message("p_miles data available, creating table...")
      create_milestone_description_table(
        resident_name = values$selected_resident$name,
        period = current_mile_period,
        source_type = "program",
        app_data = data
      )
    } else {
      message("No p_miles data available or no current period mapped")
      create_empty_milestone_table("program")
    }
  })
  
  # ============================================================================
  # COACH ILP AND SECONDARY REVIEW DISPLAYS
  # ============================================================================
  
  output$coach_ilp_summary <- renderUI({
    req(values$selected_resident)
    req(values$redcap_period)
    
    # Get resident data
    resident_data <- processed_resident_data()
    
    # Get REDCap instance number for the current period
    current_app_period <- get_current_period()
    instance <- map_period_format(
      level = values$selected_resident$Level,
      period = current_app_period,
      return_type = "instance"
    )
    
    # Try to find coach_ilp_final for this resident and period
    filtered_data <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    # Check for coach_ilp_final
    ilp_final <- NULL
    ilp_source <- "Not specified"
    
    if ("coach_ilp_final" %in% names(filtered_data)) {
      # Try to match the specific period if coach_period exists
      if ("coach_period" %in% names(filtered_data)) {
        period_rows <- filtered_data %>%
          filter(coach_period == as.character(instance))
        
        if (nrow(period_rows) > 0) {
          for (i in 1:nrow(period_rows)) {
            if (!is.na(period_rows$coach_ilp_final[i]) && period_rows$coach_ilp_final[i] != "") {
              ilp_final <- period_rows$coach_ilp_final[i]
              ilp_source <- "Current period ILP summary"
              break
            }
          }
        }
      }
      
      # If still not found, try any non-NA coach_ilp_final
      if (is.null(ilp_final)) {
        for (i in 1:nrow(filtered_data)) {
          if (!is.na(filtered_data$coach_ilp_final[i]) && filtered_data$coach_ilp_final[i] != "") {
            ilp_final <- filtered_data$coach_ilp_final[i]
            ilp_source <- "Most recent ILP summary"
            break
          }
        }
      }
    }
    
    # Try alternative fields if coach_ilp_final not found
    if (is.null(ilp_final)) {
      # Try coach_summary
      if ("coach_summary" %in% names(filtered_data)) {
        for (i in 1:nrow(filtered_data)) {
          if (!is.na(filtered_data$coach_summary[i]) && filtered_data$coach_summary[i] != "") {
            ilp_final <- filtered_data$coach_summary[i]
            ilp_source <- "Coach summary"
            break
          }
        }
      }
      
      # Try other coach review fields
      alt_fields <- c("coach_pre_rev", "coach_anyelse", "coach_wellness", "coach_career")
      field_labels <- c("Pre-review notes", "Additional comments", "Wellness notes", "Career comments")
      
      for (j in seq_along(alt_fields)) {
        field <- alt_fields[j]
        if (is.null(ilp_final) && field %in% names(filtered_data)) {
          for (i in 1:nrow(filtered_data)) {
            if (!is.na(filtered_data[[field]][i]) && filtered_data[[field]][i] != "") {
              ilp_final <- filtered_data[[field]][i]
              ilp_source <- field_labels[j]
              break
            }
          }
        }
      }
    }
    
    # Create the UI based on what we found
    if (!is.null(ilp_final) && ilp_final != "") {
      # Split text into paragraphs for better display
      paragraphs <- strsplit(ilp_final, "\n\n")[[1]]
      paragraphs <- paragraphs[paragraphs != ""]  # Remove empty paragraphs
      
      # Create a simple, clean display
      div(
        # Source indicator
        tags$small(
          class = "text-muted mb-2 d-block",
          tags$strong("Source: "), ilp_source
        ),
        
        # Text content
        div(
          style = "
            background-color: #ffffff;
            border: 1px solid #dee2e6;
            border-radius: 4px;
            padding: 15px;
            line-height: 1.6;
            max-height: 300px;
            overflow-y: auto;
          ",
          
          # Display paragraphs
          lapply(paragraphs, function(para) {
            tags$p(para, style = "margin-bottom: 10px;")
          })
        )
      )
    } else {
      # No data found - simple message
      div(
        class = "text-muted text-center p-4",
        icon("info-circle", class = "mb-2"),
        tags$p("No coach ILP summary available for this resident.")
      )
    }
  })
  
  output$secondary_review_display <- renderUI({
    req(values$selected_resident)
    
    # Get resident data
    resident_data <- processed_resident_data()
    
    # Try to find secondary review data
    filtered_data <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    # Look for second_comments field specifically
    second_comments <- NULL
    reviewer_name <- NULL
    
    if ("second_comments" %in% names(filtered_data)) {
      for (i in 1:nrow(filtered_data)) {
        if (!is.na(filtered_data$second_comments[i]) && filtered_data$second_comments[i] != "") {
          second_comments <- filtered_data$second_comments[i]
          # Get reviewer name from second_rev field if available
          if ("second_rev" %in% names(filtered_data) && 
              !is.na(filtered_data$second_rev[i]) && filtered_data$second_rev[i] != "") {
            reviewer_name <- filtered_data$second_rev[i]
          }
          break
        }
      }
    }
    
    if (!is.null(second_comments)) {
      div(
        # Reviewer name if available
        if (!is.null(reviewer_name)) {
          tags$small(
            class = "text-muted mb-2 d-block",
            tags$strong("Reviewer: "), reviewer_name
          )
        },
        
        # Comments
        div(
          style = "
          background-color: #ffffff;
          border: 1px solid #dee2e6;
          border-radius: 4px;
          padding: 15px;
          line-height: 1.6;
          max-height: 200px;
          overflow-y: auto;
        ",
          # Split into paragraphs for better readability
          lapply(strsplit(second_comments, "\n\n")[[1]], function(para) {
            if (para != "") tags$p(para, style = "margin-bottom: 8px;")
          })
        )
      )
    } else {
      div(
        class = "text-muted text-center p-3",
        icon("info-circle", class = "mb-2"),
        tags$p("No secondary review comments available.")
      )
    }
  })
  
  # ============================================================================
  # MILESTONE MODULE INTEGRATION
  # ============================================================================
  
  # Initialize the CCC milestone module
  ccc_miles_mod <- mod_ccc_miles_server(
    id = "ccc_miles",
    existing_data = reactive({
      req(values$selected_resident)
      req(values$redcap_period)
      
      data <- app_data()
      
      if (!is.null(data$p_miles)) {
        existing_milestone_data <- data$p_miles %>%
          filter(
            name == values$selected_resident$name,
            period == values$redcap_period
          )
        
        if (nrow(existing_milestone_data) > 0) {
          message("Loading existing milestone data for CCC editing")
          return(existing_milestone_data[1, ])
        }
      }
      
      message("No existing milestone data found")
      return(NULL)
    })
  )
  
  # Handle milestone entry decision
  observeEvent(input$ccc_enter_milestones, {
    if (!is.null(input$ccc_enter_milestones)) {
      if (input$ccc_enter_milestones == "1") {
        milestone_entry_state$should_enter <- TRUE
        showNotification("Milestone entry interface will be shown below", type = "message")
      } else {
        milestone_entry_state$should_enter <- FALSE
        milestone_entry_state$entry_complete <- FALSE
      }
    }
  })
  
  # Check if second comments exist (for conditional display)
  output$has_second_comments <- reactive({
    req(values$selected_resident)
    
    resident_data <- processed_resident_data()
    filtered_data <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    if ("second_comments" %in% names(filtered_data)) {
      for (i in 1:nrow(filtered_data)) {
        if (!is.na(filtered_data$second_comments[i]) && filtered_data$second_comments[i] != "") {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  })
  outputOptions(output, "has_second_comments", suspendWhenHidden = FALSE)
  
  # Second comments display for milestone section
  output$second_comments_display <- renderUI({
    req(values$selected_resident)
    
    resident_data <- processed_resident_data()
    filtered_data <- resident_data %>%
      filter(name == values$selected_resident$name)
    
    second_comments <- NULL
    reviewer_name <- NULL
    
    if ("second_comments" %in% names(filtered_data)) {
      for (i in 1:nrow(filtered_data)) {
        if (!is.na(filtered_data$second_comments[i]) && filtered_data$second_comments[i] != "") {
          second_comments <- filtered_data$second_comments[i]
          if ("second_rev" %in% names(filtered_data) && 
              !is.na(filtered_data$second_rev[i]) && filtered_data$second_rev[i] != "") {
            reviewer_name <- filtered_data$second_rev[i]
          }
          break
        }
      }
    }
    
    if (!is.null(second_comments)) {
      div(
        if (!is.null(reviewer_name)) {
          div(class = "mb-2", tags$strong("Reviewer: "), reviewer_name)
        },
        div(
          style = "background-color: #f8f9fa; padding: 15px; border-radius: 4px; border-left: 4px solid #007bff;",
          second_comments
        )
      )
    } else {
      div(
        class = "text-muted text-center p-3",
        "No secondary review comments available."
      )
    }
  })
  
  # Milestone interface content
  output$milestone_interface_content <- renderUI({
    req(values$selected_resident)
    req(input$ccc_session)
    req(input$ccc_session != "7")  # Not Intern Intro
    
    # Get milestone existence status
    milestones_available <- milestones_exist()
    
    message("Rendering milestone interface - milestones available: ", milestones_available)
    
    if (milestones_available) {
      # EXISTING MILESTONES WORKFLOW
      tagList(
        div(
          class = "alert alert-success mb-3",
          icon("check-circle", class = "me-2"),
          tags$strong("Milestones Available:"),
          " Program milestone assessments found for this period."
        ),
        
        radioButtons(
          "ccc_mile",
          "Are the milestones complete and acceptable?",
          choices = c(
            "No - Need to edit" = "0",
            "Yes - Acceptable" = "1"
          ),
          selected = character(0)
        ),
        
        # Show editing interface when milestones are not acceptable
        conditionalPanel(
          condition = "input.ccc_mile == '0'",
          hr(),
          div(
            class = "alert alert-warning mb-3",
            icon("edit", class = "me-2"),
            tags$strong("Edit Required:"),
            " Please modify the milestone assessments below."
          ),
          
          # Comments for edits
          textAreaInput(
            "ccc_mile_concerns", 
            "Comments about milestone changes:",
            rows = 3,
            placeholder = "Explain what milestone changes you made and why..."
          ),
          
          # Enhanced milestone editing module
          mod_ccc_miles_ui("ccc_miles")
        )
      )
      
    } else {
      # NO EXISTING MILESTONES WORKFLOW
      tagList(
        div(
          class = "alert alert-warning mb-3",
          icon("exclamation-triangle", class = "me-2"),
          tags$strong("No Milestones Found:"),
          " No program milestone assessments found for this period."
        ),
        
        radioButtons(
          "ccc_enter_milestones",
          "Do you want to enter milestone assessments now?",
          choices = c(
            "No - Proceed without milestones" = "0",
            "Yes - Enter milestone assessments" = "1"
          ),
          selected = character(0)
        ),
        
        # Show milestone entry interface when user selects Yes
        conditionalPanel(
          condition = "input.ccc_enter_milestones == '1'",
          hr(),
          div(
            class = "alert alert-info mb-3",
            icon("plus-circle", class = "me-2"),
            tags$strong("Milestone Entry Mode:"),
            " Please assess the relevant milestones below."
          ),
          
          # Comments for new entry
          textAreaInput(
            "ccc_mile_concerns", 
            "Comments about milestone assessment:",
            rows = 3,
            placeholder = "Explain your milestone assessment rationale..."
          ),
          
          # Milestone entry module (same module, different context)
          div(
            class = "milestone-entry-container",
            style = "border: 2px solid #28a745; border-radius: 8px; padding: 15px; background-color: #f8fff9;",
            h6("Enter Milestone Assessments", class = "text-success mb-3"),
            mod_ccc_miles_ui("ccc_miles")
          )
        ),
        
        # Show status when user selects No
        conditionalPanel(
          condition = "input.ccc_enter_milestones == '0'",
          div(
            class = "alert alert-info mt-3",
            icon("info-circle", class = "me-2"),
            "CCC review will proceed without milestone assessment."
          )
        )
      )
    }
  })
  
  # Interim submit button
  output$interim_submit_button <- renderUI({
    can_submit <- TRUE
    validation_messages <- character(0)
    
    # Check interim notes
    if (is.null(input$ccc_interim) || trimws(input$ccc_interim) == "") {
      can_submit <- FALSE
      validation_messages <- c(validation_messages, "Please provide interim review notes")
    }
    
    # Check concerns
    if (is.null(input$ccc_concern) || input$ccc_concern == "") {
      can_submit <- FALSE
      validation_messages <- c(validation_messages, "Please indicate if there are concerns")
    }
    
    # If concerns = Yes, require action selection
    if (!is.null(input$ccc_concern) && input$ccc_concern == "1") {
      if (is.null(input$ccc_action) || length(input$ccc_action) == 0) {
        can_submit <- FALSE
        validation_messages <- c(validation_messages, "Please select at least one CCC action")
      }
      if (is.null(input$ccc_competency) || length(input$ccc_competency) == 0) {
        can_submit <- FALSE
        validation_messages <- c(validation_messages, "Please select at least one competency area")
      }
    }
    
    tagList(
      if (length(validation_messages) > 0) {
        div(
          class = "alert alert-warning mb-3",
          tags$strong("Please complete the following:"),
          tags$ul(
            lapply(validation_messages, function(msg) tags$li(msg))
          )
        )
      },
      
      actionButton(
        "submit_interim_review",
        "Submit Interim Review",
        class = if (can_submit) "btn-success btn-lg" else "btn-secondary btn-lg",
        icon = icon("save"),
        disabled = !can_submit
      )
    )
  })
  
  # Final submit button for scheduled reviews
  output$final_submit_button <- renderUI({
    can_submit <- TRUE
    validation_messages <- character(0)
    
    # Check if we're in milestone section for scheduled review
    if (!is.null(input$ccc_rev_type) && input$ccc_rev_type == "1" && 
        !is.null(input$ccc_session) && input$ccc_session != "7") {
      
      milestones_available <- milestones_exist()
      
      if (milestones_available) {
        # EXISTING MILESTONES: Require acceptance decision
        if (is.null(input$ccc_mile) || input$ccc_mile == "") {
          can_submit <- FALSE
          validation_messages <- c(validation_messages, "Please indicate if milestones are acceptable")
        }
        
        # If not acceptable, require edits and comments
        if (!is.null(input$ccc_mile) && input$ccc_mile == "0") {
          if (is.null(ccc_miles_mod$scores()) || length(ccc_miles_mod$scores()) == 0) {
            can_submit <- FALSE
            validation_messages <- c(validation_messages, "Please edit the milestone assessments")
          }
          if (is.null(input$ccc_mile_concerns) || trimws(input$ccc_mile_concerns) == "") {
            can_submit <- FALSE
            validation_messages <- c(validation_messages, "Please comment on milestone changes")
          }
        }
      } else {
        # NO EXISTING MILESTONES: Require entry decision
        if (is.null(input$ccc_enter_milestones) || input$ccc_enter_milestones == "") {
          can_submit <- FALSE
          validation_messages <- c(validation_messages, "Please decide whether to enter milestone assessments")
        }
        
        # If choosing to enter, require assessments and comments
        if (!is.null(input$ccc_enter_milestones) && input$ccc_enter_milestones == "1") {
          if (is.null(ccc_miles_mod$scores()) || length(ccc_miles_mod$scores()) == 0) {
            can_submit <- FALSE
            validation_messages <- c(validation_messages, "Please enter milestone assessments")
          }
          if (is.null(input$ccc_mile_concerns) || trimws(input$ccc_mile_concerns) == "") {
            can_submit <- FALSE
            validation_messages <- c(validation_messages, "Please comment on milestone assessment")
          }
        }
      }
    }
    
    tagList(
      if (length(validation_messages) > 0) {
        div(
          class = "alert alert-warning mb-3",
          tags$strong("Please complete the following:"),
          tags$ul(
            lapply(validation_messages, function(msg) tags$li(msg))
          )
        )
      },
      
      actionButton(
        "submit_ccc_review",
        "Submit CCC Review",
        class = if (can_submit) "btn-success btn-lg" else "btn-secondary btn-lg",
        icon = icon("save"),
        disabled = !can_submit
      )
    )
  })
  
  # ============================================================================
  # CCC REVIEW FORM SUBMISSION
  # ============================================================================
  
  # Handle initial submit button click - show confirmation modal
  observeEvent(input$submit_ccc_review, {
    req(values$selected_resident)
    
    # Create summary of what will be submitted
    review_type_display <- if (!is.null(input$ccc_rev_type) && input$ccc_rev_type == "1") {
      "Scheduled Review"
    } else if (!is.null(input$ccc_rev_type) && input$ccc_rev_type == "2") {
      "Interim Review"
    } else {
      "Unknown"
    }
    
    session_display <- if (!is.null(input$ccc_session) && input$ccc_session != "") {
      switch(input$ccc_session,
             "1" = "Mid Intern",
             "2" = "End Intern", 
             "3" = "Mid PGY2",
             "4" = "End PGY2",
             "5" = "Mid PGY3",
             "6" = "Graduation",
             "7" = "Intern Intro",
             paste("Session", input$ccc_session))
    } else {
      "Not specified"
    }
    
    concerns_display <- if (!is.null(input$ccc_concern)) {
      if (input$ccc_concern == "1") "Yes" else "No"
    } else {
      "Not specified"
    }
    
    milestone_display <- if (!is.null(input$ccc_mile)) {
      if (input$ccc_mile == "1") "Acceptable" else "Not Acceptable (edits required)"
    } else {
      "Not applicable"
    }
    
    showModal(modalDialog(
      title = div(
        icon("exclamation-triangle", class = "text-warning me-2"),
        "Confirm CCC Review Submission"
      ),
      
      div(
        h5("Review Summary:"),
        tags$ul(
          tags$li(tags$strong("Resident: "), values$selected_resident$name),
          tags$li(tags$strong("Review Type: "), review_type_display),
          if (input$ccc_rev_type == "1") {
            tags$li(tags$strong("Session: "), session_display)
          },
          if (input$ccc_rev_type == "1" && !is.null(input$ccc_session) && input$ccc_session != "7") {
            tags$li(tags$strong("Milestones: "), milestone_display)
          },
          tags$li(tags$strong("Concerns: "), concerns_display)
        ),
        
        hr(),
        
        div(
          class = "alert alert-info",
          icon("info-circle", class = "me-2"),
          tags$strong("This action cannot be undone."),
          " The CCC review will be permanently saved to REDCap."
        )
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton(
          "confirm_submit_ccc",
          "Submit CCC Review",
          class = "btn-success",
          icon = icon("save")
        )
      ),
      
      easyClose = FALSE,
      size = "m"
    ))
  })
  
  # Handle confirmed submission
  observeEvent(input$confirm_submit_ccc, {
    req(values$selected_resident)
    
    removeModal()
    
    # Show processing notification
    withProgress(message = "Submitting CCC review...", {
      
      # Get REDCap connection info
      redcap_url <- app_data()$url %||% "https://redcapsurvey.slu.edu/api/"
      token <- app_data()$rdm_token
      
      # Get record ID for the selected resident
      record_id <- find_record_id(app_data(), values$selected_resident$name)
      
      # Validate we have essential data
      if (is.null(record_id) || is.null(input$ccc_rev_type)) {
        showNotification("Error: Missing essential submission data", type = "error")
        return()
      }
      
      # STEP 1: Determine CCC session/instance using consolidated function
      if (input$ccc_rev_type == "1") {
        # SCHEDULED REVIEW
        if (!is.null(input$ccc_session) && input$ccc_session != "") {
          # Use selected session directly
          ccc_session_value <- input$ccc_session
          
          # Convert session to period name for milestone submission
          session_to_period_map <- c(
            "1" = "Mid Intern",
            "2" = "End Intern", 
            "3" = "Mid PGY2",
            "4" = "End PGY2",
            "5" = "Mid PGY3",
            "6" = "Graduation",
            "7" = "Intern Intro"
          )
          milestone_period <- session_to_period_map[input$ccc_session]
          
        } else {
          # Fallback: map from current app period
          current_app_period <- get_current_period()
          milestone_period <- map_to_milestone_period(values$selected_resident$Level, current_app_period)
          
          # Convert back to session number
          period_to_session_map <- c(
            "Mid Intern" = "1",
            "End Intern" = "2", 
            "Mid PGY2" = "3",
            "End PGY2" = "4",
            "Mid PGY3" = "5",
            "Graduation" = "6",
            "Graduating" = "6",
            "Intern Intro" = "7"
          )
          ccc_session_value <- period_to_session_map[milestone_period]
        }
        
        message("Scheduled review - using session: ", ccc_session_value, " for period: ", milestone_period)
        
      } else if (input$ccc_rev_type == "2") {
        # INTERIM REVIEW - get next available instance
        ccc_session_value <- get_redcap_instance(
          level = values$selected_resident$Level,
          period = "Interim",
          review_type = "interim",
          redcap_url = redcap_url,
          redcap_token = token,
          record_id = record_id
        )
        
        milestone_period <- NULL  # Not applicable for interim reviews
        message("Interim review - using instance: ", ccc_session_value)
        
      } else {
        showNotification("Error: Invalid review type", type = "error")
        return()
      }
      
      # Validate session/instance
      if (is.null(ccc_session_value) || is.na(ccc_session_value)) {
        showNotification("Error: Could not determine CCC session/instance", type = "error")
        return()
      }
      
      # STEP 2: Handle milestone edits (ONLY for scheduled reviews with unacceptable milestones)
      milestone_edits_made <- FALSE
      
      if (input$ccc_rev_type == "1" &&  # Scheduled review only
          !is.null(input$ccc_mile) && input$ccc_mile == "0" &&  # Milestones not acceptable
          !is.null(ccc_miles_mod$scores()) && length(ccc_miles_mod$scores()) > 0) {  # Edits were made
        
        message("Processing milestone edits for unacceptable milestones...")
        
        # Submit milestone edits using consolidated function
        milestone_result <- submit_milestone_data(
          redcap_url = redcap_url,
          redcap_token = token,
          record_id = record_id,
          selected_period = milestone_period,
          resident_level = values$selected_resident$Level,
          milestone_scores = ccc_miles_mod$scores(),
          milestone_desc = list()
        )
        
        if (milestone_result$success) {
          milestone_edits_made <- TRUE
          message("Milestone edits saved successfully")
        } else {
          showNotification(
            paste("Error saving milestone edits:", milestone_result$outcome_message), 
            type = "error", 
            duration = 10
          )
          return()  # Stop if milestone save failed
        }
      }
      
      # Handle milestone entry for new assessments
      if (input$ccc_rev_type == "1" &&  # Scheduled review only
          !is.null(input$ccc_enter_milestones) && input$ccc_enter_milestones == "1" &&  # User chose to enter milestones
          !is.null(ccc_miles_mod$scores()) && length(ccc_miles_mod$scores()) > 0) {  # Assessments were made
        
        message("Processing new milestone entries...")
        
        # Submit new milestone assessments
        milestone_result <- submit_milestone_data(
          redcap_url = redcap_url,
          redcap_token = token,
          record_id = record_id,
          selected_period = milestone_period,
          resident_level = values$selected_resident$Level,
          milestone_scores = ccc_miles_mod$scores(),
          milestone_desc = list()
        )
        
        if (milestone_result$success) {
          milestone_edits_made <- TRUE
          message("New milestone assessments saved successfully")
        } else {
          showNotification(
            paste("Error saving milestone assessments:", milestone_result$outcome_message), 
            type = "error", 
            duration = 10
          )
          return()  # Stop if milestone save failed
        }
      }
      
      # STEP 3: Build CCC review data
      ccc_data <- list(
        ccc_date = format(Sys.Date(), "%Y-%m-%d"),
        ccc_rev_type = input$ccc_rev_type,
        ccc_concern = input$ccc_concern %||% "0"  # Default to No concerns
      )
      
      # Add session for scheduled reviews
      if (input$ccc_rev_type == "1") {
        ccc_data$ccc_session <- ccc_session_value
      }
      
      # Add interim notes for interim reviews
      if (input$ccc_rev_type == "2") {
        if (!is.null(input$ccc_interim) && nzchar(trimws(input$ccc_interim))) {
          ccc_data$ccc_interim <- input$ccc_interim
        }
      }
      
      # Handle checkbox fields properly for REDCap
      if (!is.null(input$ccc_concern) && input$ccc_concern == "1") {
        
        # CHECKBOX: Actions suggested by CCC (ccc_action___1, ccc_action___2, etc.)
        if (!is.null(input$ccc_action) && length(input$ccc_action) > 0) {
          message("Processing ccc_action checkbox values: ", paste(input$ccc_action, collapse = ", "))
          
          # Create individual checkbox fields for each selected action
          for (action_value in input$ccc_action) {
            field_name <- paste0("ccc_action___", action_value)
            ccc_data[[field_name]] <- "1"  # REDCap checkbox: 1 = checked, 0 = unchecked
            message("Adding checkbox field: ", field_name, " = 1")
          }
        }
        
        # CHECKBOX: Action Status (ccc_action_status___1, ccc_action_status___2, etc.)
        if (!is.null(input$ccc_action_status) && length(input$ccc_action_status) > 0) {
          message("Processing ccc_action_status checkbox values: ", paste(input$ccc_action_status, collapse = ", "))
          
          # Create individual checkbox fields for each selected status
          for (status_value in input$ccc_action_status) {
            field_name <- paste0("ccc_action_status___", status_value)
            ccc_data[[field_name]] <- "1"  # REDCap checkbox: 1 = checked
            message("Adding checkbox field: ", field_name, " = 1")
          }
        }
        
        # CHECKBOX: Competency areas (ccc_competency___1, ccc_competency___2, etc.)
        if (!is.null(input$ccc_competency) && length(input$ccc_competency) > 0) {
          message("Processing ccc_competency checkbox values: ", paste(input$ccc_competency, collapse = ", "))
          
          # Create individual checkbox fields for each selected competency
          for (competency_value in input$ccc_competency) {
            field_name <- paste0("ccc_competency___", competency_value)
            ccc_data[[field_name]] <- "1"  # REDCap checkbox: 1 = checked
            message("Adding checkbox field: ", field_name, " = 1")
          }
        }
      }
      
      # Add scheduled review specific fields
      if (input$ccc_rev_type == "1") {
        
        # ILP comments
        if (!is.null(input$ccc_ilp) && nzchar(trimws(input$ccc_ilp))) {
          ccc_data$ccc_ilp <- input$ccc_ilp
        }
        
        # Concern notes (when concerns exist) - NEW FIELD
        if (!is.null(input$ccc_concern) && input$ccc_concern == "1" &&
            !is.null(input$ccc_concern_notes) && nzchar(trimws(input$ccc_concern_notes))) {
          ccc_data$ccc_concern_notes <- input$ccc_concern_notes
        }
        
        # Follow-up actions (only if checkbox is checked)
        if (!is.null(input$has_action_items) && input$has_action_items &&
            !is.null(input$ccc_issues_follow_up) && nzchar(trimws(input$ccc_issues_follow_up))) {
          ccc_data$ccc_issues_follow_up <- input$ccc_issues_follow_up
        }
        
        # Person responsible
        if (!is.null(input$ccc_fu_resp) && nzchar(trimws(input$ccc_fu_resp))) {
          ccc_data$ccc_fu_resp <- input$ccc_fu_resp
        }
        
        # Milestone completion status (NOT for Intern Intro session = 7)
        if (!is.null(input$ccc_session) && input$ccc_session != "7") {
          if (milestone_edits_made) {
            ccc_data$ccc_mile <- "1"  # Now acceptable after edits
          } else {
            ccc_data$ccc_mile <- input$ccc_mile %||% "1"  # Default to acceptable
          }
          
          # Milestone comments
          if (!is.null(input$ccc_mile_concerns) && nzchar(trimws(input$ccc_mile_concerns))) {
            if (milestone_edits_made) {
              ccc_data$ccc_mile_notes <- paste0("Milestone edits made: ", input$ccc_mile_concerns)
            } else {
              ccc_data$ccc_mile_notes <- input$ccc_mile_concerns
            }
          }
        }
        
        # Additional comments
        if (!is.null(input$ccc_comments) && nzchar(trimws(input$ccc_comments))) {
          ccc_data$ccc_comments <- input$ccc_comments
        }
      }
      
      # Add interim review specific fields
      if (input$ccc_rev_type == "2") {
        
        # Concern notes (when concerns exist) - NEW FIELD for interim too
        if (!is.null(input$ccc_concern) && input$ccc_concern == "1" &&
            !is.null(input$ccc_concern_notes) && nzchar(trimws(input$ccc_concern_notes))) {
          ccc_data$ccc_concern_notes <- input$ccc_concern_notes
        }
        
        # Person responsible
        if (!is.null(input$ccc_fu_resp) && nzchar(trimws(input$ccc_fu_resp))) {
          ccc_data$ccc_fu_resp <- input$ccc_fu_resp
        }
        
        # Follow-up actions for interim reviews
        if (!is.null(input$has_action_items) && input$has_action_items &&
            !is.null(input$ccc_issues_follow_up) && nzchar(trimws(input$ccc_issues_follow_up))) {
          ccc_data$ccc_issues_follow_up <- input$ccc_issues_follow_up
        }
      }
      
      # Set completion status
      ccc_data$ccc_review_complete <- "2"  # 2 = Complete
      
      # Debug: Log what we're about to submit
      message("=== CCC Data to Submit (with proper checkbox fields) ===")
      message("Review Type: ", input$ccc_rev_type, " (", 
              if(input$ccc_rev_type == "1") "Scheduled" else "Interim", ")")
      message("Session/Instance: ", ccc_session_value)
      for (field in names(ccc_data)) {
        message(paste(field, ":", ccc_data[[field]]))
      }
      message("========================")
      
      # STEP 4: Build JSON for REDCap submission
      json_data <- paste0(
        '[{"record_id":"', escape_json_string(as.character(record_id)), '"',
        ',"redcap_repeat_instrument":"ccc_review"',
        ',"redcap_repeat_instance":"', escape_json_string(as.character(ccc_session_value)), '"'
      )
      
      # Add all CCC fields to JSON
      for (field in names(ccc_data)) {
        if (!is.null(ccc_data[[field]]) && !is.na(ccc_data[[field]])) {
          value <- escape_json_string(as.character(ccc_data[[field]]))
          json_data <- paste0(json_data, ',"', field, '":"', value, '"')
        }
      }
      
      json_data <- paste0(json_data, "}]")
      
      message("CCC review JSON with checkbox fields (first 500 chars): ", substr(json_data, 1, 500))
      
      # STEP 5: Submit to REDCap
      response <- tryCatch({
        httr::POST(
          url = redcap_url,
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
          encode = "form",
          httr::timeout(30)
        )
      }, error = function(e) {
        return(list(status_code = 0, error = e$message))
      })
      
      # STEP 6: Process response
      if ("error" %in% names(response)) {
        showNotification(paste("HTTP error:", response$error), type = "error", duration = 10)
        return()
      }
      
      status_code <- httr::status_code(response)
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      
      message("CCC review REDCap response status: ", status_code)
      message("CCC review REDCap response content: ", content_text)
      
      # Check for specific REDCap field errors
      if (status_code != 200 || grepl("fields were not found", content_text)) {
        message("POSSIBLE FIELD ERROR - REDCap Response: ", content_text)
        showNotification(
          paste("REDCap field error: ", content_text, 
                ". Please check your REDCap project structure."), 
          type = "error", 
          duration = 15
        )
        return()
      }
      
      if (status_code == 200) {
        tryCatch({
          if (grepl("^\\{", content_text)) {
            response_json <- jsonlite::fromJSON(content_text)
            records_updated <- as.numeric(response_json$count)
          } else {
            records_updated <- as.numeric(content_text)
          }
          
          if (!is.na(records_updated) && records_updated > 0) {
            success_message <- if (milestone_edits_made) {
              paste0("CCC review and milestone edits successfully submitted! (", records_updated, " record updated)")
            } else {
              paste0("CCC review successfully submitted! (", records_updated, " record updated)")
            }
            
            showNotification(success_message, type = "message", duration = 5)
            
            # Navigate back to dashboard
            shinyjs::hide("ccc-review-pages")
            shinyjs::hide("milestone-review-section")
            shinyjs::show("scheduled-step-2")
            shinyjs::delay(100, {
              shinyjs::show("ccc-dashboard-page")
            })
            
            # Reset resident selection and clear form
            values$selected_resident <- NULL
            values$current_period <- NULL
            values$redcap_period <- NULL
            clear_ccc_form_inputs(session)
            
          } else {
            showNotification("Error: No records were updated in REDCap. Please try again.", 
                             type = "error", duration = 10)
          }
          
        }, error = function(e) {
          message("Parse error but HTTP 200, assuming success: ", e$message)
          
          success_message <- if (milestone_edits_made) {
            "CCC review and milestone edits submitted successfully!"
          } else {
            "CCC review submitted successfully!"
          }
          
          showNotification(success_message, type = "message", duration = 5)
          
          # Navigate back to dashboard
          shinyjs::hide("ccc-review-pages")
          shinyjs::hide("milestone-review-section")
          shinyjs::show("scheduled-step-2")
          shinyjs::delay(100, {
            shinyjs::show("ccc-dashboard-page")
          })
          
          # Reset resident selection and clear form
          values$selected_resident <- NULL
          values$current_period <- NULL
          values$redcap_period <- NULL
          clear_ccc_form_inputs(session)
        })
        
      } else {
        if (grepl("Form Status field", content_text)) {
          success_message <- if (milestone_edits_made) {
            "CCC review and milestone edits saved (form status warning ignored)"
          } else {
            "CCC review saved (form status warning ignored)"
          }
          
          showNotification(success_message, type = "message", duration = 5)
          
          # Navigate back to dashboard
          shinyjs::hide("ccc-review-pages")
          shinyjs::hide("milestone-review-section")
          shinyjs::show("scheduled-step-2")
          shinyjs::delay(100, {
            shinyjs::show("ccc-dashboard-page")
          })
          
          # Reset resident selection and clear form
          values$selected_resident <- NULL
          values$current_period <- NULL
          values$redcap_period <- NULL
          clear_ccc_form_inputs(session)
          
        } else {
          showNotification(paste("REDCap error (", status_code, "):", content_text), 
                           type = "error", duration = 10)
        }
      }
    })
  })
  
  # Handle interim review submission
  observeEvent(input$submit_interim_review, {
    req(values$selected_resident)
    
    # Show processing notification
    withProgress(message = "Submitting interim review...", {
      
      # Get REDCap connection info
      redcap_url <- app_data()$url %||% "https://redcapsurvey.slu.edu/api/"
      token <- app_data()$rdm_token
      
      # Get record ID for the selected resident
      record_id <- find_record_id(app_data(), values$selected_resident$name)
      
      # Validate we have essential data
      if (is.null(record_id)) {
        showNotification("Error: Could not find record ID for resident", type = "error")
        return()
      }
      
      # Get next available instance for interim review
      ccc_session_value <- get_redcap_instance(
        level = values$selected_resident$Level,
        period = "Interim",
        review_type = "interim",
        redcap_url = redcap_url,
        redcap_token = token,
        record_id = record_id
      )
      
      # Build CCC review data for interim review
      ccc_data <- list(
        ccc_date = format(Sys.Date(), "%Y-%m-%d"),
        ccc_rev_type = "2",  # Interim review
        ccc_interim = input$ccc_interim,
        ccc_concern = input$ccc_concern %||% "0"
      )
      
      # Handle checkbox fields for concerns
      if (!is.null(input$ccc_concern) && input$ccc_concern == "1") {
        
        # Actions
        if (!is.null(input$ccc_action) && length(input$ccc_action) > 0) {
          for (action_value in input$ccc_action) {
            field_name <- paste0("ccc_action___", action_value)
            ccc_data[[field_name]] <- "1"
          }
        }
        
        # Action Status
        if (!is.null(input$ccc_action_status) && length(input$ccc_action_status) > 0) {
          for (status_value in input$ccc_action_status) {
            field_name <- paste0("ccc_action_status___", status_value)
            ccc_data[[field_name]] <- "1"
          }
        }
        
        # Competency areas
        if (!is.null(input$ccc_competency) && length(input$ccc_competency) > 0) {
          for (competency_value in input$ccc_competency) {
            field_name <- paste0("ccc_competency___", competency_value)
            ccc_data[[field_name]] <- "1"
          }
        }
      }
      
      # Person responsible
      if (!is.null(input$ccc_fu_resp) && nzchar(trimws(input$ccc_fu_resp))) {
        ccc_data$ccc_fu_resp <- input$ccc_fu_resp
      }
      
      # Follow-up actions
      if (!is.null(input$has_action_items) && input$has_action_items &&
          !is.null(input$ccc_issues_follow_up) && nzchar(trimws(input$ccc_issues_follow_up))) {
        ccc_data$ccc_issues_follow_up <- input$ccc_issues_follow_up
      }
      
      # Set completion status
      ccc_data$ccc_review_complete <- "2"  # 2 = Complete
      
      # Build JSON for REDCap submission
      json_data <- paste0(
        '[{"record_id":"', escape_json_string(as.character(record_id)), '"',
        ',"redcap_repeat_instrument":"ccc_review"',
        ',"redcap_repeat_instance":"', escape_json_string(as.character(ccc_session_value)), '"'
      )
      
      # Add all CCC fields to JSON
      for (field in names(ccc_data)) {
        if (!is.null(ccc_data[[field]]) && !is.na(ccc_data[[field]])) {
          value <- escape_json_string(as.character(ccc_data[[field]]))
          json_data <- paste0(json_data, ',"', field, '":"', value, '"')
        }
      }
      
      json_data <- paste0(json_data, "}]")
      
      # Submit to REDCap
      response <- tryCatch({
        httr::POST(
          url = redcap_url,
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
          encode = "form",
          httr::timeout(30)
        )
      }, error = function(e) {
        return(list(status_code = 0, error = e$message))
      })
      
      # Process response
      if ("error" %in% names(response)) {
        showNotification(paste("HTTP error:", response$error), type = "error", duration = 10)
        return()
      }
      
      status_code <- httr::status_code(response)
      content_text <- httr::content(response, "text", encoding = "UTF-8")
      
      if (status_code == 200) {
        showNotification("Interim review submitted successfully!", type = "message", duration = 5)
        
        # Navigate back to dashboard
        shinyjs::hide("ccc-review-pages")
        shinyjs::delay(100, {
          shinyjs::show("ccc-dashboard-page")
        })
        
        # Reset resident selection and clear form
        values$selected_resident <- NULL
        values$current_period <- NULL
        values$redcap_period <- NULL
        clear_ccc_form_inputs(session)
        
      } else {
        showNotification(paste("REDCap error (", status_code, "):", content_text), 
                         type = "error", duration = 10)
      }
    })
  })
  
} # End of server function