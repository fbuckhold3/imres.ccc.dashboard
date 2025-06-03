# Advanced Visual Milestone Assessment Module
# Enhanced version with slider inputs, visual progression, and better UX

#' Visual Milestone Assessment UI Module
#' @export
visual_milestones_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Enhanced CSS styling
    tags$head(
      tags$style(HTML("
        .milestone-container {
          background: linear-gradient(135deg, #f5f7fa 0%, #c3cfe2 100%);
          border-radius: 15px;
          padding: 20px;
          margin-bottom: 25px;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        
        .milestone-header {
          background: linear-gradient(45deg, #667eea 0%, #764ba2 100%);
          color: white;
          padding: 15px 20px;
          border-radius: 10px;
          margin-bottom: 20px;
          text-align: center;
          font-weight: bold;
          font-size: 1.2em;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
        }
        
        .milestone-card {
          background: white;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 15px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
          transition: all 0.3s ease;
          border-left: 4px solid #e9ecef;
        }
        
        .milestone-card:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
        }
        
        .milestone-card.level-1-2 { border-left-color: #dc3545; }
        .milestone-card.level-3-4 { border-left-color: #fd7e14; }
        .milestone-card.level-5-6 { border-left-color: #ffc107; }
        .milestone-card.level-7-8 { border-left-color: #20c997; }
        .milestone-card.level-9 { border-left-color: #198754; }
        
        .milestone-title {
          font-weight: 600;
          color: #2c3e50;
          margin-bottom: 10px;
          cursor: pointer;
          display: flex;
          align-items: center;
          justify-content: space-between;
        }
        
        .milestone-title:hover {
          color: #3498db;
        }
        
        .milestone-controls {
          display: flex;
          align-items: center;
          gap: 15px;
          margin-top: 10px;
        }
        
        .level-display {
          min-width: 120px;
          text-align: center;
          padding: 8px 12px;
          border-radius: 20px;
          font-weight: bold;
          font-size: 0.9em;
          transition: all 0.3s ease;
        }
        
        .level-0 { background: #6c757d; color: white; }
        .level-1-2 { background: #dc3545; color: white; }
        .level-3-4 { background: #fd7e14; color: white; }
        .level-5-6 { background: #ffc107; color: #212529; }
        .level-7-8 { background: #20c997; color: white; }
        .level-9 { background: #198754; color: white; }
        
        .milestone-slider {
          flex: 1;
          margin: 0 15px;
        }
        
        .milestone-slider .irs {
          height: 40px;
        }
        
        .milestone-slider .irs-line {
          height: 8px;
          background: linear-gradient(to right, #dc3545, #fd7e14, #ffc107, #20c997, #198754);
          border-radius: 4px;
        }
        
        .milestone-slider .irs-handle {
          width: 20px;
          height: 20px;
          background: #fff;
          border: 3px solid #007bff;
          border-radius: 50%;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.2);
        }
        
        .progress-overview {
          background: white;
          border-radius: 10px;
          padding: 20px;
          margin-bottom: 25px;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .progress-bar-custom {
          height: 25px;
          border-radius: 12px;
          background: linear-gradient(45deg, #667eea 0%, #764ba2 100%);
          color: white;
          font-weight: bold;
          line-height: 25px;
        }
        
        .action-buttons {
          background: white;
          border-radius: 10px;
          padding: 20px;
          text-align: center;
          box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
        }
        
        .btn-custom {
          margin: 0 10px;
          padding: 10px 25px;
          border-radius: 25px;
          font-weight: 600;
          transition: all 0.3s ease;
        }
        
        .btn-custom:hover {
          transform: translateY(-2px);
          box-shadow: 0 4px 8px rgba(0, 0, 0, 0.2);
        }
        
        .changes-indicator {
          position: absolute;
          top: -5px;
          right: -5px;
          background: #dc3545;
          color: white;
          border-radius: 50%;
          width: 20px;
          height: 20px;
          font-size: 0.8em;
          font-weight: bold;
          display: flex;
          align-items: center;
          justify-content: center;
          animation: pulse 1.5s infinite;
        }
        
        @keyframes pulse {
          0% { transform: scale(1); }
          50% { transform: scale(1.1); }
          100% { transform: scale(1); }
        }
        
        .milestone-image-btn {
          background: none;
          border: none;
          color: #007bff;
          cursor: pointer;
          font-size: 1.1em;
          padding: 5px;
          border-radius: 3px;
          transition: all 0.2s ease;
        }
        
        .milestone-image-btn:hover {
          background: #007bff;
          color: white;
        }
      "))
    ),
    
    # Progress Overview
    div(
      class = "progress-overview",
      h4("Assessment Progress", style = "color: #2c3e50; margin-bottom: 15px;"),
      div(
        class = "row",
        div(class = "col-md-8",
            div(
              class = "progress",
              div(id = ns("progress_bar"), class = "progress-bar progress-bar-custom", 
                  role = "progressbar", style = "width: 0%",
                  textOutput(ns("progress_text"), inline = TRUE))
            )
        ),
        div(class = "col-md-4",
            div(style = "text-align: center;",
                h5(textOutput(ns("completion_stats"), inline = TRUE), 
                   style = "color: #2c3e50; margin: 0;"))
        )
      )
    ),
    
    # Patient Care & Medical Knowledge
    div(
      class = "milestone-container",
      div(class = "milestone-header", "🏥 Patient Care & Medical Knowledge"),
      uiOutput(ns("pc_mk_visual"))
    ),
    
    # Systems-Based Practice & Practice-Based Learning
    div(
      class = "milestone-container",
      div(class = "milestone-header", "🔬 Systems-Based Practice & Practice-Based Learning"),
      uiOutput(ns("sbp_pbl_visual"))
    ),
    
    # Professionalism & Interpersonal Communication
    div(
      class = "milestone-container",
      div(class = "milestone-header", "🤝 Professionalism & Interpersonal Communication"),
      uiOutput(ns("prof_ics_visual"))
    ),
    
    # Action Buttons
    div(
      class = "action-buttons",
      div(style = "position: relative; display: inline-block;",
          actionButton(ns("reset"), "↺ Reset All", class = "btn-secondary btn-custom"),
          conditionalPanel(
            condition = "false", # Will be shown via JavaScript when changes detected
            span(class = "changes-indicator", "!")
          )
      ),
      actionButton(ns("preview"), "👁 Preview Changes", class = "btn-info btn-custom"),
      actionButton(ns("submit"), "💾 Submit Assessment", class = "btn-success btn-custom")
    ),
    
    # Status Messages
    uiOutput(ns("status_display"))
  )
}

#' Visual Milestone Assessment Server Module
#' @export
visual_milestones_server <- function(id, resident, period, miles, redcap_uri, token) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Enhanced milestone structure with descriptions
    milestone_definitions <- list(
      pc_mk = list(
        title = "Patient Care & Medical Knowledge",
        icon = "🏥",
        milestones = list(
          rep_pc1 = list(
            name = "PC1: Gather History & Physical Exam",
            description = "Ability to gather essential and accurate information"
          ),
          rep_pc2 = list(
            name = "PC2: Prioritize Differential Diagnosis",
            description = "Develops prioritized differential diagnoses"
          ),
          rep_pc3 = list(
            name = "PC3: Recommend & Interpret Tests",
            description = "Recommends and interprets common diagnostic tests"
          ),
          rep_pc4 = list(
            name = "PC4: Enter & Discuss Treatment Plans",
            description = "Develops and implements treatment plans"
          ),
          rep_pc5 = list(
            name = "PC5: Perform Procedures",
            description = "Performs basic and advanced procedures"
          ),
          rep_pc6 = list(
            name = "PC6: Provide Transfer of Care",
            description = "Provides clear and timely transfer of care"
          ),
          rep_mk1 = list(
            name = "MK1: Medical Knowledge",
            description = "Demonstrates medical knowledge"
          ),
          rep_mk2 = list(
            name = "MK2: Clinical Reasoning",
            description = "Applies medical knowledge in clinical reasoning"
          ),
          rep_mk3 = list(
            name = "MK3: Scientific Knowledge", 
            description = "Applies scientific knowledge to patient care"
          )
        )
      ),
      sbp_pbl = list(
        title = "Systems-Based Practice & Practice-Based Learning",
        icon = "🔬",
        milestones = list(
          rep_sbp1 = list(
            name = "SBP1: Patient Safety & Quality Improvement",
            description = "Works to improve patient safety and quality"
          ),
          rep_sbp2 = list(
            name = "SBP2: System Navigation",
            description = "Navigates healthcare systems effectively"
          ),
          rep_sbp3 = list(
            name = "SBP3: Physician Role in Healthcare Systems",
            description = "Understands physician role in healthcare systems"
          ),
          rep_pbl1 = list(
            name = "PBL1: Evidence-Based Practice",
            description = "Incorporates evidence into practice"
          ),
          rep_pbl2 = list(
            name = "PBL2: Reflective Practice & Commitment to Growth",
            description = "Engages in reflective practice and learning"
          )
        )
      ),
      prof_ics = list(
        title = "Professionalism & Interpersonal Communication",
        icon = "🤝",
        milestones = list(
          rep_prof1 = list(
            name = "PROF1: Professional Behavior",
            description = "Demonstrates professional behavior"
          ),
          rep_prof2 = list(
            name = "PROF2: Ethical Principles",
            description = "Applies ethical principles in practice"
          ),
          rep_prof3 = list(
            name = "PROF3: Accountability",
            description = "Demonstrates accountability for patient care"
          ),
          rep_prof4 = list(
            name = "PROF4: Well-being",
            description = "Manages personal and professional well-being"
          ),
          rep_ics1 = list(
            name = "ICS1: Patient & Family Communication",
            description = "Communicates effectively with patients and families"
          ),
          rep_ics2 = list(
            name = "ICS2: Interprofessional Communication",
            description = "Communicates with healthcare team members"
          ),
          rep_ics3 = list(
            name = "ICS3: Communication in Difficult Situations",
            description = "Handles difficult conversations effectively"
          )
        )
      )
    )
    
    # Reactive values
    values <- reactiveValues(
      current_data = NULL,
      original_data = NULL,
      changes_count = 0,
      submission_status = NULL
    )
    
    # Helper functions
    get_level_name <- function(score) {
      case_when(
        is.na(score) || score == 0 ~ "Not Assessed",
        score <= 2 ~ "Novice",
        score <= 4 ~ "Advanced Beginner",
        score <= 6 ~ "Competent", 
        score <= 8 ~ "Proficient",
        score == 9 ~ "Expert",
        TRUE ~ "Unknown"
      )
    }
    
    get_level_class <- function(score) {
      case_when(
        is.na(score) || score == 0 ~ "level-0",
        score <= 2 ~ "level-1-2",
        score <= 4 ~ "level-3-4",
        score <= 6 ~ "level-5-6",
        score <= 8 ~ "level-7-8",
        score == 9 ~ "level-9",
        TRUE ~ "level-0"
      )
    }
    
    # Fetch record ID
    record_id <- reactive({
      req(resident())
      rid <- miles %>%
        dplyr::filter(name == resident()) %>%
        dplyr::pull(record_id) %>%
        unique()
      
      if (length(rid) > 0) return(rid[1]) else return(NULL)
    })
    
    # Fetch existing data
    existing_data <- reactive({
      req(resident(), period())
      filtered_data <- miles %>%
        dplyr::filter(name == resident(), prog_mile_period == period())
      
      if (nrow(filtered_data) == 0) return(NULL)
      return(filtered_data)
    })
    
    # Initialize data
    observe({
      data <- existing_data()
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      
      if (!is.null(data) && nrow(data) > 0) {
        values$original_data <- data
        values$current_data <- data
        
        for (field in all_fields) {
          if (field %in% colnames(data)) {
            updateSliderInput(session, field, value = data[[field]][1])
          } else {
            updateSliderInput(session, field, value = 0)
          }
        }
      } else {
        empty_data <- data.frame(matrix(0, nrow = 1, ncol = length(all_fields)))
        colnames(empty_data) <- all_fields
        
        values$original_data <- empty_data
        values$current_data <- empty_data
        
        for (field in all_fields) {
          updateSliderInput(session, field, value = 0)
        }
      }
      
      values$changes_count <- 0
    })
    
    # Render milestone groups
    render_milestone_group <- function(group_key) {
      renderUI({
        group <- milestone_definitions[[group_key]]
        
        milestone_items <- lapply(names(group$milestones), function(milestone_id) {
          milestone_info <- group$milestones[[milestone_id]]
          
          current_value <- if (!is.null(values$current_data) && milestone_id %in% colnames(values$current_data)) {
            values$current_data[[milestone_id]][1]
          } else {
            0
          }
          
          div(
            class = paste("milestone-card", get_level_class(current_value)),
            div(
              class = "milestone-title",
              span(milestone_info$name),
              actionButton(
                ns(paste0("img_", milestone_id)),
                icon("image"),
                class = "milestone-image-btn",
                title = "View milestone details"
              )
            ),
            p(milestone_info$description, style = "color: #6c757d; font-size: 0.9em; margin-bottom: 10px;"),
            div(
              class = "milestone-controls",
              div(
                class = paste("level-display", get_level_class(current_value)),
                get_level_name(current_value)
              ),
              div(
                class = "milestone-slider",
                sliderInput(
                  ns(milestone_id),
                  label = NULL,
                  min = 0,
                  max = 9,
                  value = current_value,
                  step = 1,
                  width = "100%"
                )
              ),
              div(
                style = "min-width: 50px; text-align: center; font-weight: bold; font-size: 1.2em;",
                textOutput(ns(paste0("value_", milestone_id)), inline = TRUE)
              )
            )
          )
        })
        
        do.call(tagList, milestone_items)
      })
    }
    
    # Render each group
    output$pc_mk_visual <- render_milestone_group("pc_mk")
    output$sbp_pbl_visual <- render_milestone_group("sbp_pbl")
    output$prof_ics_visual <- render_milestone_group("prof_ics")
    
    # Update value displays and level indicators when sliders change
    observe({
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      
      for (field in all_fields) {
        local({
          field_local <- field
          output[[paste0("value_", field_local)]] <- renderText({
            val <- input[[field_local]] %||% 0
            as.character(val)
          })
        })
      }
    })
    
    # Calculate progress
    observe({
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      total_milestones <- length(all_fields)
      assessed_count <- 0
      total_score <- 0
      
      for (field in all_fields) {
        val <- input[[field]] %||% 0
        if (val > 0) {
          assessed_count <- assessed_count + 1
          total_score <- total_score + val
        }
      }
      
      progress_percent <- round((assessed_count / total_milestones) * 100)
      avg_score <- if (assessed_count > 0) round(total_score / assessed_count, 1) else 0
      
      output$progress_text <- renderText({
        paste0(progress_percent, "% Complete")
      })
      
      output$completion_stats <- renderText({
        paste0(assessed_count, "/", total_milestones, " Assessed (Avg: ", avg_score, ")")
      })
      
      # Update progress bar
      session$sendCustomMessage("updateProgress", list(percent = progress_percent))
    })
    
    # Track changes
    observe({
      if (!is.null(values$original_data)) {
        all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
        changes <- 0
        
        for (field in all_fields) {
          original_val <- if (field %in% colnames(values$original_data)) {
            values$original_data[[field]][1]
          } else {
            0
          }
          current_val <- input[[field]] %||% 0
          
          if (original_val != current_val) {
            changes <- changes + 1
          }
        }
        
        values$changes_count <- changes
      }
    })
    
    # Handle milestone image clicks
    observe({
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      
      for (field in all_fields) {
        local({
          field_local <- field
          observeEvent(input[[paste0("img_", field_local)]], {
            
            # Check if PNG exists in imres package
            img_path <- system.file("www", "milestones", paste0(field_local, ".png"), package = "imres")
            
            modal_content <- if (file.exists(img_path)) {
              tagList(
                div(style = "text-align: center;",
                    img(src = paste0("imres/milestones/", field_local, ".png"), 
                        style = "max-width: 100%; height: auto; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);")),
                hr(),
                div(
                  class = "alert alert-info",
                  h5("Assessment Levels:"),
                  tags$ul(
                    tags$li(tags$strong("1-2 (Novice):"), "Requires direct supervision"),
                    tags$li(tags$strong("3-4 (Advanced Beginner):"), "Requires some supervision"),
                    tags$li(tags$strong("5-6 (Competent):"), "Can work independently"),
                    tags$li(tags$strong("7-8 (Proficient):"), "Can supervise others"),
                    tags$li(tags$strong("9 (Expert):"), "Exceptional ability, teaches others")
                  )
                )
              )
            } else {
              tagList(
                div(class = "alert alert-warning",
                    h5("Milestone Image Not Available"),
                    p(paste("Detailed visual description for", field_local, "would be displayed here."))),
                div(
                  class = "alert alert-info",
                  h5("Assessment Levels:"),
                  tags$ul(
                    tags$li(tags$strong("1-2 (Novice):"), "Requires direct supervision"),
                    tags$li(tags$strong("3-4 (Advanced Beginner):"), "Requires some supervision"),
                    tags$li(tags$strong("5-6 (Competent):"), "Can work independently"),
                    tags$li(tags$strong("7-8 (Proficient):"), "Can supervise others"),
                    tags$li(tags$strong("9 (Expert):"), "Exceptional ability, teaches others")
                  )
                )
              )
            }
            
            showModal(modalDialog(
              title = paste("Milestone Details:", field_local),
              modal_content,
              easyClose = TRUE,
              size = "l",
              footer = modalButton("Close")
            ))
          })
        })
      }
    })
    
    # Reset functionality
    observeEvent(input$reset, {
      if (!is.null(values$original_data)) {
        all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
        for (field in all_fields) {
          if (field %in% colnames(values$original_data)) {
            updateSliderInput(session, field, value = values$original_data[[field]][1])
          } else {
            updateSliderInput(session, field, value = 0)
          }
        }
        values$submission_status <- list(type = "info", message = "✅ All values reset to original state.")
      }
    })
    
    # Preview changes
    observeEvent(input$preview, {
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      changes <- list()
      
      for (field in all_fields) {
        original_val <- if (!is.null(values$original_data) && field %in% colnames(values$original_data)) {
          values$original_data[[field]][1]
        } else {
          0
        }
        current_val <- input[[field]] %||% 0
        
        if (original_val != current_val) {
          changes[[field]] <- list(
            from = original_val,
            to = current_val,
            from_level = get_level_name(original_val),
            to_level = get_level_name(current_val)
          )
        }
      }
      
      if (length(changes) == 0) {
        showModal(modalDialog(
          title = "📊 Preview Changes",
          div(class = "alert alert-info", "No changes detected from original values."),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        change_items <- lapply(names(changes), function(field) {
          change <- changes[[field]]
          div(
            class = "d-flex justify-content-between align-items-center mb-2 p-2",
            style = "background: #f8f9fa; border-radius: 5px;",
            span(tags$strong(field)),
            span(
              span(change$from_level, class = paste("badge", get_level_class(change$from))),
              " → ",
              span(change$to_level, class = paste("badge", get_level_class(change$to)))
            )
          )
        })
        
        showModal(modalDialog(
          title = paste("📊 Preview Changes (", length(changes), " changes)"),
          div(
            div(class = "alert alert-warning",
                paste("You are about to submit", length(changes), "milestone changes.")),
            do.call(tagList, change_items)
          ),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("confirm_submit"), "Submit Changes", class = "btn-success")
          )
        ))
      }
    })
    
    # Submit functionality - UPDATED to use your existing helper functions
    submit_milestones <- function() {
      req(record_id(), period())
      
      if (period() %in% c("Entering Residency", "Interim Review", "Intern Intro")) {
        values$submission_status <- list(
          type = "error",
          message = "❌ Submission not allowed for 'Entering Residency', 'Interim Review', or 'Intern Intro' periods."
        )
        return()
      }
      
      # Collect all input values
      all_fields <- unlist(lapply(milestone_definitions, function(x) names(x$milestones)))
      input_values <- list()
      for (field in all_fields) {
        input_values[[field]] <- input[[field]] %||% 0
      }
      
      # UPDATED: Use your existing map_period_format function with milestone context
      instance_number <- map_period_format(
        level = resident_level, # We'll need to pass this
        period = period(),
        return_type = "instance",
        form_context = "milestone"
      )
      
      if (is.na(instance_number) || is.null(instance_number)) {
        values$submission_status <- list(
          type = "error",
          message = paste("❌ Cannot determine REDCap instance for period:", period())
        )
        return()
      }
      
      message("Using instance ", instance_number, " for period ", period(), " (will overwrite existing data)")
      
      # Create submission data using your existing structure
      submission_data <- data.frame(
        record_id = as.character(record_id()),
        redcap_repeat_instrument = "milestone_entry",
        redcap_repeat_instance = as.numeric(instance_number),
        prog_mile_period = as.numeric(instance_number),  # Period matches instance
        prog_mile_date = format(Sys.Date(), "%Y-%m-%d"),
        stringsAsFactors = FALSE
      )
      
      # Add all milestone values using existing field names
      for (field in all_fields) {
        submission_data[[field]] <- input_values[[field]]
      }
      
      # Submit using your existing submission function from redcap_submission.R
      submission_response <- submit_milestone_data(
        redcap_url = redcap_uri(),
        redcap_token = token(),
        record_id = record_id(),
        selected_period = period(),
        resident_level = resident_level, # We'll need to pass this
        milestone_scores = input_values,
        milestone_desc = list() # Empty for now, can be enhanced later
      )
      
      if (submission_response$success) {
        values$submission_status <- list(
          type = "success",
          message = paste("✅ Milestone assessment for", period(), "updated successfully! (Instance", instance_number, ")")
        )
        
        # Update stored data
        values$original_data <- submission_data
        values$current_data <- submission_data
        values$changes_count <- 0
      } else {
        values$submission_status <- list(
          type = "error",
          message = paste("❌ Submission failed:", submission_response$outcome_message)
        )
      }
    }
    
    # Handle direct submit
    observeEvent(input$submit, {
      submit_milestones()
    })
    
    # Handle confirmed submit from preview
    observeEvent(input$confirm_submit, {
      removeModal()
      submit_milestones()
    })
    
    # Render status display
    output$status_display <- renderUI({
      if (!is.null(values$submission_status)) {
        alert_class <- switch(values$submission_status$type,
                              "success" = "alert-success",
                              "error" = "alert-danger",
                              "info" = "alert-info",
                              "alert-secondary"
        )
        
        div(
          class = paste("alert", alert_class, "mt-3"),
          style = "border-radius: 10px;",
          values$submission_status$message
        )
      }
    })
    
    # Add JavaScript for progress bar animation
    session$sendCustomMessage("addProgressJS", list())
    
    # Return reactive values for parent app to use
    return(list(
      submission_status = reactive({ values$submission_status }),
      changes_count = reactive({ values$changes_count }),
      current_data = reactive({ values$current_data })
    ))
  })
}

# =============================================================================
# SIMPLIFIED MILESTONE RATING MODULE - FIXED VERSION
# =============================================================================

#' Create a more user-friendly milestone rating module with better UX
#' This is a simplified version for easier integration - FIXED
#' @export
mod_miles_rating_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Add custom CSS for milestone interface
    tags$head(
      tags$style(HTML("
        .milestone-grid {
          display: grid;
          grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
          gap: 20px;
          margin: 20px 0;
        }
        .milestone-section {
          background: #f8f9fa;
          border-radius: 10px;
          padding: 20px;
          border-left: 4px solid #007bff;
        }
        .milestone-section h4 {
          color: #007bff;
          margin-bottom: 15px;
          font-weight: 600;
        }
        .milestone-item {
          background: white;
          border-radius: 6px;
          padding: 12px;
          margin-bottom: 12px;
          border: 1px solid #dee2e6;
          transition: all 0.2s ease;
        }
        .milestone-item:hover {
          box-shadow: 0 2px 8px rgba(0,0,0,0.1);
          transform: translateY(-1px);
        }
        .milestone-label {
          font-weight: 500;
          color: #495057;
          margin-bottom: 8px;
          display: flex;
          justify-content: space-between;
          align-items: center;
        }
        .milestone-controls {
          display: flex;
          align-items: center;
          gap: 10px;
        }
        .level-badge {
          padding: 4px 8px;
          border-radius: 12px;
          font-size: 0.8em;
          font-weight: 600;
          min-width: 80px;
          text-align: center;
        }
        .level-0 { background: #6c757d; color: white; }
        .level-1-2 { background: #dc3545; color: white; }
        .level-3-4 { background: #fd7e14; color: white; }
        .level-5-6 { background: #ffc107; color: black; }
        .level-7-8 { background: #20c997; color: white; }
        .level-9 { background: #198754; color: white; }
        .milestone-slider {
          flex: 1;
          margin: 0 10px;
        }
        .submission-section {
          background: white;
          border-radius: 10px;
          padding: 20px;
          margin-top: 30px;
          border: 1px solid #dee2e6;
          text-align: center;
        }
        .progress-summary {
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 10px;
          padding: 20px;
          margin-bottom: 20px;
          text-align: center;
        }
      "))
    ),
    
    # Progress summary
    div(
      class = "progress-summary",
      h4("Milestone Assessment Progress", style = "margin-bottom: 10px;"),
      textOutput(ns("progress_summary"))
    ),
    
    # Patient Care & Medical Knowledge
    div(
      class = "milestone-section",
      h4("🏥 Patient Care & Medical Knowledge"),
      uiOutput(ns("pc_mk_milestones"))
    ),
    
    # Systems-Based Practice & Practice-Based Learning  
    div(
      class = "milestone-section",
      h4("⚙️ Systems-Based Practice & Practice-Based Learning"),
      uiOutput(ns("sbp_pbl_milestones"))
    ),
    
    # Professionalism & Interpersonal Communication
    div(
      class = "milestone-section", 
      h4("🤝 Professionalism & Interpersonal Communication"),
      uiOutput(ns("prof_ics_milestones"))
    ),
    
    # Submission section
    div(
      class = "submission-section",
      div(
        class = "d-flex justify-content-between align-items-center",
        actionButton(ns("reset_all"), "Reset All", class = "btn-outline-secondary"),
        div(
          actionButton(ns("preview_changes"), "Preview", class = "btn-info me-2"),
          actionButton(ns("done"), "Complete Assessment", class = "btn-success btn-lg")
        )
      ),
      
      # Status display
      uiOutput(ns("status_message"))
    )
  )
}

#' Simplified milestone rating server module - FIXED VERSION
#' @export
mod_miles_rating_server <- function(id, period, resident_level = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Milestone definitions - UPDATED to match your REDCap field names exactly
    milestones <- list(
      pc_mk = list(
        title = "Patient Care & Medical Knowledge",
        items = c(
          "rep_pc1" = "PC1: Gather History & Physical Exam",
          "rep_pc2" = "PC2: Prioritize Differential Diagnosis", 
          "rep_pc3" = "PC3: Recommend & Interpret Tests",
          "rep_pc4" = "PC4: Enter & Discuss Treatment Plans",
          "rep_pc5" = "PC5: Perform Procedures",
          "rep_pc6" = "PC6: Transfer of Care",
          "rep_mk1" = "MK1: Medical Knowledge",
          "rep_mk2" = "MK2: Clinical Reasoning",
          "rep_mk3" = "MK3: Scientific Knowledge"
        )
      ),
      sbp_pbl = list(
        title = "Systems-Based Practice & Practice-Based Learning",
        items = c(
          "rep_sbp1" = "SBP1: Patient Safety & Quality Improvement",
          "rep_sbp2" = "SBP2: System Navigation", 
          "rep_sbp3" = "SBP3: Physician Role in Healthcare Systems",
          "rep_pbl1" = "PBL1: Evidence-Based Practice",
          "rep_pbl2" = "PBL2: Reflective Practice & Commitment to Growth"
        )
      ),
      prof_ics = list(
        title = "Professionalism & Interpersonal Communication",
        items = c(
          "rep_prof1" = "PROF1: Professional Behavior",
          "rep_prof2" = "PROF2: Ethical Principles",
          "rep_prof3" = "PROF3: Accountability", 
          "rep_prof4" = "PROF4: Well-being",
          "rep_ics1" = "ICS1: Patient & Family Communication",
          "rep_ics2" = "ICS2: Interprofessional Communication",
          "rep_ics3" = "ICS3: Communication in Difficult Situations"
        )
      )
    )
    
    # Reactive values
    values <- reactiveValues(
      scores = list(),
      descriptions = list(),
      original_scores = list(),
      is_done = FALSE,
      data_loaded = FALSE
    )
    
    # Helper functions
    get_level_name <- function(score) {
      case_when(
        is.na(score) || score == 0 ~ "Not Assessed",
        score <= 2 ~ "Novice",
        score <= 4 ~ "Adv. Beginner", 
        score <= 6 ~ "Competent",
        score <= 8 ~ "Proficient",
        score == 9 ~ "Expert",
        TRUE ~ "Unknown"
      )
    }
    
    get_level_class <- function(score) {
      case_when(
        is.na(score) || score == 0 ~ "level-0",
        score <= 2 ~ "level-1-2",
        score <= 4 ~ "level-3-4", 
        score <= 6 ~ "level-5-6",
        score <= 8 ~ "level-7-8",
        score == 9 ~ "level-9",
        TRUE ~ "level-0"
      )
    }
    
    # Initialize scores and load existing data if available
    observe({
      req(period())
      
      if (!values$data_loaded) {
        all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
        
        # Initialize with default scores
        for (key in all_milestone_keys) {
          values$scores[[key]] <- 0
          values$original_scores[[key]] <- 0
        }
        
        # Try to load existing data from app_data (this would need to be passed from parent)
        # For now, we'll initialize with defaults and let the parent handle data loading
        message("Milestone module initialized with default scores")
        
        values$data_loaded <- TRUE
      }
    })
    
    # Function to load existing milestone data (called from parent)
    load_existing_data <- function(existing_data) {
      if (!is.null(existing_data) && nrow(existing_data) > 0) {
        all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
        
        for (key in all_milestone_keys) {
          if (key %in% names(existing_data)) {
            existing_value <- existing_data[[key]][1]
            if (!is.na(existing_value)) {
              values$scores[[key]] <- as.numeric(existing_value)
              values$original_scores[[key]] <- as.numeric(existing_value)
              
              # Update the slider input
              updateSliderInput(session, key, value = as.numeric(existing_value))
            }
          }
        }
        
        message("Loaded existing milestone data with ", 
                sum(sapply(values$scores, function(x) x > 0)), " assessed milestones")
      }
    }
    
    # Render milestone groups
    render_milestone_section <- function(section_key) {
      renderUI({
        section <- milestones[[section_key]]
        
        milestone_items <- lapply(names(section$items), function(milestone_key) {
          current_score <- values$scores[[milestone_key]] %||% 0
          
          div(
            class = "milestone-item",
            div(
              class = "milestone-label",
              span(section$items[[milestone_key]]),
              span(
                class = paste("level-badge", get_level_class(current_score)),
                get_level_name(current_score)
              )
            ),
            div(
              class = "milestone-controls",
              div(
                class = "milestone-slider",
                sliderInput(
                  ns(milestone_key),
                  label = NULL,
                  min = 0,
                  max = 9, 
                  value = current_score,
                  step = 1,
                  width = "100%"
                )
              ),
              div(
                style = "font-weight: bold; font-size: 1.1em; min-width: 30px; text-align: center;",
                textOutput(ns(paste0("score_", milestone_key)), inline = TRUE)
              )
            )
          )
        })
        
        do.call(tagList, milestone_items)
      })
    }
    
    # Render each section
    output$pc_mk_milestones <- render_milestone_section("pc_mk")
    output$sbp_pbl_milestones <- render_milestone_section("sbp_pbl")
    output$prof_ics_milestones <- render_milestone_section("prof_ics")
    
    # Update scores when sliders change
    observe({
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      
      for (key in all_milestone_keys) {
        local({
          key_local <- key
          
          # Update score display
          output[[paste0("score_", key_local)]] <- renderText({
            score <- input[[key_local]] %||% 0
            as.character(score)
          })
          
          # Update stored score
          observeEvent(input[[key_local]], {
            if (!is.null(input[[key_local]])) {
              values$scores[[key_local]] <- input[[key_local]]
            }
          }, ignoreInit = TRUE)
        })
      }
    })
    
    # Update progress summary
    output$progress_summary <- renderText({
      total_milestones <- length(unlist(lapply(milestones, function(x) names(x$items))))
      assessed_count <- sum(sapply(values$scores, function(x) x > 0))
      total_score <- sum(unlist(values$scores))
      avg_score <- if (assessed_count > 0) round(total_score / assessed_count, 1) else 0
      
      paste0(assessed_count, "/", total_milestones, " Milestones Assessed | Average Score: ", avg_score)
    })
    
    # Reset functionality
    observeEvent(input$reset_all, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      
      for (key in all_milestone_keys) {
        original_value <- values$original_scores[[key]] %||% 0
        updateSliderInput(session, key, value = original_value)
        values$scores[[key]] <- original_value
      }
      
      showNotification("All values reset to original state", type = "message")
    })
    
    # Preview changes
    observeEvent(input$preview_changes, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      changes <- list()
      
      for (key in all_milestone_keys) {
        original <- values$original_scores[[key]] %||% 0
        current <- values$scores[[key]] %||% 0
        
        if (original != current) {
          changes[[key]] <- list(
            from = original,
            to = current,
            from_level = get_level_name(original),
            to_level = get_level_name(current)
          )
        }
      }
      
      if (length(changes) == 0) {
        showModal(modalDialog(
          title = "Preview Changes",
          "No changes detected.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        change_items <- lapply(names(changes), function(key) {
          change <- changes[[key]]
          div(
            class = "d-flex justify-content-between mb-2",
            span(key),
            span(paste(change$from_level, "→", change$to_level))
          )
        })
        
        showModal(modalDialog(
          title = paste("Preview Changes (", length(changes), " changes)"),
          do.call(tagList, change_items),
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }
    })
    
    # Done functionality with validation
    observeEvent(input$done, {
      # Validate that assessment is reasonably complete
      assessed_count <- sum(sapply(values$scores, function(x) x > 0))
      total_milestones <- length(unlist(lapply(milestones, function(x) names(x$items))))
      
      if (assessed_count < (total_milestones * 0.3)) {  # Lowered threshold to 30%
        showModal(modalDialog(
          title = "Incomplete Assessment",
          paste("You have only assessed", assessed_count, "out of", total_milestones, 
                "milestones (", round(assessed_count/total_milestones*100), "%). Please complete at least 30% of the milestones before submitting."),
          easyClose = TRUE,
          footer = modalButton("Continue Editing")
        ))
        return()
      }
      
      values$is_done <- TRUE
      showNotification("Milestone assessment completed!", type = "success")
    })
    
    # Status message
    output$status_message <- renderUI({
      if (values$is_done) {
        div(
          class = "alert alert-success mt-3",
          icon("check-circle"),
          " Assessment completed! Ready for submission."
        )
      }
    })
    
    # Return reactive values for parent to access
    return(list(
      scores = reactive({ values$scores }),
      desc = reactive({ values$descriptions }),
      done = reactive({ values$is_done }),
      load_data = load_existing_data  # Function to load existing data
    ))
  })
}

# Add this JavaScript to your UI or include it in a separate JS file
milestone_js <- tags$script(HTML("
  // Custom message handlers for milestone interface
  Shiny.addCustomMessageHandler('updateProgress', function(data) {
    var progressBar = document.getElementById('progress_bar');
    if (progressBar) {
      progressBar.style.width = data.percent + '%';
      progressBar.setAttribute('aria-valuenow', data.percent);
    }
  });
  
  Shiny.addCustomMessageHandler('addProgressJS', function(data) {
    // Add any additional interactive features here
    console.log('Milestone module initialized');
  });
  
  // Add smooth transitions for slider changes
  $(document).on('input', '.milestone-slider input[type=\"range\"]', function() {
    $(this).closest('.milestone-item').addClass('milestone-changing');
    setTimeout(function() {
      $('.milestone-changing').removeClass('milestone-changing');
    }, 300);
  });
"))