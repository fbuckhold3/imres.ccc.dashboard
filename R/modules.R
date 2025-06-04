# ============================================================================
# MILESTONE DEFINITIONS - MOVED OUTSIDE FUNCTION SCOPE
# ============================================================================

# Define milestone structure globally so all functions can access it
MILESTONE_DEFINITIONS <- list(
  pc_mk = list(
    title = "Patient Care & Medical Knowledge",
    items = c(
      "rep_pc1" = "PC1: History",
      "rep_pc2" = "PC2: Physical Examination", 
      "rep_pc3" = "PC3: Clinical Reasoning",
      "rep_pc4" = "PC4: Patient Management - Inpatient",
      "rep_pc5" = "PC5: Patient Management - Outpatient",
      "rep_pc6" = "PC6: Digital Health",
      "rep_mk1" = "MK1: Applied Foundational Sciences",
      "rep_mk2" = "MK2: Therapeutic Knowledge",
      "rep_mk3" = "MK3: Knowledge of Diagnostic Testing"
    )
  ),
  sbp_pbl = list(
    title = "Systems-Based Practice & Practice-Based Learning",
    items = c(
      "rep_sbp1" = "SBP1: Patient Safety and Quality Improvement",
      "rep_sbp2" = "SBP2: System Navigation for Patient-Centered Care", 
      "rep_sbp3" = "SBP3: Physician Role in Health Care Systems",
      "rep_pbl1" = "PBLI1: Evidence-Based and Informed Practice",
      "rep_pbl2" = "PBLI2: Reflective Practice and Commitment to Personal Growth"
    )
  ),
  prof_ics = list(
    title = "Professionalism & Interpersonal Communication",
    items = c(
      "rep_prof1" = "PROF1: Professional Behavior",
      "rep_prof2" = "PROF2: Ethical Principles",
      "rep_prof3" = "PROF3: Accountability/Conscientiousness", 
      "rep_prof4" = "PROF4: Knowledge of Systemic and Individual Factors of Well-Being",
      "rep_ics1" = "ICS1: Patient- and Family-Centered Communication",
      "rep_ics2" = "ICS2: Interprofessional and Team Communication",
      "rep_ics3" = "ICS3: Communication within Health Care Systems"
    )
  )
)

# ============================================================================
# HELPER FUNCTIONS - MOVED OUTSIDE FUNCTION SCOPE
# ============================================================================

# Helper function to get correct milestone title by key
get_milestone_title <- function(milestone_key) {
  for (section_name in names(MILESTONE_DEFINITIONS)) {
    section <- MILESTONE_DEFINITIONS[[section_name]]
    if (milestone_key %in% names(section$items)) {
      return(section$items[[milestone_key]])
    }
  }
  # Fallback to key name if not found
  return(milestone_key)
}

# Helper functions for milestone levels
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

# Helper function to create milestone level guide
create_milestone_level_guide <- function() {
  div(
    class = "alert alert-light",
    h6("Assessment Levels:"),
    tags$ul(
      tags$li(tags$strong("0: "), "Not Assessed"),
      tags$li(tags$strong("1-2 (Novice): "), "Requires direct supervision"),
      tags$li(tags$strong("3-4 (Advanced Beginner): "), "Requires some supervision"),
      tags$li(tags$strong("5-6 (Competent): "), "Can work independently"),
      tags$li(tags$strong("7-8 (Proficient): "), "Can supervise others"),
      tags$li(tags$strong("9 (Expert): "), "Exceptional ability, teaches others")
    )
  )
}

# FIXED: Image display function with correct image loading
show_milestone_info <- function(milestone_key, milestone_name) {
  
  # Map milestone field names to image filenames
  image_filename <- gsub("^rep_", "", milestone_key)  # Remove 'rep_' prefix
  image_filename <- gsub("^pbl", "pbli", image_filename)  # Handle pbl -> pbli
  image_filename <- trimws(image_filename)  # Remove whitespace
  image_filename <- gsub("\\s+", "", image_filename)  # Remove internal spaces
  
  message("Looking for milestone image: '", image_filename, ".png' (from field: '", milestone_key, "')")
  
  # Check for image in LOCAL www/milestones directory
  local_img_path <- file.path("www", "milestones", paste0(image_filename, ".png"))
  img_exists <- file.exists(local_img_path)
  
  message("Local image path: ", local_img_path)
  message("Local image exists: ", img_exists)
  
  modal_content <- if (img_exists) {
    message("✅ Found local milestone image for ", milestone_key)
    tagList(
      div(
        style = "text-align: center; margin-bottom: 20px;",
        h5(milestone_name, style = "color: #007bff; margin-bottom: 15px;"),
        # FIXED: Direct image tag instead of JavaScript
        tags$img(
          src = paste0("milestones/", image_filename, ".png"), 
          style = "max-width: 100%; max-height: 500px; height: auto; border-radius: 8px; box-shadow: 0 4px 8px rgba(0,0,0,0.2);",
          alt = paste("Milestone image for", milestone_name),
          # Add error handling for missing images
          onerror = paste0("this.style.display='none'; ",
                           "this.parentNode.innerHTML='<div class=\"alert alert-warning\">Image not found: ", 
                           image_filename, ".png</div>';")
        )
      ),
      hr(),
      create_milestone_level_guide()
    )
  } else {
    message("❌ No local milestone image found for ", milestone_key, " (looking for: '", image_filename, ".png')")
    tagList(
      div(
        class = "alert alert-info",
        h5(icon("info-circle"), " Milestone Information"),
        h6(milestone_name, style = "color: #007bff; font-weight: 600;"),
        p("This milestone assesses specific competencies related to internal medicine practice."),
        p(paste("Expected image file: www/milestones/", image_filename, ".png", sep=""), 
          class = "text-muted small"),
        
        # Instructions for adding images
        div(
          class = "mt-3 p-3 bg-light rounded border",
          h6("To add milestone images:", class = "text-primary"),
          tags$ol(
            tags$li("Create directory: ", tags$code("www/milestones/")),
            tags$li("Add image file: ", tags$code(paste0(image_filename, ".png"))),
            tags$li("Refresh the app")
          )
        )
      ),
      create_milestone_level_guide()
    )
  }
  
  showModal(modalDialog(
    title = div(
      icon("graduation-cap"), 
      paste("Milestone Details:", gsub("^rep_", "", milestone_key))
    ),
    modal_content,
    easyClose = TRUE,
    size = "l",
    footer = modalButton("Close")
  ))
}

# ============================================================================
# CCC MILESTONE MODULE
# ============================================================================

#' Enhanced Milestone Rating UI Module for CCC
#' @export
mod_ccc_miles_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Your existing CSS and UI code...
    # (Keep all the CSS from your current version)
    
    # Selection summary
    div(
      class = "selection-summary",
      h5("Milestone Editing", style = "margin-bottom: 10px;"),
      p("Select individual milestones to edit, then adjust their ratings below.", style = "margin-bottom: 0;"),
      div(
        class = "mt-2",
        actionButton(ns("select_all"), "Select All", class = "btn btn-light btn-sm me-2"),
        actionButton(ns("select_none"), "Clear All", class = "btn btn-outline-light btn-sm")
      ),
      br(),
      textOutput(ns("selection_summary"))
    ),
    
    # Patient Care & Medical Knowledge
    div(
      class = "milestone-competency",
      h5("🏥 Patient Care & Medical Knowledge"),
      uiOutput(ns("pc_mk_milestones"))
    ),
    
    # Systems-Based Practice & Practice-Based Learning  
    div(
      class = "milestone-competency",
      h5("⚙️ Systems-Based Practice & Practice-Based Learning"),
      uiOutput(ns("sbp_pbl_milestones"))
    ),
    
    # Professionalism & Interpersonal Communication
    div(
      class = "milestone-competency", 
      h5("🤝 Professionalism & Interpersonal Communication"),
      uiOutput(ns("prof_ics_milestones"))
    ),
    
    # Action buttons
    div(
      class = "text-center mt-3",
      actionButton(ns("reset_selected"), "Reset Selected", class = "btn-outline-secondary me-2"),
      actionButton(ns("preview_changes"), "Preview Changes", class = "btn-info me-2"),
      div(id = ns("status_message"), class = "mt-2")
    )
  )
}

#' Enhanced Milestone Rating Server Module for CCC
#' @export
mod_ccc_miles_server <- function(id, existing_data = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Use the global milestone definitions
    milestones <- MILESTONE_DEFINITIONS
    
    # Reactive values
    values <- reactiveValues(
      scores = list(),
      original_scores = list(),
      selected_milestones = list(),
      data_loaded = FALSE
    )
    
    # FIXED: Field mapping function
    map_old_to_new_fields <- function(old_data) {
      # Mapping from old field names to new field names
      field_mapping <- c(
        "PC1" = "rep_pc1", "PC2" = "rep_pc2", "PC3" = "rep_pc3", 
        "PC4" = "rep_pc4", "PC5" = "rep_pc5", "PC6" = "rep_pc6",
        "MK1" = "rep_mk1", "MK2" = "rep_mk2", "MK3" = "rep_mk3",
        "SBP1" = "rep_sbp1", "SBP2" = "rep_sbp2", "SBP3" = "rep_sbp3",
        "PBL1" = "rep_pbl1", "PBL2" = "rep_pbl2",
        "PROF1" = "rep_prof1", "PROF2" = "rep_prof2", 
        "PROF3" = "rep_prof3", "PROF4" = "rep_prof4",
        "ICS1" = "rep_ics1", "ICS2" = "rep_ics2", "ICS3" = "rep_ics3"
      )
      
      mapped_data <- list()
      
      for (old_field in names(field_mapping)) {
        new_field <- field_mapping[old_field]
        if (old_field %in% names(old_data) && !is.na(old_data[[old_field]])) {
          mapped_data[[new_field]] <- as.numeric(old_data[[old_field]])
          message("Mapped ", old_field, " (", old_data[[old_field]], ") -> ", new_field)
        }
      }
      
      return(mapped_data)
    }
    
    # Initialize milestone data
    observe({
      if (!values$data_loaded) {
        all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
        
        # Initialize with default scores
        for (key in all_milestone_keys) {
          values$scores[[key]] <- 0
          values$original_scores[[key]] <- 0
          values$selected_milestones[[key]] <- FALSE
        }
        
        # Load existing data if provided
        if (!is.null(existing_data) && is.reactive(existing_data)) {
          data <- existing_data()
          if (!is.null(data) && nrow(data) > 0) {
            message("Loading existing milestone data...")
            message("Available columns in existing_data: ", paste(names(data), collapse=", "))
            
            # FIXED: Use field mapping
            mapped_data <- map_old_to_new_fields(data[1, ])
            
            if (length(mapped_data) > 0) {
              for (key in names(mapped_data)) {
                if (key %in% all_milestone_keys) {
                  values$scores[[key]] <- mapped_data[[key]]
                  values$original_scores[[key]] <- mapped_data[[key]]
                  updateSliderInput(session, key, value = mapped_data[[key]])
                }
              }
              
              assessed_count <- sum(sapply(mapped_data, function(x) x > 0))
              message("Loaded existing milestone data with ", assessed_count, " assessed milestones")
            } else {
              message("No milestone data found in existing data")
            }
          }
        }
        
        values$data_loaded <- TRUE
      }
    })
    
    # Render milestone sections with selection checkboxes
    render_milestone_section <- function(section_key) {
      renderUI({
        section <- milestones[[section_key]]
        
        milestone_items <- lapply(names(section$items), function(milestone_key) {
          # Get the correct title from the milestone definitions
          milestone_title <- section$items[[milestone_key]]
          current_score <- values$scores[[milestone_key]] %||% 0
          is_selected <- values$selected_milestones[[milestone_key]] %||% FALSE
          
          div(
            class = paste("milestone-item", if(is_selected) "milestone-selected" else ""),
            div(
              class = "milestone-header",
              div(
                class = "d-flex align-items-center",
                checkboxInput(
                  ns(paste0("select_", milestone_key)),
                  label = NULL,
                  value = is_selected,
                  width = "auto"
                ),
                # FIXED: Use the correct milestone title
                span(milestone_title, class = "milestone-label")
              ),
              div(
                class = "milestone-actions",
                span(
                  class = paste("level-badge", get_level_class(current_score)),
                  get_level_name(current_score)
                ),
                actionButton(
                  ns(paste0("info_", milestone_key)),
                  icon("info-circle"),
                  class = "btn btn-sm btn-outline-info",
                  title = "View milestone details",
                  style = "padding: 2px 6px;"
                )
              )
            ),
            
            # Only show slider if milestone is selected
            conditionalPanel(
              condition = paste0("input['", ns(paste0("select_", milestone_key)), "']"),
              div(
                class = "milestone-controls mt-2",
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
          )
        })
        
        do.call(tagList, milestone_items)
      })
    }
    
    # Render each section
    output$pc_mk_milestones <- render_milestone_section("pc_mk")
    output$sbp_pbl_milestones <- render_milestone_section("sbp_pbl")
    output$prof_ics_milestones <- render_milestone_section("prof_ics")
    
    # Handle milestone selection changes
    observe({
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      
      for (key in all_milestone_keys) {
        local({
          key_local <- key
          
          # Get the correct milestone title using the global function
          milestone_title <- get_milestone_title(key_local)
          
          # Update selection status
          observeEvent(input[[paste0("select_", key_local)]], {
            values$selected_milestones[[key_local]] <- input[[paste0("select_", key_local)]] %||% FALSE
          }, ignoreInit = TRUE)
          
          # Update score displays
          output[[paste0("score_", key_local)]] <- renderText({
            score <- input[[key_local]] %||% 0
            as.character(score)
          })
          
          # Update stored scores
          observeEvent(input[[key_local]], {
            if (!is.null(input[[key_local]])) {
              values$scores[[key_local]] <- input[[key_local]]
            }
          }, ignoreInit = TRUE)
          
          # FIXED: Handle info button clicks with correct titles
          observeEvent(input[[paste0("info_", key_local)]], {
            show_milestone_info(key_local, milestone_title)
          })
        })
      }
    })

    # Also add this helper function to test image availability
    check_milestone_images <- function() {
      # Define all milestone keys
      milestone_keys <- c(
        "rep_pc1", "rep_pc2", "rep_pc3", "rep_pc4", "rep_pc5", "rep_pc6",
        "rep_mk1", "rep_mk2", "rep_mk3",
        "rep_sbp1", "rep_sbp2", "rep_sbp3",
        "rep_pbl1", "rep_pbl2",
        "rep_prof1", "rep_prof2", "rep_prof3", "rep_prof4",
        "rep_ics1", "rep_ics2", "rep_ics3"
      )
      
      message("=== Checking Milestone Images ===")
      
      found_images <- 0
      missing_images <- c()
      
      for (key in milestone_keys) {
        # Apply same mapping logic
        image_filename <- gsub("^rep_", "", key)
        image_filename <- gsub("^pbl", "pbli", image_filename)
        
        img_path <- system.file("www", "milestones", paste0(image_filename, ".png"), package = "imres")
        
        if (file.exists(img_path)) {
          message("✅ Found: ", image_filename, ".png")
          found_images <- found_images + 1
        } else {
          message("❌ Missing: ", image_filename, ".png")
          missing_images <- c(missing_images, paste0(image_filename, ".png"))
        }
      }
      
      message("=== Summary ===")
      message("Found: ", found_images, "/", length(milestone_keys), " images")
      
      if (length(missing_images) > 0) {
        message("Missing images: ", paste(missing_images, collapse = ", "))
      }
      
      return(list(
        found = found_images,
        total = length(milestone_keys),
        missing = missing_images
      ))
    }
    
    # FIXED: Helper function to create milestone level guide
    create_milestone_level_guide <- function() {
      div(
        class = "alert alert-light",
        h6("Assessment Levels:"),
        tags$ul(
          tags$li(tags$strong("0: "), "Not Assessed"),
          tags$li(tags$strong("1-2 (Novice): "), "Requires direct supervision"),
          tags$li(tags$strong("3-4 (Advanced Beginner): "), "Requires some supervision"),
          tags$li(tags$strong("5-6 (Competent): "), "Can work independently"),
          tags$li(tags$strong("7-8 (Proficient): "), "Can supervise others"),
          tags$li(tags$strong("9 (Expert): "), "Exceptional ability, teaches others")
        )
      )
    }
    
    # Selection summary
    output$selection_summary <- renderText({
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      selected_count <- sum(sapply(all_milestone_keys, function(key) {
        values$selected_milestones[[key]] %||% FALSE
      }))
      
      paste0(selected_count, " of ", length(all_milestone_keys), " milestones selected for editing")
    })
    
    # Select all button
    observeEvent(input$select_all, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      for (key in all_milestone_keys) {
        updateCheckboxInput(session, paste0("select_", key), value = TRUE)
        values$selected_milestones[[key]] <- TRUE
      }
    })
    
    # Select none button
    observeEvent(input$select_none, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      for (key in all_milestone_keys) {
        updateCheckboxInput(session, paste0("select_", key), value = FALSE)
        values$selected_milestones[[key]] <- FALSE
      }
    })
    
    # Reset selected milestones
    observeEvent(input$reset_selected, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      for (key in all_milestone_keys) {
        if (values$selected_milestones[[key]]) {
          original_value <- values$original_scores[[key]] %||% 0
          updateSliderInput(session, key, value = original_value)
          values$scores[[key]] <- original_value
        }
      }
      showNotification("Selected milestones reset to original values", type = "message")
    })
    
    # Preview changes
    observeEvent(input$preview_changes, {
      all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
      changes <- list()
      
      for (key in all_milestone_keys) {
        if (values$selected_milestones[[key]]) {
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
      }
      
      if (length(changes) == 0) {
        showModal(modalDialog(
          title = "Preview Changes",
          "No changes detected in selected milestones.",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      } else {
        change_items <- lapply(names(changes), function(key) {
          change <- changes[[key]]
          div(
            class = "d-flex justify-content-between mb-2 p-2",
            style = "background: #f8f9fa; border-radius: 4px;",
            span(tags$strong(key)),
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
    
    # Return reactive values for parent to access
    return(list(
      scores = reactive({ 
        # Only return scores for selected milestones
        selected_scores <- list()
        all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
        for (key in all_milestone_keys) {
          if (values$selected_milestones[[key]]) {
            selected_scores[[key]] <- values$scores[[key]]
          }
        }
        selected_scores
      }),
      selected_milestones = reactive({ values$selected_milestones }),
      has_changes = reactive({
        all_milestone_keys <- unlist(lapply(milestones, function(x) names(x$items)))
        for (key in all_milestone_keys) {
          if (values$selected_milestones[[key]]) {
            original <- values$original_scores[[key]] %||% 0
            current <- values$scores[[key]] %||% 0
            if (original != current) return(TRUE)
          }
        }
        return(FALSE)
      })
    ))
  })
}




