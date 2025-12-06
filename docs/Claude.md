# CCC Dashboard Modernization Guide for Claude Code

**Project:** Modernize CCC Dashboard from Old RDM to RDM 2.0  
**Repository:** fbuckhold3/imres.ccc.dashboard  
**Date:** December 5, 2025  
**Pattern Apps:** fbuckhold3/imslu.coach.dash, fbuckhold3/imslu-resident-self-assessment

---

## Table of Contents

1. [Project Overview](#project-overview)
2. [Quick Reference: Variable Substitutions](#quick-reference-variable-substitutions)
3. [Architecture & Workflow](#architecture--workflow)
4. [REDCap Submission Patterns](#redcap-submission-patterns)
5. [Implementation Steps](#implementation-steps)
6. [Code Examples](#code-examples)
7. [Testing Checklist](#testing-checklist)

---

## Project Overview

### Goal
Modernize the CCC (Clinical Competency Committee) dashboard to work with the new RDM 2.0 REDCap system using patterns from successfully updated coaching and self-assessment apps.

### Key Changes Required
1. ✅ **Data Loading:** Replace old data loading with `gmed::load_rdm_complete(raw_or_label = "raw")`
2. ✅ **Variable Names:** Update 3 field names (minimal changes!)
3. ✅ **Period Detection:** Implement auto-detection using gmed functions
4. ✅ **UI Pattern:** Full resident table view with DT search/filter (like coaching app)
5. ✅ **Workflow:** Scheduled (instances 1-7) vs Interim (instances 8+) reviews

### Good News
**Most fields unchanged!** Only 3 variables need updating:
- `archived` → `res_archive`
- Remove `ccc_mile_concerns` (doesn't exist in RDM 2.0)
- Remove `ccc_concern_notes` (doesn't exist in RDM 2.0)

---

## Quick Reference: Variable Substitutions

### Resident Data Fields

| Current Code | RDM 2.0 Field | Status | Action |
|-------------|---------------|--------|--------|
| `name` | `name` | ✅ No change | Keep |
| `access_code` | `access_code` | ✅ No change | Keep |
| `coach` | `coach` | ✅ No change | Keep |
| `second_rev` | `second_rev` | ✅ No change | Keep |
| `archived` | `res_archive` | ⚠️ **CHANGE** | Find/replace |
| `Level` | **Calculated** | ⚠️ Verify | Ensure calculation works |

### CCC Review Fields (ccc_review form - REPEATING)

| Current Code | RDM 2.0 Field | Status | Action |
|-------------|---------------|--------|--------|
| `ccc_date` | `ccc_date` | ✅ No change | Keep |
| `ccc_rev_type` | `ccc_rev_type` | ✅ No change | Keep (1=Scheduled, 2=Interim) |
| `ccc_session` | `ccc_session` | ✅ No change | Keep (1-7 period codes) |
| `ccc_interim` | `ccc_interim` | ✅ No change | Keep |
| `ccc_concern` | `ccc_concern` | ✅ No change | Keep |
| `ccc_action` | `ccc_action` | ✅ No change | Keep (checkbox ___1 to ___8) |
| `ccc_fu_resp` | `ccc_fu_resp` | ✅ No change | Keep |
| `ccc_action_status` | `ccc_action_status` | ✅ No change | Keep (checkbox ___1 to ___4) |
| `ccc_competency` | `ccc_competency` | ✅ No change | Keep (checkbox ___1 to ___7) |
| `ccc_ilp` | `ccc_ilp` | ✅ No change | Keep |
| `ccc_mile` | `ccc_mile` | ✅ No change | Keep |
| `ccc_mile_notes` | `ccc_mile_notes` | ✅ No change | Keep |
| `ccc_issues_follow_up` | `ccc_issues_follow_up` | ✅ No change | Keep |
| `ccc_comments` | `ccc_comments` | ✅ No change | Keep |
| `ccc_mile_concerns` | ❌ **REMOVE** | 🗑️ Delete | Not in RDM 2.0 |
| `ccc_concern_notes` | ❌ **REMOVE** | 🗑️ Delete | Not in RDM 2.0 |

### Milestone Entry Fields (milestone_entry form - REPEATING)

**All 22 milestone fields unchanged!** ✅

| Field Pattern | Examples | Status |
|--------------|----------|--------|
| `rep_pc*` | `rep_pc1` through `rep_pc6` | ✅ No change |
| `rep_mk*` | `rep_mk1` through `rep_mk3` | ✅ No change |
| `rep_sbp*` | `rep_sbp1` through `rep_sbp3` | ✅ No change |
| `rep_pbl*` | `rep_pbl1` through `rep_pbl2` | ✅ No change |
| `rep_prof*` | `rep_prof1` through `rep_prof4` | ✅ No change |
| `rep_ics*` | `rep_ics1` through `rep_ics3` | ✅ No change |
| `rep_*_desc` | Level 5 descriptions | ✅ No change |
| `prog_mile_date` | Entry date | ✅ No change |
| `prog_mile_period` | Period code (1-7) | ✅ No change |

### Period Codes (Unchanged)

```r
period_labels <- c(
  "1" = "Mid Intern",
  "2" = "End Intern", 
  "3" = "Mid PGY2",
  "4" = "End PGY2",
  "5" = "Mid PGY3",
  "6" = "Graduation",
  "7" = "Intern Intro"
)
```

---

## Architecture & Workflow

### Main App Flow

```
1. Login (access code)
   ↓
2. Full Resident Table (DT with search/filter)
   - Show: Name, Level, Coach, Period, Review Status
   - Click row → Select resident
   ↓
3. Review Type Selection
   ├─ Scheduled Review (1-7) ────→ 4a
   └─ Interim Review (8+) ────────→ 4b
   
4a. Scheduled Review Workflow
   ├─ Auto-detect period (with dropdown override)
   ├─ Display coaching data
   ├─ Display existing milestones
   ├─ Milestone review section:
   │  ├─ If milestones exist:
   │  │  ├─ Show spider plot
   │  │  ├─ Show numeric scores
   │  │  ├─ Radio: Acceptable? Yes/No
   │  │  └─ If No: Show edit interface
   │  └─ If no milestones:
   │     ├─ Warning message
   │     └─ Radio: Proceed without / Enter now
   └─ Submit to instance = period code
   
4b. Interim Review Workflow
   ├─ No period needed
   ├─ Show interim form only
   └─ Submit to instance = next available (8+)
```

### Data Loading Pattern (Use gmed)

```r
# In globals.R
app_data <- gmed::load_rdm_complete(
  redcap_url = REDCAP_CONFIG$url,
  rdm_token = REDCAP_CONFIG$rdm_token,
  raw_or_label = "raw"  # CRITICAL: Always use raw
)

# Access components:
# app_data$residents           # Resident demographic data
# app_data$all_forms$ccc_review        # CCC review data
# app_data$all_forms$milestone_entry   # Program milestones
# app_data$data_dict           # Data dictionary for lookups
```

### Period Detection

```r
# Get current period using gmed
current_period <- gmed::get_current_period()
# Returns: "Mid Intern", "End Intern", etc.

# Map to REDCap code for ccc_session
period_to_code <- c(
  "Mid Intern" = "1",
  "End Intern" = "2",
  "Mid PGY2" = "3",
  "End PGY2" = "4",
  "Mid PGY3" = "5",
  "Graduation" = "6",
  "Graduating" = "6",
  "Intern Intro" = "7"
)

ccc_session_code <- period_to_code[[current_period]]
```

### Instance Number Logic

```r
# SCHEDULED REVIEWS: Instance = Period Code (1-7)
get_ccc_instance_scheduled <- function(period_code) {
  return(as.numeric(period_code))
}

# INTERIM REVIEWS: Instance = Next Available (8+)
get_ccc_instance_interim <- function(record_id, redcap_url, redcap_token) {
  # Fetch existing CCC reviews
  result <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = "ccc_review"
    ),
    encode = "form"
  )
  
  existing_data <- jsonlite::fromJSON(
    httr::content(result, "text", encoding = "UTF-8")
  )
  
  if (nrow(existing_data) == 0) return(8)
  
  # Filter to ccc_review instrument
  ccc_data <- existing_data %>%
    filter(redcap_repeat_instrument == "ccc_review")
  
  if (nrow(ccc_data) == 0) return(8)
  
  # Get max instance, ensure at least 8
  max_instance <- max(as.numeric(ccc_data$redcap_repeat_instance), na.rm = TRUE)
  return(max(max_instance + 1, 8))
}
```

---

## REDCap Submission Patterns

### PATTERN 1: OVERWRITE (Fixed Instances) - CCC Scheduled Reviews

**Used for:** Scheduled CCC reviews tied to specific periods

**Characteristics:**
- Instance number = period code (1-7)
- Each instance represents a specific evaluation period
- Updates overwrite existing data for that instance
- Examples: Mid Intern = instance 1, End Intern = instance 2, etc.

**Submission Code:**

```r
submit_ccc_scheduled_review <- function(redcap_url, redcap_token, record_id, 
                                        ccc_data, period_code) {
  
  # Build submission data
  submission <- list(
    record_id = record_id,
    redcap_repeat_instrument = "ccc_review",
    redcap_repeat_instance = as.character(period_code),  # 1-7
    ccc_date = format(Sys.Date(), "%Y-%m-%d"),
    ccc_rev_type = "1",  # Scheduled
    ccc_session = period_code,
    ccc_concern = ccc_data$ccc_concern,
    ccc_ilp = ccc_data$ccc_ilp,
    ccc_mile = ccc_data$ccc_mile,
    ccc_mile_notes = ccc_data$ccc_mile_notes,
    ccc_issues_follow_up = ccc_data$ccc_issues_follow_up,
    ccc_comments = ccc_data$ccc_comments,
    ccc_fu_resp = ccc_data$ccc_fu_resp
  )
  
  # Add checkbox fields
  for (i in 1:8) {
    field_name <- paste0("ccc_action___", i)
    submission[[field_name]] <- if (as.character(i) %in% ccc_data$ccc_action) "1" else "0"
  }
  
  for (i in 1:4) {
    field_name <- paste0("ccc_action_status___", i)
    submission[[field_name]] <- if (as.character(i) %in% ccc_data$ccc_action_status) "1" else "0"
  }
  
  for (i in 1:7) {
    field_name <- paste0("ccc_competency___", i)
    submission[[field_name]] <- if (as.character(i) %in% ccc_data$ccc_competency) "1" else "0"
  }
  
  # Convert to JSON
  json_data <- jsonlite::toJSON(list(submission), auto_unbox = TRUE)
  
  # Submit
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "import",
      format = "json",
      type = "flat",
      overwriteBehavior = "normal",
      data = json_data,
      returnContent = "count",
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  # Check response
  if (httr::status_code(response) == 200) {
    return(list(success = TRUE, message = "CCC review submitted successfully"))
  } else {
    content <- httr::content(response, "text", encoding = "UTF-8")
    return(list(success = FALSE, message = content))
  }
}
```

### PATTERN 2: ADDITIVE (Continuous Instances) - CCC Interim Reviews

**Used for:** Interim CCC reviews (ad hoc, as needed)

**Characteristics:**
- Each submission creates a new instance
- Instance numbers increment automatically (8, 9, 10, ...)
- Never overwrites existing data
- No period/session value needed

**Submission Code:**

```r
submit_ccc_interim_review <- function(redcap_url, redcap_token, record_id, 
                                      ccc_data) {
  
  # Get next available instance (8+)
  next_instance <- get_ccc_instance_interim(record_id, redcap_url, redcap_token)
  
  # Build submission data
  submission <- list(
    record_id = record_id,
    redcap_repeat_instrument = "ccc_review",
    redcap_repeat_instance = as.character(next_instance),
    ccc_date = format(Sys.Date(), "%Y-%m-%d"),
    ccc_rev_type = "2",  # Interim
    # NO ccc_session for interim reviews
    ccc_interim = ccc_data$ccc_interim,
    ccc_concern = ccc_data$ccc_concern,
    ccc_comments = ccc_data$ccc_comments
    # ... other fields as needed
  )
  
  # Add checkbox fields if needed
  # (same pattern as scheduled review)
  
  # Convert to JSON
  json_data <- jsonlite::toJSON(list(submission), auto_unbox = TRUE)
  
  # Submit (same as above)
  response <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "import",
      format = "json",
      type = "flat",
      overwriteBehavior = "normal",
      data = json_data,
      returnContent = "count",
      returnFormat = "json"
    ),
    encode = "form"
  )
  
  if (httr::status_code(response) == 200) {
    return(list(success = TRUE, message = "Interim review submitted"))
  } else {
    content <- httr::content(response, "text", encoding = "UTF-8")
    return(list(success = FALSE, message = content))
  }
}
```

### Milestone Entry Submission

**Same pattern as scheduled reviews** - instance = period code (1-7)

```r
submit_milestone_edits <- function(redcap_url, redcap_token, record_id,
                                   milestone_data, period_code) {
  
  submission <- list(
    record_id = record_id,
    redcap_repeat_instrument = "milestone_entry",
    redcap_repeat_instance = as.character(period_code),
    prog_mile_date = format(Sys.Date(), "%Y-%m-%d"),
    prog_mile_period = period_code,
    
    # Patient Care
    rep_pc1 = milestone_data$rep_pc1,
    rep_pc2 = milestone_data$rep_pc2,
    rep_pc3 = milestone_data$rep_pc3,
    rep_pc4 = milestone_data$rep_pc4,
    rep_pc5 = milestone_data$rep_pc5,
    rep_pc6 = milestone_data$rep_pc6,
    
    # Medical Knowledge
    rep_mk1 = milestone_data$rep_mk1,
    rep_mk2 = milestone_data$rep_mk2,
    rep_mk3 = milestone_data$rep_mk3,
    
    # Systems-Based Practice
    rep_sbp1 = milestone_data$rep_sbp1,
    rep_sbp2 = milestone_data$rep_sbp2,
    rep_sbp3 = milestone_data$rep_sbp3,
    
    # Practice-Based Learning
    rep_pbl1 = milestone_data$rep_pbl1,
    rep_pbl2 = milestone_data$rep_pbl2,
    
    # Professionalism
    rep_prof1 = milestone_data$rep_prof1,
    rep_prof2 = milestone_data$rep_prof2,
    rep_prof3 = milestone_data$rep_prof3,
    rep_prof4 = milestone_data$rep_prof4,
    
    # Interpersonal Communication
    rep_ics1 = milestone_data$rep_ics1,
    rep_ics2 = milestone_data$rep_ics2,
    rep_ics3 = milestone_data$rep_ics3
    
    # Add _desc fields if Level 5 ratings exist
  )
  
  # Submit (same POST pattern as above)
  # ...
}
```

---

## Implementation Steps

### Step 1: Setup & Package Installation

```bash
# Install gmed package from GitHub
remotes::install_github('fbuckhold3/gmed')

# Update .Renviron
RDM_TOKEN=your_new_rdm2_token
ACCESS_CODE=your_access_code
```

### Step 2: Update globals.R

```r
# Load required packages
library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(gmed)  # ADD THIS

# REDCap configuration
REDCAP_CONFIG <- list(
  url = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  rdm_token = Sys.getenv("RDM_TOKEN")
)

# Data loading function
load_app_data <- function() {
  gmed::load_rdm_complete(
    redcap_url = REDCAP_CONFIG$url,
    rdm_token = REDCAP_CONFIG$rdm_token,
    raw_or_label = "raw"
  )
}

# Period detection helpers
get_current_period <- function() {
  gmed::get_current_period()
}

period_to_code <- c(
  "Mid Intern" = "1",
  "End Intern" = "2",
  "Mid PGY2" = "3",
  "End PGY2" = "4",
  "Mid PGY3" = "5",
  "Graduation" = "6",
  "Graduating" = "6",
  "Intern Intro" = "7"
)

code_to_period <- setNames(names(period_to_code), period_to_code)
```

### Step 3: Find/Replace Variable Names

**Use your editor's find/replace across all R files:**

1. Find: `archived`  
   Replace: `res_archive`  
   Context: When filtering residents

2. Find: `ccc_mile_concerns`  
   Action: **DELETE** all references

3. Find: `ccc_concern_notes`  
   Action: **DELETE** all references

**Specific locations to check:**
- `clear_ccc_form_inputs()` function
- UI definitions
- Any observers/validators
- Form submission code

### Step 4: Update clear_ccc_form_inputs()

```r
# IN server.R or helpers.R
clear_ccc_form_inputs <- function(session) {
  # Clear text inputs
  updateTextInput(session, "ccc_fu_resp", value = "")
  updateTextInput(session, "ccc_comments", value = "")
  
  # Clear text areas
  updateTextAreaInput(session, "ccc_interim", value = "")
  updateTextAreaInput(session, "ccc_ilp", value = "")
  updateTextAreaInput(session, "ccc_mile_notes", value = "")
  updateTextAreaInput(session, "ccc_issues_follow_up", value = "")
  
  # DELETE THESE TWO LINES:
  # updateTextAreaInput(session, "ccc_mile_concerns", value = "")
  # updateTextAreaInput(session, "ccc_concern_notes", value = "")
  
  # Clear radio buttons
  updateRadioButtons(session, "ccc_rev_type", selected = character(0))
  updateRadioButtons(session, "ccc_concern", selected = character(0))
  updateRadioButtons(session, "ccc_mile", selected = character(0))
  
  # Clear select inputs
  updateSelectInput(session, "ccc_session", selected = "")
  
  # Clear checkboxes
  updateCheckboxGroupInput(session, "ccc_action", selected = character(0))
  updateCheckboxGroupInput(session, "ccc_action_status", selected = character(0))
  updateCheckboxGroupInput(session, "ccc_competency", selected = character(0))
}
```

### Step 5: Build Resident Table Module (Like Coaching App)

```r
# modules/mod_resident_table.R

mod_resident_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
        h3("CCC Review Dashboard"),
        p("Click on a resident to begin CCC review"),
        DT::dataTableOutput(ns("residents_table"))
      )
    )
  )
}

mod_resident_table_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # Process resident data
    residents_summary <- reactive({
      req(app_data())
      
      data <- app_data()
      resident_data <- data$residents
      
      # Filter non-archived residents with coaches
      resident_data %>%
        filter(
          res_archive != "1",  # UPDATED from archived
          !is.na(coach),
          coach != ""
        ) %>%
        select(name, Level, coach, second_rev, access_code) %>%
        distinct()
    })
    
    # Render table
    output$residents_table <- DT::renderDataTable({
      DT::datatable(
        residents_summary(),
        selection = 'single',
        options = list(
          pageLength = 25,
          searching = TRUE,
          ordering = TRUE
        ),
        rownames = FALSE
      )
    })
    
    # Return selected resident
    selected_resident <- reactive({
      req(input$residents_table_rows_selected)
      residents_summary()[input$residents_table_rows_selected, ]
    })
    
    return(selected_resident)
  })
}
```

### Step 6: Build CCC Review Module

```r
# modules/mod_ccc_review.R

mod_ccc_review_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # Step 1: Review Type Selection
    radioButtons(ns("ccc_rev_type"),
                 "Review Type:",
                 choices = c("Scheduled" = "1", "Interim" = "2")),
    
    # Conditional: Scheduled Review
    conditionalPanel(
      condition = paste0("input['", ns("ccc_rev_type"), "'] == '1'"),
      
      # Period selection with auto-detection
      selectInput(ns("ccc_session"),
                  "Review Period:",
                  choices = c(
                    "Mid Intern" = "1",
                    "End Intern" = "2",
                    "Mid PGY2" = "3",
                    "End PGY2" = "4",
                    "Mid PGY3" = "5",
                    "Graduation" = "6",
                    "Intern Intro" = "7"
                  )),
      
      # CCC review fields
      radioButtons(ns("ccc_concern"), "Any concerns?",
                   choices = c("Yes" = "1", "No" = "0")),
      
      # Conditional: If concerns
      conditionalPanel(
        condition = paste0("input['", ns("ccc_concern"), "'] == '1'"),
        checkboxGroupInput(ns("ccc_action"), "CCC Actions:",
                          choices = c(
                            "Remediation plan" = "1",
                            "Probation" = "2",
                            "Referral for professionalism" = "3",
                            "Coach follow up" = "4",
                            "Meet with PD and/or CCC Chair" = "5",
                            "Meet with Chiefs" = "6",
                            "Other (see notes)" = "7",
                            "Referral to behavioral health" = "8"
                          )),
        checkboxGroupInput(ns("ccc_competency"), "Competency Areas:",
                          choices = c(
                            "Patient Care" = "1",
                            "Medical Knowledge" = "2",
                            "Systems-based Practice" = "3",
                            "Practice-based Learning" = "4",
                            "Professionalism" = "5",
                            "Interpersonal Communication" = "6",
                            "Not a competence concern" = "7"
                          ))
      ),
      
      # Text fields
      textAreaInput(ns("ccc_ilp"), "Comments on ILP:"),
      textAreaInput(ns("ccc_mile_notes"), "Notes on milestones:"),
      textAreaInput(ns("ccc_issues_follow_up"), "Follow-up items:"),
      textInput(ns("ccc_comments"), "Other comments:"),
      
      # Submit button
      actionButton(ns("submit_scheduled"), "Submit Scheduled Review",
                   class = "btn-primary")
    ),
    
    # Conditional: Interim Review
    conditionalPanel(
      condition = paste0("input['", ns("ccc_rev_type"), "'] == '2'"),
      
      textAreaInput(ns("ccc_interim"), "Interim Review Summary:"),
      radioButtons(ns("ccc_concern_interim"), "Any concerns?",
                   choices = c("Yes" = "1", "No" = "0")),
      textInput(ns("ccc_comments_interim"), "Comments:"),
      
      actionButton(ns("submit_interim"), "Submit Interim Review",
                   class = "btn-primary")
    )
  )
}

mod_ccc_review_server <- function(id, selected_resident, app_data) {
  moduleServer(id, function(input, output, session) {
    
    # Auto-populate period on load
    observe({
      req(selected_resident())
      
      current_period <- get_current_period()
      period_code <- period_to_code[[current_period]]
      
      if (!is.null(period_code)) {
        updateSelectInput(session, "ccc_session", selected = period_code)
      }
    })
    
    # Handle scheduled review submission
    observeEvent(input$submit_scheduled, {
      req(selected_resident())
      req(input$ccc_session)
      
      # Gather form data
      ccc_data <- list(
        ccc_concern = input$ccc_concern,
        ccc_action = input$ccc_action,
        ccc_competency = input$ccc_competency,
        ccc_ilp = input$ccc_ilp,
        ccc_mile_notes = input$ccc_mile_notes,
        ccc_issues_follow_up = input$ccc_issues_follow_up,
        ccc_comments = input$ccc_comments
      )
      
      # Submit
      result <- submit_ccc_scheduled_review(
        redcap_url = REDCAP_CONFIG$url,
        redcap_token = REDCAP_CONFIG$rdm_token,
        record_id = selected_resident()$record_id,
        ccc_data = ccc_data,
        period_code = input$ccc_session
      )
      
      if (result$success) {
        showNotification("CCC review submitted successfully!", type = "message")
      } else {
        showNotification(paste("Error:", result$message), type = "error")
      }
    })
    
    # Handle interim review submission
    observeEvent(input$submit_interim, {
      req(selected_resident())
      
      ccc_data <- list(
        ccc_interim = input$ccc_interim,
        ccc_concern = input$ccc_concern_interim,
        ccc_comments = input$ccc_comments_interim
      )
      
      result <- submit_ccc_interim_review(
        redcap_url = REDCAP_CONFIG$url,
        redcap_token = REDCAP_CONFIG$rdm_token,
        record_id = selected_resident()$record_id,
        ccc_data = ccc_data
      )
      
      if (result$success) {
        showNotification("Interim review submitted!", type = "message")
      } else {
        showNotification(paste("Error:", result$message), type = "error")
      }
    })
  })
}
```

---

## Code Examples

### Complete Checkbox Handling

```r
# Building checkbox fields for submission
build_checkbox_fields <- function(input_values, field_base, num_options) {
  fields <- list()
  
  for (i in 1:num_options) {
    field_name <- paste0(field_base, "___", i)
    fields[[field_name]] <- if (as.character(i) %in% input_values) "1" else "0"
  }
  
  return(fields)
}

# Usage:
action_fields <- build_checkbox_fields(input$ccc_action, "ccc_action", 8)
status_fields <- build_checkbox_fields(input$ccc_action_status, "ccc_action_status", 4)
comp_fields <- build_checkbox_fields(input$ccc_competency, "ccc_competency", 7)

# Merge into submission
submission <- c(submission, action_fields, status_fields, comp_fields)
```

### Period Translation

```r
# Get current period name
current_period <- gmed::get_current_period()  # "Mid Intern"

# Convert to code for REDCap
period_code <- period_to_code[[current_period]]  # "1"

# Convert code back to name for display
period_name <- code_to_period[[period_code]]  # "Mid Intern"

# Use in filter
milestone_data %>%
  filter(prog_mile_period == period_code)
```

### Fetching Existing Data

```r
fetch_ccc_review <- function(record_id, period_code, redcap_url, redcap_token) {
  result <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = "ccc_review"
    ),
    encode = "form"
  )
  
  data <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"))
  
  # Filter for specific instance
  existing <- data %>%
    filter(
      redcap_repeat_instrument == "ccc_review",
      redcap_repeat_instance == period_code
    )
  
  if (nrow(existing) > 0) {
    return(existing[1, ])
  } else {
    return(NULL)
  }
}
```

---

## Testing Checklist

### Phase 1: Setup & Data Loading
- [ ] gmed package installed successfully
- [ ] App loads without errors
- [ ] Data loads using `gmed::load_rdm_complete()`
- [ ] Can access `app_data$residents`
- [ ] Can access `app_data$all_forms$ccc_review`
- [ ] Can access `app_data$all_forms$milestone_entry`
- [ ] `res_archive` field exists (not `archived`)

### Phase 2: Resident Table
- [ ] Table displays all non-archived residents
- [ ] Can search/filter residents
- [ ] Clicking resident selects correctly
- [ ] Selected resident data passes to review module

### Phase 3: Period Detection
- [ ] Current period auto-detects correctly
- [ ] Period dropdown pre-populated
- [ ] Can override period selection
- [ ] Period code maps correctly (1-7)

### Phase 4: Scheduled Review
- [ ] Form displays when "Scheduled" selected
- [ ] All CCC fields present (except deleted ones)
- [ ] Checkbox groups work correctly
- [ ] Can display existing coaching data
- [ ] Can display existing milestones
- [ ] Submission succeeds
- [ ] Data appears in REDCap at correct instance (1-7)
- [ ] Checkbox fields formatted correctly (___1, ___2, etc.)

### Phase 5: Interim Review
- [ ] Form displays when "Interim" selected
- [ ] No period field shown
- [ ] Submission succeeds
- [ ] Data appears at instance 8+
- [ ] Sequential instances work (8, 9, 10...)

### Phase 6: Milestone Review
- [ ] Can display existing program milestones
- [ ] Spider plot renders correctly
- [ ] Numeric scores display correctly
- [ ] Can edit milestones if needed
- [ ] Milestone edits submit to correct form
- [ ] Milestone edits use correct instance number

### Phase 7: Data Integrity
- [ ] No `ccc_mile_concerns` references remain
- [ ] No `ccc_concern_notes` references remain
- [ ] All checkbox fields properly formatted
- [ ] Dates in YYYY-MM-DD format
- [ ] Instance numbers correct
- [ ] Can view submitted data in REDCap

### Phase 8: Error Handling
- [ ] Invalid access code shows error
- [ ] Missing required fields prevented
- [ ] REDCap errors display to user
- [ ] Network errors handled gracefully

---

## Quick Start Commands

```r
# 1. Install gmed
remotes::install_github('fbuckhold3/gmed')

# 2. Test data loading
test_data <- gmed::load_rdm_complete(
  redcap_url = "https://redcap.wustl.edu/redcap/srvrs/prod_v3_1_0_001/redcap/api/",
  rdm_token = Sys.getenv("RDM_TOKEN"),
  raw_or_label = "raw"
)

# 3. Check structure
names(test_data)
names(test_data$residents)
names(test_data$all_forms$ccc_review)
head(test_data$residents$res_archive)  # Should exist

# 4. Test period detection
current_period <- gmed::get_current_period()
print(current_period)

# 5. Run app
shiny::runApp()
```

---

## Key Reminders

1. **Always load as "raw":** `raw_or_label = "raw"`
2. **Always submit as "raw":** Use codes, not labels
3. **Display as labels:** Convert using data dictionary for UI only
4. **Scheduled = 1-7:** Instance matches period code
5. **Interim = 8+:** Sequential, independent instances
6. **Checkbox format:** Must use `field___1`, `field___2`, etc.
7. **Three field changes only:** `res_archive`, delete 2 non-existent fields
8. **gmed required:** Install from GitHub first

---

## Support Resources

- **Pattern Apps:**
  - `fbuckhold3/imslu.coach.dash` - Data loading, period detection
  - `fbuckhold3/imslu-resident-self-assessment` - Multi-step workflow
  
- **gmed Package:**
  - `fbuckhold3/gmed` - Data loading and helper functions
  
- **Data Dictionary:**
  - `/mnt/project/rdm2_data_dict_10_22_25.csv` - Field definitions

---

## Contact

For questions or issues during implementation, refer to:
1. This guide
2. Pattern apps listed above
3. gmed package documentation
4. REDCap API documentation

**Good luck with the modernization!** 🚀