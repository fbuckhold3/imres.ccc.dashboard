# ============================================================================
# FIXED UI - Grammar/Structure Issue Corrected
# Replace your entire ui.R with this corrected version
# ============================================================================

ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    primary = "#2c3e50",        # Dark blue-gray (professional)
    secondary = "#34495e",      # Slightly lighter blue-gray
    success = "#27ae60",        # Modern green
    warning = "#f39c12",        # Warm orange
    danger = "#e74c3c",         # Modern red
    info = "#3498db",           # Bright blue (used sparingly)
    bg = "#ffffff",             # Clean white
    fg = "#2c3e50"              # Dark text
  ),
  
  # Enable shinyjs
  useShinyjs(),
  
  # Load CSS from separate file
  includeCSS("www/styles.css"),
  
  # Page title
  tags$head(
    tags$title("IMSLU CCC Dashboard")
  ),
  
  # Application header
  fluidRow(
    column(12,
           div(
             class = "p-3 bg-primary text-white",
             h1("IMSLU CCC Dashboard", class = "text-center")
           )
    )
  ),
  
  # Main content container
  div(
    id = "main-content",
    
    # ============================================================================
    # ACCESS CODE LOGIN PAGE
    # ============================================================================
    div(
      id = "login-page",
      fluidRow(
        column(
          width = 6,
          offset = 3,
          card(
            card_header(
              div(
                class = "text-center",
                icon("shield-alt", class = "fa-2x text-primary mb-2"),
                h3("Welcome to IMSLU Dashboard", class = "mb-0")
              )
            ),
            card_body(
              div(
                class = "text-center mb-4",
                p("This application provides access to coaching sessions and CCC reviews for IMSLU residents."),
                p("Please enter your access code to continue.", class = "text-muted")
              ),
              
              div(
                class = "form-group",
                textInput(
                  "access_code", 
                  "Access Code",
                  placeholder = "Enter your access code...",
                  width = "100%"
                ),
                div(
                  class = "text-center mt-3",
                  actionButton(
                    "submit_access", 
                    "Submit", 
                    class = "btn-primary btn-lg",
                    icon = icon("sign-in-alt")
                  )
                )
              ),
              
              br(),
              div(
                id = "access_error", 
                class = "alert alert-danger", 
                style = "display: none;",
                icon("exclamation-triangle"),
                " Invalid access code. Please try again."
              )
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # NAVIGATION SELECTION PAGE
    # ============================================================================
    div(
      id = "navigation-page",
      style = "display: none;",
      fluidRow(
        column(
          width = 10,
          offset = 1,
          div(
            class = "text-center mb-4",
            h2("Select Your Dashboard", class = "text-primary"),
            p("Choose the type of review you want to conduct:", class = "lead text-muted")
          )
        )
      ),
      
      fluidRow(
        # ILP and Milestone Review Option
        column(
          width = 5,
          offset = 1,
          div(
            class = "navigation-card coaching-card",
            card(
              card_body(
                div(
                  class = "text-center p-4",
                  icon("chalkboard-teacher", class = "fa-4x text-primary mb-3"),
                  h3("ILP and Milestone Review", class = "text-primary mb-3"),
                  p("Review resident progress and conduct milestone assessments including:", class = "mb-3"),
                  tags$ul(
                    class = "text-left",
                    tags$li("ILP Review"),
                    tags$li("Milestone Data"),
                    tags$li("Secondary Review Comments"),
                    tags$li("CCC Review Entry")
                  ),
                  actionButton(
                    "select_coaching",
                    "Start CCC Review",
                    class = "btn-primary btn-lg mt-3",
                    icon = icon("arrow-right")
                  )
                )
              )
            )
          )
        ),
        
        # Follow-up List Option
        column(
          width = 5,
          div(
            class = "navigation-card ccc-card",
            card(
              card_body(
                div(
                  class = "text-center p-4",
                  icon("clipboard-check", class = "fa-4x text-success mb-3"),
                  h3("Follow-up List", class = "text-success mb-3"),
                  div(
                    class = "badge bg-warning text-dark mb-3",
                    "Coming Soon"
                  ),
                  p("Track and manage follow-up actions for residents:", class = "mb-3"),
                  tags$ul(
                    class = "text-left",
                    tags$li("Review coaching summaries"),
                    tags$li("Assess milestone progress"),
                    tags$li("Identify concerns"),
                    tags$li("Plan follow-up actions"),
                    tags$li("Document CCC decisions"),
                    tags$li("Track resident progress")
                  ),
                  actionButton(
                    "select_ccc",
                    "Start Follow-up Review",
                    class = "btn-success btn-lg mt-3",
                    icon = icon("arrow-right"),
                    disabled = TRUE
                  )
                )
              )
            )
          )
        )
      ),
      
      # Back button
      fluidRow(
        column(
          width = 12,
          div(
            class = "text-center mt-4",
            actionButton(
              "back_to_login",
              "← Back to Login",
              class = "btn-secondary",
              icon = icon("arrow-left")
            )
          )
        )
      )
    ),
    
    # ============================================================================
    # CCC PAGES - COMPLETE STRUCTURE
    # ============================================================================
    div(
      id = "ccc-pages",
      style = "display: none;",
      
      # Navigation breadcrumb
      fluidRow(
        column(12,
               div(
                 class = "breadcrumb-nav mb-3",
                 actionButton("ccc_back_to_nav", "← Back to Dashboard Selection", 
                              class = "btn-link text-decoration-none"),
                 span(" > ILP and Milestone Review", class = "text-muted")
               )
        )
      ),
      
      # ============================================================================
      # CCC DASHBOARD PAGE (Resident table)
      # ============================================================================
      div(
        id = "ccc-dashboard-page",
        
        # Enhanced Filter buttons with larger, solid styling
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Filter Options",
                          `data-card-type` = "filters"
              ),
              card_body(
                # Centered heading
                div(
                  class = "text-center mb-3",
                  h5("Quick Filters", class = "text-primary fw-bold")
                ),
                
                # Enhanced filter buttons - larger and solid
                div(
                  class = "d-flex flex-wrap justify-content-center gap-3 mb-4",
                  actionButton("filter_by_level", 
                               "Sort by Level", 
                               class = "btn-primary btn-lg px-4 py-2"),
                  actionButton("filter_fully_complete", 
                               "Fully Complete", 
                               class = "btn-success btn-lg px-4 py-2"),
                  actionButton("filter_self_done_others_pending", 
                               "Self-Eval Done", 
                               class = "btn-warning btn-lg px-4 py-2"),
                  actionButton("filter_coach_done_second_pending", 
                               "Coach Done", 
                               class = "btn-info btn-lg px-4 py-2"),
                  actionButton("filter_reviews_done_ccc_pending", 
                               "Reviews Done", 
                               class = "btn-secondary btn-lg px-4 py-2"),
                  actionButton("clear_filters", 
                               "Clear Filters", 
                               class = "btn-danger btn-lg px-4 py-2")
                ),
                
                # Help text
                div(
                  class = "text-center",
                  tags$small(
                    class = "text-muted",
                    "Use these filters to show specific subsets of residents based on completion status."
                  )
                )
              )
            )
          )
        ),
        
        # Enhanced Main residents table with bigger search bar
        fluidRow(
          column(
            width = 12,
            card(
              card_header("All Residents - ILP and Milestone Review Status",
                          `data-card-type` = "resident-table"
              ),
              card_body(
                # Centered search instructions
                div(
                  class = "text-center mb-4",
                  p(
                    class = "fw-bold table-instruction fs-5 text-primary",
                    "Click on a resident row to start the review process"
                  )
                ),
                
                # Enhanced search bar - centered and larger
                div(
                  class = "d-flex justify-content-center mb-4",
                  div(
                    class = "col-md-6 col-lg-4",
                    div(
                      class = "input-group input-group-lg",
                      tags$input(
                        type = "text",
                        class = "form-control form-control-lg",
                        id = "global_search",
                        placeholder = "Search residents by name, level, coach...",
                        style = "font-size: 18px; padding: 12px 16px; border-radius: 8px;"
                      ),
                      span(
                        class = "input-group-text",
                        icon("search", class = "text-primary")
                      )
                    )
                  )
                ),
                
                # The data table
                DT::dataTableOutput("ccc_residents_table")
              )
            )
          )
        )
      ), # FIXED: This closes the ccc-dashboard-page div properly
      
      # ============================================================================
      # CCC REVIEW PAGES (Individual resident review)
      # ============================================================================
      div(
        id = "ccc-review-pages",
        style = "display: none;",
        
        # Back button for review pages
        fluidRow(
          column(12,
                 div(
                   class = "mb-3",
                   actionButton("back_to_dashboard", "← Back to Dashboard", 
                                class = "btn-secondary"),
                   span(" > Review Details", class = "text-muted ms-2")
                 )
          )
        ),
        
        # Milestone Plots Section
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Milestone Assessments",
                          `data-card-type` = "milestone-plots"
              ),
              card_body(
                fluidRow(
                  # Self-Assessment Milestones Plot
                  column(
                    width = 6,
                    div(
                      class = "text-center mb-3",
                      h5("Current Self-Assessment", class = "text-primary"),
                      p("Resident's self-evaluation for this period", class = "text-muted small")
                    ),
                    div(
                      class = "milestone-plot-container",
                      style = "min-height: 400px;",
                      plotOutput("self_milestones_plot", height = "400px")
                    )
                  ),
                  
                  # Program Milestones Plot  
                  column(
                    width = 6,
                    div(
                      class = "text-center mb-3",
                      h5("Current Program Assessment", class = "text-success"),
                      p("Faculty assessment for this period", class = "text-muted small")
                    ),
                    div(
                      class = "milestone-plot-container", 
                      style = "min-height: 400px;",
                      plotOutput("program_milestones_plot", height = "400px")
                    )
                  )
                )
              )
            )
          )
        ),
        
        br(), # Add some spacing before the main review form
        
        fluidRow(
          # LEFT COLUMN - Coach ILP Summary, Secondary Review, CCC Comments, and Concerns
          column(
            width = 6,
            
            # Coach ILP Summary - ONLY for Scheduled Reviews
            conditionalPanel(
              condition = "input.ccc_rev_type == '1'",
              card(
                card_header("Coach ILP Summary",
                            `data-card-type` = "coach-ilp"
                ),
                card_body(
                  uiOutput("coach_ilp_summary")
                )
              ),
              br()
            ),
            
            # Secondary Review Summary - ONLY for Scheduled Reviews
            conditionalPanel(
              condition = "input.ccc_rev_type == '1'",
              card(
                card_header("Secondary Review Summary",
                            `data-card-type` = "secondary-review"
                ),
                card_body(
                  div(
                    class = "secondary-review-display",
                    uiOutput("secondary_review_display")
                  )
                )
              ),
              br()
            ),
            
            # CCC Comments on ILP - ONLY for Scheduled Reviews
            conditionalPanel(
              condition = "input.ccc_rev_type == '1'",
              card(
                card_header("CCC Comments on ILP",
                            `data-card-type` = "ccc-ilp"
                ),
                card_body(
                  textAreaInput(
                    "ccc_ilp",
                    label = NULL,
                    rows = 4,
                    width = "100%",
                    placeholder = "Enter CCC comments about the resident's ILP (Individual Learning Plan)..."
                  )
                )
              ),
              br()
            ),
            
            # CCC Concerns and Actions (ALWAYS SHOWN ON LEFT)
            card(
              card_header("CCC Concerns",
                          `data-card-type` = "ccc-concerns"
              ),
              card_body(
                # Concerns - available for both scheduled and interim reviews
                radioButtons(
                  "ccc_concern",
                  "Any concerns of the CCC?",
                  choices = c(
                    "No" = "0",
                    "Yes" = "1"
                  ),
                  selected = character(0)
                ),
                
                # CCC Actions and Competencies - APPEARS when concerns = Yes (for ANY review type)
                conditionalPanel(
                  condition = "input.ccc_concern == '1'",
                  div(
                    class = "alert alert-warning mb-3",
                    tags$p(
                      tags$strong("Concerns have been identified."),
                      " Please specify the actions and competency areas below."
                    )
                  ),
                  
                  # Actions suggested by CCC - available for both scheduled and interim
                  checkboxGroupInput(
                    "ccc_action",
                    "Actions suggested by CCC:",
                    choices = c(
                      "Remediation plan" = "1",
                      "Probation" = "2", 
                      "Referral for professionalism" = "3",
                      "Coach follow up" = "4",
                      "Meet with PD and or CCC Chair" = "5",
                      "Meet with Chiefs" = "6",
                      "Other (see notes)" = "7"
                    ),
                    selected = character(0)
                  ),
                  
                  # Action Status - appears when actions are selected (for any review type)
                  conditionalPanel(
                    condition = "input.ccc_action && input.ccc_action.length > 0",
                    checkboxGroupInput(
                      "ccc_action_status",
                      "Status of action item:",
                      choices = c(
                        "Initiation" = "1",
                        "Ongoing" = "2",
                        "Resolved" = "3",
                        "Recurring" = "4"
                      ),
                      selected = character(0)
                    )
                  ),
                  
                  # Competency areas - available for both scheduled and interim
                  checkboxGroupInput(
                    "ccc_competency",
                    "Which area(s) of competence, if any? (can select more than one):",
                    choices = c(
                      "Patient Care" = "1",
                      "Medical Knowledge" = "2",
                      "Systems-based Practice" = "3", 
                      "Practice-based Learning and Improvement" = "4",
                      "Professionalism" = "5",
                      "Interpersonal Communication Skills" = "6",
                      "Not a competence concern" = "7"
                    ),
                    selected = character(0)
                  )
                ),
                
                # Action Items Checkbox - available for both review types
                hr(),
                checkboxInput(
                  "has_action_items",
                  "Are there action items for program follow-up?",
                  value = FALSE
                ),
                
                # Issues for follow up - appears when action items checkbox is checked (any review type)
                conditionalPanel(
                  condition = "input.has_action_items",
                  textAreaInput(
                    "ccc_issues_follow_up",
                    "Issues for the Program to deal with or follow up / action items:",
                    rows = 4,
                    width = "100%",
                    placeholder = "Enter any issues for program follow-up..."
                  )
                ),
                
                # General Comments - ONLY for Scheduled Reviews
                conditionalPanel(
                  condition = "input.ccc_rev_type == '1'",
                  textAreaInput(
                    "ccc_comments",
                    "Additional Comments:",
                    rows = 4,
                    placeholder = "Enter any additional comments about this resident's review..."
                  )
                )
              )
            )
          ),
          
          # RIGHT COLUMN
          column(
            width = 6,
            
            # Review Type and Basic Information
            card(
              card_header("CCC Review - Basic Information",
                          `data-card-type` = "ccc-basic"
              ),
              card_body(
                # Review Type
                radioButtons(
                  "ccc_rev_type",
                  "Review Type:",
                  choices = c(
                    "Scheduled Review" = "1",
                    "Interim Review" = "2"
                  ),
                  selected = character(0)
                ),
                
                # Session (for scheduled reviews ONLY)
                conditionalPanel(
                  condition = "input.ccc_rev_type == '1'",
                  selectInput(
                    "ccc_session",
                    "Review Session:",
                    choices = c(
                      "Select session..." = "",
                      "Mid Intern" = "1",
                      "End Intern" = "2",
                      "Mid PGY2" = "3",
                      "End PGY2" = "4",
                      "Mid PGY3" = "5",
                      "Graduation" = "6",
                      "Intern Intro" = "7"
                    ),
                    selected = ""
                  ),
                  
                  # Milestone completion (for scheduled reviews EXCEPT Intern Intro)
                  conditionalPanel(
                    condition = "input.ccc_session != '7'",
                    radioButtons(
                      "ccc_mile",
                      "Are the milestones complete and acceptable?",
                      choices = c(
                        "No" = "0",
                        "Yes" = "1"
                      ),
                      selected = character(0)
                    )
                  ),
                  
                  # Intern Intro Note (when session = "7")
                  conditionalPanel(
                    condition = "input.ccc_session == '7'",
                    div(
                      class = "alert alert-info mt-3",
                      icon("info-circle", class = "me-2"),
                      tags$strong("Intern Introduction Review"),
                      tags$p(
                        class = "mb-0 mt-2",
                        "This is an introductory review for new interns. Milestone assessments are not required during this period."
                      )
                    )
                  )
                ),
                
                # Interim Notes (for interim reviews ONLY)
                conditionalPanel(
                  condition = "input.ccc_rev_type == '2'",
                  div(
                    class = "mt-3",
                    h6("Interim Review Notes:", class = "text-primary"),
                    textAreaInput(
                      "ccc_interim",
                      label = NULL,
                      rows = 6,
                      width = "100%",
                      placeholder = "Enter interim review notes and observations..."
                    )
                  )
                )
              )
            ),
            
            br(),
            
            # Milestone editing section - ONLY for Scheduled Reviews when milestones = No AND NOT Intern Intro
            conditionalPanel(
              condition = "input.ccc_rev_type == '1' && input.ccc_session != '7' && input.ccc_mile == '0'",
              card(
                card_header(
                  div(
                    class = "d-flex justify-content-between align-items-center",
                    h5("Edit Milestone Assessments", class = "mb-0 text-warning"),
                    tags$small("Select and edit specific milestones that need corrections", class = "text-muted")
                  )
                ),
                card_body(
                  # Comments about milestone changes
                  textAreaInput(
                    "ccc_mile_concerns", 
                    label = "Comments about milestone changes:",
                    rows = 3,
                    width = "100%",
                    placeholder = "Explain what milestone changes you made and why..."
                  ),
                  
                  # Enhanced milestone editing module
                  uiOutput("ccc_milestone_module_ui")
                )
              )
            ),
            
            # Placeholder for Scheduled Reviews when milestones are acceptable (NOT Intern Intro)
            conditionalPanel(
              condition = "input.ccc_rev_type == '1' && input.ccc_session != '7' && input.ccc_mile == '1'",
              div(
                class = "text-center p-4",
                style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 10px; margin-top: 20px;",
                icon("check-circle", class = "fa-2x text-success mb-3"),
                h5("Milestones Acceptable", class = "text-success"),
                p("No milestone editing required.", class = "text-muted")
              )
            ),
            
            # Placeholder for Intern Intro Reviews
            conditionalPanel(
              condition = "input.ccc_rev_type == '1' && input.ccc_session == '7'",
              div(
                class = "text-center p-4",
                style = "background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%); border-radius: 10px; margin-top: 20px;",
                icon("user-graduate", class = "fa-2x text-warning mb-3"),
                h5("Intern Introduction Review", class = "text-warning"),
                p("Welcome review for new interns. Focus on orientation, concerns, and initial observations.", class = "text-muted")
              )
            ),
            
            # Placeholder for Interim Reviews
            conditionalPanel(
              condition = "input.ccc_rev_type == '2'",
              div(
                class = "text-center p-4",
                style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 10px; margin-top: 20px;",
                icon("clipboard-list", class = "fa-2x text-info mb-3"),
                h5("Interim Review", class = "text-info"),
                p("Complete the interim notes above and indicate any concerns on the left.", class = "text-muted")
              )
            ),
            
            # Submit button - ALWAYS at bottom right
            div(
              class = "text-center mt-4",
              uiOutput("ccc_submit_button")
            )
          )
        )
      )
    ) # End of ccc-pages div
  ) # End of main-content div
) # End of page_fluid