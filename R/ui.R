# Fixed UI Definition - Main Content Area
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
                    "Start ILP Review",
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
    # CCC PAGES
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
        
        # Filter buttons
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Filter Options",
                          `data-card-type` = "filters"
                          ),
              card_body(
                div(
                  class = "d-flex flex-wrap gap-2 mb-3",
                  actionButton("filter_by_level", "Sort by Level", class = "btn-outline-primary btn-sm"),
                  actionButton("filter_fully_complete", "Fully Complete", class = "btn-outline-success btn-sm"),
                  actionButton("filter_self_done_others_pending", "Self-Eval Done", class = "btn-outline-warning btn-sm"),
                  actionButton("filter_coach_done_second_pending", "Coach Done", class = "btn-outline-info btn-sm"),
                  actionButton("filter_reviews_done_ccc_pending", "Reviews Done", class = "btn-outline-secondary btn-sm"),
                  actionButton("clear_filters", "Clear Filters", class = "btn-outline-danger btn-sm")
                ),
                tags$small(
                  class = "text-muted",
                  "Use these filters to show specific subsets of residents based on completion status."
                )
              )
            )
          )
        ),
        
        # Main residents table
        fluidRow(
          column(
            width = 12,
            card(
              card_header("All Residents - ILP and Milestone Review Status",
                          `data-card-type` = "resident-table"
                          ),
              card_body(
                p(
                  class = "mb-3 text-center fw-bold table-instruction",
                  "Click on a resident row to start the review process"
                ),
                DT::dataTableOutput("ccc_residents_table")
              )
            )
          )
        )
      ),
      
      # ============================================================================
      # CCC REVIEW PAGES (Individual resident review)
      # ============================================================================
      div(
        id = "ccc-review-pages",
        style = "display: none;",
        
        # Resident info panel
        fluidRow(
          column(
            width = 12,
            div(
              class = "resident-info-panel p-3 bg-light border rounded mb-4",
              fluidRow(
                column(2, h5("Resident:", textOutput("display_resident_name", inline = TRUE))),
                column(2, h5("Level:", textOutput("display_resident_level", inline = TRUE))),
                column(2, h5("Access Code:", textOutput("display_access_code", inline = TRUE))),
                column(3, h5("Primary Coach:", textOutput("display_primary_coach", inline = TRUE))),
                column(3, h5("Period:", textOutput("display_current_period", inline = TRUE)))
              ),
              div(
                class = "mt-2",
                actionButton("back_to_dashboard", "← Back to Dashboard", 
                             class = "btn-secondary btn-sm")
              )
            )
          )
        ),
        
        # Milestone plots
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Milestone Assessments",
                          `data-card-type` = "milestones"
                          ),
              card_body(
                fluidRow(
                  column(6, 
                         h5("Current Self-Assessment"),
                         plotOutput("self_milestones_plot", height = "400px")
                  ),
                  column(6, 
                         h5("Current Program Assessment"),
                         plotOutput("program_milestones_plot", height = "400px")
                  )
                )
              )
            )
          )
        ),
        
        # Coach ILP Summary, Secondary Review Summary, and Basic CCC Questions
        fluidRow(
          column(
            width = 6,
            card(
              card_header("Secondary Review Summary"),
              card_body(
                fluidRow(
                  # LEFT COLUMN - Review Details
                  column(
                    width = 6,
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
                    
                    # Session (for scheduled reviews) - moved to left column
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
                      )
                    ),
                    
                    # Concerns
                    radioButtons(
                      "ccc_concern",
                      "Are there any concerns about this resident?",
                      choices = c(
                        "No" = "0",
                        "Yes" = "1"
                      ),
                      selected = character(0)
                    ),
                    
                    # Additional fields when concerns = Yes
                    conditionalPanel(
                      condition = "input.ccc_concern == '1'",
                      div(
                        class = "alert alert-warning",
                        tags$p(
                          tags$strong("Please describe the concerns:"),
                          "Your comments will be reviewed by the CCC."
                        ),
                        textAreaInput(
                          "ccc_concern_details", 
                          label = NULL,
                          rows = 4,
                          width = "100%",
                          placeholder = "Describe the specific concerns about this resident..."
                        )
                      )
                    ),
                    
                    # Additional Comments Section
                    radioButtons(
                      "ccc_has_additional_comments",
                      "Do you have additional comments?",
                      choices = c(
                        "No" = "0",
                        "Yes" = "1"
                      ),
                      selected = "0"
                    ),
                    
                    # Additional Comments Text Box (appears when Yes is selected)
                    conditionalPanel(
                      condition = "input.ccc_has_additional_comments == '1'",
                      div(
                        class = "mt-3",
                        textAreaInput(
                          "ccc_comments",
                          "Additional Comments:",
                          rows = 5,
                          placeholder = "Enter any additional comments about this resident's review..."
                        )
                      )
                    )
                  ),
                  
                  # RIGHT COLUMN - Milestone Assessment
                  column(
                    width = 6,
                    # Milestone completion (for scheduled reviews) - moved to right column
                    conditionalPanel(
                      condition = "input.ccc_rev_type == '1'",
                      div(
                        class = "milestone-assessment-section",
                        h5("Milestone Assessment", class = "text-primary mb-3"),
                        radioButtons(
                          "ccc_mile",
                          "Are the milestones complete and acceptable?",
                          choices = c(
                            "No" = "0",
                            "Yes" = "1"
                          ),
                          selected = character(0)
                        ),
                        
                        conditionalPanel(
                          condition = "input.ccc_mile == '0'",
                          div(
                            class = "alert alert-warning mb-4",
                            tags$p(
                              tags$strong("Milestone concerns detected:"),
                              "Please review and edit the milestone assessments below, then provide comments about your changes."
                            ),
                            textAreaInput(
                              "ccc_mile_concerns", 
                              label = "Comments about milestone changes:",
                              rows = 3,
                              width = "100%",
                              placeholder = "Explain what milestone changes you made and why..."
                            )
                          ),
                          
                          # Deploy the milestone editing module
                          div(
                            class = "milestone-edit-section",
                            card(
                              card_header(
                                div(
                                  class = "d-flex justify-content-between align-items-center",
                                  h5("Edit Milestone Assessments", class = "mb-0 text-warning"),
                                  tags$small("Make corrections to the milestone ratings below", class = "text-muted")
                                )
                              ),
                              card_body(
                                # This will render the milestone module for editing
                                uiOutput("ccc_milestone_module_ui")
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ),
                
                # Validation and submit (full width at bottom)
                div(
                  class = "text-center mt-4",
                  hr(),
                  uiOutput("ccc_submit_button")
                )
              )
            )
          )
        )
      )
    )
  ) # End of main-content div
) #