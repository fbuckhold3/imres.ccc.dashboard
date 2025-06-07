# ============================================================================
# MINOR UI FIXES - Preserving Your Original Unfolding Logic
# Only fixing specific issues, not restructuring
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
  
  # JavaScript for opening resident dashboard with auto-filled access code
  tags$script(HTML("
    function openResidentDashboard() {
      // Get the access code from the displayed text
      var accessCodeElement = document.querySelector('[data-access-code]');
      var accessCode = '';
      
      if (accessCodeElement) {
        accessCode = accessCodeElement.textContent || accessCodeElement.innerText;
        accessCode = accessCode.trim();
      }
      
      // Base URL for the resident dashboard
      var baseUrl = 'https://01958bd2-2c58-32d8-e506-7e75564664d5.share.connect.posit.cloud';
      
      // Open new window with the dashboard
      var newWindow = window.open(baseUrl, '_blank', 'width=1200,height=800,scrollbars=yes,resizable=yes');
      
      // If we have an access code, try to auto-fill it after a short delay
      if (accessCode && newWindow) {
        setTimeout(function() {
          try {
            // Try to access the new window and fill the access code
            var accessInput = newWindow.document.querySelector('input[id*=\"access\"], input[placeholder*=\"access\"], input[placeholder*=\"code\"]');
            if (accessInput) {
              accessInput.value = accessCode;
              // Try to trigger the submit or enter key
              var event = new KeyboardEvent('keydown', {
                key: 'Enter',
                code: 'Enter',
                which: 13,
                keyCode: 13,
                bubbles: true
              });
              accessInput.dispatchEvent(event);
            }
          } catch (e) {
            // Cross-origin restrictions may prevent auto-fill
            console.log('Auto-fill blocked by browser security - user will need to enter access code manually');
          }
        }, 2000); // Wait 2 seconds for page to load
      }
    }
  ")),
  
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
    # CCC PAGES - COMPLETE STRUCTURE (KEEPING YOUR ORIGINAL LOGIC)
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
      # CCC REVIEW PAGES (Individual resident review) - YOUR ORIGINAL LOGIC
      # ============================================================================
      div(
        id = "ccc-review-pages",
        style = "display: none;",
        
        # Back button and resident info header
        fluidRow(
          column(12,
                 div(
                   class = "d-flex justify-content-between align-items-center mb-4 p-3 bg-light rounded",
                   div(
                     actionButton("back_to_dashboard", "← Back to Dashboard", 
                                  class = "btn-secondary"),
                     span(" > Review Details", class = "text-muted ms-2")
                   ),
                   div(
                     class = "resident-info-header text-end",
                     h4(textOutput("display_resident_name", inline = TRUE), 
                        class = "text-primary mb-1"),
                     div(
                       class = "text-muted",
                       "Coach: ", textOutput("display_primary_coach", inline = TRUE), " | ",
                       "Access Code: ", 
                       tags$code(
                         textOutput("display_access_code", inline = TRUE),
                         class = "bg-warning text-dark px-2 py-1 rounded"
                       ),
                       # Link to open resident dashboard in new window
                       tags$a(
                         href = "#",
                         onclick = "openResidentDashboard()",
                         title = "Open Resident Dashboard in New Window",
                         class = "text-primary ms-2",
                         icon("external-link-alt")
                       )
                     )
                   )
                 )
          )
        ),
        
        # STEP 1: Initial Review Setup (Always Visible)
        fluidRow(
          column(
            width = 12,
            card(
              card_header("Step 1: Review Setup",
                          `data-card-type` = "review-setup"
              ),
              card_body(
                fluidRow(
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
                    )
                  ),
                  column(
                    width = 6,
                    # Session (only for scheduled reviews)
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
                    )
                  )
                )
              )
            )
          )
        ),
        
        br(),
        
        div(
          id = "review-content-section",
          
          # Show content only after review type is selected
          conditionalPanel(
            condition = "input.ccc_rev_type != '' && input.ccc_rev_type != null",
            
            # INTERIM REVIEW CONTENT (Full Width) - ENHANCED STYLING
            conditionalPanel(
              condition = "input.ccc_rev_type == '2'",
              fluidRow(
                column(
                  width = 12,
                  card(
                    card_header(
                      div(
                        class = "d-flex align-items-center",
                        icon("clipboard-list", class = "fa-2x text-warning me-3"),
                        div(
                          h4("Interim Review Details", class = "text-warning mb-1"),
                          tags$small("Document interim concerns and observations", class = "text-muted")
                        )
                      ),
                      `data-card-type` = "interim-review"
                    ),
                    card_body(
                      # Enhanced Interim Notes with gradient background
                      div(
                        class = "mb-4 p-4",
                        style = "background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%); border-radius: 12px; border-left: 5px solid #ffc107;",
                        div(
                          class = "d-flex align-items-center mb-3",
                          icon("edit", class = "fa-lg text-warning me-2"),
                          h5("Notes on Concern from CCC", class = "text-warning mb-0")
                        ),
                        textAreaInput(
                          "ccc_interim",
                          label = NULL,
                          rows = 8,
                          width = "100%",
                          placeholder = "Enter detailed notes about the interim concerns, observations, and specific issues that prompted this review..."
                        )
                      ),
                      
                      # Enhanced Concerns Section with modern styling
                      div(
                        class = "concerns-section p-4 mb-4",
                        style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 12px; border-left: 5px solid #6c757d;",
                        div(
                          class = "d-flex align-items-center mb-3",
                          icon("exclamation-triangle", class = "fa-lg text-secondary me-2"),
                          h5("CCC Concern Assessment", class = "text-secondary mb-0")
                        ),
                        
                        radioButtons(
                          "ccc_concern",
                          "Any concerns of the CCC?",
                          choices = c(
                            "No" = "0",
                            "Yes" = "1"
                          ),
                          selected = character(0)
                        ),
                        
                        # Enhanced Concern details (when Yes is selected)
                        conditionalPanel(
                          condition = "input.ccc_concern == '1'",
                          div(
                            class = "alert alert-warning mb-4",
                            style = "border-left: 5px solid #ffc107; background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);",
                            div(
                              class = "d-flex align-items-center",
                              icon("warning", class = "fa-lg text-warning me-2"),
                              div(
                                tags$strong("Concerns Identified"),
                                tags$p("Please specify the actions and competency areas below.", class = "mb-0 mt-1")
                              )
                            )
                          ),
                          
                          fluidRow(
                            column(
                              width = 6,
                              div(
                                class = "action-section p-3 mb-3",
                                style = "background: #ffffff; border-radius: 8px; border: 1px solid #dee2e6;",
                                h6("CCC Actions", class = "text-primary mb-3"),
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
                                    "Other (see notes)" = "7",
                                    "Referral to Behavioral Health" = "8"
                                  ),
                                  selected = character(0)
                                ),
                                
                                # Action Status
                                conditionalPanel(
                                  condition = "input.ccc_action && input.ccc_action.length > 0",
                                  hr(),
                                  h6("Action Status", class = "text-info mb-3"),
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
                                )
                              )
                            ),
                            column(
                              width = 6,
                              div(
                                class = "competency-section p-3 mb-3",
                                style = "background: #ffffff; border-radius: 8px; border: 1px solid #dee2e6;",
                                h6("Competency Areas", class = "text-success mb-3"),
                                checkboxGroupInput(
                                  "ccc_competency",
                                  "Which area(s) of competence, if any?",
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
                              )
                            )
                          ),
                          
                          # Action Items section for interim reviews
                          div(
                            class = "action-items-section p-4 mb-3",
                            style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 12px; border-left: 5px solid #2196f3;",
                            div(
                              class = "d-flex align-items-center mb-4",
                              icon("tasks", class = "fa-xl text-primary me-3"),
                              h4("Action Items", class = "text-primary mb-0")
                            ),
                            
                            # Action Items Checkbox
                            div(
                              class = "mb-3",
                              checkboxInput(
                                "has_action_items",
                                "Are there action items for program follow-up?",
                                value = FALSE
                              )
                            ),
                            
                            # Show additional fields when action items checkbox is checked
                            conditionalPanel(
                              condition = "input.has_action_items",
                              fluidRow(
                                column(
                                  width = 6,
                                  textInput(
                                    "ccc_fu_resp",
                                    "Person Responsible:",
                                    placeholder = "Enter person responsible for follow-up..."
                                  )
                                ),
                                column(
                                  width = 12,
                                  textAreaInput(
                                    "ccc_issues_follow_up",
                                    "Issues for the Program to deal with or follow up:",
                                    rows = 4,
                                    placeholder = "Enter specific issues for program follow-up..."
                                  )
                                )
                              )
                            )
                          )
                        )
                      ),
                      
                      # Enhanced Submit button for interim reviews
                      div(
                        class = "text-center mt-4 p-4",
                        style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 12px;",
                        uiOutput("interim_submit_button")
                      )
                    )
                  )
                )
              )
            ),
            
            # SCHEDULED REVIEW - STEP 2: Enhanced Left Column Content
            conditionalPanel(
              condition = "input.ccc_rev_type == '1' && input.ccc_session != ''",
              
              div(
                id = "scheduled-step-2",
                fluidRow(
                  column(
                    width = 12,
                    card(
                      card_header(
                        div(
                          class = "d-flex align-items-center",
                          icon("chalkboard-teacher", class = "fa-2x text-primary me-3"),
                          div(
                            h4("Scheduled Review Details", class = "text-primary mb-1"),
                            tags$small("Review ILP summary, secondary comments, and document CCC assessment", class = "text-muted")
                          )
                        ),
                        `data-card-type` = "scheduled-step2"
                      ),
                      card_body(
                        # Enhanced Coach ILP Summary
                        div(
                          class = "ilp-summary-section mb-4 p-4",
                          style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 12px; border-left: 5px solid #2196f3;",
                          div(
                            class = "d-flex align-items-center mb-3",
                            icon("user-tie", class = "fa-lg text-primary me-2"),
                            h5("Coach ILP Summary", class = "text-primary mb-0")
                          ),
                          div(
                            class = "coach-ilp-display",
                            uiOutput("coach_ilp_summary")
                          )
                        ),
                        
                        # Enhanced Secondary Review Summary
                        div(
                          class = "secondary-review-section mb-4 p-4",
                          style = "background: linear-gradient(135deg, #f3e5f5 0%, #e1bee7 100%); border-radius: 12px; border-left: 5px solid #9c27b0;",
                          div(
                            class = "d-flex align-items-center mb-3",
                            icon("user-check", class = "fa-lg text-purple me-2"),
                            h5("Secondary Review Summary", class = "mb-0", style = "color: #9c27b0;")
                          ),
                          div(
                            class = "secondary-review-display",
                            uiOutput("secondary_review_display")
                          )
                        ),
                        
                        # Enhanced CCC Comments on ILP
                        div(
                          class = "ccc-comments-section mb-4 p-4",
                          style = "background: linear-gradient(135deg, #e8f5e8 0%, #c8e6c9 100%); border-radius: 12px; border-left: 5px solid #4caf50;",
                          div(
                            class = "d-flex align-items-center mb-3",
                            icon("comments", class = "fa-lg text-success me-2"),
                            h5("CCC Comments on ILP", class = "text-success mb-0")
                          ),
                          textAreaInput(
                            "ccc_ilp",
                            label = NULL,
                            rows = 4,
                            width = "100%",
                            placeholder = "Enter CCC comments about the resident's ILP (Individual Learning Plan)..."
                          )
                        ),
                        
                        # Enhanced CCC Concerns
                        div(
                          class = "concerns-section p-4 mb-4",
                          style = "background: linear-gradient(135deg, #fff3e0 0%, #ffe0b2 100%); border-radius: 12px; border-left: 5px solid #ff9800;",
                          div(
                            class = "d-flex align-items-center mb-3",
                            icon("exclamation-triangle", class = "fa-lg text-warning me-2"),
                            h5("CCC Concerns", class = "text-warning mb-0")
                          ),
                          
                          radioButtons(
                            "ccc_concern",
                            "Any concerns of the CCC?",
                            choices = c(
                              "No" = "0",
                              "Yes" = "1"
                            ),
                            selected = character(0)
                          ),
                          
                          # Enhanced concern details
                          conditionalPanel(
                            condition = "input.ccc_concern == '1'",
                            div(
                              class = "alert alert-warning mb-4",
                              style = "border-left: 5px solid #ffc107; background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);",
                              div(
                                class = "d-flex align-items-center",
                                icon("warning", class = "fa-lg text-warning me-2"),
                                div(
                                  tags$strong("Concerns Identified"),
                                  tags$p("Please specify the actions and competency areas below.", class = "mb-0 mt-1")
                                )
                              )
                            ),
                            
                            # Enhanced Notes on Concern for scheduled reviews when concerns exist - WIDER TEXT BOX
                            div(
                              class = "mb-4 p-4",
                              style = "background: #ffffff; border-radius: 8px; border: 2px solid #ffc107;",
                              div(
                                class = "d-flex align-items-center mb-3",
                                icon("edit", class = "fa-lg text-warning me-2"),
                                h6("Notes on Concern from CCC", class = "text-warning mb-0")
                              ),
                              div(
                                class = "row",
                                div(
                                  class = "col-12",  # Full width
                                  textAreaInput(
                                    "ccc_concern_notes",
                                    label = NULL,
                                    rows = 6,
                                    width = "100%",  # Ensure full width
                                    placeholder = "Enter detailed notes about the concerns identified by the CCC..."
                                  )
                                )
                              )
                            ),
                            
                            fluidRow(
                              column(
                                width = 6,
                                div(
                                  class = "action-section p-3 mb-3",
                                  style = "background: #ffffff; border-radius: 8px; border: 1px solid #dee2e6;",
                                  h6("CCC Actions", class = "text-primary mb-3"),
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
                                      "Other (see notes)" = "7",
                                      "Referral to Behavioral Health" = "8"
                                    ),
                                    selected = character(0)
                                  ),
                                  
                                  # Action Status
                                  conditionalPanel(
                                    condition = "input.ccc_action && input.ccc_action.length > 0",
                                    hr(),
                                    h6("Action Status", class = "text-info mb-3"),
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
                                  )
                                )
                              ),
                              column(
                                width = 6,
                                div(
                                  class = "competency-section p-3 mb-3",
                                  style = "background: #ffffff; border-radius: 8px; border: 1px solid #dee2e6;",
                                  h6("Competency Areas", class = "text-success mb-3"),
                                  checkboxGroupInput(
                                    "ccc_competency",
                                    "Which area(s) of competence, if any?",
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
                                )
                              )
                            ),
                            
                            # Action Items section for scheduled reviews
                            div(
                              class = "action-items-section p-4 mb-3",
                              style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); border-radius: 12px; border-left: 5px solid #2196f3;",
                              div(
                                class = "d-flex align-items-center mb-4",
                                icon("tasks", class = "fa-xl text-primary me-3"),
                                h4("Action Items", class = "text-primary mb-0")
                              ),
                              
                              # Action Items Checkbox
                              div(
                                class = "mb-3",
                                checkboxInput(
                                  "has_action_items",
                                  "Are there action items for program follow-up?",
                                  value = FALSE
                                )
                              ),
                              
                              # Show additional fields when action items checkbox is checked
                              conditionalPanel(
                                condition = "input.has_action_items",
                                fluidRow(
                                  column(
                                    width = 6,
                                    textInput(
                                      "ccc_fu_resp",
                                      "Person Responsible:",
                                      placeholder = "Enter person responsible for follow-up..."
                                    )
                                  ),
                                  column(
                                    width = 12,
                                    textAreaInput(
                                      "ccc_issues_follow_up",
                                      "Issues for the Program to deal with or follow up:",
                                      rows = 4,
                                      placeholder = "Enter specific issues for program follow-up..."
                                    )
                                  )
                                )
                              )
                            )
                          )
                        ),
                        
                        # Enhanced Additional Comments
                        div(
                          class = "additional-comments-section mb-4 p-4",
                          style = "background: linear-gradient(135deg, #f9f9f9 0%, #f0f0f0 100%); border-radius: 12px; border-left: 5px solid #607d8b;",
                          div(
                            class = "d-flex align-items-center mb-3",
                            icon("sticky-note", class = "fa-lg me-2", style = "color: #607d8b;"),
                            h5("Additional Comments", class = "mb-0", style = "color: #607d8b;")
                          ),
                          textAreaInput(
                            "ccc_comments",
                            label = NULL,
                            rows = 4,
                            placeholder = "Enter any additional comments about this resident's review..."
                          )
                        ),
                        
                        # Enhanced Next button for scheduled reviews
                        div(
                          class = "text-center mt-4 p-4",
                          style = "background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%); border-radius: 12px;",
                          actionButton(
                            "proceed_to_milestones",
                            "Next: Milestone Review →",
                            class = "btn-primary btn-lg px-5 py-3",
                            icon = icon("arrow-right"),
                            style = "border-radius: 25px; font-weight: 600; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        
        # STEP 3: Milestone Review Section (Only for Scheduled Reviews) - YOUR ORIGINAL LOGIC
        div(
          id = "milestone-review-section",
          style = "display: none;",
          
          # Fixed Header with Graphs
          div(
            class = "milestone-graphs-header sticky-top bg-white border-bottom shadow-sm p-3 mb-4",
            style = "z-index: 1020;",
            
            # Milestone Plots - Fixed at top
            fluidRow(
              column(
                width = 6,
                div(
                  class = "text-center mb-2",
                  h6("Current Self-Assessment", class = "text-primary mb-1"),
                  tags$small("Resident's self-evaluation", class = "text-muted")
                ),
                div(
                  class = "milestone-plot-container",
                  style = "height: 300px; border: 1px solid #dee2e6; border-radius: 4px;",
                  plotOutput("self_milestones_plot", height = "300px")
                )
              ),
              column(
                width = 6,
                div(
                  class = "text-center mb-2",
                  h6("Current Program Assessment", class = "text-success mb-1"),
                  tags$small("Faculty assessment", class = "text-muted")
                ),
                div(
                  class = "milestone-plot-container", 
                  style = "height: 300px; border: 1px solid #dee2e6; border-radius: 4px;",
                  plotOutput("program_milestones_plot", height = "300px")
                )
              )
            )
          ),
          
          # Scrollable Milestone Content
          div(
            class = "milestone-content-scrollable",
            style = "padding-top: 20px;",
            
            # Secondary Review Comments (if available)
            conditionalPanel(
              condition = "output.has_second_comments",
              fluidRow(
                column(
                  width = 12,
                  card(
                    card_header("Secondary Review Comments",
                                `data-card-type` = "second-comments"
                    ),
                    card_body(
                      uiOutput("second_comments_display")
                    )
                  )
                )
              ),
              br()
            ),
            
            # Milestone Interface
            fluidRow(
              column(
                width = 12,
                card(
                  card_header("Step 3: Milestone Assessment",
                              `data-card-type` = "milestone-assessment"
                  ),
                  card_body(
                    # Special note for Intern Intro
                    conditionalPanel(
                      condition = "input.ccc_session == '7'",
                      div(
                        class = "alert alert-info",
                        icon("info-circle", class = "me-2"),
                        tags$strong("Intern Introduction Review:"),
                        " Milestone assessments are not required for this period."
                      )
                    ),
                    
                    # Milestone interface for other sessions
                    conditionalPanel(
                      condition = "input.ccc_session != '7'",
                      uiOutput("milestone_interface_content")
                    )
                  )
                )
              )
            ),
            
            br(),
            
            # Final Submit Button
            div(
              class = "text-center mb-4",
              actionButton(
                "back_to_step2",
                "← Back to Review Details",
                class = "btn-secondary me-3"
              ),
              uiOutput("final_submit_button", inline = TRUE)
            )
          )
        )
      ) # End of ccc-review-pages div
    ) # End of ccc-pages div
  ) # End of main-content div
) # End of page_fluid
