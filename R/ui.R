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

  # JS: visual selection state for radio buttons and checkboxes
  tags$script(HTML("
    $(document).ready(function() {

      // --- Radio button highlight ---
      function syncRadioState(container) {
        container.find('.radio').removeClass('shiny-selected');
        container.find('input[type=radio]:checked').closest('.radio').addClass('shiny-selected');
      }
      $(document).on('change', 'input[type=radio]', function() {
        syncRadioState($(this).closest('.shiny-input-container'));
      });
      // Init on load and whenever Shiny re-renders
      $(document).on('shiny:value shiny:inputchanged', function() {
        $('.shiny-input-container').each(function() {
          syncRadioState($(this));
        });
      });

      // --- Checkbox highlight ---
      function syncCheckboxState(cb) {
        if (cb.prop('checked')) {
          cb.closest('.checkbox').addClass('shiny-cb-selected');
        } else {
          cb.closest('.checkbox').removeClass('shiny-cb-selected');
        }
      }
      $(document).on('change', 'input[type=checkbox]', function() {
        syncCheckboxState($(this));
      });

    });
  ")),
  
  # Enhanced JavaScript for opening resident dashboard with auto-filled access code
  tags$script(HTML("
    function openResidentDashboard() {
      // Get the access code from the displayed text - multiple approaches
      var accessCode = '';
      
      // Try different selectors to find the access code
      var codeElement = document.querySelector('code.access-code-display') || 
                       document.querySelector('code.bg-warning') || 
                       document.querySelector('[data-access-code]') ||
                       document.querySelector('#display_access_code');
      
      if (codeElement) {
        accessCode = codeElement.textContent || codeElement.innerText;
        accessCode = accessCode.trim();
      }
      
      // Fallback: try to get from any code element
      if (!accessCode) {
        var codeElements = document.querySelectorAll('code');
        for (var i = 0; i < codeElements.length; i++) {
          var text = codeElements[i].textContent || codeElements[i].innerText;
          if (text && text.trim().length >= 4 && text.trim().length <= 20) {
            accessCode = text.trim();
            break;
          }
        }
      }
      
      console.log('Access code found:', accessCode);
      
      // Base URL for the resident dashboard
      var baseUrl = 'https://01958bd2-2c58-32d8-e506-7e75564664d5.share.connect.posit.cloud';
      
      // Open new window with the dashboard
      var newWindow = window.open(baseUrl, '_blank', 'width=1200,height=800,scrollbars=yes,resizable=yes');
      
      // If we have an access code, show it to the user for manual entry
      if (accessCode && newWindow) {
        // Create a temporary notification to show the access code
        setTimeout(function() {
          alert('Resident Dashboard Access Code: ' + accessCode + '\\n\\nPlease enter this code in the new window that just opened.');
        }, 1000);
      } else if (newWindow) {
        setTimeout(function() {
          alert('Resident Dashboard opened in new window.\\n\\nPlease check for the access code on this page and enter it manually.');
        }, 1000);
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
        
        # ENHANCED Back button and resident info header with PROMINENT dashboard link
        fluidRow(
          column(12,
                 div(
                   class = "resident-header-section mb-4 p-4 bg-light rounded shadow-sm",
                   fluidRow(
                     # Left side - Back button and breadcrumb
                     column(
                       width = 4,
                       div(
                         class = "d-flex align-items-center",
                         actionButton("back_to_dashboard", "← Back to Dashboard", 
                                      class = "btn-secondary me-3"),
                         span(" > Review Details", class = "text-muted")
                       )
                     ),
                     
                     # Center - Resident info
                     column(
                       width = 4,
                       div(
                         class = "text-center",
                         h4(textOutput("display_resident_name", inline = TRUE), 
                            class = "text-primary mb-2 fw-bold"),
                         div(
                           class = "text-muted mb-2",
                           "Coach: ", textOutput("display_primary_coach", inline = TRUE)
                         ),
                         div(
                           class = "access-code-section",
                           "Access Code: ",
                           tags$code(
                             textOutput("display_access_code", inline = TRUE),
                             class = "access-code-display bg-warning text-dark px-3 py-2 rounded fw-bold fs-6"
                           )
                         )
                       )
                     ),
                     
                     # Right side - Resident Dashboard Link + quick references
                     column(
                       width = 4,
                       div(
                         class = "text-end d-flex flex-column align-items-end gap-2",
                         # Resident Dashboard button
                         tags$button(
                           class = "btn btn-info btn-lg px-4 py-3 resident-dashboard-btn",
                           onclick = "openResidentDashboard()",
                           style = "border-radius:12px;font-weight:600;box-shadow:0 4px 8px rgba(0,0,0,0.15);background:linear-gradient(135deg,#17a2b8 0%,#138496 100%);border:none;",
                           div(
                             class = "d-flex align-items-center justify-content-center",
                             icon("external-link-alt", class = "fa-lg me-2"),
                             div(
                               div("Open Resident", class = "fw-bold"),
                               div("Dashboard", class = "fw-bold"),
                               tags$small("(New Window)", class = "text-light opacity-75")
                             )
                           )
                         ),
                         # ITE Nomogram + Step 3 status row
                         div(
                           class = "d-flex align-items-center gap-2 flex-wrap justify-content-end",
                           tags$a(
                             href = "https://019c6e98-bc22-40f0-d94c-ec16c5110b29.share.connect.posit.cloud",
                             target = "_blank",
                             class = "btn btn-outline-info btn-sm px-2",
                             icon("chart-line", class = "me-1"), "ITE Nomogram"
                           ),
                           uiOutput("step3_status_display")
                         )
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
              card_header(
                div(class = "d-flex align-items-center",
                    div(class = "step-badge me-2", "1"),
                    "Review Setup"),
                `data-card-type` = "review-setup"
              ),
              card_body(
                fluidRow(
                  column(
                    width = 6,
                    radioButtons(
                      "ccc_rev_type",
                      "Review Type:",
                      choices = c(
                        "Scheduled Review" = "1",
                        "Interim Review"   = "2"
                      ),
                      selected = character(0)
                    )
                  ),
                  column(
                    width = 6,
                    conditionalPanel(
                      condition = "input.ccc_rev_type == '1'",
                      selectInput(
                        "ccc_session",
                        "Review Session:",
                        choices = c(
                          "Select session..." = "",
                          "Mid Intern"  = "1",
                          "End Intern"  = "2",
                          "Mid PGY2"   = "3",
                          "End PGY2"   = "4",
                          "Mid PGY3"   = "5",
                          "Graduation" = "6",
                          "Intern Intro" = "7"
                        ),
                        selected = "",
                        width = "100%"
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
                              h4("Program Follow-up Items", class = "text-primary mb-0")
                            ),
                            
                            # Action Items Checkbox - NOW ALWAYS VISIBLE (not dependent on concerns)
                            div(
                              class = "mb-3",
                              checkboxInput(
                                "has_action_items",
                                "Are there action items for program follow-up?",
                                value = FALSE
                              ),
                              tags$small(
                                class = "text-muted",
                                "Check this box if there are any administrative or follow-up items needed, regardless of whether there are CCC concerns."
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
                                  width = 6,
                                  dateInput(
                                    "ccc_fu_date",
                                    "Follow-up Date:",
                                    value = NULL,
                                    min = Sys.Date()
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
            
            # SCHEDULED REVIEW - STEP 2
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
                          div(class = "step-badge me-2", "2"),
                          div(
                            h5("Scheduled Review Details", class = "mb-0"),
                            tags$small("Review resident data, then complete the CCC assessment below",
                                       class = "text-muted")
                          )
                        )
                      ),
                      card_body(

                        # ── REFERENCE DATA TOGGLES ────────────────────────────────────
                        div(
                          class = "reference-data-bar mb-3 p-2 rounded",
                          style = "background:#f0f4ff; border:1px solid #c5d5f0;",
                          div(
                            class = "d-flex flex-wrap align-items-center gap-2",
                            span(class = "fw-semibold text-primary small me-1",
                                 icon("eye", class = "me-1"), "View:"),
                            # Evaluations toggle
                            tags$button(
                              class = "btn btn-outline-primary btn-sm",
                              type  = "button",
                              `data-bs-toggle`  = "collapse",
                              `data-bs-target`  = "#refEvalCollapse",
                              `aria-expanded`   = "false",
                              icon("clipboard-list", class = "me-1"), "Evaluations & Plus/Delta"
                            ),
                            # ILP toggle
                            tags$button(
                              class = "btn btn-outline-success btn-sm",
                              type  = "button",
                              `data-bs-toggle`  = "collapse",
                              `data-bs-target`  = "#refIlpCollapse",
                              `aria-expanded`   = "false",
                              icon("bullseye", class = "me-1"), "ILP Goals"
                            ),
                            # Board / ITE toggle
                            tags$button(
                              class = "btn btn-outline-warning btn-sm",
                              type  = "button",
                              `data-bs-toggle`  = "collapse",
                              `data-bs-target`  = "#refBoardsCollapse",
                              `aria-expanded`   = "false",
                              icon("graduation-cap", class = "me-1"), "Board / ITE"
                            ),
                            # Milestone Progression toggle
                            tags$button(
                              class = "btn btn-outline-secondary btn-sm",
                              type  = "button",
                              `data-bs-toggle`  = "collapse",
                              `data-bs-target`  = "#refMilesCollapse",
                              `aria-expanded`   = "false",
                              icon("chart-line", class = "me-1"), "Milestone History"
                            )
                          ),

                          # ── Evaluations & Plus/Delta collapse ──────────────────────
                          div(class = "collapse mt-2", id = "refEvalCollapse",
                            div(
                              class = "border rounded bg-white p-2",
                              tags$ul(
                                class = "nav nav-tabs mb-2",
                                id = "evalTabNav",
                                tags$li(class = "nav-item",
                                  tags$a(class = "nav-link active", `data-bs-toggle` = "tab",
                                         `href` = "#evalTablePane", "Evaluations")),
                                tags$li(class = "nav-item",
                                  tags$a(class = "nav-link", `data-bs-toggle` = "tab",
                                         `href` = "#pdTablePane", "Plus / Delta"))
                              ),
                              div(
                                class = "tab-content",
                                div(class = "tab-pane fade show active", id = "evalTablePane",
                                    mod_eval_table_ui("ccc_eval_table")),
                                div(class = "tab-pane fade", id = "pdTablePane",
                                    mod_plus_delta_table_ui("ccc_plus_delta", title = NULL))
                              )
                            )
                          ),

                          # ── ILP Goals collapse ─────────────────────────────────────
                          div(class = "collapse mt-2", id = "refIlpCollapse",
                            div(
                              class = "border rounded bg-white p-3",
                              div(class = "d-flex align-items-center mb-2",
                                  icon("bullseye", class = "text-success me-2"),
                                  tags$strong("Resident ILP Goals (most recent submission)")),
                              uiOutput("current_ilp_display")
                            )
                          ),

                          # ── Board / ITE collapse ───────────────────────────────────
                          div(class = "collapse mt-2", id = "refBoardsCollapse",
                            div(
                              class = "border rounded bg-white p-2",
                              mod_seval_boards_display_ui("ccc_boards")
                            )
                          ),

                          # ── Milestone History collapse ─────────────────────────────
                          div(class = "collapse mt-2", id = "refMilesCollapse",
                            div(
                              class = "border rounded bg-white p-2",
                              div(class = "d-flex align-items-center mb-1",
                                  icon("chart-line", class = "text-secondary me-2"),
                                  tags$strong("Milestone Progression Over Time")),
                              tags$small(class = "text-muted d-block mb-2",
                                         "Green = Program ratings · Blue = Self-ratings"),
                              plotOutput("milestone_progression_plot", height = "280px")
                            )
                          )
                        ), # end reference-data-bar

                        hr(class = "my-3"),

                        # ── PREVIOUS CCC ILP NOTE ──────────────────────────────────
                        div(
                          class = "prev-ccc-section mb-3 p-3",
                          style = "background:linear-gradient(135deg,#fff8e1 0%,#ffecb3 100%);border-radius:10px;border-left:5px solid #ffc107;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("history", class = "fa-lg text-warning me-2"),
                              h6("Previous CCC ILP Note", class = "text-warning mb-0 fw-bold")),
                          uiOutput("prev_ccc_ilp_note")
                        ),

                        # ── COACH ILP SUMMARY ──────────────────────────────────────
                        div(
                          class = "mb-3 p-3",
                          style = "background:linear-gradient(135deg,#e3f2fd 0%,#bbdefb 100%);border-radius:10px;border-left:5px solid #2196f3;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("user-tie", class = "fa-lg text-primary me-2"),
                              h6("Coach ILP Summary", class = "text-primary mb-0")),
                          uiOutput("coach_ilp_summary")
                        ),

                        # ── SECONDARY REVIEW SUMMARY ───────────────────────────────
                        div(
                          class = "mb-3 p-3",
                          style = "background:linear-gradient(135deg,#f3e5f5 0%,#e1bee7 100%);border-radius:10px;border-left:5px solid #9c27b0;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("user-check", class = "fa-lg me-2", style = "color:#9c27b0"),
                              h6("Secondary Review Summary", class = "mb-0", style = "color:#9c27b0;")),
                          uiOutput("secondary_review_display")
                        ),

                        hr(class = "my-3"),

                        # ── CCC COMMENTS ON ILP ────────────────────────────────────
                        div(
                          class = "mb-3 p-3",
                          style = "background:linear-gradient(135deg,#e8f5e8 0%,#c8e6c9 100%);border-radius:10px;border-left:5px solid #4caf50;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("comments", class = "fa-lg text-success me-2"),
                              h6("CCC Comments on ILP", class = "text-success mb-0")),
                          textAreaInput(
                            "ccc_ilp", label = NULL, rows = 4, width = "100%",
                            placeholder = "Enter CCC comments about the resident's ILP..."
                          )
                        ),

                        # ── CCC CONCERNS ───────────────────────────────────────────
                        div(
                          class = "mb-3 p-3",
                          style = "background:linear-gradient(135deg,#fff3e0 0%,#ffe0b2 100%);border-radius:10px;border-left:5px solid #ff9800;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("exclamation-triangle", class = "fa-lg text-warning me-2"),
                              h6("CCC Concerns", class = "text-warning mb-0")),

                          radioButtons(
                            "ccc_concern",
                            "Any concerns of the CCC?",
                            choices = c("No" = "0", "Yes" = "1"),
                            selected = character(0)
                          ),

                          conditionalPanel(
                            condition = "input.ccc_concern == '1'",

                            div(
                              class = "alert alert-warning mb-3 py-2",
                              icon("warning", class = "me-2"),
                              tags$strong("Concerns identified."),
                              " Please specify actions and competency areas."
                            ),

                            textAreaInput(
                              "ccc_concern_notes", label = "Notes on Concern:",
                              rows = 4, width = "100%",
                              placeholder = "Describe the concerns identified by the CCC..."
                            ),

                            fluidRow(
                              column(6,
                                div(
                                  class = "p-2 rounded border bg-white",
                                  tags$strong("CCC Actions", class = "text-primary d-block mb-2"),
                                  checkboxGroupInput(
                                    "ccc_action", label = NULL,
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
                                  conditionalPanel(
                                    condition = "input.ccc_action && input.ccc_action.length > 0",
                                    hr(class = "my-2"),
                                    tags$strong("Action Status", class = "text-info d-block mb-1"),
                                    checkboxGroupInput(
                                      "ccc_action_status", label = NULL,
                                      choices = c(
                                        "Initiation" = "1", "Ongoing" = "2",
                                        "Resolved" = "3",  "Recurring" = "4"
                                      ),
                                      selected = character(0)
                                    )
                                  )
                                )
                              ),
                              column(6,
                                div(
                                  class = "p-2 rounded border bg-white",
                                  tags$strong("Competency Areas", class = "text-success d-block mb-2"),
                                  checkboxGroupInput(
                                    "ccc_competency", label = NULL,
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

                            # Program follow-up items
                            div(
                              class = "mt-3 p-3",
                              style = "background:linear-gradient(135deg,#e3f2fd 0%,#bbdefb 100%);border-radius:10px;border-left:5px solid #2196f3;",
                              div(class = "d-flex align-items-center mb-2",
                                  icon("tasks", class = "text-primary me-2"),
                                  tags$strong("Program Follow-up Items", class = "text-primary")),
                              checkboxInput(
                                "has_action_items",
                                "Are there action items for program follow-up?",
                                value = FALSE
                              ),
                              tags$small(class = "text-muted", "Check if administrative or follow-up items are needed."),
                              conditionalPanel(
                                condition = "input.has_action_items",
                                fluidRow(
                                  column(6,
                                    textInput("ccc_fu_resp", "Person Responsible:",
                                              placeholder = "Enter name...")),
                                  column(6,
                                    dateInput("ccc_fu_date", "Follow-up Date:",
                                              value = NULL, min = Sys.Date())),
                                  column(12,
                                    textAreaInput("ccc_issues_follow_up",
                                                  "Issues for the Program:", rows = 3,
                                                  placeholder = "Specific issues to follow up on..."))
                                )
                              )
                            )
                          ) # end ccc_concern == 1 panel
                        ),

                        # ── ADDITIONAL COMMENTS ────────────────────────────────────
                        div(
                          class = "mb-3 p-3",
                          style = "background:linear-gradient(135deg,#f9f9f9 0%,#f0f0f0 100%);border-radius:10px;border-left:5px solid #607d8b;",
                          div(class = "d-flex align-items-center mb-2",
                              icon("sticky-note", class = "fa-lg me-2", style = "color:#607d8b;"),
                              h6("Additional Comments", class = "mb-0", style = "color:#607d8b;")),
                          textAreaInput(
                            "ccc_comments", label = NULL, rows = 3,
                            placeholder = "Any additional comments for this review..."
                          )
                        ),

                        # ── NEXT BUTTON ────────────────────────────────────────────
                        div(
                          class = "text-center mt-3 pt-3 border-top",
                          actionButton(
                            "proceed_to_milestones",
                            "Next: Milestone Review →",
                            class = "btn-primary btn-lg px-5 py-3",
                            icon = icon("arrow-right"),
                            style = "border-radius:25px;font-weight:600;box-shadow:0 4px 8px rgba(0,0,0,0.1);"
                          )
                        )

                      ) # end card_body
                    ) # end card
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
          
          # Milestone Plots Section
          fluidRow(
            column(
              width = 12,
              card(
                card_header("Step 3: Milestone Assessment",
                            `data-card-type` = "milestone-plots"
                ),
                card_body(
                  # Milestone Plots
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
                      ),
                      
                      # Self-Assessment Description Table (Collapsible)
                      div(
                        class = "mt-3",
                        tags$button(
                          class = "btn btn-outline-primary btn-sm",
                          type = "button",
                          `data-bs-toggle` = "collapse",
                          `data-bs-target` = "#selfDescriptionsCollapse",
                          `aria-expanded` = "false",
                          `aria-controls` = "selfDescriptionsCollapse",
                          icon("chevron-down", class = "me-2"),
                          "Show Self-Assessment Descriptions"
                        ),
                        div(
                          class = "collapse mt-2",
                          id = "selfDescriptionsCollapse",
                          div(
                            class = "card card-body milestone-description-card",
                            style = "background-color: #f8f9ff; border: 1px solid #e3f2fd;",
                            h6("Self-Assessment Milestone Descriptions", class = "text-primary mb-3"),
                            div(
                              style = "max-height: 300px; overflow-y: auto;",
                              DT::dataTableOutput("self_milestone_descriptions")
                            )
                          )
                        )
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
                      ),
                      
                      # Program Assessment Description Table (Collapsible)
                      div(
                        class = "mt-3",
                        tags$button(
                          class = "btn btn-outline-success btn-sm",
                          type = "button",
                          `data-bs-toggle` = "collapse",
                          `data-bs-target` = "#programDescriptionsCollapse",
                          `aria-expanded` = "false",
                          `aria-controls` = "programDescriptionsCollapse",
                          icon("chevron-down", class = "me-2"),
                          "Show Program Assessment Descriptions"
                        ),
                        div(
                          class = "collapse mt-2",
                          id = "programDescriptionsCollapse",
                          div(
                            class = "card card-body milestone-description-card",
                            style = "background-color: #f0fff4; border: 1px solid #d4edda;",
                            h6("Program Assessment Milestone Descriptions", class = "text-success mb-3"),
                            div(
                              style = "max-height: 300px; overflow-y: auto;",
                              DT::dataTableOutput("program_milestone_descriptions")
                            )
                          )
                        )
                      )
                    )
                  ),
                  
                  br(),

                  # Live Milestone Preview (updates when sliders change)
                  uiOutput("milestone_changes_banner"),
                  div(
                    id = "live-milestone-preview",
                    class = "collapse",
                    div(
                      class = "mb-4 p-3",
                      style = "background: #fffde7; border: 2px solid #ffc107; border-radius: 10px;",
                      div(
                        class = "d-flex align-items-center mb-2",
                        icon("sync-alt", class = "fa-lg text-warning me-2"),
                        h6("Live Milestone Preview", class = "text-warning mb-0 fw-bold"),
                        tags$small(" — updates as you adjust sliders", class = "text-muted ms-2")
                      ),
                      plotOutput("live_milestone_plot", height = "320px")
                    )
                  ),

                  br(),

                  # Milestone Assessment Interface
                  div(
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
                  ),
                  
                  # Navigation buttons
                  div(
                    class = "text-center mt-4 pt-4 border-top",
                    actionButton(
                      "back_to_step2",
                      "← Back to Review Details",
                      class = "btn-secondary me-3"
                    ),
                    uiOutput("final_submit_button", inline = TRUE)
                  )
                )
              )
            )
          )
        )
      ) # End of ccc-review-pages div
    ) # End of ccc-pages div
  ) # End of main-content div
) # End of page_fluid