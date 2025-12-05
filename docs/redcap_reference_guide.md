# REDCap Reference Guide for R Development

## Table of Contents
1. [Data Dictionary Column Names](#data-dictionary-column-names)
2. [Repeating Instruments](#repeating-instruments)
3. [Field Types and Values](#field-types-and-values)
4. [Common Patterns](#common-patterns)
5. [API Endpoints](#api-endpoints)
6. [Best Practices](#best-practices)

---

## Data Dictionary Column Names

REDCap data dictionaries have **different column names** depending on how they're retrieved.

### API Export (metadata endpoint)
When fetching via REDCap API using metadata export:

| Standard Name | API Column Name |
|--------------|-----------------|
| Field Name | `field_name` |
| Form Name | `form_name` |
| Section Header | `section_header` |
| Field Type | `field_type` |
| Field Label | `field_label` |
| Choices/Calculations | `select_choices_or_calculations` |
| Field Note | `field_note` |
| Text Validation | `text_validation_type_or_show_slider_number` |
| Min Value | `text_validation_min` |
| Max Value | `text_validation_max` |
| Identifier | `identifier` |
| Branching Logic | `branching_logic` |
| Required Field | `required_field` |
| Custom Alignment | `custom_alignment` |
| Question Number | `question_number` |
| Matrix Group | `matrix_group_name` |
| Matrix Ranking | `matrix_ranking` |
| Field Annotation | `field_annotation` |

### CSV Export
When downloading the data dictionary as CSV from REDCap UI:

| Standard Name | CSV Column Name |
|--------------|-----------------|
| Field Name | `Variable...Field.Name` or `Variable / Field Name` |
| Form Name | `Form.Name` or `Form Name` |
| Field Label | `Field.Label` or `Field Label` |
| Choices/Calculations | `Choices..Calculations..OR.Slider.Labels` or `Choices, Calculations, OR Slider Labels` |

**Note:** CSV exports use dots (`.`) or spaces where API uses underscores (`_`)

### Standardization Function

Use this function to handle both formats:

```r
standardize_dict_names <- function(dict) {
  if (is.null(dict)) return(NULL)
  
  name_mapping <- list(
    field_name = c("field_name", "Variable / Field Name", "Variable...Field.Name"),
    form_name = c("form_name", "Form Name", "Form.Name"),
    field_label = c("field_label", "Field Label", "Field.Label"),
    field_type = c("field_type", "Field Type", "Field.Type"),
    choices = c("select_choices_or_calculations", 
                "Choices, Calculations, OR Slider Labels", 
                "Choices..Calculations..OR.Slider.Labels"),
    branching_logic = c("branching_logic", "Branching Logic", "Branching.Logic")
  )
  
  for (standard_name in names(name_mapping)) {
    possible_names <- name_mapping[[standard_name]]
    for (poss_name in possible_names) {
      if (poss_name %in% names(dict)) {
        names(dict)[names(dict) == poss_name] <- standard_name
        break
      }
    }
  }
  
  return(dict)
}
```

---

## Repeating Instruments

### Key Fields in Data Export

When exporting data with repeating instruments, REDCap adds these fields:

- `redcap_repeat_instrument` - Name of the repeating form (empty for non-repeating)
- `redcap_repeat_instance` - Instance number (1, 2, 3, etc.)

### Two Patterns for Repeating Data

#### Pattern 1: OVERWRITE (Fixed Instances)
Used for forms tied to specific periods (e.g., evaluations by semester)

**Characteristics:**
- Instance number = period or time point
- Each instance represents a specific evaluation period
- Updates overwrite existing data for that instance
- Examples: coaching reviews, CCC reviews, self-evaluations

**Code Pattern:**
```r
# Get instance number based on period/level
instance <- get_redcap_instance(
  level = resident_level,
  period = evaluation_period,
  review_type = "scheduled"
)

# Submit (overwrites if exists)
submit_overwrite_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = data,
  instrument_name = "coach_rev",
  period = period,
  level = level
)
```

#### Pattern 2: ADDITIVE (Continuous Instances)
Used for forms that continuously add new entries

**Characteristics:**
- Each submission creates a new instance
- Instance numbers increment automatically
- Never overwrites existing data
- Examples: scholarship, faculty evaluations, presentations

**Code Pattern:**
```r
# Get next available instance
next_instance <- get_next_additive_instance(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  instrument_name = "scholarship"
)

# Submit new instance
submit_additive_data(
  redcap_url = url,
  redcap_token = token,
  record_id = record_id,
  form_data = data,
  instrument_name = "scholarship"
)
```

### Getting Next Instance Number

```r
get_next_additive_instance <- function(redcap_url, redcap_token, record_id, instrument_name) {
  # Fetch existing instances
  result <- httr::POST(
    url = redcap_url,
    body = list(
      token = redcap_token,
      content = "record",
      action = "export",
      format = "json",
      type = "flat",
      records = record_id,
      forms = instrument_name
    ),
    encode = "form"
  )
  
  data <- jsonlite::fromJSON(httr::content(result, "text", encoding = "UTF-8"))
  
  if (nrow(data) == 0) return(1)
  
  # Filter to this instrument only
  data <- data[data$redcap_repeat_instrument == instrument_name, ]
  
  if (nrow(data) == 0) return(1)
  
  # Get max instance and add 1
  max_instance <- max(as.numeric(data$redcap_repeat_instance), na.rm = TRUE)
  return(max_instance + 1)
}
```

---

## Field Types and Values

### Dropdown/Radio Fields

**Format in data dictionary:**
```
"1, Label One | 2, Label Two | 3, Label Three"
```

**Parsing function:**
```r
parse_choices <- function(choices_raw) {
  if (is.na(choices_raw) || choices_raw == "") return(NULL)
  
  choices <- strsplit(choices_raw, "\\|")[[1]]
  lookup <- list()
  
  for (choice in choices) {
    parts <- trimws(strsplit(choice, ",")[[1]])
    if (length(parts) >= 2) {
      value <- parts[1]
      label <- paste(parts[-1], collapse = ",")
      lookup[[value]] <- label
    }
  }
  
  return(lookup)
}
```

### Yes/No Fields

**Standard encoding:**
- `"1"` = Yes
- `"0"` = No

**Usage:**
```r
# In data
schol_ps = "1"  # Patient safety = Yes

# In UI
radioButtons("field", "Question?", choices = c("Yes" = "1", "No" = "0"))
```

### Checkbox Fields

**Encoding:**
- Checked = `"1"`
- Unchecked = `"0"` or `NA`

**In REDCap data:**
```r
# Each checkbox option becomes a separate column
field___1  # Option 1
field___2  # Option 2
field___3  # Option 3
```

### Date Fields

**Format:** `YYYY-MM-DD` (ISO 8601)

**Usage:**
```r
# Store in REDCap
date_field = format(Sys.Date(), "%Y-%m-%d")

# Date ranges (for data dictionary)
date:field:start
date:field:end
date:field:is_datetime  # 0 or 1
```

### Text/Notes Fields

**Types:**
- `text` - Single line
- `notes` - Multi-line (textarea)

**Validation types:**
- `email`
- `number`
- `integer`
- `phone`
- `date_ymd`
- `datetime_ymd`

---

## Common Patterns

### 1. Conditional Logic Based on Type

Many forms have different fields based on a "type" selector:

```r
# Example: Scholarship entry
if (type == "1") {
  # Show QI-specific fields
} else if (type == "3") {
  # Show research-specific fields
}
```

**Implementation with conditionalPanel:**
```r
conditionalPanel(
  condition = paste0("input['", ns("type_field"), "'] == '1'"),
  # Fields for type 1
)
```

### 2. Multi-Step Workflows

For complex data entry:

```r
state <- reactiveValues(
  step = "select_type",
  data = list()
)

# Step progression
observeEvent(input$continue, {
  if (state$step == "step_1") {
    # Submit step 1 data
    # Move to step 2
    state$step <- "step_2"
  }
})
```

### 3. Dynamic Field Display

Pull field labels from data dictionary:

```r
get_label <- function(field_name, data_dict) {
  dict_row <- data_dict[data_dict$field_name == field_name, ]
  if (nrow(dict_row) == 0) return(field_name)
  return(dict_row$field_label[1])
}
```

### 4. Vectorized Choice Labels

Convert coded values to labels for display:

```r
get_choice_label <- function(field_name, values, data_dict) {
  dict_row <- data_dict[data_dict$field_name == field_name, ]
  if (nrow(dict_row) == 0) return(values)
  
  choices_raw <- dict_row$choices[1]
  if (is.na(choices_raw)) return(values)
  
  # Parse choices
  lookup <- parse_choices(choices_raw)
  
  # Vectorized lookup
  sapply(values, function(val) {
    if (is.na(val)) return(NA_character_)
    label <- lookup[[as.character(val)]]
    if (is.null(label)) return(as.character(val))
    return(label)
  })
}
```

---

## API Endpoints

### Export Records
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "record",
    action = "export",
    format = "json",
    type = "flat",
    records = record_id,        # Optional: specific records
    forms = "form_name",        # Optional: specific forms
    fields = "field1,field2",   # Optional: specific fields
    rawOrLabel = "raw",         # "raw" for codes, "label" for labels
    rawOrLabelHeaders = "raw",
    exportCheckboxLabel = "false",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Import Records
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "record",
    action = "import",
    format = "json",
    type = "flat",
    overwriteBehavior = "normal",  # or "overwrite"
    data = jsonlite::toJSON(data_df, auto_unbox = TRUE),
    returnContent = "count",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Export Metadata (Data Dictionary)
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "metadata",
    format = "json",
    returnFormat = "json"
  ),
  encode = "form"
)
```

### Export Project Information
```r
httr::POST(
  url = redcap_url,
  body = list(
    token = token,
    content = "project",
    format = "json",
    returnFormat = "json"
  ),
  encode = "form"
)
```

---

## Best Practices

### 1. Always Use Proper Field Names
```r
# DON'T hardcode labels
data$`Field Label` <- value

# DO use field names
data$field_name <- value
```

### 2. Handle NA Values
```r
# REDCap uses empty strings, not NA
scholarship_data <- list(
  field1 = ifelse(is.null(input$field1) || input$field1 == "", NA, input$field1)
)
```

### 3. Validate Before Submission
```r
# Check required fields
if (is.na(data$required_field) || data$required_field == "") {
  # Show error
  return()
}
```

### 4. Use Transactions for Related Data
```r
# Submit base data first
result1 <- submit_data(base_data)

if (result1$success) {
  # Then submit related data
  result2 <- submit_data(related_data)
}
```

### 5. Provide User Feedback
```r
# After submission
if (result$success) {
  # Refresh display immediately
  refresh_callback()
  
  # Show success message
  showNotification("Data saved successfully!", type = "message")
}
```

### 6. Error Handling
```r
tryCatch({
  result <- httr::POST(url, body = body, encode = "form")
  
  if (httr::status_code(result) == 200) {
    # Success
  } else {
    # Handle error
    error_text <- httr::content(result, "text", encoding = "UTF-8")
    warning("REDCap error: ", error_text)
  }
}, error = function(e) {
  warning("API call failed: ", e$message)
})
```

### 7. Data Dictionary Caching
```r
# Cache data dictionary at app startup
app_data <- list(
  data_dict = fetch_metadata(token, url),
  # ... other data
)

# Reuse throughout the app
get_field_label(field_name, app_data$data_dict)
```

### 8. Separate Display and Submission Logic
```r
# Display module (gmed package)
display_scholarship <- function(data, data_dict) {
  # Transform data for display
}

# Submission module (app-specific)
scholarship_entry_server <- function(id, ...) {
  # Handle form submission
}
```

### 9. Use Namespacing in Modules
```r
# In module UI
ns <- NS(id)
textInput(ns("field_name"), "Label")

# In module server
moduleServer(id, function(input, output, session) {
  # input$field_name is automatically namespaced
})
```

### 10. Test with Empty Data
```r
# Always handle empty data frames
if (is.null(data) || nrow(data) == 0) {
  return(empty_result)
}
```

---

## Troubleshooting Common Issues

### Issue: "attempt to use zero-length variable name"
**Cause:** Using `sprintf()` with `ns()` in conditionalPanel
**Fix:** Use `paste0()` instead
```r
# BAD
condition = sprintf("input['%s'] == '1'", ns("field"))

# GOOD
condition = paste0("input['", ns("field"), "'] == '1'")
```

### Issue: Numeric values instead of labels in display
**Cause:** Not applying choice labels from data dictionary
**Fix:** Use vectorized `get_choice_label()` function

### Issue: Modal buttons not working
**Cause:** Button IDs not properly namespaced
**Fix:** Namespace modal button IDs
```r
actionButton(ns("modal_btn"), "Click")
```

### Issue: Data not refreshing after submission
**Cause:** Not calling refresh callback
**Fix:** Always call refresh after successful submission
```r
if (result$success) {
  if (!is.null(refresh_callback)) refresh_callback()
}
```

### Issue: Repeating instances overwriting each other
**Cause:** Using fixed instance numbers for additive data
**Fix:** Use `get_next_additive_instance()` for new entries

---

## Resources

- **REDCap API Documentation:** Check your institution's REDCap API page
- **gmed Package:** Internal package with REDCap helper functions
- **REDCapR Package:** Alternative R package for REDCap integration

---

**Document Version:** 1.0  
**Last Updated:** October 2025  
**Maintained by:** GMED Development Team
