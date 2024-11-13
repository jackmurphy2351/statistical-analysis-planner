# =================================================================
# Your Epi Companion
# Copyright © 2024, John D. Murphy, PhD MPH
# Licensed under the GNU General Public License v3.0
# =================================================================


# install.packages(c("shiny", "shinydashboard", "markdown", "DT", "rmarkdown", "officer"))

# Load required libraries 
library(shiny)
library(shinydashboard)
library(markdown)
library(DT)
library(rmarkdown)
library(officer)  # For Word document creation

# Get effect measure interpretation
get_effect_measure_interpretation <- function(outcome_type, study_design, predictor_type, effect_measure) {
  # First check if any inputs are NULL or empty
  if(is.null(outcome_type) || is.null(study_design) || 
     is.null(predictor_type) || is.null(effect_measure)) {
    return("Please complete all selections to see effect measure interpretation.")
  }
  
  base_interpretation <- switch(outcome_type,
                                "continuous" = {
                                  if(predictor_type == "cont_pred") {
                                    "The regression coefficient (β₁) represents the average change in the outcome for each one-unit increase in the exposure, holding all other variables constant."
                                  } else if(predictor_type == "cat_pred") {
                                    "The effect measure represents the difference in means between exposure groups."
                                  } else {
                                    "Effect interpretation will be shown once predictor type is selected."
                                  }
                                },
                                "binary" = {
                                  if(effect_measure == "difference") {
                                    switch(study_design,
                                           "cohort" = "The risk difference (RD) represents the absolute difference in risk between exposed and unexposed groups.",
                                           "case_control" = "Risk differences cannot be directly estimated from case-control studies without external information about the population prevalence of exposure.",
                                           "The risk difference represents the absolute difference in probability of the outcome between exposed and unexposed groups."
                                    )
                                  } else {
                                    switch(study_design,
                                           "cohort" = "The risk ratio (RR) represents the relative risk comparing exposed to unexposed groups.",
                                           "case_control" = "The odds ratio (OR) represents the ratio of odds of exposure in cases compared to controls.",
                                           "nested_cc" = "The rate ratio represents the ratio of incidence rates comparing exposed to unexposed person-time.",
                                           "The odds ratio (OR) represents the ratio of odds of the outcome comparing exposed to unexposed groups."
                                    )
                                  }
                                },
                                "survival" = "The hazard ratio (HR) represents the ratio of instantaneous risk of the outcome comparing exposed to unexposed groups.",
                                "count" = "The rate ratio represents the ratio of incidence rates comparing exposed to unexposed groups.",
                                "ordinal" = "The proportional odds ratio represents the odds of having a higher versus lower outcome level.",
                                # Default case if none match
                                "Please select an outcome type to see effect measure interpretation."
  )
  
  return(base_interpretation)
}

# UI definition
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Your Epi Companion"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("How-to Guide", tabName = "howto", icon = icon("question-circle")),
      menuItem("Analysis Guide", tabName = "guide", icon = icon("dashboard")),
      menuItem("Study Design", tabName = "design", icon = icon("clipboard")),
      menuItem("Glossary", tabName = "glossary", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
    /* Original Layout & Structure CSS */
    .tooltip-inner {
      max-width: 300px;
      text-align: left;
    }
    .disabled-radio {
      color: #999;
      cursor: not-allowed;
    }
    .disabled-radio input {
      cursor: not-allowed;
    }
    .measure-note {
      color: #666;
      font-style: italic;
      font-size: 0.9em;
      margin-left: 20px;
      margin-top: 5px;
    }
    .how-to-card {
      background: white;
      border-radius: 8px;
      padding: 20px;
      margin-bottom: 20px;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    .how-to-title {
      font-size: 24px;
      margin-bottom: 20px;
      text-align: center;
    }
    .step-title {
      font-size: 18px;
      font-weight: bold;
      margin-bottom: 10px;
    }
    .step-content {
      color: #34495e;
      margin-left: 25px;
    }
    .pro-tip {
      background: #f8f9fa;
      padding: 10px;
      margin-top: 10px;
    }
    .emoji-bullet {
      margin-right: 8px;
    }

    /* Terracotta Radio Button Styling */
    input[type='radio'] {
      appearance: none;
      -webkit-appearance: none;
      -moz-appearance: none;
      width: 16px;
      height: 16px;
      border: 2px solid #ccc;
      border-radius: 50%;
      outline: none;
      margin-right: 5px;
      position: relative;
      cursor: pointer;
      background-color: #fff;
    }

    input[type='radio']:checked {
      border-color: #e07a5f;
      background-color: #fff;
    }

    input[type='radio']:checked::before {
      content: '';
      position: absolute;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      width: 8px;
      height: 8px;
      border-radius: 50%;
      background-color: #e07a5f;
    }

    input[type='radio']:hover {
      border-color: #d36c51;
    }

    /* Green Theme CSS */
    .skin-green .main-header .logo {
      background-color: #2d5a27;
    }
    .skin-green .main-header .logo:hover {
      background-color: #366c2f;
    }
    .skin-green .main-header .navbar {
      background-color: #2d5a27;
    }
    .skin-green .left-side, .skin-green .main-sidebar, .skin-green .wrapper {
      background-color: #1b391a;
    }
    .skin-green .sidebar-menu > li.active > a,
    .skin-green .sidebar-menu > li:hover > a {
      background-color: #366c2f;
      border-left-color: #4a9142;
    }
    .skin-green .sidebar a {
      color: #fff;
    }
    .skin-green .sidebar-menu > li > .treeview-menu {
      background-color: #1b391a;
    }
    .skin-green .sidebar-menu > li:hover > a,
    .skin-green .sidebar-menu > li.active > a {
      color: #fff;
      background: #366c2f;
      border-left-color: #4a9142;
    }
    .skin-green .sidebar-menu > li > .treeview-menu {
      margin: 0 1px;
      background: #1b391a;
    }

    /* Box Styling - Both Primary and Info */
    .box.box-primary, .box.box-info {
      border-top-color: #2d5a27;
    }
    .box.box-primary > .box-header,
    .box.box-info > .box-header {
      background-color: #2d5a27;
      color: #ffffff;
    }
    .box.box-solid.box-primary,
    .box.box-solid.box-info {
      border: 1px solid #2d5a27;
    }
    .box.box-solid.box-primary > .box-header,
    .box.box-solid.box-info > .box-header {
      background: #2d5a27;
      background-color: #2d5a27;
      color: #ffffff;
    }

    /* Button and Link Styling */
    .btn-primary {
      background-color: #2d5a27;
      border-color: #264d21;
    }
    .btn-primary:hover, .btn-primary:focus, .btn-primary:active {
      background-color: #366c2f !important;
      border-color: #2d5a27 !important;
    }
    .pro-tip {
      border-left: 4px solid #2d5a27;
    }
    .text-primary {
      color: #2d5a27 !important;
    }
    .progress-bar {
      background-color: #2d5a27;
    }
    ::selection {
      background: #4a9142;
      color: white;
    }
    a {
      color: #366c2f;
    }
    a:hover, a:focus {
      color: #4a9142;
    }
    .how-to-title {
      color: #2d5a27;
    }
    .step-title {
      color: #2d5a27;
    }
    
    /* Additional Elements and Overrides */
    .nav-tabs-custom > .nav-tabs > li.active {
      border-top-color: #2d5a27;
    }
    .bg-primary, .bg-info {
      background-color: #2d5a27 !important;
    }
    .panel-primary > .panel-heading,
    .panel-info > .panel-heading {
      background-color: #2d5a27;
      border-color: #2d5a27;
      color: #ffffff;
    }
  "))
    ),
    
    tabItems(
      # How-to Guide Tab
      tabItem(tabName = "howto",
              fluidRow(
                box(
                  width = 12,
                  div(class = "how-to-card",
                      h2(class = "how-to-title", 
                         "Welcome to Your Epi Analysis Companion! 📈",
                         style = "color: #2c3e50; font-weight: bold;"
                      ),
                      p("Think of this app as your friendly neighborhood epidemiologist - here to guide you through the sometimes-puzzling world of study analysis!", 
                        style = "text-align: center; font-size: 16px; color: #666;"
                      ),
                      
                      # Getting Started Section
                      div(class = "how-to-card",
                          h3(class = "step-title", "📚 Getting Started"),
                          div(class = "step-content",
                              tags$ol(
                                tags$li(
                                  strong("Select your study design"), 
                                  p("Choose from the dropdown menu. Not sure about your design? Click the 'Study Design' tab for a quick refresher.")
                                ),
                                tags$li(
                                  strong("Enter your sample size"), 
                                  p("Don't worry if it's not exact - we'll adjust recommendations based on whether you're working with a small or large sample.")
                                ),
                                tags$li(
                                  strong("Specify your variables"), 
                                  p("Tell us about your outcome and predictor variables. Some choices might be automatic based on your study design.")
                                )
                              )
                          )
                      ),
                      
                      # Fine-tuning Section
                      div(class = "how-to-card",
                          h3(class = "step-title", "🔧 Fine-tuning Your Analysis"),
                          div(class = "step-content",
                              p("Consider these optional but important elements:"),
                              tags$ul(
                                tags$li("Additional considerations (repeated measures, clustered data)"),
                                tags$li("Matching specifications for case-control studies"),
                                tags$li("Choice between difference and ratio measures")
                              )
                          )
                      ),
                      
                      # Results Section
                      div(class = "how-to-card",
                          h3(class = "step-title", "📊 Getting Your Results"),
                          div(class = "step-content",
                              p("Your analysis plan will include:"),
                              tags$ul(
                                tags$li("Recommended primary and alternative analyses"),
                                tags$li("Important assumptions to check"),
                                tags$li("Effect measure interpretation"),
                                tags$li("Sample R code to get you started")
                              ),
                              p("Download your plan in HTML, Word, or Markdown format - perfect for sharing with colleagues!")
                          )
                      ),
                      
                      # Pro Tips Section
                      div(class = "how-to-card",
                          h3(class = "step-title", "💡 Pro Tips"),
                          div(class = "step-content",
                              tags$ul(
                                tags$li(span(class = "emoji-bullet", "🔍"), "Use the Glossary tab when you need a quick refresh on epidemiological terms"),
                                tags$li(span(class = "emoji-bullet", "📊"), "Check the Study Design tab for detailed comparisons of different study types"),
                                tags$li(span(class = "emoji-bullet", "ℹ️"), "Hover over information icons throughout the app for additional context"),
                                tags$li(span(class = "emoji-bullet", "🤔"), "Not sure about something? The app includes explanations and justifications for all recommendations")
                              )
                          )
                      )
                  ),
                  
                  # Ready to Start
                  div(class = "how-to-card",
                      p("Ready to start? Click any option in the sidebar to begin, or just work through the form from top to bottom!", 
                        style = "text-align: center; font-size: 16px; color: #666; font-style: italic;"
                      )
                  )
                )
              )
      ),
      
      # Analysis Guide Tab
      tabItem(tabName = "guide",
              fluidRow(
                box(
                  width = 12,
                  title = "Statistical Analysis Decision Tree",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Study design question
                  radioButtons("studyDesign", "What is your study design?",
                               choices = c(
                                 "Cross-sectional" = "cross_sectional",
                                 "Cohort study" = "cohort",
                                 "Case-control" = "case_control",
                                 "Nested case-control" = "nested_cc",
                                 "Case-cohort" = "case_cohort",
                                 "Randomized controlled trial" = "rct",
                                 "Ecological study" = "ecological"
                               )
                  ),
                  
                  conditionalPanel(
                    condition = "input.studyDesign == 'case_control'",
                    radioButtons("matchingDesign", 
                                 "Is this a matched case-control study?",
                                 choices = c(
                                   "Unmatched" = "unmatched",
                                   "Matched on confounders" = "matched_fixed",
                                   "Matched on risk-set (time)" = "matched_time"
                                 ),
                                 selected = "unmatched"
                    ),
                    conditionalPanel(
                      condition = "input.matchingDesign != 'unmatched'",
                      numericInput("matchRatio",
                                   "What is the matching ratio (controls per case)?",
                                   value = 1,
                                   min = 1),
                      conditionalPanel(
                        condition = "input.matchRatio > 4",
                        div(style = "color: #856404; background-color: #fff3cd; padding: 8px; border: 1px solid #ffeeba; border-radius: 4px;",
                            HTML("<small><i class='fas fa-info-circle'></i> Statistical efficiency gains diminish rapidly beyond 4 controls per case due to marginal information added.</small>"))
                      )
                    )
                  ),
                  
                  # Effect measure type with conditional display
                  uiOutput("effectMeasureUI"),
                  
                  # Sample size input
                  numericInput("sampleSize", 
                               "What is your approximate sample size?",
                               value = 100,
                               min = 1
                  ),
                  
                  # Outcome variable type with conditional display
                  conditionalPanel(
                    condition = "input.studyDesign != 'case_control' && input.studyDesign != 'nested_cc'",
                    radioButtons("outcomeType", "What is your outcome variable type?",
                                 choices = c(
                                   "Continuous (e.g., blood pressure, BMI)" = "continuous",
                                   "Binary (e.g., disease yes/no)" = "binary",
                                   "Time-to-event (e.g., survival data)" = "survival",
                                   "Count data (e.g., number of cases)" = "count",
                                   "Ordinal (e.g., disease severity)" = "ordinal"
                                 ))
                  ),
                  conditionalPanel(
                    condition = "input.studyDesign == 'case_control' || input.studyDesign == 'nested_cc'",
                    radioButtons("outcomeType", "What is your outcome variable type?",
                                 choices = c(
                                   "Binary (case-control status)" = "binary"
                                 ),
                                 selected = "binary")
                  ),
                  
                  # Predictor type question
                  conditionalPanel(
                    condition = "input.outcomeType",
                    radioButtons("predictorType", "What is your main predictor/exposure variable?",
                                 choices = c(
                                   "Continuous (e.g., age, dose)" = "cont_pred",
                                   "Categorical (e.g., treatment groups)" = "cat_pred",
                                   "Multiple predictors" = "multiple_pred"
                                 )
                    )
                  ),
                  
                  # Additional questions for categorical predictors
                  conditionalPanel(
                    condition = "input.predictorType == 'cat_pred'",
                    radioButtons("groupNum", "How many groups are you comparing?",
                                 choices = c(
                                   "Two groups" = "two",
                                   "More than two groups" = "multiple"
                                 )
                    )
                  ),
                  
                  # Additional considerations
                  checkboxGroupInput("additionalConsiderations", 
                                     "Additional considerations (select all that apply):",
                                     choices = c(
                                       "Repeated measures" = "repeated",
                                       "Clustered/hierarchical data" = "clustered",
                                       "Missing data" = "missing",
                                       "Multiple testing" = "multiple_testing"
                                     )
                  ),
                  
                  # Results box
                  uiOutput("analysisResult"),
                  
                  # Optional user information
                  textInput("userName", 
                            "Name (optional):",
                            placeholder = "Enter your name"
                  ),
                  
                  textInput("userInstitution", 
                            "Institution (optional):",
                            placeholder = "Enter your institution"
                  ),
                  
                  # Download options
                  selectInput("downloadFormat", "Download Format:",
                              choices = c(
                                "HTML" = "html",
                                "Word" = "docx",
                                "Markdown" = "md"
                              ),
                              selected = "html"
                  ),
                  
                  # Download button
                  downloadButton("downloadReport", "Download Analysis Plan")
                )
              )
      ),
      
      # Study Design Tab
      tabItem(tabName = "design",
              fluidRow(
                box(
                  width = 12,
                  title = "Study Design Guidance",
                  status = "info",
                  solidHeader = TRUE,
                  DT::dataTableOutput("designTable")
                )
              )
      ),
      
      tabItem(tabName = "glossary",
              fluidRow(
                box(
                  width = 12,
                  title = "Statistical and Epidemiological Terms",
                  status = "info",
                  solidHeader = TRUE,
                  DT::dataTableOutput("glossaryTable")
                )
              )
      ),
      
      # About Tab
      tabItem(tabName = "about",
              box(
                width = 12,
                title = "About This Tool",
                "This enhanced epidemiological analysis guide helps researchers select appropriate statistical analyses based on study design, sample size, and data characteristics. The recommendations incorporate standard epidemiological practices and important methodological considerations.",
                hr(),
                "Developed using R Shiny by epidemiologists for epidemiologists."
              )
      )
    )
  )
)

# Server logic
server <- function(input, output, session) {  # Added session parameter here
  
  output$glossaryTable <- DT::renderDataTable({
    data.frame(
      Term = c(
        "Odds Ratio (OR)",
        "Risk Ratio (RR)",
        "Hazard Ratio (HR)",
        "Rate Ratio",
        "Risk Difference",
        "Conditional Logistic Regression",
        "Matching",
        "Confounding",
        "Effect Modification",
        "Rare Disease Assumption",
        "Time-at-risk",
        "Person-time",
        "Incidence Rate",
        "Prevalence",
        "Effect Measure",
        "Bias"
      ),
      Definition = c(
        "The ratio of odds of exposure in cases compared to controls. Approximates the risk ratio when the outcome is rare (<10% prevalence).",
        "The ratio of risk (probability) of outcome in exposed compared to unexposed groups. Can only be directly estimated in cohort studies.",
        "The ratio of instantaneous risk (hazard) of outcome comparing exposed to unexposed groups in survival analysis.",
        "The ratio of incident cases per person-time between exposed and unexposed groups.",
        "The absolute difference in risk (probability) between exposed and unexposed groups. Useful for public health impact assessment.",
        "A statistical method for analyzing matched case-control data that accounts for the matching in the study design.",
        "A design strategy where cases and controls are paired on potential confounding factors to control for their effects.",
        "A variable that influences both the exposure and outcome, potentially creating a spurious association.",
        "A variable that modifies the effect of exposure on the outcome, creating different effects in different subgroups.",
        "The assumption that disease occurrence is rare (<10%), allowing the odds ratio to approximate the risk ratio.",
        "The duration during which a subject is at risk of experiencing the outcome.",
        "The sum of time-at-risk across all subjects in a study.",
        "The number of new cases per unit of person-time.",
        "The proportion of existing cases in a population at a specific time point.",
        "A measure quantifying the association between exposure and outcome (e.g., odds ratio, risk ratio).",
        "Systematic error in the design, conduct, or analysis of a study that results in incorrect estimates of association."
      ),
      Example = c(
        "OR = 2.5 means the odds of exposure were 2.5 times higher in cases than controls",
        "RR = 1.5 means 50% higher risk in the exposed group",
        "HR = 2.0 means twice the instantaneous risk at any time point",
        "Rate ratio = 1.3 means 30% higher incidence rate in exposed",
        "Risk difference = 0.15 means 15 percentage points higher risk in exposed",
        "Used when cases and controls are matched on age and sex",
        "Matching each lung cancer case to a control of same age, sex, and smoking status",
        "Age might confound the relationship between gray hair and heart disease",
        "The effect of alcohol on blood pressure might vary by gender",
        "Useful in rare diseases like specific cancers",
        "Time from study entry until event or censoring",
        "100 people followed for 2 years = 200 person-years",
        "50 cases per 1000 person-years",
        "200 cases among 1000 people = 20% prevalence",
        "Choosing between risk ratio and risk difference",
        "Selection bias in case-control studies if controls aren't from the same source population"
      ),
      check.names = FALSE
    )
  }, 
  options = list(
    pageLength = 10,
    order = list(list(0, 'asc')),
    dom = 'ftip'
  ))
  
  # Dynamic effect measure UI
  output$effectMeasureUI <- renderUI({
    # Define which study designs allow difference measures
    difference_allowed <- input$studyDesign %in% c("cohort", "rct", "cross_sectional", "ecological")
    
    # Define measure availability by outcome type
    measure_availability <- if(!is.null(input$outcomeType)) {
      switch(input$outcomeType,
             "survival" = list(allowed = "ratio", message = "Only ratio measures (hazard ratios) are available for survival analysis"),
             "count" = list(allowed = "ratio", message = "Only ratio measures (rate ratios) are available for count data"),
             "ordinal" = list(allowed = "ratio", message = "Only ratio measures (proportional odds ratios) are available for ordinal outcomes"),
             list(allowed = "both", message = NULL)
      )
    } else {
      list(allowed = "both", message = NULL)
    }
    
    # Combine study design and outcome type constraints
    final_allowed <- if(measure_availability$allowed == "ratio") {
      "ratio"
    } else if(!difference_allowed) {
      "ratio"
    } else {
      "both"
    }
    
    # Generate explanation text
    explanation <- if(!difference_allowed) {
      switch(input$studyDesign,
             "case_control" = "Difference measures cannot be estimated from case-control studies (odds differences are not interpretable)",
             "nested_cc" = "Difference measures cannot be estimated from nested case-control studies (rate differences require full cohort data)",
             "case_cohort" = "Difference measures cannot be estimated from case-cohort studies (risk differences require full cohort data)",
             NULL
      )
    } else {
      measure_availability$message
    }
    
    tagList(
      radioButtons("effectMeasure", 
                   "What type of effect measure do you want to estimate?",
                   choices = c(
                     "Difference measure (e.g., risk difference, rate difference)" = "difference",
                     "Ratio measure (e.g., risk ratio, rate ratio)" = "ratio"
                   ),
                   selected = if(final_allowed == "ratio") "ratio" else NULL),
      if(!is.null(explanation)) {
        div(class = "measure-note", 
            icon("info-circle"), 
            explanation)
      }
    )
  })  # Close renderUI for effectMeasureUI
  
  # Add reactive validation
  observe({
    # Disable difference measure if not allowed
    if(!is.null(input$studyDesign)) {
      difference_allowed <- input$studyDesign %in% c("cohort", "rct", "cross_sectional", "ecological")
      if(!difference_allowed && input$effectMeasure == "difference") {
        updateRadioButtons(session, "effectMeasure", selected = "ratio")
      }
    }
    
    # Enforce outcome-specific constraints
    if(!is.null(input$outcomeType)) {
      ratio_only <- input$outcomeType %in% c("survival", "count", "ordinal")
      if(ratio_only && input$effectMeasure == "difference") {
        updateRadioButtons(session, "effectMeasure", selected = "ratio")
      }
    }
    
    # Force binary outcome for case-control studies
    if(input$studyDesign %in% c("case_control", "nested_cc")) {
      updateRadioButtons(session, 
                         "outcomeType",
                         selected = "binary")
    }
  })  # Close observe
  
  # Study design guidance table
  output$designTable <- DT::renderDataTable({
    data.frame(
      Design = c("Cross-sectional", "Cohort", "Case-control", 
                 "Nested case-control", "Case-cohort", "RCT", "Ecological"),
      Description = c(
        "Examines exposure and outcome at one time point",
        "Follows subjects over time, from exposure to outcome",
        "Starts with outcome and looks back at exposures",
        "Case-control sampled from within a cohort study",
        "Random subcohort plus all cases from main cohort",
        "Experimental design with randomized exposure",
        "Analysis at population level, not individual"
      ),
      `Common Analyses` = c(
        "Prevalence ratios, cross-sectional regression",
        "Risk/rate ratios, hazard ratios, survival analysis",
        "Odds ratios, conditional logistic regression",
        "Rate ratios, hazard ratios via conditional likelihood",
        "Pseudolikelihood methods, weighted Cox models",
        "Treatment effects, intention-to-treat analysis",
        "Correlation analysis, ecological regression"
      ),
      check.names = FALSE
    )
  }, options = list(pageLength = 7))
  
  # Analysis results UI
  output$analysisResult <- renderUI({
    # Check if sample size is missing or not numeric
    if(is.null(input$sampleSize) || is.na(input$sampleSize)) {
      return(HTML("<div class='alert alert-warning'>
        <strong>⚠️ Warning:</strong> Please input your sample size.
      </div>"))
    }
    
    # Validate case-control design has binary outcome
    if((input$studyDesign %in% c("case_control", "nested_cc")) && 
       input$outcomeType != "binary") {
      return(HTML("<div class='alert alert-warning'>
    <strong>⚠️ Error:</strong> Case-control studies must have binary outcomes.
    Please select 'Binary' as the outcome type.
  </div>"))
    }
    
    # Existing sample size warning
    sample_size_warning <- if(input$sampleSize < 30) {
      "<div class='alert alert-warning'>
        <strong>⚠️ Small Sample Size Warning:</strong> Consider non-parametric methods and exact tests.
      </div>"
    } else {
      ""
    }
    
    # Main analysis recommendations based on outcome type
    main_analysis_md <- switch(input$outcomeType,
                               "continuous" = get_continuous_analysis_md(input$predictorType, input$groupNum),
                               "binary" = get_binary_analysis_md(input$studyDesign, input$predictorType, input$effectMeasure, input$matchingDesign),
                               "survival" = get_survival_analysis_md(input$predictorType),
                               "count" = get_count_analysis_md(),
                               "ordinal" = get_ordinal_analysis_md(input$predictorType)
    )
    
    # Get justification text with proper error handling
    justification_text <- tryCatch({
      get_justification_text(
        outcome_type = input$outcomeType,
        study_design = input$studyDesign,
        predictor_type = input$predictorType,
        group_num = input$groupNum,
        effect_measure = input$effectMeasure
      )
    }, error = function(e) {
      "Justification text will be generated based on your selections."
    })
    
    # Get effect measure interpretation
    effect_interpretation <- get_effect_measure_interpretation(input$outcomeType, 
                                                               input$studyDesign, 
                                                               input$predictorType, 
                                                               input$effectMeasure)
    
    
    # Get R code template
    r_code <- get_r_code_template(input$outcomeType, 
                                  input$studyDesign, 
                                  input$predictorType, 
                                  input$effectMeasure,
                                  if(!is.null(input$matchingDesign)) input$matchingDesign else "unmatched")
    
    # Additional considerations text
    additional_considerations <- ""
    additional_list <- list(
      repeated = "### Repeated Measures Analysis
- Mixed effects models
- GEE (Generalized Estimating Equations)
- Repeated measures ANOVA",
      clustered = "### Clustering Adjustment
- Multilevel/hierarchical models
- Cluster-robust standard errors",
      missing = "### Missing Data Handling
- Multiple imputation
- Sensitivity analyses
- Complete case analysis (if MCAR)",
      multiple_testing = "### Multiple Comparison Adjustment
- Bonferroni correction
- False Discovery Rate (FDR)
- Holm's method"
    )
    
    if(length(input$additionalConsiderations) > 0) {
      additional_considerations <- paste(
        sapply(input$additionalConsiderations, 
               function(x) additional_list[[x]]),
        collapse = "\n\n"
      )
    }
    
    # Convert markdown to HTML for display
    HTML(markdown::markdownToHTML(
      text = paste(
        sample_size_warning,
        "## Recommended Analysis Plan",
        main_analysis_md,
        if(additional_considerations != "") {
          paste("## Additional Considerations", additional_considerations, sep = "\n\n")
        },
        "## Justification",
        justification_text,
        "## Interpretation of Effect Measure",
        effect_interpretation,
        "## Sample R Code",
        r_code,
        sep = "\n\n"
      ),
      fragment.only = TRUE
    ))
  })  # Close renderUI for analysisResult
  
  # Helper function to convert coded values to display text
  get_display_text <- function(coded_value, type = "study_design") {
    switch(type,
           "study_design" = tolower(switch(coded_value,
                                           "cross-sectional" = "Cross-sectional",
                                           "cohort" = "Cohort study",
                                           "case-control" = "Case-control",  # Fixed hyphenation here
                                           "rct" = "Randomized controlled trial",
                                           "ecological" = "Ecological study",
                                           gsub("_", "-", coded_value)
           )),
           "predictor_type" = tolower(switch(coded_value,
                                             "cont_pred" = "Continuous",
                                             "cat_pred" = "Categorical",
                                             "multiple_pred" = "Multiple predictors",
                                             gsub("_", "-", coded_value)
           )),
           tolower(gsub("_", "-", coded_value))
    )
  }
  
  # Helper functions for markdown-formatted analysis recommendations
  get_continuous_analysis_md <- function(predictor_type, group_num) {
    if(predictor_type == "cont_pred") {
      return("### Primary Analysis
- Simple Linear Regression

### Hypothesis Test
- t-test for slope coefficient (H₀: β = 0)

### Small Sample Alternative
- Spearman's rank correlation
- Bootstrap confidence intervals for regression coefficients

### Alternatives
- Polynomial regression if relationship is non-linear
- Quantile regression if heteroscedasticity is present
- Spline regression for complex relationships

### Assumptions to Check
- Linearity (scatter plots)
- Normality of residuals (QQ plots)
- Homoscedasticity (residual plots)
- Independence")
    } else if(predictor_type == "cat_pred") {
      if(group_num == "two") {
        return("### Primary Analysis
- Independent t-test

### Hypothesis Test
- Two-sided t-test (H₀: μ₁ = μ₂)

### Small Sample Alternative
- Mann-Whitney U test
- Exact Wilcoxon rank-sum test

### Effect Size Measures
- Cohen's d
- Mean difference with 95% CI")
      } else {
        return("### Primary Analysis
- One-way ANOVA

### Hypothesis Test
- F-test (H₀: all μᵢ are equal)

### Small Sample Alternative
- Kruskal-Wallis test
- Exact permutation test

### Post-hoc Analysis
- Tukey's HSD
- Bonferroni-adjusted pairwise comparisons
- Scheffe's method")
      }
    }
  }
  
  get_binary_analysis_md <- function(study_design, predictor_type, effect_measure = NULL, matching_design = "unmatched") {
    if(study_design == "case_control") {
      if(matching_design == "matched_fixed") {
        return("### Primary Analysis
- Conditional logistic regression
- Accounts for matched sets through stratification
- Matches treated as nuisance parameters

### Hypothesis Test
- Wald test or likelihood ratio test (H₀: OR = 1)
- Score test may be preferred with sparse data

### Small Sample Alternative
- Exact conditional logistic regression
- McNemar's test (for 1:1 matching with binary exposure)

### Model Diagnostics
- Residual plots within matching strata
- Influence diagnostics
- Assessment of matching efficiency")
      } else if(matching_design == "matched_time") {
        return("### Primary Analysis
- Risk-set conditional logistic regression
- Accounts for time-dependent matching
- Allows for time-varying exposures/covariates

### Hypothesis Test
- Wald test or likelihood ratio test (H₀: OR = 1)
- Score test for sparse data

### Small Sample Alternative
- Exact conditional logistic regression
- Time-stratified analysis

### Model Diagnostics
- Time-varying coefficient tests
- Residual plots
- Test of proportional hazards within match sets")
      } else {
        return("### Primary Analysis
- Unconditional logistic regression
- Useful for frequency-matched or unmatched designs

### Hypothesis Test
- Wald test or likelihood ratio test (H₀: OR = 1)

### Small Sample Alternative
- Exact logistic regression
- Fisher's exact test (for categorical predictors)

### Model Diagnostics
- Hosmer-Lemeshow test
- ROC curve analysis
- Classification metrics")
      }
    } else {
      base_text <- switch(study_design,
                          "cohort" = {
                            if(effect_measure == "difference") {
                              "### Primary Analysis
- Identity-link binomial regression (Risk Differences)
- Alternative: standardized risk differences

### Note
- These models may face convergence issues
- Consider robust standard errors
- May need alternative optimization algorithms

### Hypothesis Test
- Wald chi-square test (H₀: RD = 0)"
                            } else {
                              "### Primary Analysis
- Log-binomial regression (Risk Ratios)
- Alternative: modified Poisson regression if convergence fails

### Hypothesis Test
- Wald chi-square test (H₀: RR = 1)
- Score test may be more reliable with sparse data"
                            }
                          },
                          "### Primary Analysis
- Logistic regression (Odds Ratios)
- Consider GEE for clustered data

### Hypothesis Test
- Wald chi-square test (H₀: OR = 1)
- Likelihood ratio test for nested models"
      )
      
      return(paste0(base_text, "

### Small Sample Alternative
- Exact logistic regression
- Fisher's exact test (for categorical predictors)
- Firth's penalized likelihood for separation

### Model Diagnostics
- Hosmer-Lemeshow test
- ROC curve analysis
- Classification metrics
- Influence diagnostics"))
    }
  }
  
  get_survival_analysis_md <- function(predictor_type) {
    return("### Primary Analysis
- Cox Proportional Hazards

### Hypothesis Test
- Likelihood ratio test (H₀: HR = 1)

### Small Sample Alternative
- Exact logistic regression for discrete time points
- Permutation tests for Kaplan-Meier differences

### Alternatives
- Kaplan-Meier curves with log-rank test
- Parametric survival models (Weibull, exponential)
- Competing risks analysis

### Model Diagnostics
- Proportional hazards assumption
- Schoenfeld residuals
- Martingale residuals")
  }
  
  get_count_analysis_md <- function() {
    return("### Primary Analysis
- Poisson regression

### Hypothesis Test
- Likelihood ratio test (H₀: Rate Ratio = 1)

### Small Sample Alternative
- Exact Poisson test for simple comparisons
- Bootstrap confidence intervals

### Alternatives
- Negative binomial regression (overdispersion)
- Zero-inflated models
- Hurdle models

### Model Diagnostics
- Dispersion test
- Goodness-of-fit tests
- Residual analysis")
  }
  
  get_ordinal_analysis_md <- function(predictor_type) {
    return("### Primary Analysis
- Ordinal logistic regression

### Hypothesis Test
- Score test (H₀: OR = 1 across all levels)

### Small Sample Alternative
- Exact Wilcoxon-Mann-Whitney test (two groups)
- Exact Kruskal-Wallis test (multiple groups)

### Alternatives
- Proportional odds model
- Adjacent-category model
- Continuation-ratio model

### Assumptions to Check
- Proportional odds assumption
- Scale of measurement
- Response categories distribution")
  }
  
  get_justification_text <- function(outcome_type, study_design = NULL, predictor_type = NULL, group_num = NULL, effect_measure = NULL) {
    # First, handle missing or NULL inputs
    if(is.null(outcome_type) || is.null(study_design)) {
      return("Please complete all required selections to generate justification text.")
    }
    
    base_justification <- switch(outcome_type,
                                 "continuous" = {
                                   if(is.null(predictor_type)) return("Please select predictor type.")
                                   if(predictor_type == "cont_pred") {
                                     "Linear regression quantifies the relationship between continuous variables through the slope coefficient (β). The t-test for the slope evaluates the null hypothesis β=0. Non-parametric methods like Spearman's correlation are distribution-free alternatives for non-normal data. Bootstrap confidence intervals provide robust parameter estimates without assuming normality. We assess linearity, homoscedasticity, and normality of residuals through diagnostic plots to validate model assumptions and ensure unbiased estimation."
                                   } else if(group_num == "two") {
                                     "Independent t-tests compare means between groups under the assumption of normality and homogeneous variances. Effect sizes (Cohen's d) standardize mean differences for comparability across studies. When normality is violated or with small samples, the Mann-Whitney test provides a distribution-free alternative based on ranks. We evaluate equality of variances using Levene's test and adjust degrees of freedom using Welch's correction when necessary."
                                   } else {
                                     "One-way ANOVA extends the t-test to multiple groups while maintaining family-wise error rate at α. The omnibus F-test evaluates equality of all group means simultaneously. Post-hoc procedures like Tukey's HSD control for multiple comparisons while maintaining adequate power. The Kruskal-Wallis test offers a non-parametric alternative when ANOVA assumptions are violated, with post-hoc Dunn tests for pairwise comparisons."
                                   }
                                 },
                                 "binary" = {
                                   if(is.null(study_design)) return("Please select study design.")
                                   switch(study_design,
                                          "cohort" = "Log-binomial regression directly estimates risk ratios, avoiding the rare disease assumption needed for odds ratio interpretation. The model uses maximum likelihood estimation with a log link function. The Wald test evaluates coefficient significance, while likelihood ratio tests assess overall model fit. Exact methods implement complete enumeration for small samples where asymptotic approximations fail. ROC analysis and classification metrics evaluate predictive performance.",
                                          "case_control" = "Conditional logistic regression accounts for matched designs by conditioning on the matching strata, eliminating nuisance parameters. The model provides valid odds ratios while controlling for matching factors through stratification. Exact methods are preferred when asymptotic assumptions are violated. Model diagnostics assess functional form of continuous predictors and identify influential observations. ROC curves evaluate discriminative ability.",
                                          "nested_cc" = "Conditional logistic regression accounts for risk-set matching, providing valid rate ratio estimates. The analysis preserves the time-dependent structure of the underlying cohort while gaining efficiency through sampling. Matched analysis controls for time-varying confounders included in the matching criteria. Model diagnostics and influence statistics remain crucial despite the sampling design.",
                                          "case_cohort" = "Weighted Cox regression accommodates the sampling design while providing valid hazard ratio estimates. The analysis requires robust variance estimation to account for the sampling scheme. Time-varying weights reflect the sampling probabilities and maintain valid inference. Model diagnostics should account for the sampling design when assessing model fit.",
                                          # Default case if none of the above match
                                          "Logistic regression models the log odds as a linear function of predictors, yielding odds ratios through exponentiated coefficients. The Wald test evaluates parameter significance, while the Hosmer-Lemeshow test assesses model fit. Exact methods provide valid inference for small samples. ROC analysis evaluates discrimination, while classification metrics assess prediction accuracy at chosen probability thresholds."
                                   )
                                 },
                                 "survival" = "Cox proportional hazards regression models instantaneous risk without specifying a baseline hazard function, accommodating censored observations through partial likelihood. The model assumes proportional hazards, tested via Schoenfeld residuals. Kaplan-Meier curves provide non-parametric survival estimates. Stratification handles non-proportional hazards in categorical predictors. For small samples, exact or permutation methods maintain Type I error rates. Martingale residuals assess functional form of continuous predictors.",
                                 "count" = "Poisson regression models log counts using maximum likelihood estimation, assuming equidispersion (variance equals mean). The likelihood ratio test evaluates parameter significance. Negative binomial models accommodate overdispersion through an additional parameter. Zero-inflation models mixture distributions for excess zeros. We assess dispersion through the deviance/df ratio. Exact Poisson tests provide valid inference for small samples, while robust standard errors adjust for mild violation of assumptions.",
                                 "ordinal" = "Ordinal logistic regression models cumulative probabilities using proportional odds, assuming identical effects across response levels. The score test evaluates parameter significance while preserving ordinal information. The proportional odds assumption is assessed through likelihood ratio tests of separate slope models. Adjacent-category models offer an alternative when proportional odds fails. Score tests maintain nominal Type I error rates better than Wald tests for small samples.",
                                 # Default case if none of the above match
                                 "Please select an outcome type to generate justification text."
    )
    
    # Add effect measure context if available
    effect_measure_text <- if(!is.null(effect_measure)) {
      switch(effect_measure,
             "difference" = "\n\nDifference measures better identify subpopulations where interventions will have the largest public health impact. While rare exposures may show large relative effects, more lives might be saved by addressing common exposures with smaller differences.",
             "ratio" = "\n\nRatio measures can be stable across populations with different baseline risks, which is useful for understanding biological mechanisms and meta-analysis. However, this same property means they may mask important differences in absolute impact. Consider reporting both ratio and difference measures when possible, as they provide complementary information for clinical and public health decision-making.",
             ""  # Default empty string if neither selected
      )
    } else {
      ""
    }
    
    # Return combined justification
    paste0(base_justification, effect_measure_text)
  }
  
  # Updated get_r_code_template function with more comprehensive code examples
  get_r_code_template <- function(outcome_type, study_design, predictor_type, effect_measure, matching_design = "unmatched") {
    # Check for NULL inputs
    if(is.null(outcome_type) || is.null(study_design) || 
       is.null(predictor_type) || is.null(effect_measure)) {
      return("# R code template will be generated once all selections are made")
    }
    
    base_code <- switch(outcome_type,
                        "continuous" = {
                          if(predictor_type == "cont_pred") {
                            "# Linear regression with continuous predictor
model <- lm(outcome ~ exposure + covariates, data = data)

# Model diagnostics
par(mfrow = c(2,2))
plot(model)

# Summary statistics
summary(model)
confint(model)"
                          } else if(predictor_type == "cat_pred") {
                            "# For categorical predictors
if(length(unique(exposure)) == 2) {
  # Independent t-test
  t.test(outcome ~ exposure, data = data)
} else {
  # ANOVA
  model <- aov(outcome ~ factor(exposure) + covariates, data = data)
  summary(model)
  
  # Post-hoc tests
  TukeyHSD(model)
}"
                          } else {
                            "# Code template will be shown once predictor type is selected"
                          }
                        },
                        "binary" = {
                          if(study_design == "case_control") {
                            if(matching_design == "matched_fixed" || matching_design == "matched_time") {
                              "# Conditional logistic regression for matched design
library(survival)
match_model <- clogit(case_status ~ exposure + covariates + 
                      strata(matching_id),
                      data = data)

# Model summary and diagnostics
summary(match_model)
exp(cbind(OR = coef(match_model),
          confint(match_model)))"
                            } else {
                              "# Unconditional logistic regression
model <- glm(case_status ~ exposure + covariates,
             family = binomial(link = 'logit'),
             data = data)

# Model diagnostics
summary(model)
exp(coef(model))  # Convert to odds ratios"
                            }
                          } else {
                            "# Standard logistic regression
model <- glm(outcome ~ exposure + covariates,
             family = binomial(link = 'logit'),
             data = data)

# Model diagnostics
summary(model)
exp(coef(model))  # Convert to odds ratios"
                          }
                        },
                        "survival" = "# Cox proportional hazards
library(survival)
cox_model <- coxph(Surv(time, event) ~ exposure + covariates,
                   data = data)

# Test proportional hazards
cox.zph(cox_model)",
                        "count" = "# Poisson regression
pois_model <- glm(outcome ~ exposure + offset(log(time_at_risk)) + 
                  covariates,
                  family = poisson(link = 'log'),
                  data = data)",
                        "ordinal" = "# Ordinal logistic regression
library(MASS)
ord_model <- polr(ordered(outcome) ~ exposure + covariates,
                  data = data)",
                        # Default case if none match
                        "# Code template will be shown once outcome type is selected"
    )
    
    # Return code with markdown formatting
    paste("```r", base_code, "```", sep = "\n")
  }
  
  # Download handler for analysis plan
  output$downloadReport <- downloadHandler(
    filename = function() {
      ext <- switch(input$downloadFormat,
                    "html" = ".html",
                    "docx" = ".docx",
                    "md" = ".md"
      )
      paste0("analysis-plan-", Sys.Date(), ext)
    },
    content = function(file) {
      # Main analysis recommendations based on outcome type
      main_analysis_md <- switch(input$outcomeType,
                                 "continuous" = get_continuous_analysis_md(input$predictorType, input$groupNum),
                                 "binary" = get_binary_analysis_md(input$studyDesign, input$predictorType, input$effectMeasure),
                                 "survival" = get_survival_analysis_md(input$predictorType),
                                 "count" = get_count_analysis_md(),
                                 "ordinal" = get_ordinal_analysis_md(input$predictorType)
      )
      
      
      # Get formatted display text
      study_design_text <- get_display_text(input$studyDesign, "study_design")
      predictor_type_text <- get_display_text(input$predictorType, "predictor_type")
      outcome_type_text <- tolower(input$outcomeType)
      
      # Get effect measure interpretation
      effect_interpretation <- get_effect_measure_interpretation(input$outcomeType,
                                                                 input$studyDesign,
                                                                 input$predictorType,
                                                                 input$effectMeasure)
      
      # Create temporary Rmd file
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      
      rmd_content <- sprintf('---
title: "Statistical Analysis Plan"
date: "%s"
output:
  html_document:
    theme: bootstrap
    toc: true
  word_document:
    toc: true
  md_document:
    variant: gfm
---

# Report Information

**Author:** %s  
**Institution:** %s  
**Date:** %s

## Study Information

- **Study Design:** %s
- **Sample Size:** %d
- **Outcome Type:** %s
- **Predictor Type:** %s

## Analysis Recommendations

%s%s

## Additional Considerations

%s

## Justification

%s

## Interpretation of Effect Measure
      
%s

## Sample R Code

The following R code provides a template for implementing the recommended analyses:

%s

---
*Generated by the Epidemiological Analysis Guide App*
',
                             Sys.Date(),
                             if(nchar(input$userName) > 0) input$userName else "Not specified",
                             if(nchar(input$userInstitution) > 0) input$userInstitution else "Not specified",
                             format(Sys.Date(), "%B %d, %Y"),
                             study_design_text,
                             as.integer(input$sampleSize),
                             outcome_type_text,
                             predictor_type_text,
                             if(input$sampleSize < 30) {
                               "**⚠️ Small Sample Size Warning:** Consider non-parametric methods and exact tests.\n\n## Recommended Analysis Plan\n\n"
                             } else "## Recommended Analysis Plan\n\n",
                             main_analysis_md,
                             if(length(input$additionalConsiderations) > 0) {
                               paste("- ", sapply(input$additionalConsiderations, function(x) {
                                 switch(x,
                                        "repeated" = "Use repeated measures methods (Mixed effects models, GEE, Repeated measures ANOVA)",
                                        "clustered" = "Account for clustering (Multilevel models, Cluster-robust standard errors)",
                                        "missing" = "Handle missing data (Multiple imputation, Sensitivity analyses)",
                                        "multiple_testing" = "Adjust for multiple comparisons (Bonferroni, FDR, Holm's method)"
                                 )
                               }), collapse = "\n")
                             } else {
                               "No additional considerations selected"
                             },
                             get_justification_text(input$outcomeType, input$studyDesign, input$predictorType, input$groupNum),
                             effect_interpretation,
                             get_r_code_template(input$outcomeType, input$studyDesign, input$predictorType, input$effectMeasure)
      )
      
      # Write the content to the temporary file
      writeLines(rmd_content, tempReport)
      
      # Render the report
      rmarkdown::render(tempReport,
                        output_format = switch(input$downloadFormat,
                                               "html" = "html_document",
                                               "docx" = "word_document",
                                               "md" = "md_document"
                        ),
                        output_file = file,
                        quiet = TRUE)
    }
  )
  }


# Run the app
shinyApp(ui = ui, server = server)
 
 # The app is now ready to be run. You can save the code to a file named  app.R  and run it in RStudio by clicking the “Run App” button. 
 # The app will open in your default web browser and you can interact with it to generate statistical analysis recommendations based on your study design, sample size, and data characteristics. 
 # Here is a screenshot of the app in action: 
 # Summary 
 # In this tutorial, we discussed how to create an enhanced epidemiological analysis guide using R Shiny. The app provides a user-friendly interface for researchers to select appropriate statistical analyses based on their study design, sample size, and data characteristics. 
 # The app includes a decision tree to guide users through the analysis selection process, displays recommended analysis plans based on user inputs, and allows users to download the analysis plan in various formats (HTML, PDF, Word, Markdown). 
 # The app is a useful tool for epidemiologists and researchers to quickly identify appropriate statistical methods for their studies and ensure that they are following best practices in data analysis. 
 # To learn more about creating web applications in R, consider the following tutorials: 
 # 
 # How to Create a Simple Web Application in R with Shiny
 # How to Create a Dashboard in R with Shiny and Flexdashboard
 # How to Create a Web Application in R with Shiny and Google Analytics Data