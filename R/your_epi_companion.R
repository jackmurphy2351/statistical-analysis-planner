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

# =================== HELPER FUNCTIONS ===================

`%||%` <- function(x, y) if (is.null(x)) y else x

# Get effect measure interpretation
get_effect_measure_interpretation <- function(outcome_type, study_design, predictor_type, effect_measure) {
  if(is.null(outcome_type) || is.null(study_design) || 
     is.null(predictor_type) || is.null(effect_measure)) {
    return("Please complete all selections to see effect measure interpretation.")
  }
  
  # Add new interpretation for survival time differences
  if(outcome_type == "survival" && effect_measure == "difference") {
    return("The difference in median survival time represents the absolute difference in the time at which 50% of subjects in each group have experienced the event. This provides a directly interpretable measure of the exposure's impact on survival in the original time scale.")
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


# Calculate power function
calculate_power <- function(design, outcome, params) {
  # Validate inputs
  if(is.null(params$alpha) || is.null(params$sample_size)) {
    return(list(
      error = TRUE,
      message = "Please complete all required fields"
    ))
  }
  
  if(outcome == "binary" && design == "case_control") {
    # Validate case-control specific parameters
    if(is.null(params$or) || is.null(params$p0) || is.null(params$ratio)) {
      return(list(
        error = TRUE,
        message = "Please complete all required fields for case-control design"
      ))
    }
    
    # Calculate p1 from odds ratio and p0
    p1 <- (params$or * params$p0) / (1 + params$p0 * (params$or - 1))
    
    # Calculate number of cases
    n_cases <- params$sample_size / (1 + params$ratio)
    
    # Calculate average exposure proportion
    p_avg <- (params$p0 + p1) / 2
    
    # Default to one-sided if not specified
    params$sided_test <- if(is.null(params$sided_test)) "one" else params$sided_test
    
    # Get critical value based on one/two-sided test
    z_alpha <- if(params$sided_test == "one") {
      qnorm(1 - params$alpha)
    } else {
      qnorm(1 - params$alpha/2)
    }
    
    if(is.null(params$match_design) || params$match_design == "unmatched") {
      # Calculate z_beta using the correct formula for case-control studies
      z_beta <- (sqrt(n_cases) * abs(p1 - params$p0) - 
                   z_alpha * sqrt((1 + 1/params$ratio) * p_avg * (1-p_avg))) /
        sqrt(p1*(1-p1)/params$ratio + params$p0*(1-params$p0))
      
      power <- pnorm(z_beta)
      
      return(list(
        power = power,
        power_pct = paste0(round(power * 100, 1), "%"),
        n_cases = n_cases,
        n_controls = n_cases * params$ratio,
        method = paste0(
          "Power calculation for case-control studies\n",
          "Using ", if(params$sided_test == "one") "one" else "two",
          "-sided test with α = ", params$alpha
        )
      ))
    } else if(params$match_design == "matched") {
      # Validate matched-specific parameters
      if(is.null(params$phi)) {
        return(list(
          error = TRUE,
          message = "Please specify the within-pair correlation for matched design"
        ))
      }
      
      # Calculate discordant pair probability
      pi_d <- p1*(1-params$p0) + params$p0*(1-p1) - 
        2*params$phi*sqrt(params$p0*(1-params$p0)*p1*(1-p1))
      
      # Calculate probability of exposure in cases given discordance
      p1_d <- p1*(1-params$p0)/pi_d
      
      # Calculate power for matched design
      z_beta <- sqrt(n_cases * pi_d) * (2*p1_d - 1) - z_alpha
      
      power <- pnorm(z_beta)
      
      return(list(
        power = power,
        power_pct = paste0(round(power * 100, 1), "%"),
        n_pairs = n_cases,
        method = paste0(
          "Power calculation for matched case-control studies\n",
          "Using ", if(params$sided_test == "one") "one" else "two",
          "-sided test with α = ", params$alpha
        )
      ))
    }
  }
  
  else if(outcome == "continuous") {
    if(design == "rct_crossover") {
      # Power for paired t-test
      effect_size <- params$diff/(params$sd * sqrt(2))
      z_beta <- effect_size * sqrt(params$sample_size) - qnorm(1-params$alpha/2)
      power <- pnorm(z_beta)
      
      list(
        power = power,
        power_pct = paste0(round(power * 100, 1), "%"),
        effect_size = effect_size,
        method = "Power calculation for crossover design"
      )
      
    } else {
      # Power for two-sample t-test
      n1 <- params$sample_size/(1 + params$ratio)
      n2 <- n1 * params$ratio
      effect_size <- params$diff/params$sd
      z_beta <- effect_size * sqrt((n1*n2)/(n1+n2)) - qnorm(1-params$alpha/2)
      power <- pnorm(z_beta)
      
      list(
        power = power,
        power_pct = paste0(round(power * 100, 1), "%"),
        n_group1 = n1,
        n_group2 = n2,
        effect_size = effect_size,
        method = "Power calculation for two-sample t-test"
      )
    }
  }
  
  else if(outcome == "survival") {
    # Power for survival analysis
    n_per_group <- params$sample_size/2
    p_events <- params$p0 * (1-params$dropouts)
    n_events <- n_per_group * 2 * p_events
    
    z_beta <- sqrt(n_events * params$p0 * (1-params$p0)) * abs(log(params$hr)) - 
      qnorm(1-params$alpha/2)
    power <- pnorm(z_beta)
    
    list(
      power = power,
      power_pct = paste0(round(power * 100, 1), "%"),
      n_events_expected = n_events,
      method = "Power calculation using Schoenfeld's formula"
    )
  }
}

# Sample size calculation function
calculate_sample_size <- function(design, outcome, params) {
  if(outcome == "binary") {
    if(design %in% c("cohort", "cross_sectional", "rct_parallel")) {
      # Using arc-sine transformation for proportions
      p_avg <- (params$p0 + params$p1)/2
      h <- 2*asin(sqrt(params$p1)) - 2*asin(sqrt(params$p0))
      n <- ceiling((2 * (qnorm(1-params$alpha/2) + qnorm(params$power))^2) / h^2)
      
      list(
        n_per_group = n,
        total_n = 2*n,
        method = "Arc-sine transformation for proportions"
      )
      
    } else if(design == "case_control") {
      # Validate required parameters
      if(any(sapply(c("or", "p0", "ratio", "alpha", "power"), function(x) is.null(params[[x]])))) {
        return(list(
          error = TRUE,
          message = "Please complete all required fields for case-control design"
        ))
      }
      
      # Ensure all parameters are numeric
      tryCatch({
        params$or <- as.numeric(params$or)
        params$p0 <- as.numeric(params$p0)
        params$ratio <- as.numeric(params$ratio)
        params$alpha <- as.numeric(params$alpha)
        params$power <- as.numeric(params$power)
      }, error = function(e) {
        return(list(
          error = TRUE,
          message = "Invalid numeric values in parameters"
        ))
      })
      
      # Calculate p1 from odds ratio and p0
      p1 <- (params$or * params$p0)/(1 + params$p0 * (params$or - 1))
      
      # Calculate average exposure proportion
      p_avg <- (params$p0 + p1)/2
      
      # Default to one-sided if not specified
      params$sided_test <- if(is.null(params$sided_test)) "one" else params$sided_test
      
      # Get critical value based on one/two-sided test
      z_alpha <- if(params$sided_test == "one") {
        qnorm(1 - params$alpha)
      } else {
        qnorm(1 - params$alpha/2)
      }
      
      if(is.null(params$match_design) || params$match_design == "unmatched") {
        # Schlesselman's formula for unmatched case-control
        n_cases <- ceiling(
          (z_alpha * sqrt((1+1/params$ratio)*p_avg*(1-p_avg)) + 
             qnorm(params$power) * sqrt(p1*(1-p1)/params$ratio + params$p0*(1-params$p0)))^2 /
            (p1-params$p0)^2
        )
        
        list(
          n_cases = n_cases,
          n_controls = ceiling(n_cases * params$ratio),
          total_n = ceiling(n_cases * (1 + params$ratio)),
          method = paste0(
            "Schlesselman's formula for unmatched case-control studies\n",
            "Using ", if(params$sided_test == "one") "one" else "two",
            "-sided test with α = ", params$alpha
          )
        )
        
      } else if(params$match_design == "matched") {
        # Validate matched-specific parameters
        if(is.null(params$phi)) {
          return(list(
            error = TRUE,
            message = "Please specify the within-pair correlation for matched design"
          ))
        }
        
        params$phi <- as.numeric(params$phi)
        
        # Gail's formula for matched case-control studies
        # Calculate discordant pair probability
        pi_d <- p1*(1-params$p0) + params$p0*(1-p1) - 
          2*params$phi*sqrt(params$p0*(1-params$p0)*p1*(1-p1))
        
        # Calculate probability of exposure in cases given discordance
        p1_d <- p1*(1-params$p0)/pi_d
        
        # Calculate required number of discordant pairs
        n_pairs <- ceiling(
          ((z_alpha + qnorm(params$power))/(2*p1_d - 1))^2 / pi_d
        )
        
        list(
          n_pairs = n_pairs,
          total_n = 2*n_pairs,  # Total number of subjects (cases + controls)
          method = paste0(
            "Gail's formula for matched case-control studies (1973)\n",
            "Using ", if(params$sided_test == "one") "one" else "two",
            "-sided test with α = ", params$alpha
          )
        )
      }
    }
  } else if(outcome == "continuous") {
    if(design == "rct_crossover") {
      # Paired t-test formula
      n <- ceiling(2 * (qnorm(1-params$alpha/2) + qnorm(params$power))^2 * 
                     (params$sd^2) / (params$diff^2))
      
      list(
        n_total = n,
        method = "Paired t-test formula for crossover design"
      )
      
    } else {
      # Two-sample t-test formula
      n1 <- ceiling((1 + 1/params$ratio) * 
                      (qnorm(1-params$alpha/2) + qnorm(params$power))^2 * 
                      params$sd^2 / params$diff^2)
      
      list(
        n_group1 = n1,
        n_group2 = ceiling(n1 * params$ratio),
        total_n = n1 * (1 + params$ratio),
        method = "Two-sample t-test formula"
      )
    }
  } else if(outcome == "survival") {
    # Schoenfeld's formula for survival
    n_events <- ceiling((qnorm(1-params$alpha/2) + qnorm(params$power))^2 / 
                          (log(params$hr)^2 * params$p0 * (1-params$p0)))
    
    # Adjust for dropouts
    n_total <- ceiling(n_events / (params$p0 * (1-params$dropouts)))
    
    list(
      n_events_needed = n_events,
      n_per_group = ceiling(n_total/2),
      total_n = n_total,
      method = "Schoenfeld's formula for survival analysis"
    )
  }
}

# Display equation function
equation_display <- function(design, outcome) {
  req(design, outcome)
  
  if(outcome == "binary") {
    if(design == "case_control") {
      HTML("
        <div style='font-family: monospace; padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
          <strong>Sample Size Formula (Unmatched Case-Control):</strong><br><br>
          n<sub>cases</sub> = (z<sub>α</sub>√[(1+1/r)p̄(1-p̄)] + z<sub>β</sub>√[p₁(1-p₁)/r + p₀(1-p₀)])² / (p₁-p₀)²<br><br>
          where:<br>
          • r = control:case ratio<br>
          • p̄ = (p₁ + p₀)/2<br>
          • p₁ = odds ratio × p₀ / (1 + p₀(odds ratio - 1))<br>
          • z<sub>α</sub> = one-sided or two-sided critical value<br>
          • z<sub>β</sub> = power
        </div>")
    } else if(design %in% c("cohort", "cross_sectional", "rct_parallel")) {
      HTML("
        <div style='font-family: monospace; padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
          <strong>Sample Size Formula (Binary Outcome):</strong><br><br>
          n = 2(z<sub>α/2</sub> + z<sub>β</sub>)²[p₁(1-p₁) + p₀(1-p₀)] / (p₁-p₀)²<br><br>
          where:<br>
          • p₁ = probability in exposed/treatment group<br>
          • p₀ = probability in unexposed/control group<br>
          • z<sub>α/2</sub> = two-sided critical value<br>
          • z<sub>β</sub> = power
        </div>")
    }
  } else if(outcome == "continuous") {
    if(design == "rct_crossover") {
      HTML("
        <div style='font-family: monospace; padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
          <strong>Sample Size Formula (Paired Design):</strong><br><br>
          n = 2(z<sub>α/2</sub> + z<sub>β</sub>)²σ² / δ²<br><br>
          where:<br>
          • σ = standard deviation of differences<br>
          • δ = minimum detectable difference<br>
          • z<sub>α/2</sub> = two-sided critical value<br>
          • z<sub>β</sub> = power
        </div>")
    } else {
      HTML("
        <div style='font-family: monospace; padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
          <strong>Sample Size Formula (Two Independent Groups):</strong><br><br>
          n₁ = (1 + 1/r)(z<sub>α/2</sub> + z<sub>β</sub>)²σ² / δ²<br><br>
          where:<br>
          • r = n₂/n₁ (allocation ratio)<br>
          • σ = common standard deviation<br>
          • δ = minimum detectable difference<br>
          • z<sub>α/2</sub> = two-sided critical value<br>
          • z<sub>β</sub> = power
        </div>")
    }
  } else if(outcome == "survival") {
    HTML("
      <div style='font-family: monospace; padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
        <strong>Sample Size Formula (Survival Analysis):</strong><br><br>
        n = (z<sub>α/2</sub> + z<sub>β</sub>)² / (p₀(1-p₀)(log HR)²(1-d))<br><br>
        where:<br>
        • p₀ = event rate in control group<br>
        • HR = hazard ratio<br>
        • d = dropout rate<br>
        • z<sub>α/2</sub> = two-sided critical value<br>
        • z<sub>β</sub> = power
      </div>")
  }
}

tooltip_content <- list(
  ss_design = HTML("
    <strong>Study Design:</strong><br>
    - <u>Cohort Study:</u> Following groups over time from exposure to outcome<br>
    - <u>Case-Control:</u> Comparing past exposures between cases and controls<br>
    - <u>Cross-sectional:</u> Measuring exposure and outcome at one time point<br>
    - <u>Clinical Trial:</u> Randomized intervention study<br>
    Different designs require different sample size calculations due to their underlying assumptions.
  "),
  
  ss_outcome = HTML("
    <strong>Outcome Type:</strong><br>
    - <u>Binary:</u> Yes/no outcome (e.g., disease status)<br>
    - <u>Continuous:</u> Measured values (e.g., blood pressure)<br>
    - <u>Time-to-event:</u> Duration until event occurs<br>
    The outcome type determines which statistical test and power calculation method to use.
  "),
  
  ss_alpha = HTML("
    <strong>Type I Error (α):</strong><br>
    - Probability of falsely rejecting the null hypothesis<br>
    - Conventional level is 0.05 (5% false positive rate)<br>
    - More stringent values (e.g., 0.01) reduce false positives but require larger sample sizes
  "),
  
  ss_power = HTML("
    <strong>Power (1-β):</strong><br>
    - Probability of detecting a true effect if one exists<br>
    - Conventional level is 0.80 (80% chance of detection)<br>
    - Higher power (e.g., 0.90) requires larger sample sizes<br>
    - β is the Type II error rate (false negative rate)
  "),
  
  ss_or = HTML("
    <strong>Odds Ratio:</strong><br>
    - Measure of association in case-control studies<br>
    - OR = 1: No association<br>
    - OR > 1: Positive association<br>
    - OR < 1: Negative association<br>
    Smaller differences from 1.0 require larger sample sizes to detect.
  ")
)

# UI definition
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Your Epi Companion"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("How-to Guide", tabName = "howto", icon = icon("question-circle")),
      menuItem("Analysis Guide", tabName = "guide", icon = icon("dashboard")),
      menuItem("Sample Size & Power", tabName = "samplesize", icon = icon("calculator")),
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
                                   min = 1
                      ),
                      conditionalPanel(
                        condition = "input.matchRatio > 4",
                        div(style = "color: #856404; background-color: #fff3cd; padding: 8px; border: 1px solid #ffeeba; border-radius: 4px;",
                            HTML("<small><i class='fas fa-info-circle'></i> Statistical efficiency gains diminish rapidly beyond 4 controls per case due to marginal information added.</small>")
                        )
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
                  
                  conditionalPanel(
                    condition = "input.studyDesign",
                    uiOutput("outcomeTypeUI")
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
      
      # Sample Size Tab
      # Find the tabItem for "samplesize" and replace it with:
      tabItem(tabName = "samplesize",
              fluidRow(
                box(
                  width = 12,
                  title = "Sample Size & Power Calculator",
                  status = "primary",
                  solidHeader = TRUE,
                  
                  # Calculator mode selection
                  div(
                    style = "margin-bottom: 20px;",
                    radioButtons("calc_mode", "What would you like to calculate?",
                                 choices = c(
                                   "Sample Size (given desired power)" = "sample_size",
                                   "Power (given sample size)" = "power"
                                 ),
                                 selected = "sample_size"
                    )
                  ),
                  
                  # Study design selection with tooltip
                  div(
                    style = "margin-bottom: 20px;",
                    selectInput("ss_design", 
                                label = span(
                                  "Study Design",
                                  tags$i(class = "fas fa-info-circle",
                                         style = "margin-left: 5px; cursor: pointer;",
                                         `data-toggle` = "tooltip",
                                         `data-html` = "true",
                                         title = tooltip_content$ss_design)
                                ),
                                choices = c(
                                  "Cohort Study" = "cohort",
                                  "Case-Control Study" = "case_control",
                                  "Cross-sectional Study" = "cross_sectional",
                                  "Clinical Trial (Parallel)" = "rct_parallel",
                                  "Clinical Trial (Crossover)" = "rct_crossover"
                                )
                    )
                  ),
                  
                  # Outcome type selection with tooltip
                  div(
                    style = "margin-bottom: 20px;",
                    selectInput("ss_outcome", 
                                label = span(
                                  "Outcome Type",
                                  tags$i(class = "fas fa-info-circle",
                                         style = "margin-left: 5px; cursor: pointer;",
                                         `data-toggle` = "tooltip",
                                         `data-html` = "true",
                                         title = tooltip_content$ss_outcome)
                                ),
                                choices = c(
                                  "Binary (e.g., disease yes/no)" = "binary",
                                  "Continuous (e.g., blood pressure)" = "continuous",
                                  "Time-to-event" = "survival"
                                )
                    )
                  ),
                  
                  # Matching and test type selection for case-control studies
                  conditionalPanel(
                    condition = "input.ss_design == 'case_control'",
                    div(
                      style = "margin-bottom: 20px;",
                      radioButtons("ss_match_design",
                                   "Matching Design:",
                                   choices = c(
                                     "Unmatched case-control" = "unmatched",
                                     "Matched case-control" = "matched"
                                   ),
                                   selected = "unmatched"
                      )
                    ),
                    div(
                      style = "margin-bottom: 20px;",
                      radioButtons("ss_sided_test",
                                   "Test Type:",
                                   choices = c(
                                     "One-sided test" = "one",
                                     "Two-sided test" = "two"
                                   ),
                                   selected = "one"
                      )
                    )
                  ),
                  
                  # Common parameters with tooltips
                  numericInput("ss_alpha", 
                               label = span(
                                 "Type I Error (α)",
                                 tags$i(class = "fas fa-info-circle",
                                        style = "margin-left: 5px; cursor: pointer;",
                                        `data-toggle` = "tooltip",
                                        `data-html` = "true",
                                        title = tooltip_content$ss_alpha)
                               ),
                               value = 0.05,
                               min = 0.01,
                               max = 0.1,
                               step = 0.01),
                  
                  # Calculation mode specific inputs with tooltips
                  conditionalPanel(
                    condition = "input.calc_mode == 'power'",
                    numericInput("ss_n", 
                                 label = span(
                                   "Total Sample Size",
                                   tags$i(class = "fas fa-info-circle",
                                          style = "margin-left: 5px; cursor: pointer;",
                                          `data-toggle` = "tooltip",
                                          `data-html` = "true",
                                          title = "The total number of participants in your study. This should include all groups or arms of the study combined.")
                                 ),
                                 value = 100,
                                 min = 2,
                                 step = 1)
                  ),
                  
                  conditionalPanel(
                    condition = "input.calc_mode == 'sample_size'",
                    numericInput("ss_power", 
                                 label = span(
                                   "Desired Power (1-β)",
                                   tags$i(class = "fas fa-info-circle",
                                          style = "margin-left: 5px; cursor: pointer;",
                                          `data-toggle` = "tooltip",
                                          `data-html` = "true",
                                          title = tooltip_content$ss_power)
                                 ),
                                 value = 0.80,
                                 min = 0.6,
                                 max = 0.99,
                                 step = 0.05)
                  ),
                  
                  # Dynamic parameter inputs
                  uiOutput("ss_params"),
                  
                  # Equation display
                  uiOutput("equation_display"),
                  
                  # Results display
                  uiOutput("ss_results")
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
      
      # Glossary Tab
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
    )  # Add this to close tabItems()
  )  # Add this to close dashboardBody()
)  # Add this to close dashboardPage()

# Server logic
server <- function(input, output, session) {  # Added session parameter here
  
  # Initialize reactive values
  rv <- reactiveValues(
    params_ready = FALSE,
    calculation_inputs = NULL
  )
  
  observe({
    # Track parameter readiness
    req(input$ss_design, input$ss_outcome, input$calc_mode)
    
    # Initialize inputs list
    inputs <- list(
      design = input$ss_design,
      outcome = input$ss_outcome,
      mode = input$calc_mode,
      alpha = input$ss_alpha
    )
    
    # Add mode-specific parameters
    if(input$calc_mode == "power") {
      req(input$ss_n)
      inputs$sample_size <- as.numeric(input$ss_n)
    } else {
      req(input$ss_power)
      inputs$power <- as.numeric(input$ss_power)
    }
    
    # Add this line to explicitly capture the sided_test parameter
    if(input$ss_design == "case_control") {
      inputs$sided_test <- input$ss_sided_test
    }
    
    # Validate all required inputs are present and numeric
    tryCatch({
      # Add outcome-specific parameters
      if(input$ss_outcome == "binary") {
        if(input$ss_design == "case_control") {
          req(input$ss_or, input$ss_p0, input$ss_ratio)
          inputs$or <- as.numeric(input$ss_or)
          inputs$p0 <- as.numeric(input$ss_p0)
          inputs$ratio <- as.numeric(input$ss_ratio)
          inputs$sided_test <- input$ss_sided_test %||% "one"
          inputs$match_design <- input$ss_match_design %||% "unmatched"
          
          if(inputs$match_design == "matched") {
            req(input$ss_phi)
            inputs$phi <- as.numeric(input$ss_phi)
          }
        } else if(input$ss_design %in% c("cohort", "cross_sectional", "rct_parallel")) {
          req(input$ss_p0, input$ss_p1)
          inputs$p0 <- as.numeric(input$ss_p0)
          inputs$p1 <- as.numeric(input$ss_p1)
        }
      } else if(input$ss_outcome == "continuous") {
        req(input$ss_diff, input$ss_sd)
        inputs$diff <- as.numeric(input$ss_diff)
        inputs$sd <- as.numeric(input$ss_sd)
        if(input$ss_design != "rct_crossover") {
          req(input$ss_ratio)
          inputs$ratio <- as.numeric(input$ss_ratio)
        }
      } else if(input$ss_outcome == "survival") {
        req(input$ss_hr, input$ss_p0, input$ss_dropouts)
        inputs$hr <- as.numeric(input$ss_hr)
        inputs$p0 <- as.numeric(input$ss_p0)
        inputs$dropouts <- as.numeric(input$ss_dropouts)
      }
      
      # If we get here, all inputs are valid
      rv$calculation_inputs <- inputs
      rv$params_ready <- TRUE
      
    }, error = function(e) {
      rv$params_ready <- FALSE
    })
  })
  
  # Initialize tooltips for dynamic content
  observe({
    # Ensure tooltips are initialized after any dynamic content loads
    session$sendCustomMessage(type = "handler-tooltip-init", message = list())
  })
  
  observe({
    # Force binary outcome for case-control studies in sample size calculator
    if(input$ss_design == "case_control" && 
       !is.null(input$ss_outcome) && 
       input$ss_outcome != "binary") {
      updateSelectInput(session, 
                        "ss_outcome",
                        selected = "binary")
      showNotification(
        "Case-control studies can only have binary outcomes. Outcome type has been updated.",
        type = "warning",
        duration = 5
      )
    }
  })
  
  # Add the equation display output:
  output$equation_display <- renderUI({
    equation_display(input$ss_design, input$ss_outcome)
  })
  
  # Render the parameter inputs UI
  output$ss_params <- renderUI({
    req(input$ss_design, input$ss_outcome, input$calc_mode)
    
    params <- list()
    
    if(input$ss_design == "case_control") {
      
      params$or <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_or",
                     "Minimum detectable odds ratio:",
                     value = 2.0,
                     min = 1.1,
                     max = 10,
                     step = 0.1)
      )
      
      params$p0 <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_p0",
                     "Expected exposure prevalence in controls:",
                     value = 0.1,
                     min = 0.001,
                     max = 0.999,
                     step = 0.01)
      )
      
      params$ratio <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_ratio",
                     "Control to case ratio:",
                     value = 1,
                     min = 0.5,
                     max = 4,
                     step = 0.5)
      )
    } else if(input$ss_outcome == "binary" && 
                input$ss_design %in% c("cohort", "cross_sectional", "rct_parallel")) {
      params$p0 <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_p0",
                     label = span(
                       "Probability in Unexposed/Control Group (p₀)",
                       tags$i(class = "fas fa-info-circle",
                              style = "margin-left: 5px; cursor: pointer;",
                              `data-toggle` = "tooltip",
                              `data-html` = "true",
                              title = "Expected probability of outcome in the unexposed/control group")
                     ),
                     value = 0.1,
                     min = 0.001,
                     max = 0.999,
                     step = 0.01)
      )
      
      params$p1 <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_p1",
                     label = span(
                       "Probability in Exposed/Treatment Group (p₁)",
                       tags$i(class = "fas fa-info-circle",
                              style = "margin-left: 5px; cursor: pointer;",
                              `data-toggle` = "tooltip",
                              `data-html` = "true",
                              title = "Expected probability of outcome in the exposed/treatment group")
                     ),
                     value = 0.2,
                     min = 0.001,
                     max = 0.999,
                     step = 0.01)
      )
      
      params$effect_note <- div(
        style = "margin-bottom: 20px; color: #666; font-style: italic; padding-left: 20px;",
        sprintf("Based on your entered probabilities:\n
          Risk Difference = %.3f\n
          Relative Risk = %.2f", 
                input$ss_p1 - input$ss_p0,
                input$ss_p1/input$ss_p0)
      )
      
      params$ratio <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_ratio",
                     "Group allocation ratio (n₂/n₁):",
                     value = 1,
                     min = 0.5,
                     max = 4,
                     step = 0.5)
      )
    }    else if(input$ss_outcome == "continuous") {
      params$effect_size <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_effect_size",
                     label = span(
                       "Effect Size (Cohen's d)",
                       tags$i(class = "fas fa-info-circle",
                              style = "margin-left: 5px; cursor: pointer;",
                              `data-toggle` = "tooltip",
                              `data-html` = "true",
                              title = "Standardized effect size (difference in means divided by pooled standard deviation). Around 0.2 is small, 0.5 is medium, 0.8 is large.")
                     ),
                     value = 0.5,
                     min = 0.1,
                     max = 2.0,
                     step = 0.1)
      )
      
      params$diff <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_diff",
                     "Minimum detectable difference:",
                     value = 0.5,
                     min = 0.1,
                     max = 100,
                     step = 0.1)
      )
      
      params$sd <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_sd",
                     "Expected standard deviation:",
                     value = 1,
                     min = 0.1,
                     max = 100,
                     step = 0.1)
      )
      
      if(input$ss_design != "rct_crossover") {
        params$ratio <- div(
          style = "margin-bottom: 20px;",
          numericInput("ss_ratio",
                       "Group allocation ratio (n2/n1):",
                       value = 1,
                       min = 0.5,
                       max = 4,
                       step = 0.5)
        )
      }
    } else if(input$ss_outcome == "survival") {
      params$hr <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_hr",
                     "Minimum detectable hazard ratio:",
                     value = 0.7,
                     min = 0.1,
                     max = 2,
                     step = 0.05)
      )
      
      params$p0 <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_p0",
                     "Expected event rate in control group:",
                     value = 0.3,
                     min = 0.01,
                     max = 0.99,
                     step = 0.05)
      )
      
      params$dropouts <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_dropouts",
                     "Expected dropout rate:",
                     value = 0.1,
                     min = 0,
                     max = 0.5,
                     step = 0.05)
      )
    }
    
    # Conditionally add phi parameter for matched case-control studies
    if(!is.null(input$ss_match_design) && input$ss_match_design == "matched") {
      params$phi <- div(
        style = "margin-bottom: 20px;",
        numericInput("ss_phi",
                     "Within-pair correlation of exposure (φ):",
                     value = 0.2,
                     min = 0,
                     max = 1,
                     step = 0.1)
      )
    }
    
    # Return all parameters
    do.call(tagList, params)
  })
  
  # Render results
  output$ss_results <- renderUI({
    req(rv$params_ready, rv$calculation_inputs)
    
    tryCatch({
      # Calculate based on selected mode
      results <- if(rv$calculation_inputs$mode == "sample_size") {
        calculate_sample_size(rv$calculation_inputs$design, 
                              rv$calculation_inputs$outcome, 
                              rv$calculation_inputs)
      } else {
        calculate_power(rv$calculation_inputs$design, 
                        rv$calculation_inputs$outcome, 
                        rv$calculation_inputs)
      }
      
      # Display results
      div(
        class = "well",
        h4(if(rv$calculation_inputs$mode == "sample_size") 
          "Sample Size Calculation Results" 
          else "Power Calculation Results"),
        tags$ul(
          lapply(names(results), function(name) {
            if(name != "method") {
              tags$li(
                strong(gsub("_", " ", tools::toTitleCase(name)), ": "),
                results[[name]]
              )
            }
          }),
          tags$li(
            strong("Method: "),
            results$method
          )
        ),
        tags$div(
          class = "alert alert-info",
          HTML("<strong>Note:</strong> These calculations assume:
        <ul>
          <li>Independent observations</li>
          <li>No adjustment for covariates or multiple comparisons</li>
          <li>Complete data collection</li>
        </ul>
        Consider increasing the calculated sample size by 10-20% to account for potential losses.")
        )
      )
    }, error = function(e) {
      div(
        class = "alert alert-danger",
        icon("exclamation-circle"),
        "Unable to calculate results. Please check your inputs and try again.",
        tags$br(),
        tags$small(class = "text-muted", "Error details: ", e$message)
      )
    })
  })
  
  # Add this new output definition
  output$outcomeTypeUI <- renderUI({
    # Define available outcome choices based on study design
    outcome_choices <- if(input$studyDesign == "cross_sectional") {
      # Cross-sectional studies can't measure time-to-event outcomes
      c(
        "Continuous (e.g., blood pressure, BMI)" = "continuous",
        "Binary (e.g., disease yes/no)" = "binary",
        "Count data (e.g., number of cases)" = "count",
        "Ordinal (e.g., disease severity)" = "ordinal"
      )
    } else if(input$studyDesign %in% c("case_control", "nested_cc")) {
      # Case-control studies must have binary outcomes
      c("Binary (case-control status)" = "binary")
    } else {
      # All outcome types available for other designs
      c(
        "Continuous (e.g., blood pressure, BMI)" = "continuous",
        "Binary (e.g., disease yes/no)" = "binary",
        "Time-to-event (e.g., survival data)" = "survival",
        "Count data (e.g., number of cases)" = "count",
        "Ordinal (e.g., disease severity)" = "ordinal"
      )
    }
    
    # Create the radio buttons
    radioButtons("outcomeType", 
                 "What is your outcome variable type?",
                 choices = outcome_choices)
  })
  
  # Add this new validation observer
  observe({
    # If user somehow selects an invalid combination
    if(input$studyDesign == "cross_sectional" && !is.null(input$outcomeType) && input$outcomeType == "survival") {
      showNotification(
        "Time-to-event outcomes cannot be measured in cross-sectional studies. Please select a different outcome type.",
        type = "warning"
      )
      # Reset outcome type
      updateRadioButtons(session, "outcomeType", selected = character(0))
    }
  })
  
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
    
    # Add early return if study design is not yet selected
    if(is.null(input$studyDesign)) {
      return()
    }
    
    # Define which study designs allow difference measures
    difference_allowed <- input$studyDesign %in% c("cohort", "rct", "cross_sectional", "ecological")
    
    # Define measure availability by outcome type
    measure_availability <- if(!is.null(input$outcomeType)) {
      switch(input$outcomeType,
             "survival" = list(allowed = "both", message = "Both difference and ratio measures are available for survival analysis"),
             "count" = list(allowed = "ratio", message = "Only ratio measures (rate ratios) are available for count data"),
             "ordinal" = list(allowed = "ratio", message = "Only ratio measures (proportional odds ratios) are available for ordinal outcomes"),
             list(allowed = "both", message = NULL)
      )
    } else {
      return()  # Return early if outcome type not yet selected
    }
    
    # Combine study design and outcome type constraints
    final_allowed <- if(measure_availability$allowed == "ratio") {
      "ratio"
    } else if(!difference_allowed && input$outcomeType != "survival") {
      "ratio"
    } else {
      "both"
    }
    
    # Generate explanation text with NULL check
    explanation <- if(!difference_allowed && input$outcomeType != "survival") {
      switch(input$studyDesign,
             "case_control" = "Difference measures cannot be estimated from case-control studies (odds differences are not interpretable)",
             "nested_cc" = "Difference measures cannot be estimated from nested case-control studies (rate differences require full cohort data)",
             "case_cohort" = "Difference measures cannot be estimated from case-cohort studies (risk differences require full cohort data)",
             NULL
      )
    } else {
      measure_availability$message
    }
    
    # Create choices with appropriate labels for survival outcomes
    choices <- if(!is.null(input$outcomeType) && input$outcomeType == "survival") {
      c(
        "Difference measure (median survival time difference)" = "difference",
        "Ratio measure (hazard ratio)" = "ratio"
      )
    } else {
      c(
        "Difference measure (e.g., risk difference, rate difference)" = "difference",
        "Ratio measure (e.g., risk ratio, rate ratio)" = "ratio"
      )
    }
    
    tagList(
      radioButtons("effectMeasure", 
                   "What type of effect measure do you want to estimate?",
                   choices = choices,
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
      if(!difference_allowed && input$effectMeasure == "difference" && input$outcomeType != "survival") {  # Added survival check
        updateRadioButtons(session, "effectMeasure", selected = "ratio")
      }
    }
    
    # Enforce outcome-specific constraints
    if(!is.null(input$outcomeType)) {
      ratio_only <- input$outcomeType %in% c("count", "ordinal")  # Removed "survival" from this list
      if(ratio_only && input$effectMeasure == "difference") {
        updateRadioButtons(session, "effectMeasure", selected = "ratio")
      }
    }
    
    # Force binary outcome for case-control studies
    if(input$studyDesign %in% c("case_control", "nested_cc") && 
       !is.null(input$outcomeType) && 
       input$outcomeType != "binary") {
      updateRadioButtons(session, 
                         "outcomeType",
                         selected = "binary")
      showNotification(
        "Case-control studies can only have binary outcomes. Outcome type has been updated.",
        type = "warning",
        duration = 5
      )
    }
  })
  
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
    # First check if we have all required inputs
    if(is.null(input$outcomeType) || is.null(input$studyDesign)) {
      return(HTML("<div class='alert alert-info'>
        Please complete the selections above to see analysis recommendations.
      </div>"))
    }
    
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
    
    # Get sample size warning if needed
    sample_size_warning <- if(input$sampleSize < 30) {
      "<div class='alert alert-warning'>
        <strong>⚠️ Small Sample Size Warning:</strong> Consider non-parametric methods and exact tests.
      </div>"
    } else {
      ""
    }
    
    # Get the appropriate analysis recommendations
    main_analysis_md <- if(!is.null(input$outcomeType)) {
      switch(input$outcomeType,
             "continuous" = get_continuous_analysis_md(input$predictorType, input$groupNum),
             "binary" = get_binary_analysis_md(input$studyDesign, input$predictorType, input$effectMeasure, input$matchingDesign),
             "survival" = get_survival_analysis_md(input$predictorType, input$effectMeasure),  # Add effect_measure parameter
             "count" = get_count_analysis_md(),
             "ordinal" = get_ordinal_analysis_md(input$predictorType),
             "Please select an outcome type to see analysis recommendations.")
    } else {
      "Please select an outcome type to see analysis recommendations."
    }
    
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
    effect_interpretation <- get_effect_measure_interpretation(
      input$outcomeType, 
      input$studyDesign, 
      input$predictorType, 
      input$effectMeasure
    )
    
    # Get R code template
    r_code <- get_r_code_template(
      input$outcomeType, 
      input$studyDesign, 
      input$predictorType, 
      input$effectMeasure,
      if(!is.null(input$matchingDesign)) input$matchingDesign else "unmatched"
    )
    
    # Process additional considerations
    additional_considerations <- ""
    if(length(input$additionalConsiderations) > 0) {
      additional_list <- list(
        repeated = "### Repeated Measures Analysis\n- Mixed effects models\n- GEE\n- Repeated measures ANOVA",
        clustered = "### Clustering Adjustment\n- Multilevel models\n- Cluster-robust standard errors",
        missing = "### Missing Data Handling\n- Multiple imputation\n- Sensitivity analyses",
        multiple_testing = "### Multiple Comparison Adjustment\n- Bonferroni correction\n- FDR\n- Holm's method"
      )
      
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
- Matching variables treated as nuisance parameters

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
  
  get_survival_analysis_md <- function(predictor_type, effect_measure) {
    # Ensure effect_measure has a default value if not provided
    if(is.null(effect_measure)) {
      effect_measure <- "ratio"  # Default to ratio if not specified
    }
    
    if(effect_measure == "difference") {
      return("### Primary Analysis
- Restricted mean survival time (RMST) differences
- Median survival time differences using Kaplan-Meier estimates

### Hypothesis Test
- Test of survival curve equality using log-rank test
- Confidence intervals for median differences via bootstrapping

### Small Sample Alternative
- Permutation tests for survival differences
- Exact confidence intervals for median differences

### Alternatives
- Restricted mean survival time (RMST) differences at specific time points
- Differences in survival probabilities at fixed time points
- Pseudo-observation based regression for survival differences

### Model Diagnostics
- Assessment of censoring patterns between groups
- Test of equality of censoring distributions
- Evaluation of follow-up adequacy")
    } else {
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
    
    if(outcome_type == "survival" && effect_measure == "difference") {
      return("Differences in median survival time provide an intuitive measure of treatment effect that is directly interpretable in the original time scale. Unlike hazard ratios, these differences maintain their interpretation even when proportional hazards is violated. The method requires estimation of the median survival in each group through Kaplan-Meier curves, with bootstrap resampling recommended for confidence interval construction. RMST differences offer an alternative when median survival is not reached in one or both groups. While survival differences may vary across follow-up time, they provide valuable complementary information to ratio measures for clinical decision making and communication of results to non-technical audiences.")
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
    
    if(outcome_type == "survival" && effect_measure == "difference") {
      return("```r
# Load required packages
library(survival)
library(boot)

# Fit Kaplan-Meier curves
km_fit <- survfit(Surv(time, event) ~ exposure, data = data)

# Calculate median survival times
med_surv <- summary(km_fit)$table[,'median']
med_diff <- diff(med_surv)

# Bootstrap function for confidence intervals
boot_med_diff <- function(data, indices) {
  # Resample data
  boot_data <- data[indices,]
  
  # Fit KM curves to bootstrap sample
  boot_km <- survfit(Surv(time, event) ~ exposure, data = boot_data)
  boot_med <- summary(boot_km)$table[,'median']
  
  # Return median difference
  return(diff(boot_med))
}

# Perform bootstrap
boot_results <- boot(data = data, 
                    statistic = boot_med_diff,
                    R = 1000)

# Calculate 95% CI
ci <- boot.ci(boot_results, type = 'bca')

# For RMST differences at specific timepoint
library(survRM2)
rmst_diff <- rmst2(time = data$time,
                   status = data$event,
                   arm = data$exposure,
                   tau = max(data$time))

# Visualize survival curves
plot(km_fit, 
     xlab = 'Time',
     ylab = 'Survival Probability',
     main = 'Kaplan-Meier Survival Curves')
```")
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
                                 "survival" = get_survival_analysis_md(input$predictorType, input$effectMeasure),  # Add effect_measure parameter
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