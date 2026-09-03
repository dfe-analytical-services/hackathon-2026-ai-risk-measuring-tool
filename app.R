# ============================================================
# AI RiskCheck
# Responsible AI Risk Self-Assessment Tool
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(DT)
library(scales)
library(httr2)
library(jsonlite)

# ============================================================
# 1. ASSESSMENT MATRIX
# ============================================================

questions <- tribble(

  ~id, ~domain, ~question, ~help_text, ~type, ~weight,

  # PURPOSE ---------------------------------------------------

  "purpose_1",
  "Purpose and appropriateness",
  "Is the problem or use case clearly defined?",
  "AI should address a clear analytical problem or user need.",
  "control",
  1,

  "purpose_2",
  "Purpose and appropriateness",
  "Have non-AI alternatives been considered?",
  "Consider whether conventional analytical or automation methods could meet the same need.",
  "control",
  1,

  "purpose_3",
  "Purpose and appropriateness",
  "Does using AI provide a clear and measurable benefit?",
  "Benefits could include improved efficiency, quality, accessibility or consistency.",
  "control",
  1,


  # AI INVOLVEMENT --------------------------------------------

  "ai_1",
  "Extent of AI use",
  "How much responsibility is delegated to AI?",
  "Consider whether AI assists, generates, analyses, recommends or makes decisions.",
  "risk",
  2,

  "ai_2",
  "Extent of AI use",
  "Does the AI produce recommendations that could influence decisions?",
  "AI-generated recommendations usually require greater assurance than simple assistance.",
  "risk",
  2,

  "ai_3",
  "Extent of AI use",
  "Can the AI trigger actions without human approval?",
  "Examples include sending messages, updating systems or changing data.",
  "risk",
  3,


  # DATA ------------------------------------------------------

  "data_1",
  "Data and privacy",
  "What is the highest sensitivity of data processed by the AI?",
  "Consider public, internal, personal, sensitive or confidential information.",
  "risk",
  3,

  "data_2",
  "Data and privacy",
  "Is only the minimum necessary data supplied to the AI?",
  "Data should be minimised where possible.",
  "control",
  2,

  "data_3",
  "Data and privacy",
  "Are data retention arrangements understood?",
  "Consider whether prompts, files or outputs are retained.",
  "control",
  2,

  "data_4",
  "Data and privacy",
  "Is it understood whether inputs can be used to train the model?",
  "Users should understand how information supplied to the AI may subsequently be used.",
  "control",
  2,

  "data_5",
  "Data and privacy",
  "Has privacy or data protection advice been obtained where required?",
  "This is particularly important where personal data is involved.",
  "control",
  2,


  # IMPACT ----------------------------------------------------

  "impact_1",
  "Impact and consequences",
  "How serious would the consequences be if the AI output were materially wrong?",
  "Consider impacts on analysis, policy, funding, services or individuals.",
  "risk",
  4,

  "impact_2",
  "Impact and consequences",
  "How widely could an incorrect AI output affect users or stakeholders?",
  "Consider whether impact is limited to one analyst or could extend to the public or identifiable individuals.",
  "risk",
  3,

  "impact_3",
  "Impact and consequences",
  "Could the AI influence policy, funding or operational decisions?",
  "Consider both direct and indirect influence.",
  "risk",
  3,


  # QUALITY ---------------------------------------------------

  "quality_1",
  "Accuracy and analytical quality",
  "Are AI-generated outputs independently checked?",
  "Important AI-generated analytical outputs should be independently validated.",
  "control",
  3,

  "quality_2",
  "Accuracy and analytical quality",
  "Are numerical claims checked against authoritative source data?",
  "This is particularly important for analytical and statistical outputs.",
  "control",
  3,

  "quality_3",
  "Accuracy and analytical quality",
  "Is AI-generated code reviewed before use?",
  "Generated code should be tested and understood by a competent analyst.",
  "control",
  2,

  "quality_4",
  "Accuracy and analytical quality",
  "Has the system been tested against known examples?",
  "A representative test dataset can help identify systematic errors.",
  "control",
  2,

  "quality_5",
  "Accuracy and analytical quality",
  "Are accuracy or performance measures recorded?",
  "Performance should be measurable where possible.",
  "control",
  2,

  "quality_6",
  "Accuracy and analytical quality",
  "Are model, prompt or configuration changes retested?",
  "Changes should not be introduced without checking their impact.",
  "control",
  2,


  # FAIRNESS --------------------------------------------------

  "fairness_1",
  "Bias, fairness and ethics",
  "Could AI outputs affect people or groups differently?",
  "Consider demographic groups and protected characteristics.",
  "risk",
  3,

  "fairness_2",
  "Bias, fairness and ethics",
  "Has potential bias or differential impact been assessed?",
  "Consider whether model performance differs across groups.",
  "control",
  2,

  "fairness_3",
  "Bias, fairness and ethics",
  "Could false positives or false negatives disproportionately affect a group?",
  "Consider who could be harmed by incorrect AI outputs.",
  "risk",
  2,


  # HUMAN OVERSIGHT -------------------------------------------

  "human_1",
  "Human oversight",
  "What level of human review takes place before AI outputs are used?",
  "Meaningful human review is stronger than simple approval.",
  "control",
  4,

  "human_2",
  "Human oversight",
  "Is there a named person accountable for the final output?",
  "Accountability should remain clear even when AI is used.",
  "control",
  3,

  "human_3",
  "Human oversight",
  "Can the reviewer realistically identify and challenge AI errors?",
  "Consider expertise, workload and access to supporting information.",
  "control",
  3,


  # SECURITY --------------------------------------------------

  "security_1",
  "Security and robustness",
  "Is the AI service approved for the information being processed?",
  "Consider departmental security and information assurance requirements.",
  "control",
  4,

  "security_2",
  "Security and robustness",
  "Are appropriate access controls in place?",
  "Access should be limited to authorised users.",
  "control",
  2,

  "security_3",
  "Security and robustness",
  "Could users or external documents manipulate the AI system?",
  "For LLMs this can include prompt injection or malicious retrieved content.",
  "risk",
  3,

  "security_4",
  "Security and robustness",
  "Can the AI access other systems or tools?",
  "Tool-enabled AI can create additional operational and security risks.",
  "risk",
  3,

  "security_5",
  "Security and robustness",
  "Are logs or audit records retained?",
  "Logs can support monitoring, investigation and assurance.",
  "control",
  2,


  # TRANSPARENCY ----------------------------------------------

  "transparency_1",
  "Transparency and explainability",
  "Is the use of AI documented?",
  "It should be clear where AI contributes materially to the analytical process.",
  "control",
  2,

  "transparency_2",
  "Transparency and explainability",
  "Are users informed where AI materially contributes to outputs?",
  "Disclosure should be proportionate to the role AI plays.",
  "control",
  2,

  "transparency_3",
  "Transparency and explainability",
  "Are limitations and uncertainties communicated?",
  "Users should understand important limitations of AI-supported outputs.",
  "control",
  2,

  "transparency_4",
  "Transparency and explainability",
  "Can important conclusions be traced back to evidence or source information?",
  "Traceability supports analytical assurance and reproducibility.",
  "control",
  3,

  "transparency_5",
  "Transparency and explainability",
  "Can affected users challenge an AI-supported outcome where appropriate?",
  "Higher-impact uses may require routes for contestability or redress.",
  "control",
  2,


  # GOVERNANCE ------------------------------------------------

  "governance_1",
  "Governance and lifecycle",
  "Is there a named project owner?",
  "Ownership should remain clear throughout the AI lifecycle.",
  "control",
  2,

  "governance_2",
  "Governance and lifecycle",
  "Is there a named analytical owner?",
  "Someone should remain accountable for analytical quality.",
  "control",
  2,

  "governance_3",
  "Governance and lifecycle",
  "Is AI performance monitored after implementation?",
  "Models and use patterns may change over time.",
  "control",
  3,

  "governance_4",
  "Governance and lifecycle",
  "Is there an escalation process for incidents or unexpected behaviour?",
  "Teams should know what to do when AI creates an unexpected risk.",
  "control",
  3,

  "governance_5",
  "Governance and lifecycle",
  "Is there a process for stopping or decommissioning the AI system?",
  "Operational AI systems should have a clear exit process.",
  "control",
  2,

  "governance_6",
  "Governance and lifecycle",
  "Are model, prompt or data-source changes controlled?",
  "Material changes should be documented and reassessed.",
  "control",
  2,


  # SKILLS ----------------------------------------------------

  "skills_1",
  "Skills and capability",
  "Does the team have sufficient AI expertise?",
  "Teams should understand the technology being used and its limitations.",
  "control",
  2,

  "skills_2",
  "Skills and capability",
  "Does the team have sufficient analytical expertise?",
  "AI should support rather than replace appropriate analytical expertise.",
  "control",
  2,

  "skills_3",
  "Skills and capability",
  "Have relevant specialist teams been consulted where required?",
  "This could include analytical QA, security, information assurance, legal or data protection.",
  "control",
  2
)


# ============================================================
# 2. RESPONSE OPTIONS
# ============================================================

risk_options <- c(
  "None / negligible" = 0,
  "Low" = 1,
  "Moderate" = 2,
  "High" = 3,
  "Very high" = 4
)

control_options <- c(
  "Not implemented" = 0,
  "Limited" = 1,
  "Partially implemented" = 2,
  "Mostly implemented" = 3,
  "Fully implemented" = 4
)


# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- page_navbar(

  title = "🕵️ AI RiskCheck",

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),

  # ----------------------------------------------------------
  # START PAGE
  # ----------------------------------------------------------

  nav_panel(

    "Start",

    div(

      class = "container mt-4",

      card(

        card_header(
          h2("🕵️ AI RiskCheck")
        ),

        h4("Responsible AI Risk Self-Assessment"),

        p(
          "AI RiskCheck helps analysts identify potential risks associated ",
          "with using artificial intelligence in analytical work."
        ),

        p(
          "It considers the nature of the AI use case, the potential ",
          "consequences if something goes wrong, and the safeguards ",
          "used to manage those risks."
        ),

        hr(),

        strong("AI RiskCheck is a decision-support tool."),

        p(
          "It does not replace professional judgement, analytical QA, ",
          "information assurance, security, legal or data protection processes."
        )

      ),

      br(),

      card(

        card_header(
          h4("Tell AI RiskCheck about your use case")
        ),

        textInput(
          "project_name",
          "Project / use case name",
          placeholder = "e.g. Publication QA assistant"
        ),

        textAreaInput(
          "project_description",
          "Briefly describe how AI is being used",
          rows = 5,
          placeholder = paste(
            "For example: An LLM is used to compare statistical",
            "publication commentary against underlying CSV data."
          )
        ),

        selectInput(
          "lifecycle",
          "Current lifecycle stage",
          choices = c(
            "Exploring",
            "Prototype",
            "Pilot",
            "Operational",
            "Scaled"
          )
        ),

        selectInput(
          "ai_type",
          "Type of AI",
          choices = c(
            "Generative AI / LLM",
            "LLM with RAG",
            "AI agent",
            "Machine learning model",
            "Natural language processing",
            "Computer vision",
            "Recommendation system",
            "Other"
          )
        ),

        selectInput(
          "audience",
          "Who will use or be affected by the output?",
          choices = c(
            "Individual analyst",
            "Analytical team",
            "Internal DfE users",
            "Policy / operational teams",
            "Senior decision makers",
            "Public",
            "Identifiable individuals"
          )
        )

      )

    )

  ),


  # ----------------------------------------------------------
  # ASSESSMENT PAGE
  # ----------------------------------------------------------

  nav_panel(

    "Assessment",

    layout_sidebar(

      sidebar = sidebar(

        h4("AI RiskCheck progress"),

        textOutput("progress_text"),

        br(),

        uiOutput("progress_bar"),

        hr(),

        p(
          "Higher-risk answers will increase the inherent risk score."
        ),

        p(
          "Strong safeguards reduce the residual risk."
        )

      ),

      div(

        class = "p-3",

        uiOutput("questions_ui")

      )

    )

  ),


  # ----------------------------------------------------------
  # RESULTS PAGE
  # ----------------------------------------------------------

  nav_panel(

    "Results",

    div(
      accordion(

        accordion_panel(
          "Overall Risk Summary",

          fluidRow(
            column(4, uiOutput("risk_badge")),
            column(4, h2(textOutput("residual_score_text"))),
            column(4, h2(textOutput("control_label")))
          )
        ),

        accordion_panel(
          "Risk Profile",
          plotOutput("risk_plot", height = "450px")
        ),

        accordion_panel(
          "Areas Requiring Attention",
          DTOutput("risk_table")
        ),

        accordion_panel(
          "Recommended Assurance",
          uiOutput("recommendations")
        ),

        accordion_panel(
          "What Should Happen Next?",
          uiOutput("overall_action")
        )
      ),

      card(
        full_screen = TRUE,
        card_header(
          "🕵️ AI RiskCheck interpretation"
        ),

        p(
          class = "text-muted",
          paste(
            "Generate a plain-English interpretation using the project",
            "details and completed assessment answers."
          )
        ),

        actionButton(
          "generate_ai_summary",
          "Generate AI interpretation",
          class = "btn-primary",
          icon = icon("wand-magic-sparkles")
        ),

        br(),
        br(),

        uiOutput("ai_summary")
      ),

      br(),

      downloadButton(
        "download_assessment",
        "Download AI RiskCheck assessment"
      ),

      br(),
      br()

    )

  )

)



# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {


  # ----------------------------------------------------------
  # DISPLAY QUESTIONS
  # ----------------------------------------------------------

  output$questions_ui <- renderUI({

    domains <- unique(questions$domain)

    tagList(

      map(

        domains,

        function(domain_name) {

          domain_questions <- questions %>%
            filter(domain == domain_name)

          card(

            class = "mb-4",

            card_header(
              h4(domain_name)
            ),

            tagList(

              map(

                seq_len(nrow(domain_questions)),

                function(i) {

                  q <- domain_questions[i, ]

                  choices <-
                    if (q$type == "risk") {
                      risk_options
                    } else {
                      control_options
                    }

                  div(

                    class = "mb-4",

                    h5(q$question),

                    p(
                      class = "text-muted",
                      q$help_text
                    ),

                    radioButtons(
                      inputId = q$id,
                      label = NULL,
                      choices = choices,
                      selected = character(0),
                      inline = FALSE
                    ),

                    hr()

                  )

                }

              )

            )

          )

        }

      )

    )

  })


  # ----------------------------------------------------------
  # COLLECT ANSWERS
  # ----------------------------------------------------------

  answers <- reactive({

    questions %>%

      mutate(

        response = map_dbl(

          id,

          function(question_id) {

            value <- input[[question_id]]

            if (
              is.null(value) ||
              length(value) == 0 ||
              value == ""
            ) {

              NA_real_

            } else {

              as.numeric(value)

            }

          }

        )

      )

  })


  # ----------------------------------------------------------
  # PROGRESS
  # ----------------------------------------------------------

  completion <- reactive({

    mean(
      !is.na(answers()$response)
    )

  })


  output$progress_text <- renderText({

    completed <-
      sum(
        !is.na(answers()$response)
      )

    paste0(
      completed,
      " of ",
      nrow(questions),
      " questions completed"
    )

  })


  output$progress_bar <- renderUI({

    progress <-
      round(
        completion() * 100
      )

    div(

      class = "progress",

      div(

        class = "progress-bar",

        role = "progressbar",

        style = paste0(
          "width:",
          progress,
          "%"
        ),

        paste0(
          progress,
          "%"
        )

      )

    )

  })


  # ----------------------------------------------------------
  # SCORE ANSWERS
  # ----------------------------------------------------------

  scored_answers <- reactive({

    answers() %>%

      mutate(

        adjusted_score =
          case_when(

            type == "risk" ~ response,

            # Reverse controls:
            # fully implemented = low residual concern
            # not implemented = high residual concern

            type == "control" ~ 4 - response,

            TRUE ~ NA_real_

          ),

        weighted_score =
          adjusted_score * weight,

        maximum_score =
          4 * weight

      )

  })


  # ----------------------------------------------------------
  # INHERENT RISK
  # ----------------------------------------------------------

  inherent_score <- reactive({

    dat <- answers() %>%
      filter(
        type == "risk",
        !is.na(response)
      )

    if (nrow(dat) == 0) {
      return(0)
    }

    sum(
      dat$response * dat$weight
    ) /
      sum(
        4 * dat$weight
      ) * 100

  })


  # ----------------------------------------------------------
  # CONTROL STRENGTH
  # ----------------------------------------------------------

  control_score <- reactive({

    dat <- answers() %>%
      filter(
        type == "control",
        !is.na(response)
      )

    if (nrow(dat) == 0) {
      return(0)
    }

    sum(
      dat$response * dat$weight
    ) /
      sum(
        4 * dat$weight
      ) * 100

  })


  # ----------------------------------------------------------
  # RED FLAGS
  # ----------------------------------------------------------

  red_flags <- reactive({

    flags <- character(0)

    # High impact + weak human oversight

    if (
      !is.null(input$impact_1) &&
      !is.null(input$human_1)
    ) {

      if (
        as.numeric(input$impact_1) >= 3 &&
        as.numeric(input$human_1) <= 1
      ) {

        flags <- c(
          flags,
          "High-impact AI use has insufficient human oversight."
        )

      }

    }


    # Sensitive data + unapproved service

    if (
      !is.null(input$data_1) &&
      !is.null(input$security_1)
    ) {

      if (
        as.numeric(input$data_1) >= 3 &&
        as.numeric(input$security_1) <= 1
      ) {

        flags <- c(
          flags,
          "Sensitive information may be processed using an AI service without sufficient approval."
        )

      }

    }


    # Autonomous action + weak oversight

    if (
      !is.null(input$ai_3) &&
      !is.null(input$human_1)
    ) {

      if (
        as.numeric(input$ai_3) >= 3 &&
        as.numeric(input$human_1) <= 1
      ) {

        flags <- c(
          flags,
          "AI can trigger actions with insufficient human approval."
        )

      }

    }


    flags

  })


  # ----------------------------------------------------------
  # RESIDUAL RISK
  # ----------------------------------------------------------

  residual_score <- reactive({

    inherent <- inherent_score()

    controls <- control_score()

    # Controls can reduce inherent risk,
    # but never completely remove it.

    score <-
      inherent *
      (
        1 -
          (controls / 100 * 0.60)
      )

    score <- max(
      score,
      0
    )


    # Red flag override

    if (
      length(red_flags()) > 0
    ) {

      score <- max(
        score,
        75
      )

    }

    score

  })


  # ----------------------------------------------------------
  # RISK LABEL
  # ----------------------------------------------------------

  risk_label <- reactive({

    score <- residual_score()

    if (
      length(red_flags()) > 0 &&
      score >= 75
    ) {

      return("STOP / ESCALATE")

    }

    case_when(

      score < 20 ~ "LOW",

      score < 40 ~ "MODERATE",

      score < 60 ~ "HIGH",

      score < 75 ~ "VERY HIGH",

      TRUE ~ "STOP / ESCALATE"

    )

  })


  # ----------------------------------------------------------
  # LABEL HELPERS
  # ----------------------------------------------------------

  score_to_label <- function(score) {

    case_when(

      score < 20 ~ "Low",

      score < 40 ~ "Moderate",

      score < 60 ~ "High",

      score < 75 ~ "Very high",

      TRUE ~ "Critical"

    )

  }


  control_to_label <- function(score) {

    case_when(

      score >= 80 ~ "Strong",

      score >= 60 ~ "Good",

      score >= 40 ~ "Moderate",

      score >= 20 ~ "Weak",

      TRUE ~ "Very weak"

    )

  }

  # ============================================================
  # DATABRICKS LLM
  # ============================================================

  config <- jsonlite::fromJSON("config.json")

  databricks_token <- config$DATABRICKS_TOKEN

  if (
    is.null(databricks_token) ||
    !nzchar(databricks_token)
  ) {
    stop("DATABRICKS_TOKEN is missing from config.json")
  }


  call_databricks_llm <- function(prompt) {

    response <-
      httr2::request(
        paste0(
          "https://adb-5037484389568426.6.azuredatabricks.net",
          "/serving-endpoints/chat/completions"
        )
      ) %>%
      httr2::req_headers(
        Authorization = paste("Bearer", databricks_token)
      ) %>%
      httr2::req_body_json(
        list(
          model = "databricks-claude-opus-4-7",
          messages = list(
            list(
              role = "system",
              content = paste(
                "You are an expert Responsible AI assessor.",
                "Base your assessment only on the supplied information.",
                "Do not invent missing facts.",
                "Clearly identify incomplete assessment information."
              )
            ),
            list(
              role = "user",
              content = prompt
            )
          ),
          max_tokens = 2000
        ),
        auto_unbox = TRUE
      ) %>%
      httr2::req_timeout(60) %>%
      httr2::req_error(
        body = function(resp) {
          paste(
            "Databricks request failed:",
            httr2::resp_body_string(resp)
          )
        }
      ) %>%
      httr2::req_perform()

    result <- httr2::resp_body_json(
      response,
      simplifyVector = FALSE
    )

    result$choices[[1]]$message$content
  }


  # ----------------------------------------------------------
  # SUMMARY OUTPUTS
  # ----------------------------------------------------------

  output$inherent_label <- renderText({

    score_to_label(
      inherent_score()
    )

  })


  output$control_label <- renderText({

    control_to_label(
      control_score()
    )

  })


  output$inherent_score_text <- renderText({

    paste0(
      round(inherent_score()),
      "%"
    )

  })


  output$control_score_text <- renderText({

    paste0(
      round(control_score()),
      "%"
    )

  })


  output$residual_score_text <- renderText({

    paste0(
      round(residual_score()),
      "%"
    )

  })


  # ----------------------------------------------------------
  # RISK BADGE
  # ----------------------------------------------------------

  output$risk_badge <- renderUI({

    label <- risk_label()

    badge_class <-
      case_when(

        label == "LOW" ~ "success",

        label == "MODERATE" ~ "warning",

        label == "HIGH" ~ "warning",

        label == "VERY HIGH" ~ "danger",

        TRUE ~ "danger"

      )

    span(

      class =
        paste0(
          "badge bg-",
          badge_class
        ),

      style =
        "font-size:24px; padding:15px;",

      paste(
        "AI Risk:",
        label
      )

    )

  })


  # ----------------------------------------------------------
  # DOMAIN SCORES
  # ----------------------------------------------------------

  domain_scores <- reactive({

    scored_answers() %>%

      filter(
        !is.na(adjusted_score)
      ) %>%

      group_by(domain) %>%

      summarise(

        risk_score =
          100 *
          sum(weighted_score) /
          sum(maximum_score),

        .groups = "drop"

      ) %>%

      arrange(
        desc(risk_score)
      )

  })

  # ----------------------------------------------------------
  # LLM Prompt
  # ----------------------------------------------------------

  llm_prompt <- reactive({

    scores <- domain_scores()

    answer_summary <-
      scored_answers() %>%
      filter(!is.na(response)) %>%
      transmute(
        answer = paste0(
          "- Domain: ", domain,
          "\n  Question: ", question,
          "\n  Response score: ", response, " out of 4",
          "\n  Concern score: ", adjusted_score, " out of 4"
        )
      ) %>%
      pull(answer) %>%
      paste(collapse = "\n")

    flag_text <-
      if (length(red_flags()) == 0) {
        "None identified."
      } else {
        paste(
          paste0("- ", red_flags()),
          collapse = "\n"
        )
      }

    paste0(
      "Assess the following Responsible AI use case.\n\n",

      "PROJECT INFORMATION\n",
      "Project name: ",
      input$project_name,
      "\n",

      "Description: ",
      input$project_description,
      "\n",

      "Lifecycle stage: ",
      input$lifecycle,
      "\n",

      "Type of AI: ",
      input$ai_type,
      "\n",

      "Audience or affected users: ",
      input$audience,
      "\n\n",

      "CALCULATED ASSESSMENT\n",
      "Overall risk label: ",
      risk_label(),
      "\n",

      "Residual risk: ",
      round(residual_score(), 1),
      "%\n",

      "Inherent risk: ",
      round(inherent_score(), 1),
      "%\n",

      "Control strength: ",
      round(control_score(), 1),
      "%\n\n",

      "DOMAIN CONCERN SCORES\n",
      paste0(
        "- ",
        scores$domain,
        ": ",
        round(scores$risk_score, 1),
        "%",
        collapse = "\n"
      ),
      "\n\n",

      "ASSESSMENT ANSWERS\n",
      answer_summary,
      "\n\n",

      "MANDATORY RED FLAGS\n",
      flag_text,
      "\n\n",

      "Provide the following sections:\n",
      "1. Overall assessment\n",
      "2. Principal risks\n",
      "3. Existing strengths and safeguards\n",
      "4. Priority actions\n",
      "5. Information still required\n\n",

      "Use clear British English. ",
      "Do not recalculate or contradict the supplied numerical scores. ",
      "Do not infer controls that have not been recorded. ",
      "Limit each section to 50 words except principal risks"
    )
  })

  output$ai_summary <- renderUI({

  if (input$generate_ai_summary == 0) {

      return(
        div(
          class = "alert alert-info",
          paste(
            "Complete the Start and Assessment pages, then select",
            "'Generate AI interpretation'."
          )
        )
      )

    }

    summary_text <- ai_summary_result()

    div(
      class = "p-3 border rounded bg-light",

      tags$div(
        style = "white-space: pre-wrap;",
        summary_text
      ),

      hr(),

      tags$small(
        class = "text-muted",
        paste(
          "This interpretation was generated by AI.",
          "Review it before relying on it."
        )
      )
    )
  })

  ai_summary_result <-
  eventReactive(
    input$generate_ai_summary,
    {

      req(nzchar(trimws(input$project_name)))
      req(nzchar(trimws(input$project_description)))
      req(completion() == 1)

      withProgress(
        message = "Generating AI RiskCheck interpretation",
        value = 0.5,
        {

          tryCatch(

            call_databricks_llm(
              llm_prompt()
            ),

            error = function(e) {

              paste0(
                "The AI interpretation could not be generated. ",
                conditionMessage(e)
              )

            }

          )

        }
      )

    },
    ignoreInit = TRUE
  )

  # ----------------------------------------------------------
  # RISK PROFILE CHART
  # ----------------------------------------------------------

  output$risk_plot <- renderPlot({

    dat <- domain_scores()

    req(
      nrow(dat) > 0
    )

    ggplot(
      dat,
      aes(
        x = reorder(
          domain,
          risk_score
        ),
        y = risk_score
      )
    ) +

      geom_col() +

      coord_flip() +

      geom_text(

        aes(
          label =
            paste0(
              round(risk_score),
              "%"
            )
        ),

        hjust = -0.1

      ) +

      scale_y_continuous(

        limits = c(
          0,
          110
        ),

        labels =
          function(x) {
            paste0(
              x,
              "%"
            )
          }

      ) +

      labs(

        x = NULL,

        y = "Risk / control concern",

        title =
          "AI RiskCheck risk profile"

      ) +

      theme_minimal(

        base_size = 13

      )

  })


  # ----------------------------------------------------------
  # AREAS REQUIRING ATTENTION
  # ----------------------------------------------------------

  output$risk_table <- renderDT({

    dat <-
      scored_answers() %>%

      filter(
        !is.na(adjusted_score)
      ) %>%

      arrange(
        desc(adjusted_score),
        desc(weight)
      ) %>%

      mutate(

        Concern =
          case_when(

            adjusted_score >= 4 ~
              "Very high",

            adjusted_score >= 3 ~
              "High",

            adjusted_score >= 2 ~
              "Moderate",

            adjusted_score >= 1 ~
              "Low",

            TRUE ~
              "No concern"

          )

      ) %>%

      select(

        Domain = domain,

        Question = question,

        Concern

      ) %>%

      filter(
        Concern %in%
          c(
            "Very high",
            "High",
            "Moderate"
          )
      )

    datatable(

      dat,

      options = list(

        pageLength = 10,

        dom = "tip"

      ),

      rownames = FALSE

    )

  })


  # ----------------------------------------------------------
  # OVERALL ACTION
  # ----------------------------------------------------------

  output$overall_action <- renderUI({

    label <- risk_label()

    if (
      label == "LOW"
    ) {

      tagList(

        h4("Proceed with standard assurance"),

        p(
          "The use case currently appears to present relatively limited AI risk."
        ),

        tags$ul(

          tags$li(
            "Apply normal analytical QA."
          ),

          tags$li(
            "Document where AI has been used."
          ),

          tags$li(
            "Review the assessment if the use case changes."
          )

        )

      )

    } else if (
      label == "MODERATE"
    ) {

      tagList(

        h4("Proceed with additional controls"),

        tags$ul(

          tags$li(
            "Document AI limitations."
          ),

          tags$li(
            "Introduce enhanced analytical QA."
          ),

          tags$li(
            "Confirm appropriate human oversight."
          ),

          tags$li(
            "Review data and security arrangements."
          )

        )

      )

    } else if (
      label %in%
        c(
          "HIGH",
          "VERY HIGH"
        )
    ) {

      tagList(

        h4("Enhanced assurance required"),

        tags$ul(

          tags$li(
            "Complete formal analytical QA."
          ),

          tags$li(
            "Establish a representative evaluation dataset."
          ),

          tags$li(
            "Document acceptance criteria."
          ),

          tags$li(
            "Review security and data risks."
          ),

          tags$li(
            "Confirm accountability and human oversight."
          ),

          tags$li(
            "Consider specialist assurance before deployment."
          )

        )

      )

    } else {

      tagList(

        h4("⛔ Do not proceed without review"),

        p(
          "AI RiskCheck has identified one or more mandatory escalation conditions."
        ),

        tags$ul(

          lapply(
            red_flags(),
            tags$li
          )

        ),

        p(
          strong(
            "Resolve these issues and reassess the use case before implementation."
          )
        )

      )

    }

  })


  # ----------------------------------------------------------
  # RECOMMENDATIONS
  # ----------------------------------------------------------

  output$recommendations <- renderUI({

    scores <- domain_scores()

    high_domains <-
      scores %>%

      filter(
        risk_score >= 40
      )

    recommendations <-
      character(0)


    if (
      "Accuracy and analytical quality" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Introduce independent analytical QA and test AI outputs against authoritative data."
        )

    }


    if (
      "Data and privacy" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Review data minimisation, retention, privacy and model-training arrangements."
        )

    }


    if (
      "Human oversight" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Strengthen meaningful human review and clearly identify who is accountable for the final output."
        )

    }


    if (
      "Security and robustness" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Complete appropriate security and information assurance review."
        )

    }


    if (
      "Bias, fairness and ethics" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Assess whether AI performance or outcomes differ across relevant groups."
        )

    }


    if (
      "Transparency and explainability" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Document AI use, limitations, evidence sources and important assumptions."
        )

    }


    if (
      "Governance and lifecycle" %in%
        high_domains$domain
    ) {

      recommendations <-
        c(
          recommendations,
          "Establish ownership, monitoring, change control and incident escalation arrangements."
        )

    }


    if (
      length(recommendations) == 0
    ) {

      recommendations <-
        "No major additional assurance activities were identified. Continue to apply proportionate analytical QA and review the assessment if the AI use case changes."

    }


    tagList(

      tags$ul(

        lapply(
          recommendations,
          tags$li
        )

      )

    )

  })


  # ----------------------------------------------------------
  # DOWNLOAD
  # ----------------------------------------------------------

  output$download_assessment <- downloadHandler(

    filename = function() {

      paste0(
        "AI_RiskCheck_Assessment_",
        Sys.Date(),
        ".csv"
      )

    },

    content = function(file) {

      results <-
        scored_answers() %>%

        mutate(

          project_name =
            input$project_name,

          lifecycle_stage =
            input$lifecycle,

          ai_type =
            input$ai_type,

          overall_risk =
            risk_label(),

          inherent_risk =
            round(
              inherent_score(),
              1
            ),

          control_strength =
            round(
              control_score(),
              1
            ),

          residual_risk =
            round(
              residual_score(),
              1
            )

        )

      write.csv(
        results,
        file,
        row.names = FALSE
      )

    }

  )

}


# ============================================================
# RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)