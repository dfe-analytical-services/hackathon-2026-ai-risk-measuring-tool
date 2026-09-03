# ============================================================
# AI RISKCHECK
# Responsible AI Risk Self-Assessment Tool
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)
library(ggplot2)
library(DT)

# Databricks
library(DBI)
library(odbc)

# Guidance retrieval
library(rvest)
library(xml2)
library(pdftools)



library(scales)

library(rmarkdown)

library(knitr)


# ============================================================
# 2. APP SETTINGS
# ============================================================

APP_NAME <- "AI RiskCheck"
APP_VERSION <- "1.6 Prototype"

APP_LAST_UPDATED <- "3 September 2026"
REFERENCES_LAST_REVIEWED <- "3 September 2026"

TOTAL_ASSESSMENT_QUESTIONS <- 25

CLASSIFICATION_LABEL <-
  "OFFICIAL-SENSITIVE — INTERNAL USE ONLY — DfE EMPLOYEES ONLY"


# ============================================================
# 3. LOCAL STORAGE
# ============================================================

SAVED_CASE_DIR <- "saved_cases"
FEEDBACK_DIR <- "feedback"
GUIDANCE_CACHE_DIR <- "guidance_cache"

dir.create(
  SAVED_CASE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  FEEDBACK_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  GUIDANCE_CACHE_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)


FEEDBACK_FILE <- file.path(
  FEEDBACK_DIR,
  "ai_riskcheck_feedback.csv"
)


# ============================================================
# 4. DATABRICKS CONNECTION
# ============================================================

#connect_to_databricks <- function() {
  
 # sql_path <- Sys.getenv(
    "DATABRICKS_SQL_PATH"
 # )
  
#  databricks_host <- Sys.getenv(
 #   "DATABRICKS_HOST"
#  )
  
 # databricks_token <- Sys.getenv(
 #   "DATABRICKS_TOKEN"
 # )
  
  
 # if (!nzchar(sql_path)) {
    
  #  stop(
      "DATABRICKS_SQL_PATH environment variable is missing."
 #   )
    
 # }
  
  
 # if (!nzchar(databricks_host)) {
    
 #   stop(
  #    "DATABRICKS_HOST environment variable is missing."
  #  )
    
 # }
  
  
 # if (!nzchar(databricks_token)) {
    
 #   stop(
      "DATABRICKS_TOKEN environment variable is missing."
  #  )
 #   
 # }
  
  
 # DBI::dbConnect(
    
 #   drv = odbc::databricks(),
    
  #  httpPath = sql_path,
    
  #  workspace = databricks_host,
    
  #  uid = "token",
    
   # pwd = databricks_token,
    
  #  useNativeQuery = FALSE
    
 # )
#}


# ============================================================
# 5. GUIDANCE ASSISTANT MODEL
# ============================================================

GUIDANCE_LLM_ENDPOINT <-
  "databricks-claude-opus-4-7"


# ============================================================
# 6. RESPONSE OPTIONS
# ============================================================

risk_options <- c(
  
  "None / negligible" = "0",
  "Low" = "1",
  "Moderate" = "2",
  "High" = "3",
  "Very high" = "4",
  "Not relevant" = "NR"
  
)


control_options <- c(
  
  "No" = "0",
  "Mostly no" = "1",
  "Partly" = "2",
  "Mostly yes" = "3",
  "Yes" = "4",
  "Not relevant" = "NR"
  
)


# ============================================================
# 7. 25-QUESTION RISK ASSESSMENT MATRIX
# ============================================================

questions <- tribble(
  
  ~id,
  ~domain,
  ~question,
  ~help_text,
  ~type,
  ~weight,
  ~source,
  ~principle,
  ~recommended_action,
  
  
  # ==========================================================
  # 1. PURPOSE, VALUE & APPROPRIATENESS
  # ==========================================================
  
  "purpose_1",
  "Purpose, value & appropriateness",
  "Is there a clearly defined problem or user need that the AI is intended to address?",
  paste(
    "AI should address a clearly understood problem or user need",
    "rather than being introduced simply because the technology is available."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide; AI Opportunities Action Plan",
  "Clear purpose and user need",
  paste(
    "Clearly define the problem, intended users and expected outcome",
    "before developing or implementing the AI solution."
  ),
  
  
  "purpose_2",
  "Purpose, value & appropriateness",
  "Have non-AI alternatives been considered and is AI a proportionate solution?",
  paste(
    "A conventional analytical, statistical, automation or digital approach",
    "may sometimes meet the need more simply and with less risk."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Use the right tool for the job",
  paste(
    "Compare the AI approach with reasonable non-AI alternatives",
    "and document why AI is appropriate."
  ),
  
  
  "purpose_3",
  "Purpose, value & appropriateness",
  "Are the expected benefits and measures of success clearly defined?",
  paste(
    "The use case should have measurable objectives so its value",
    "and effectiveness can be assessed."
  ),
  "control",
  5,
  "AI Opportunities Action Plan; NAO Good Practice Guide",
  "Evaluate value and outcomes",
  paste(
    "Define measurable benefits, success criteria and how the team",
    "will determine whether the use case is successful."
  ),
  
  
  # ==========================================================
  # 2. IMPACT & HUMAN OVERSIGHT
  # ==========================================================
  
  "impact_1",
  "Impact & human oversight",
  "How serious could the consequences be if the AI output were incorrect, incomplete or misleading?",
  paste(
    "Consider impacts on analytical conclusions, publications, policy,",
    "funding, services, operations, public trust or individuals."
  ),
  "risk",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Understand AI limitations and consequences",
  paste(
    "Increase testing, review and assurance in proportion to",
    "the potential consequences of an incorrect output."
  ),
  
  
  "impact_2",
  "Impact & human oversight",
  "Could the AI output influence significant policy, operational, funding, rights, entitlement or individual decisions?",
  paste(
    "AI that materially informs consequential decisions requires",
    "stronger assurance and meaningful human involvement."
  ),
  "risk",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Meaningful human control and accountability",
  paste(
    "Introduce enhanced assurance and documented human review",
    "before AI outputs influence consequential decisions."
  ),
  
  
  "impact_3",
  "Impact & human oversight",
  "Can the AI make decisions, trigger actions or change systems without human approval?",
  paste(
    "Greater AI autonomy can increase the consequences of errors,",
    "unexpected behaviour or malicious inputs."
  ),
  "risk",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Meaningful human control",
  paste(
    "Require human approval for consequential actions unless autonomous",
    "operation has been explicitly assessed, justified and assured."
  ),
  
  
  "human_1",
  "Impact & human oversight",
  "Is there meaningful human review by someone able to identify, challenge and intervene on AI outputs?",
  paste(
    "Human oversight should be meaningful. Simply approving an output",
    "without sufficient expertise, evidence or time is not enough."
  ),
  "control",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Meaningful human control",
  paste(
    "Introduce documented human review by a suitably competent person",
    "with authority and evidence to challenge the AI."
  ),
  
  
  # ==========================================================
  # 3. DATA, PRIVACY & LEGAL
  # ==========================================================
  
  "data_1",
  "Data, privacy & legal",
  "Is the data used by the AI accurate, sufficiently complete, representative and suitable for the intended purpose?",
  paste(
    "Poor-quality, incomplete or unrepresentative data can reduce",
    "reliability and contribute to misleading or biased outputs."
  ),
  "control",
  5,
  "NAO Good Practice Guide; Data and AI Ethics Framework",
  "Data quality and fitness for purpose",
  paste(
    "Assess and document data quality, provenance, representativeness,",
    "known limitations and fitness for purpose."
  ),
  
  
  "data_2",
  "Data, privacy & legal",
  "Does the AI process personal, sensitive, confidential or unpublished departmental information?",
  paste(
    "More sensitive information creates greater privacy, confidentiality,",
    "security and information-management risk."
  ),
  "risk",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Privacy, confidentiality and lawful use",
  paste(
    "Confirm that the AI service and environment are approved for",
    "the highest sensitivity of information being processed."
  ),
  
  
  "data_3",
  "Data, privacy & legal",
  "Are the legal basis, data provenance, licensing, copyright and permitted uses understood?",
  paste(
    "Teams should understand whether data, documents, code and other",
    "content can lawfully and appropriately be supplied to and used by AI."
  ),
  "control",
  5,
  "Data and AI Ethics Framework; UK Government AI Playbook; NAO Good Practice Guide",
  "Lawful use and data governance",
  paste(
    "Document relevant legal basis, provenance, licences, copyright",
    "restrictions and permitted uses."
  ),
  
  
  "data_4",
  "Data, privacy & legal",
  "Are data retention, processing location and provider use of prompts, files and outputs understood?",
  paste(
    "Teams should understand where information is processed, how long",
    "it is retained and whether it may be reused for training or other purposes."
  ),
  "control",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework; NAO Good Practice Guide",
  "Privacy and data governance",
  paste(
    "Confirm and document retention, processing location, deletion",
    "and provider training or reuse arrangements."
  ),
  
  
  # ==========================================================
  # 4. QUALITY, TESTING & RELIABILITY
  # ==========================================================
  
  "quality_1",
  "Quality, testing & reliability",
  "Has the AI been tested using representative examples, including relevant edge cases and known failure scenarios?",
  paste(
    "Testing should reflect realistic use as well as difficult, unusual",
    "or deliberately problematic inputs."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Testing, evaluation and robustness",
  paste(
    "Create a representative evaluation set and test normal cases,",
    "edge cases and known failure modes."
  ),
  
  
  "quality_2",
  "Quality, testing & reliability",
  "Are important AI-generated facts, evidence, citations, calculations, analytical conclusions or code independently verified?",
  paste(
    "This is particularly important where AI generates code,",
    "deliverables, evidence summaries or quantitative outputs."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Accuracy and analytical assurance",
  paste(
    "Verify important AI-generated content against authoritative evidence,",
    "source data or appropriate code and analytical testing."
  ),
  
  
  "quality_3",
  "Quality, testing & reliability",
  "Are measurable acceptance criteria and performance measures defined for the AI use case?",
  paste(
    "Teams should understand what acceptable performance looks like",
    "and what level of failure would prevent deployment."
  ),
  "control",
  5,
  "NAO Good Practice Guide; AI Opportunities Action Plan",
  "Evaluation and evidence",
  paste(
    "Define measurable performance criteria, acceptable error thresholds",
    "and conditions for proceeding, revising or stopping."
  ),
  
  
  "quality_4",
  "Quality, testing & reliability",
  "Can important AI-assisted outputs be traced to their evidence and reproduced or reconstructed where necessary?",
  paste(
    "Traceability and reproducibility support quality assurance,",
    "investigation and understanding of how outputs were produced."
  ),
  "control",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Transparency and reproducibility",
  paste(
    "Retain records of models, prompts, data sources, retrieved documents,",
    "settings and analytical decisions."
  ),
  
  
  # ==========================================================
  # 5. FAIRNESS, TRANSPARENCY & STAKEHOLDERS
  # ==========================================================
  
  "ethics_1",
  "Fairness, transparency & stakeholders",
  "Could the AI create unfair, biased or materially different outcomes for particular people or groups?",
  paste(
    "Consider protected characteristics, accessibility, vulnerable groups,",
    "representation in data and differences in model performance."
  ),
  "risk",
  5,
  "Data and AI Ethics Framework; UK Government AI Playbook",
  "Fairness and prevention of harm",
  paste(
    "Identify relevant groups and evaluate whether outputs, error rates",
    "or impacts differ unfairly between them."
  ),
  
  
  "transparency_1",
  "Fairness, transparency & stakeholders",
  "Is it clear to users where AI has been used and what its important limitations and uncertainties are?",
  paste(
    "Users should understand material AI involvement so that they can",
    "interpret outputs appropriately and avoid over-reliance."
  ),
  "control",
 5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Transparency and explainability",
  paste(
    "Document and communicate where AI is used, important limitations",
    "and when outputs should not be relied on."
  ),
  
  
  "stakeholder_1",
  "Fairness, transparency & stakeholders",
  "Have the people who use, rely on or may be affected by the AI been identified, and have wider impacts and routes for feedback or challenge been considered?",
  paste(
    "Consider end users, affected groups, accessibility, public trust",
    "and ways to report or challenge problems."
  ),
  "control",
  5,
  "Data and AI Ethics Framework; NAO Good Practice Guide",
  "Stakeholder engagement and societal impact",
  paste(
    "Identify relevant stakeholders and provide proportionate routes",
    "for feedback, challenge and redress."
  ),
  
  
  # ==========================================================
  # 6. SECURITY, PLATFORM & SUPPLIER
  # ==========================================================
  
  "security_1",
  "Security, platform & supplier",
  "Is the AI service or platform approved for the intended use, data and level of access?",
  paste(
    "The AI environment should meet departmental security,",
    "data-handling and access requirements."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Secure and appropriate AI use",
  paste(
    "Confirm that the platform is approved for the intended use",
    "and information before implementation."
  ),
  
  
  "security_2",
  "Security, platform & supplier",
  "Have relevant AI-specific security threats and access risks been assessed and mitigated?",
  paste(
    "Relevant risks may include prompt injection, data leakage,",
    "malicious retrieved content, model manipulation and excessive permissions."
  ),
  "control",
  5,
  "UK Government AI Playbook; NAO Good Practice Guide",
  "Security by design",
  paste(
    "Assess AI-specific threats, restrict access using least privilege",
    "and test important security controls."
  ),
  
  
  "supplier_1",
  "Security, platform & supplier",
  "Are important model, platform and supplier dependencies understood and appropriately managed?",
  paste(
    "Consider model ownership, open or proprietary models, supplier terms,",
    "resilience, model changes, versioning and dependency on third parties."
  ),
  "control",
  5,
  "NAO Good Practice Guide; UK Government AI Playbook",
  "Commercial and supplier risk",
  paste(
    "Document supplier and model dependencies, terms, versioning,",
    "resilience arrangements and mitigations."
  ),
  
  
  # ==========================================================
  # 7. GOVERNANCE, ACCOUNTABILITY & SKILLS
  # ==========================================================
  
  "governance_1",
  "Governance, accountability & skills",
  "Is there clear ownership, accountability and an appropriate route for review, escalation and incident management?",
  paste(
    "AI use cases should have clear responsibility for the system and",
    "its outputs, with routes to escalate concerns and manage incidents."
  ),
  "control",
  5,
  "NAO Use of AI in Government; UK Government AI Playbook; Data and AI Ethics Framework",
  "Accountability and organisational assurance",
  paste(
    "Assign a named accountable owner and document review, escalation",
    "and incident-management arrangements."
  ),
  
  
  "skills_1",
  "Governance, accountability & skills",
  "Does the team have sufficient AI, analytical and domain expertise to use, evaluate and challenge the system effectively?",
  paste(
    "Safe AI adoption requires people who understand the technology,",
    "its limitations, analytical context and subject matter."
  ),
  "control",
  5,
  "NAO Use of AI in Government; NAO Good Practice Guide; UK Government AI Playbook",
  "Skills and capability",
  paste(
    "Ensure appropriate AI, analytical and domain expertise is available",
    "and seek specialist support where necessary."
  ),
  
  
  # ==========================================================
  # 8. LIFECYCLE, MONITORING & SCALE
  # ==========================================================
  
  "lifecycle_1",
  "Lifecycle, monitoring & scale",
  "Is AI performance monitored after deployment and are material model, prompt, data or configuration changes controlled and retested?",
  paste(
    "AI behaviour may change because models, prompts, data sources,",
    "providers, user behaviour or surrounding environments change."
  ),
  "control",
  5,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Lifecycle management and continuous assurance",
  paste(
    "Introduce monitoring, change control and proportionate retesting",
    "following material changes."
  ),
  
  
  "lifecycle_2",
  "Lifecycle, monitoring & scale",
  "Before moving from exploration or pilot into operational or scaled use, has the AI been formally evaluated and can it be paused or stopped if necessary?",
  paste(
    "A successful prototype should not automatically become an operational",
    "service. Evidence, controls and ownership should be reviewed before scaling."
  ),
  "control",
  5,
  "NAO Good Practice Guide; AI Opportunities Action Plan; UK Government AI Playbook",
  "Pilot, evaluate and scale responsibly",
  paste(
    "Complete formal evaluation before operational deployment and establish",
    "pause, rollback or decommissioning arrangements."
  )
  
)


stopifnot(
  nrow(questions) ==
    TOTAL_ASSESSMENT_QUESTIONS
)


# ============================================================
# 8. GUIDANCE DOCUMENTS
# ============================================================

guidance_documents <- tribble(
  
  ~id,
  ~title,
  ~organisation,
  ~date,
  ~purpose,
  ~url,
  ~format,
  
  
  "nao_ai_government",
  "Use of artificial intelligence in government",
  "National Audit Office",
  "March 2024",
  "Review of government AI adoption, governance, skills, testing and scaling.",
  "https://www.nao.org.uk/wp-content/uploads/2024/03/use-of-artificial-intelligence-in-government.pdf",
  "pdf",
  
  
  "nao_good_practice",
  "Good practice guide for organisations using AI",
  "National Audit Office",
  "May 2026",
  "Good-practice guidance covering governance, data, security, evaluation and scaling.",
  "https://www.nao.org.uk/wp-content/uploads/2026/05/good-practice-guide-for-organisations-using-ai.pdf",
  "pdf",
  
  
  "ai_playbook",
  "Artificial Intelligence Playbook for the UK Government",
  "UK Government",
  "2025",
  "Government guidance for safe, responsible and effective AI use.",
  "https://www.gov.uk/government/publications/ai-playbook-for-the-uk-government/artificial-intelligence-playbook-for-the-uk-government-html",
  "html",
  
  
  "ai_action_plan",
  "AI Opportunities Action Plan",
  "UK Government",
  "2025",
  "Government approach to identifying, piloting, evaluating and scaling AI opportunities.",
  "https://www.gov.uk/government/publications/ai-opportunities-action-plan/ai-opportunities-action-plan",
  "html",
  
  
  "data_ai_ethics",
  "Data and AI Ethics Framework",
  "UK Government",
  "2025",
  "Framework covering transparency, accountability, fairness, privacy and societal impact.",
  "https://www.gov.uk/government/publications/data-ethics-framework/data-and-ai-ethics-framework",
  "html"
  
)


# ============================================================
# 9. GUIDANCE TEXT FUNCTIONS
# ============================================================

clean_guidance_text <- function(text) {
  
  text %>%
    
    str_replace_all(
      "\r",
      " "
    ) %>%
    
    str_replace_all(
      "\n{3,}",
      "\n\n"
    ) %>%
    
    str_replace_all(
      "[ \t]+",
      " "
    ) %>%
    
    str_trim()
  
}


read_html_guidance <- function(url) {
  
  page <-
    rvest::read_html(
      url
    )
  
  
  main_content <-
    page %>%
    rvest::html_elements(
      "main"
    )
  
  
  if (
    length(main_content) ==
    0
  ) {
    
    main_content <- page
    
  }
  
  
  text <-
    main_content %>%
    rvest::html_text2()
  
  
  clean_guidance_text(
    
    paste(
      text,
      collapse = "\n"
    )
    
  )
  
}


read_pdf_guidance <- function(
    url,
    document_id
) {
  
  local_file <-
    file.path(
      GUIDANCE_CACHE_DIR,
      paste0(
        document_id,
        ".pdf"
      )
    )
  
  
  if (
    !file.exists(
      local_file
    )
  ) {
    
    download.file(
      
      url = url,
      
      destfile =
        local_file,
      
      mode = "wb",
      
      quiet = TRUE
      
    )
    
  }
  
  
  pages <-
    
    suppressWarnings(
      
      suppressMessages(
        
        pdftools::pdf_text(
          local_file
        )
        
      )
      
    )
  
  
  if (
    length(pages) ==
    0 ||
    all(
      !nzchar(
        trimws(pages)
      )
    )
  ) {
    
    stop(
      paste(
        "No readable text could be extracted from",
        document_id
      )
    )
    
  }
  
  
  clean_guidance_text(
    
    paste(
      pages,
      collapse =
        "\n\n"
    )
    
  )
  
}


# ============================================================
# 10. LOAD / CACHE GUIDANCE
# ============================================================

load_guidance_documents <- function() {
  
  cache_file <-
    file.path(
      GUIDANCE_CACHE_DIR,
      "guidance_content.rds"
    )
  
  
  if (
    file.exists(
      cache_file
    )
  ) {
    
    message(
      "Loading AI RiskCheck guidance from local cache..."
    )
    
    
    return(
      readRDS(
        cache_file
      )
    )
    
  }
  
  
  message(
    "Building AI RiskCheck guidance cache..."
  )
  
  
  guidance_output <-
    
    map_dfr(
      
      seq_len(
        nrow(
          guidance_documents
        )
      ),
      
      function(i) {
        
        document <-
          guidance_documents[
            i,
          ]
        
        
        message(
          "Loading guidance: ",
          document$title
        )
        
        
        document_text <-
          
          tryCatch(
            
            {
              
              if (
                document$format ==
                "pdf"
              ) {
                
                read_pdf_guidance(
                  
                  url =
                    document$url,
                  
                  document_id =
                    document$id
                  
                )
                
              } else {
                
                read_html_guidance(
                  document$url
                )
                
              }
              
            },
            
            
            error = function(e) {
              
              warning(
                paste(
                  "Could not load",
                  document$title,
                  ":",
                  conditionMessage(e)
                )
              )
              
              ""
              
            }
            
          )
        
        
        tibble(
          
          id =
            document$id,
          
          title =
            document$title,
          
          organisation =
            document$organisation,
          
          url =
            document$url,
          
          text =
            document_text
          
        )
        
      }
      
    )
  
  
  saveRDS(
    guidance_output,
    cache_file
  )
  
  
  message(
    "Guidance cache created."
  )
  
  
  guidance_output
  
}


guidance_content <-
  load_guidance_documents()


# ============================================================
# 11. CHUNK GUIDANCE
# ============================================================

chunk_guidance <- function(
    text,
    chunk_size = 3500,
    overlap = 350
) {
  
  if (
    is.null(text) ||
    !nzchar(text)
  ) {
    
    return(
      character(0)
    )
    
  }
  
  
  total_length <-
    nchar(text)
  
  
  starts <-
    seq(
      from = 1,
      to = total_length,
      by =
        chunk_size -
        overlap
    )
  
  
  map_chr(
    
    starts,
    
    function(start_position) {
      
      end_position <-
        min(
          start_position +
            chunk_size -
            1,
          total_length
        )
      
      
      substr(
        text,
        start_position,
        end_position
      )
      
    }
    
  )
  
}


guidance_chunks <-
  
  guidance_content %>%
  
  mutate(
    
    chunks =
      map(
        text,
        chunk_guidance
      )
    
  ) %>%
  
  tidyr::unnest_longer(
    chunks,
    values_to =
      "chunk_text"
  ) %>%
  
  group_by(
    id
  ) %>%
  
  mutate(
    chunk_number =
      row_number()
  ) %>%
  
  ungroup()


# ============================================================
# 12. GUIDANCE RETRIEVAL
# ============================================================

retrieve_guidance <- function(
    question,
    top_n = 10
) {
  
  terms <-
    
    question %>%
    
    str_to_lower() %>%
    
    str_replace_all(
      "[^a-z0-9 ]",
      " "
    ) %>%
    
    str_split(
      "\\s+"
    ) %>%
    
    unlist()
  
  
  stop_words <- c(
    
    "the",
    "and",
    "for",
    "with",
    "that",
    "this",
    "what",
    "which",
    "when",
    "where",
    "why",
    "how",
    "are",
    "can",
    "does",
    "should",
    "about",
    "from",
    "have",
    "into",
    "use",
    "using",
    "would",
    "could",
    "please",
    "tell",
    "guidance",
    "guidelines"
    
  )
  
  
  terms <-
    
    unique(
      
      terms[
        nchar(terms) >=
          3 &
          !(terms %in%
              stop_words)
      ]
      
    )
  
  
  if (
    length(terms) ==
    0
  ) {
    
    return(
      
      guidance_chunks %>%
        
        group_by(
          id
        ) %>%
        
        slice_head(
          n = 1
        ) %>%
        
        ungroup()
      
    )
    
  }
  
  
  scored <-
    
    guidance_chunks %>%
    
    mutate(
      
      lower_text =
        str_to_lower(
          chunk_text
        ),
      
      
      relevance_score =
        map_dbl(
          
          lower_text,
          
          function(text_value) {
            
            sum(
              
              vapply(
                
                terms,
                
                function(term) {
                  
                  str_detect(
                    text_value,
                    fixed(term)
                  )
                  
                },
                
                logical(1)
                
              )
              
            )
            
          }
          
        )
      
    ) %>%
    
    arrange(
      desc(
        relevance_score
      )
    )
  
  
  matches <-
    scored %>%
    
    filter(
      relevance_score >
        0
    )
  
  
  if (
    nrow(matches) >
    0
  ) {
    
    matches %>%
      slice_head(
        n = top_n
      )
    
  } else {
    
    scored %>%
      slice_head(
        n = top_n
      )
    
  }
  
}


# ============================================================
# 13. BUILD GUIDANCE CONTEXT
# ============================================================

build_guidance_context <- function(
    retrieved
) {
  
  if (
    nrow(retrieved) ==
    0
  ) {
    
    return(
      "No relevant guidance was retrieved."
    )
    
  }
  
  
  paste(
    
    map_chr(
      
      seq_len(
        nrow(retrieved)
      ),
      
      function(i) {
        
        paste0(
          
          "DOCUMENT: ",
          retrieved$title[i],
          
          "\nORGANISATION: ",
          retrieved$organisation[i],
          
          "\nDOCUMENT CHUNK: ",
          retrieved$chunk_number[i],
          
          "\n\n",
          
          retrieved$chunk_text[i]
          
        )
        
      }
      
    ),
    
    collapse =
      "\n\n------------------------------------\n\n"
    
  )
  
}


# ============================================================
# 14. CALL DATABRICKS GUIDANCE LLM
# ============================================================

call_guidance_llm <- function(
    question,
    context
) {
  
  conn <-
    connect_to_databricks()
  
  
  on.exit(
    
    DBI::dbDisconnect(
      conn
    ),
    
    add = TRUE
    
  )
  
  
  system_prompt <- paste(
    
    "You are the AI Spy: Chat box assistant.",
    
    "You support Department for Education analysts",
    
    "who need to understand responsible AI guidance.",
    
    "",
    
    "The user will describe an AI problem, scenario or question.",
    
    "",
    
    "Use ONLY the guidance context supplied to you.",
    
    "Do not rely on general knowledge to fill gaps.",
    
    "Do not invent guidance, policy, DfE requirements, approvals,",
    
    "legal requirements, security requirements or governance rules.",
    
    "",
    
    "If the supplied guidance does not contain enough information",
    
    "to answer the question, state this clearly.",
    
    "",
    
    "Explain which guidance applies to the user's problem.",
    
    "Explain practical considerations supported by the guidance.",
    
    "Identify the relevant source document names.",
    
    "",
    
    "Use this response structure:",
    
    "",
    
    "Relevant guidance",
    
    "Explain the main guidance relevant to the situation.",
    
    "",
    
    "What to consider",
    
    "Give concise practical considerations.",
    
    "",
    
    "Relevant guidance documents",
    
    "List the relevant documents.",
    
    "",
    
    "Where legal, security, data protection, information assurance",
    
    "or formal departmental interpretation is needed, advise the user",
    
    "to contact the appropriate specialist team.",
    
    "",
    
    "Use clear plain English."
    
  )
  
  
  full_prompt <-
    paste0(
      
      system_prompt,
      
      "\n\n",
      
      "=====================================\n",
      "GUIDANCE CONTEXT\n",
      "=====================================\n\n",
      
      context,
      
      "\n\n",
      
      "=====================================\n",
      "USER QUESTION OR PROBLEM\n",
      "=====================================\n\n",
      
      question
      
    )
  
  
  endpoint_sql <-
    DBI::dbQuoteString(
      conn,
      GUIDANCE_LLM_ENDPOINT
    )
  
  
  prompt_sql <-
    DBI::dbQuoteString(
      conn,
      full_prompt
    )
  
  
  query <-
    paste0(
      
      "SELECT ai_query(",
      
      endpoint_sql,
      
      ", ",
      
      prompt_sql,
      
      ") AS response"
      
    )
  
  
  result <-
    DBI::dbGetQuery(
      conn,
      query
    )
  
  
  if (
    nrow(result) ==
    0 ||
    !"response" %in%
    names(result)
  ) {
    
    stop(
      "No response was returned from the Databricks model."
    )
    
  }
  
  
  as.character(
    result$response[[1]]
  )
  
}


# ============================================================
# 15. CSS
# ============================================================

app_css <- HTML("

body {
  background:#f4f8fb;
  color:#0b0c0c;
  font-size:16px;
}

.navbar {
  background:#003764 !important;
  border-bottom:5px solid #347CA9;
}

.navbar-brand {
  color:white !important;
  font-weight:800;
}

.ai-riskcheck-title {
  display:flex;
  align-items:center;
  gap:12px;
  color:white;
  font-size:21px;
  font-weight:800;
}

.navbar-nav .nav-link {
  color:white !important;
  font-weight:700;
  margin-right:5px;
  padding:10px 15px !important;
  border-radius:5px 5px 0 0;
}

.navbar-nav .nav-item:nth-child(1) .nav-link {
  background:#1d70b8;
}

.navbar-nav .nav-item:nth-child(2) .nav-link {
  background:#158187;
}

.navbar-nav .nav-item:nth-child(3) .nav-link {
  background:#54319f;
}

.navbar-nav .nav-item:nth-child(4) .nav-link {
  background:#0f7a52;
}

.navbar-nav .nav-item:nth-child(5) .nav-link {
  background:#347CA9;
}

.navbar-nav .nav-link:hover {
  filter:brightness(90%);
  text-decoration:underline;
}

.navbar-nav .nav-link.active {
  background:white !important;
  color:#0b0c0c !important;
  border-bottom:5px solid #ffdd00;
}


/* CLASSIFICATION */

.classification-banner {
  background:#d4351c;
  color:white;
  padding:10px 20px;
  text-align:center;
  font-weight:800;
  letter-spacing:.3px;
  border-bottom:3px solid #8b1a0e;
}


/* HEADINGS */

h1,
h2,
h3 {
  color:#003764;
  font-weight:800;
}

h2 {
  border-bottom:4px solid #347CA9;
  padding-bottom:8px;
}

h4,
h5 {
  font-weight:700;
}


/* CARDS */

.card {
  border:1px solid #b1b4b6;
  border-radius:7px;
  background:white;
  box-shadow:0 2px 6px rgba(0,0,0,.07);
  overflow:visible;
}

.card-header {
  background:#f4f8fb;
  border-bottom:4px solid #347CA9;
  font-weight:800;
}

.card-header h3,
.card-header h4 {
  margin:0;
  font-weight:800;
}

.card-body {
  padding:22px;
}


/* SAVE */

.save-card {
  border-left:7px solid #158187;
}

.use-case-id-box {
  background:#eef7f7;
  border:2px solid #158187;
  border-radius:5px;
  padding:15px;
}


/* ASSESSMENT */

.question-number {
  display:inline-block;
  background:#003764;
  color:white;
  font-weight:800;
  min-width:31px;
  height:31px;
  line-height:31px;
  text-align:center;
  border-radius:50%;
  flex-shrink:0;
}

.question-title {
  font-size:1.12rem;
  font-weight:750;
  line-height:1.4;
}

.ai-info-icon {
  display:inline-flex;
  align-items:center;
  justify-content:center;
  width:25px;
  height:25px;
  border-radius:50%;
  background:#1d70b8;
  color:white;
  font-size:15px;
  font-weight:800;
  cursor:help;
  flex-shrink:0;
}

.notes-label {
  font-weight:700;
  margin-top:8px;
}


/* INPUTS */

textarea.form-control,
input.form-control,
select.form-select {
  border:2px solid #0b0c0c;
}

textarea.form-control:focus,
input.form-control:focus,
select.form-select:focus {
  border-color:#0b0c0c;
  box-shadow:0 0 0 4px #ffdd00;
}


/* PROGRESS */

.progress {
  height:25px;
}

.progress-bar {
  background:#1d70b8;
  font-weight:800;
}


/* RESULTS */

.result-card-overall {
  border-top:7px solid #1d70b8;
}

.result-card-inherent {
  border-top:7px solid #f47738;
}

.result-card-controls {
  border-top:7px solid #0f7a52;
}


/* GUIDANCE CHAT */

.guidance-chat-card {
  border-left:7px solid #54319f;
}

.guidance-chat-card .card-header {
  background:#f6f3fb;
}

.guidance-chat-window {
  background:#f8f8f8;
  border:1px solid #b1b4b6;
  border-radius:6px;
  padding:20px;
  min-height:300px;
  max-height:600px;
  overflow-y:auto;
}

.user-message {
  background:#e5f1f8;
  border-left:5px solid #1d70b8;
  padding:14px;
  margin-bottom:16px;
  border-radius:4px;
}

.assistant-message {
  background:white;
  border-left:5px solid #158187;
  padding:14px;
  margin-bottom:16px;
  border-radius:4px;
}

.message-text {
  white-space:pre-wrap;
  margin-top:8px;
  margin-bottom:8px;
}

.source-box {
  background:#f3f2f1;
  border-left:4px solid #505a5f;
  padding:10px;
  margin-top:12px;
  font-size:14px;
}


/* FEEDBACK */

.feedback-card {
  border-left:7px solid #158187;
}

.feedback-card .card-header {
  background:#eef7f7;
}


/* ALERTS */

.alert-info {
  border-left:5px solid #1d70b8;
}

.alert-warning {
  border-left:5px solid #ffdd00;
}

.alert-danger {
  border-left:5px solid #d4351c;
}

.alert-success {
  border-left:5px solid #00703c;
}


/* TABLE */

table.dataTable thead th {
  background:#003764 !important;
  color:white !important;
  font-weight:700;
}


/* FOCUS */

a:focus,
button:focus,
input:focus,
textarea:focus,
select:focus {
  outline:4px solid #ffdd00 !important;
  outline-offset:2px;
}

")


# ============================================================
# 16. USER INTERFACE
# ============================================================

ui <- page_navbar(
  
  title = div(
    
    class =
      "ai-riskcheck-title",
    
    tags$img(
      src =
        "dfe-logo.png",
      height =
        "40px",
      alt =
        "Department for Education logo"
    ),
    
    strong(
      APP_NAME
    )
    
  ),
  
  
  theme =
    bs_theme(
      version = 5,
      bootswatch =
        "flatly"
    ),
  
  
  header =
    tagList(
      
      tags$head(
        tags$style(
          app_css
        )
      ),
      
      div(
        class =
          "classification-banner",
        CLASSIFICATION_LABEL
      )
      
    ),
  
  
  # ==========================================================
  # TAB 1 - OVERVIEW
  # ==========================================================
  
  nav_panel(
    
    "Overview",
    
    div(
      
      class =
        "container mt-4",
      
      
      card(
        
        card_body(
          
          h1(
            APP_NAME
          ),
          
          h4(
            "Responsible AI Risk Self-Assessment Tool"
          ),
          
          p(
            
            class =
              "lead",
            
            paste(
              "A simple and consistent way for analytical teams",
              "to identify and assess risks associated with",
              "AI-assisted analytical work."
            )
            
          ),
          
          p(
            paste(
              "AI RiskCheck can be used when AI supports code generation,",
              "analytical outputs, evidence summaries, data analysis,",
              "decision support or automated analytical processes."
            )
          ),
          
          
          div(
            
            class =
              "alert alert-info",
            
            strong(
              "Important: "
            ),
            
            paste(
              "AI RiskCheck supports professional judgement.",
              "It does not replace analytical QA, information assurance,",
              "security, data protection, legal, commercial or other",
              "required departmental assurance processes."
            )
            
          ),
          
          
          div(
            
            class =
              "alert alert-secondary",
            
            strong(
              "Questions or support: "
            ),
            
            paste(
              "If you are unsure how to complete the assessment",
              "or interpret your results, contact the AOE Centre of Excellence team."
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          h3(
            "How to use AI RiskCheck"
          )
        ),
        
        card_body(
          
          tags$ol(
            
            tags$li(
              strong(
                "Overview — "
              ),
              "understand the tool and when to use it."
            ),
            
            tags$li(
              strong(
                "AI & Model Profile — "
              ),
              "register, describe, save and resume your AI use case."
            ),
            
            tags$li(
              strong(
                "Risk Assessment — "
              ),
              "complete the 25 assessment questions."
            ),
            
            tags$li(
              strong(
                "Results — "
              ),
              "review risks, escalation conditions and assurance actions."
            ),
            
            tags$li(
              strong(
                "References — "
              ),
              "view guidance, ask the Guidance Assistant questions and provide feedback."
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          h3(
            "When should I use AI RiskCheck?"
          )
        ),
        
        card_body(
          
          tags$ul(
            
            tags$li(
              "Generating or assisting with analytical code"
            ),
            
            tags$li(
              "Creating analytical commentary or deliverables"
            ),
            
            tags$li(
              "Summarising evidence or research"
            ),
            
            tags$li(
              "Analysing or classifying data"
            ),
            
            tags$li(
              "Generating quantitative outputs"
            ),
            
            tags$li(
              "Supporting policy or operational decisions"
            ),
            
            tags$li(
              "Automating analytical workflows"
            ),
            
            tags$li(
              "Using LLMs, RAG, AI agents or machine-learning models"
            ),
            
            tags$li(
              "Moving an AI prototype or pilot into operational use"
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          h3(
            "What does AI RiskCheck assess?"
          )
        ),
        
        card_body(
          
          p(
            "The 25 questions cover eight areas:"
          ),
          
          fluidRow(
            
            column(
              
              width = 6,
              
              tags$ul(
                tags$li("Purpose, value & appropriateness"),
                tags$li("Impact & human oversight"),
                tags$li("Data, privacy & legal"),
                tags$li("Quality, testing & reliability")
              )
              
            ),
            
            column(
              
              width = 6,
              
              tags$ul(
                tags$li("Fairness, transparency & stakeholders"),
                tags$li("Security, platform & supplier"),
                tags$li("Governance, accountability & skills"),
                tags$li("Lifecycle, monitoring & scale")
              )
              
            )
            
          )
          
        )
        
      ),
      
      br()
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 2 - AI & MODEL PROFILE
  # ==========================================================
  
  nav_panel(
    
    "AI & Model Profile",
    
    div(
      
      class =
        "container mt-4",
      
      h2(
        "AI & Model Profile"
      ),
      
      
      # ======================================================
      # SAVE / RESUME
      # ======================================================
      
      card(
        
        class =
          "save-card mb-4",
        
        card_header(
          h3(
            "Register, save or resume your AI use case"
          )
        ),
        
        card_body(
          
          p(
            paste(
              "Give the assessment a Use Case ID.",
              "You can then save your work and return to it later."
            )
          ),
          
          
          fluidRow(
            
            column(
              
              width = 6,
              
              textInput(
                "use_case_id",
                "Use Case ID",
                placeholder =
                  "e.g. AIR-2026-0001"
              )
              
            ),
            
            column(
              
              width = 6,
              
              textInput(
                "assessment_owner",
                "Assessment owner / team",
                placeholder =
                  "e.g. Teacher Analysis Division"
              )
              
            )
            
          ),
          
          
          fluidRow(
            
            column(
              
              width = 4,
              
              actionButton(
                "generate_case_id",
                "Generate Use Case ID",
                class =
                  "btn btn-outline-primary"
              )
              
            ),
            
            column(
              
              width = 4,
              
              actionButton(
                "save_case",
                "Save work",
                class =
                  "btn btn-primary"
              )
              
            ),
            
            column(
              
              width = 4,
              
              actionButton(
                "refresh_saved_cases",
                "Refresh saved cases",
                class =
                  "btn btn-outline-secondary"
              )
              
            )
            
          ),
          
          br(),
          
          
          div(
            
            class =
              "use-case-id-box",
            
            selectInput(
              "saved_case_select",
              "Resume a saved assessment",
              choices =
                character(0)
            ),
            
            actionButton(
              "load_case",
              "Load selected assessment",
              class =
                "btn btn-success"
            )
            
          ),
          
          br(),
          
          uiOutput(
            "save_status"
          )
          
        )
        
      ),
      
      
      # ======================================================
      # USE CASE
      # ======================================================
      
      card(
        
        card_header(
          h3(
            "About your AI use case"
          )
        ),
        
        card_body(
          
          textInput(
            "project_name",
            "Project / use case name"
          ),
          
          textAreaInput(
            "project_description",
            "Describe how AI is being used",
            rows = 5,
            width = "100%",
            placeholder =
              paste(
                "Describe the problem, how AI is being used,",
                "what it produces and how the output will be used."
              )
          ),
          
          
          checkboxGroupInput(
            
            "ai_uses",
            
            "How is AI being used?",
            
            choices = c(
              "Generating or assisting with code",
              "Generating analytical commentary or written deliverables",
              "Summarising evidence, research or documents",
              "Analysing or classifying data",
              "Generating statistics, calculations or quantitative outputs",
              "Supporting policy or operational decision-making",
              "Automating an analytical process or workflow",
              "Searching or retrieving information",
              "Interacting with other systems or tools",
              "Other"
            )
            
          ),
          
          
          fluidRow(
            
            column(
              
              width = 6,
              
              selectInput(
                
                "lifecycle",
                
                "Lifecycle stage",
                
                choices = c(
                  "Exploring",
                  "Prototype",
                  "Pilot",
                  "Operational",
                  "Scaled operational service"
                )
                
              )
              
            ),
            
            column(
              
              width = 6,
              
              selectInput(
                
                "audience",
                
                "Who uses or may be affected by the output?",
                
                choices = c(
                  "Individual analyst",
                  "Analytical team",
                  "Internal DfE users",
                  "Policy / operational teams",
                  "Senior decision makers",
                  "External organisations",
                  "Public",
                  "Identifiable individuals"
                )
                
              )
              
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # TECHNOLOGY
      # ======================================================
      
      card(
        
        card_header(
          h3(
            "AI technology"
          )
        ),
        
        card_body(
          
          selectInput(
            
            "ai_type",
            
            "What type of AI is being used?",
            
            choices = c(
              "Generative AI / LLM",
              "LLM with RAG",
              "AI agent / tool-using LLM",
              "Machine learning / predictive model",
              "Natural language processing",
              "Computer vision",
              "Recommendation system",
              "Other"
            )
            
          ),
          
          
          selectInput(
            
            "environment",
            
            "AI environment",
            
            choices = c(
              "DfE-controlled environment",
              "Other government-controlled environment",
              "Approved external service",
              "Open-source / open-weight model hosted internally",
              "External commercial service",
              "Public AI service",
              "Unknown"
            )
            
          ),
          
          
          selectInput(
            
            "provider",
            
            "Model / AI provider",
            
            choices = c(
              "DfE internal service",
              "Microsoft / Azure OpenAI",
              "OpenAI",
              "Anthropic",
              "Google",
              "Meta",
              "Mistral",
              "Open-source / open-weight model",
              "Other commercial provider",
              "Other",
              "Unknown"
            )
            
          ),
          
          
          textInput(
            "model_name",
            "Model name",
            placeholder =
              "e.g. Claude, GPT, Gemini or Llama"
          ),
          
          
          textInput(
            "model_version",
            "Model version"
          ),
          
          
          selectInput(
            
            "hosting",
            
            "Where is the model hosted?",
            
            choices = c(
              "DfE-managed environment",
              "Approved government cloud environment",
              "On-premise / local",
              "External SaaS / cloud provider",
              "Public web service",
              "Unknown"
            )
            
          ),
          
          
          selectInput(
            
            "access_method",
            
            "How is the AI accessed?",
            
            choices = c(
              "DfE application",
              "Databricks",
              "API",
              "Web interface",
              "Locally hosted",
              "Embedded within another product",
              "Other"
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # LLM USAGE
      # ======================================================
      
      conditionalPanel(
        
        condition = "
          input.ai_type == 'Generative AI / LLM' ||
          input.ai_type == 'LLM with RAG' ||
          input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h3(
              "LLM usage"
            )
          ),
          
          card_body(
            
            numericInput(
              "monthly_tokens",
              "Estimated monthly token usage",
              value = 0,
              min = 0
            ),
            
            numericInput(
              "monthly_cost",
              "Estimated monthly AI cost (£)",
              value = 0,
              min = 0
            ),
            
            selectInput(
              "usage_monitoring",
              "Is model / API usage monitored?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "usage_limits",
              "Are usage or spending limits in place?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # RAG
      # ======================================================
      
      conditionalPanel(
        
        condition = "
          input.ai_type == 'LLM with RAG' ||
          input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h3(
              "RAG / retrieval"
            )
          ),
          
          card_body(
            
            selectInput(
              "rag_external",
              "Can retrieved information contain external or untrusted content?",
              choices = c(
                "No",
                "Limited",
                "Yes",
                "Unknown"
              )
            ),
            
            selectInput(
              "rag_permissions",
              "Are retrieval permissions appropriately restricted?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "prompt_injection_testing",
              "Has prompt-injection or malicious-document behaviour been tested?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # AGENT
      # ======================================================
      
      conditionalPanel(
        
        condition =
          "input.ai_type == 'AI agent / tool-using LLM'",
        
        card(
          
          card_header(
            h3(
              "AI agent permissions"
            )
          ),
          
          card_body(
            
            selectInput(
              
              "agent_actions",
              
              "Can the AI execute actions or use external tools?",
              
              choices = c(
                "No",
                "Low-impact actions only",
                "Yes - with human approval",
                "Yes - autonomously"
              )
              
            ),
            
            selectInput(
              
              "agent_permissions",
              
              "Are tool permissions restricted using least privilege?",
              
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
              
            )
            
          )
          
        )
        
      ),
      
      br()
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 3 - RISK ASSESSMENT
  # ==========================================================
  
  nav_panel(
    
    "Risk Assessment",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4(
          "Assessment progress"
        ),
        
        textOutput(
          "progress_text"
        ),
        
        br(),
        
        uiOutput(
          "progress_bar"
        ),
        
        hr(),
        
        p(
          strong(
            "25 questions"
          )
        ),
        
        p(
          paste(
            "Not relevant responses count as completed questions",
            "but are excluded from scoring."
          )
        ),
        
        p(
          paste(
            "Use the notes field to record evidence, context,",
            "assumptions or rationale."
          )
        )
        
      ),
      
      
      div(
        
        class =
          "p-3",
        
        uiOutput(
          "questions_ui"
        ),
        
        br(),
        
        
        card(
          
          class =
            "mb-4",
          
          card_header(
            h3(
              "Anything else we should know?"
            )
          ),
          
          card_body(
            
            p(
              paste(
                "Record additional risks, assumptions, dependencies",
                "or context not covered by the 25 questions."
              )
            ),
            
            textAreaInput(
              
              "additional_considerations",
              
              label = NULL,
              
              rows = 6,
              
              width =
                "100%",
              
              placeholder =
                paste(
                  "For example: known limitations, unusual dependencies,",
                  "outstanding decisions or additional assurance required."
                )
              
            )
            
          )
          
        )
        
      )
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 4 - RESULTS
  # ==========================================================
  
  nav_panel(
    
    "Results",
    
    div(
      
      class =
        "container-fluid mt-4",
      
      h2(
        "AI RiskCheck Assessment Results"
      ),
      
      uiOutput(
        "project_heading"
      ),
      
      br(),
      
      
      fluidRow(
        
        column(
          
          width = 4,
          
          card(
            
            class =
              "result-card-overall",
            
            card_header(
              "Overall rating"
            ),
            
            card_body(
              
              uiOutput(
                "risk_badge"
              ),
              
              br(),
              br(),
              
              h3(
                textOutput(
                  "residual_score_text"
                )
              ),
              
              p(
                "Residual risk"
              )
              
            )
            
          )
          
        ),
        
        
        column(
          
          width = 4,
          
          card(
            
            class =
              "result-card-inherent",
            
            card_header(
              "Inherent risk"
            ),
            
            card_body(
              
              h2(
                textOutput(
                  "inherent_label"
                )
              ),
              
              h4(
                textOutput(
                  "inherent_score_text"
                )
              ),
              
              p(
                "Risk before safeguards are taken into account."
              )
              
            )
            
          )
          
        ),
        
        
        column(
          
          width = 4,
          
          card(
            
            class =
              "result-card-controls",
            
            card_header(
              "Control strength"
            ),
            
            card_body(
              
              h2(
                textOutput(
                  "control_label"
                )
              ),
              
              h4(
                textOutput(
                  "control_score_text"
                )
              ),
              
              p(
                "Strength of safeguards currently in place."
              )
              
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Risk profile"
        ),
        
        card_body(
          
          plotOutput(
            "risk_plot",
            height =
              "450px"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Decision summary"
        ),
        
        card_body(
          
          uiOutput(
            "decision_summary"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Escalation / stop conditions"
        ),
        
        card_body(
          
          uiOutput(
            "flag_output"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Key areas requiring attention"
        ),
        
        card_body(
          
          uiOutput(
            "key_risks"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Recommended assurance activities"
        ),
        
        card_body(
          
          uiOutput(
            "recommendations"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Additional considerations"
        ),
        
        card_body(
          
          uiOutput(
            "additional_considerations_output"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "Detailed assessment"
        ),
        
        card_body(
          
          DTOutput(
            "risk_table"
          )
          
        )
        
      ),
      
      br(),
      
      
      card(
        
        card_header(
          "AI system profile"
        ),
        
        card_body(
          
          uiOutput(
            "system_profile"
          )
          
        )
        
      ),
      
      br(),
      
      
      downloadButton(
        "download_assessment",
        "Download AI RiskCheck assessment"
      ),
            br(),
      downloadButton(
"download_report",
"Download HTML report",
class = "btn-primary"
      ),
      
      br(),
      br()
      
    )
    
  ),
  
  
  # ==========================================================
  # TAB 5 - REFERENCES
  # ==========================================================
  
  nav_panel(
    
    "References",
    
    div(
      
      class =
        "container mt-4",
      
      h2(
        "References and supporting guidance"
      ),
      
      p(
        paste(
          "These documents provide the main evidence base used to",
          "develop the AI RiskCheck assessment."
        )
      ),
      
      
      # ======================================================
      # APP INFORMATION
      # ======================================================
      
      card(
        
        class =
          "mb-4",
        
        card_header(
          h3(
            "AI RiskCheck information"
          )
        ),
        
        card_body(
          
          tags$table(
            
            class =
              "table table-striped",
            
            tags$tbody(
              
              tags$tr(
                tags$th(
                  "Application"
                ),
                tags$td(
                  APP_NAME
                )
              ),
              
              tags$tr(
                tags$th(
                  "Version"
                ),
                tags$td(
                  APP_VERSION
                )
              ),
              
              tags$tr(
                tags$th(
                  "Classification"
                ),
                tags$td(
                  CLASSIFICATION_LABEL
                )
              ),
              
              tags$tr(
                tags$th(
                  "Assessment questions"
                ),
                tags$td(
                  TOTAL_ASSESSMENT_QUESTIONS
                )
              ),
              
              tags$tr(
                tags$th(
                  "App last updated"
                ),
                tags$td(
                  APP_LAST_UPDATED
                )
              ),
              
              tags$tr(
                tags$th(
                  "References last reviewed"
                ),
                tags$td(
                  REFERENCES_LAST_REVIEWED
                )
              ),
              
              tags$tr(
                tags$th(
                  "Primary audience"
                ),
                tags$td(
                  "DfE employees / analytical teams"
                )
              ),
              
              tags$tr(
                tags$th(
                  "Methodology status"
                ),
                tags$td(
                  "Prototype - scoring and thresholds require internal validation"
                )
              )
              
            )
            
          )
          
        )
        
      ),
      
      
      h3(
        "Guidance documents"
      ),
      
      uiOutput(
        "reference_cards"
      ),
      
      br(),
      
      
      # ======================================================
      # GUIDANCE CHAT
      # ======================================================
      
      card(
        
        class =
          "guidance-chat-card mb-4",
        
        card_header(
          h3(
            "AI Spy: Chat box assistant"
          )
        ),
        
        card_body(
          
          p(
            paste(
              "Describe your AI problem or ask a question.",
              "The assistant will identify the relevant guidance",
              "and tell you which guidance documents are relevant."
            )
          ),
          
          
          div(
            
            class =
              "alert alert-info",
            
            strong(
              "Grounded guidance assistant: "
            ),
            
            paste(
              "The assistant is instructed to answer from the",
              "guidance documents listed above.",
              "If the available guidance does not answer the question,",
              "it should say so."
            )
            
          ),
          
          
          div(
            
            class =
              "guidance-chat-window",
            
            uiOutput(
              "guidance_chat_history"
            )
            
          ),
          
          br(),
          
          
          textAreaInput(
            
            "guidance_question",
            
            "Describe your problem or ask a question",
            
            placeholder =
              paste(
                "For example: I am using an LLM to summarise unpublished",
                "departmental information. What guidance should I consider?"
              ),
            
            rows =
              4,
            
            width =
              "100%"
            
          ),
          
          
          fluidRow(
            
            column(
              
              width =
                4,
              
              actionButton(
                "ask_guidance",
                "Ask Guidance Assistant",
                class =
                  "btn btn-primary"
              )
              
            ),
            
            column(
              
              width =
                4,
              
              actionButton(
                "clear_guidance_chat",
                "Clear conversation",
                class =
                  "btn btn-outline-secondary"
              )
              
            )
            
          ),
          
          br(),
          
          uiOutput(
            "guidance_status"
          )
          
        )
        
      ),
      
      br(),
      
      
      # ======================================================
      # FEEDBACK
      # ======================================================
      
      card(
        
        class =
          "feedback-card mb-4",
        
        card_header(
          h3(
            "Feedback on AI RiskCheck"
          )
        ),
        
        card_body(
          
          p(
            paste(
              "AI RiskCheck is being developed iteratively.",
              "Your feedback will help improve the assessment,",
              "guidance, scoring and user experience."
            )
          ),
          
          
          div(
            
            class =
              "alert alert-light",
            
            strong(
              "Please do not include unnecessary sensitive personal information."
            )
            
          ),
          
          
          fluidRow(
            
            column(
              
              width =
                6,
              
              textInput(
                "feedback_name",
                "Name or team (optional)"
              )
              
            ),
            
            column(
              
              width =
                6,
              
              textInput(
                "feedback_case_id",
                "Use Case ID (optional)"
              )
              
            )
            
          ),
          
          
          selectInput(
            
            "feedback_type",
            
            "What is your feedback about?",
            
            choices = c(
              "Overall experience",
              "Risk assessment questions",
              "Scoring / risk rating",
              "AI & Model Profile",
              "Save / resume functionality",
              "Results and recommendations",
              "Guidance Assistant",
              "References / guidance",
              "Accessibility / usability",
              "Technical issue",
              "Missing risk or topic",
              "Other"
            )
            
          ),
          
          
          radioButtons(
            
            "feedback_rating",
            
            "How useful did you find AI RiskCheck?",
            
            choices = c(
              "Very useful" = "5",
              "Useful" = "4",
              "Neither useful nor unhelpful" = "3",
              "Not very useful" = "2",
              "Not useful" = "1"
            ),
            
            selected =
              character(0)
            
          ),
          
          
          textAreaInput(
            
            "feedback_comments",
            
            "Tell us more",
            
            rows =
              5,
            
            width =
              "100%"
            
          ),
          
          
          textAreaInput(
            
            "feedback_missing",
            
            "Are there any risks, questions or guidance that you think are missing? (optional)",
            
            rows =
              4,
            
            width =
              "100%"
            
          ),
          
          
          actionButton(
            "submit_feedback",
            "Submit feedback",
            class =
              "btn btn-primary"
          ),
          
          br(),
          br(),
          
          uiOutput(
            "feedback_status"
          )
          
        )
        
      ),
      
      br()
      
    )
    
  )
  
)


# ============================================================
# 17. SERVER
# ============================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ==========================================================
  # REACTIVE MESSAGES
  # ==========================================================
  
  save_message <-
    reactiveVal(NULL)
  
  
  feedback_message <-
    reactiveVal(NULL)
  
  
  feedback_message_type <-
    reactiveVal(
      "success"
    )
  
  
  guidance_status <-
    reactiveVal(NULL)
  
  
  guidance_chat <-
    reactiveVal(
      
      tibble(
        
        role =
          character(),
        
        message =
          character(),
        
        sources =
          character()
        
      )
      
    )
  
  
  # ==========================================================
  # SAVE STATUS
  # ==========================================================
  
  output$save_status <- renderUI({
    
    msg <-
      save_message()
    
    
    if (
      is.null(msg)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      class =
        "alert alert-info",
      msg
    )
    
  })
  
  
  # ==========================================================
  # SAVED CASE HELPERS
  # ==========================================================
  
  get_saved_cases <- function() {
    
    files <-
      list.files(
        
        SAVED_CASE_DIR,
        
        pattern =
          "\\.rds$",
        
        full.names =
          FALSE
        
      )
    
    
    if (
      length(files) ==
      0
    ) {
      
      return(
        character(0)
      )
      
    }
    
    
    sort(
      
      sub(
        "\\.rds$",
        "",
        files
      )
      
    )
    
  }
  
  
  # ==========================================================
  # GENERATE USE CASE ID
  # ==========================================================
  
  observeEvent(
    
    input$generate_case_id,
    
    {
      
      existing_cases <-
        get_saved_cases()
      
      
      current_year <-
        format(
          Sys.Date(),
          "%Y"
        )
      
      
      year_cases <-
        existing_cases[
          
          grepl(
            
            paste0(
              "^AIR-",
              current_year,
              "-"
            ),
            
            existing_cases
            
          )
          
        ]
      
      
      if (
        length(year_cases) ==
        0
      ) {
        
        next_number <-
          1
        
      } else {
        
        numbers <-
          suppressWarnings(
            
            as.integer(
              
              sub(
                
                paste0(
                  "^AIR-",
                  current_year,
                  "-"
                ),
                
                "",
                
                year_cases
                
              )
              
            )
            
          )
        
        
        numbers <-
          numbers[
            !is.na(numbers)
          ]
        
        
        next_number <-
          
          if (
            length(numbers) ==
            0
          ) {
            
            1
            
          } else {
            
            max(numbers) +
              1
            
          }
        
      }
      
      
      new_id <-
        sprintf(
          
          "AIR-%s-%04d",
          
          current_year,
          
          next_number
          
        )
      
      
      updateTextInput(
        
        session,
        
        "use_case_id",
        
        value =
          new_id
        
      )
      
      
      save_message(
        paste(
          "New Use Case ID generated:",
          new_id
        )
      )
      
    }
    
  )
  
  
  # ==========================================================
  # QUESTION UI
  # ==========================================================
  
  output$questions_ui <- renderUI({
    
    domains <-
      unique(
        questions$domain
      )
    
    
    question_counter <-
      0
    
    
    tagList(
      
      map(
        
        domains,
        
        function(domain_name) {
          
          domain_questions <-
            questions %>%
            
            filter(
              domain ==
                domain_name
            )
          
          
          card(
            
            class =
              "mb-4",
            
            card_header(
              
              div(
                
                style = "
                  display:flex;
                  justify-content:space-between;
                  align-items:center;
                ",
                
                h3(
                  style =
                    "margin-bottom:0;",
                  domain_name
                ),
                
                span(
                  paste0(
                    nrow(
                      domain_questions
                    ),
                    " questions"
                  )
                )
                
              )
              
            ),
            
            
            card_body(
              
              tagList(
                
                map(
                  
                  seq_len(
                    nrow(
                      domain_questions
                    )
                  ),
                  
                  function(i) {
                    
                    question_counter <<-
                      question_counter +
                      1
                    
                    
                    q <-
                      domain_questions[
                        i,
                      ]
                    
                    
                    choices <-
                      
                      if (
                        q$type ==
                        "risk"
                      ) {
                        
                        risk_options
                        
                      } else {
                        
                        control_options
                        
                      }
                    
                    
                    div(
                      
                      class =
                        "mb-4",
                      
                      
                      div(
                        
                        style = "
                          display:flex;
                          gap:8px;
                          align-items:flex-start;
                          margin-bottom:12px;
                        ",
                        
                        span(
                          class =
                            "question-number",
                          question_counter
                        ),
                        
                        span(
                          class =
                            "question-title",
                          q$question
                        ),
                        
                        
                        bslib::tooltip(
                          
                          span(
                            class =
                              "ai-info-icon",
                            "i"
                          ),
                          
                          div(
                            
                            style = "
                              max-width:400px;
                              text-align:left;
                            ",
                            
                            p(
                              q$help_text
                            ),
                            
                            hr(),
                            
                            p(
                              strong(
                                "Reference: "
                              ),
                              q$source
                            ),
                            
                            p(
                              strong(
                                "Relevant principle: "
                              ),
                              q$principle
                            ),
                            
                            p(
                              strong(
                                "Recommended action: "
                              ),
                              q$recommended_action
                            )
                            
                          ),
                          
                          placement =
                            "right"
                          
                        )
                        
                      ),
                      
                      
                      radioButtons(
                        
                        inputId =
                          q$id,
                        
                        label =
                          NULL,
                        
                        choices =
                          choices,
                        
                        selected =
                          character(0)
                        
                      ),
                      
                      
                      div(
                        class =
                          "notes-label",
                        "Optional notes / rationale"
                      ),
                      
                      
                      textAreaInput(
                        
                        inputId =
                          paste0(
                            q$id,
                            "_notes"
                          ),
                        
                        label =
                          NULL,
                        
                        rows =
                          2,
                        
                        width =
                          "100%",
                        
                        placeholder =
                          paste(
                            "Add context, evidence, assumptions or",
                            "explain why you selected Not relevant."
                          )
                        
                      ),
                      
                      hr()
                      
                    )
                    
                  }
                  
                )
                
              )
              
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  outputOptions(
    output,
    "questions_ui",
    suspendWhenHidden =
      FALSE
  )
  
  
  # ==========================================================
  # COLLECT ANSWERS
  # ==========================================================
  
  answers <- reactive({
    
    questions %>%
      
      mutate(
        
        
        raw_response =
          map_chr(
            
            id,
            
            function(question_id) {
              
              value <-
                input[[question_id]]
              
              
              if (
                is.null(value) ||
                length(value) ==
                0 ||
                identical(
                  value,
                  ""
                )
              ) {
                
                return(
                  NA_character_
                )
                
              }
              
              
              as.character(
                value
              )
              
            }
            
          ),
        
        
        not_relevant =
          !is.na(
            raw_response
          ) &
          raw_response ==
          "NR",
        
        
        response =
          map_dbl(
            
            raw_response,
            
            function(value) {
              
              if (
                is.na(value) ||
                value ==
                "NR"
              ) {
                
                return(
                  NA_real_
                )
                
              }
              
              
              as.numeric(
                value
              )
              
            }
            
          ),
        
        
        notes =
          map_chr(
            
            id,
            
            function(question_id) {
              
              note_value <-
                input[[paste0(
                  question_id,
                  "_notes"
                )]]
              
              
              if (
                is.null(note_value) ||
                length(note_value) ==
                0 ||
                identical(
                  note_value,
                  ""
                )
              ) {
                
                return("")
                
              }
              
              
              as.character(
                note_value
              )
              
            }
            
          )
        
      )
    
  })
  
  
  # ==========================================================
  # RESPONSE LABEL
  # ==========================================================
  
  response_label <- function(
    type,
    raw_response
  ) {
    
    if (
      is.na(
        raw_response
      )
    ) {
      
      return(
        "Not answered"
      )
      
    }
    
    
    if (
      raw_response ==
      "NR"
    ) {
      
      return(
        "Not relevant"
      )
      
    }
    
    
    value <-
      as.numeric(
        raw_response
      )
    
    
    if (
      type ==
      "risk"
    ) {
      
      labels <- c(
        "None / negligible",
        "Low",
        "Moderate",
        "High",
        "Very high"
      )
      
    } else {
      
      labels <- c(
        "No",
        "Mostly no",
        "Partly",
        "Mostly yes",
        "Yes"
      )
      
    }
    
    
    labels[
      value + 1
    ]
    
  }
  
  
  # ==========================================================
  # CASE DATA
  # ==========================================================
  
  case_data <- reactive({
    
    list(
      
      use_case_id =
        input$use_case_id,
      
      assessment_owner =
        input$assessment_owner,
      
      saved_date =
        as.character(
          Sys.time()
        ),
      
      project_name =
        input$project_name,
      
      project_description =
        input$project_description,
      
      ai_uses =
        input$ai_uses,
      
      lifecycle =
        input$lifecycle,
      
      audience =
        input$audience,
      
      ai_type =
        input$ai_type,
      
      environment =
        input$environment,
      
      provider =
        input$provider,
      
      model_name =
        input$model_name,
      
      model_version =
        input$model_version,
      
      hosting =
        input$hosting,
      
      access_method =
        input$access_method,
      
      monthly_tokens =
        input$monthly_tokens,
      
      monthly_cost =
        input$monthly_cost,
      
      usage_monitoring =
        input$usage_monitoring,
      
      usage_limits =
        input$usage_limits,
      
      rag_external =
        input$rag_external,
      
      rag_permissions =
        input$rag_permissions,
      
      prompt_injection_testing =
        input$prompt_injection_testing,
      
      agent_actions =
        input$agent_actions,
      
      agent_permissions =
        input$agent_permissions,
      
      answers =
        answers(),
      
      additional_considerations =
        input$additional_considerations
      
    )
    
  })
  
  
  # ==========================================================
  # SAVE CASE
  # ==========================================================
  
  observeEvent(
    
    input$save_case,
    
    {
      
      case_id <-
        trimws(
          input$use_case_id
        )
      
      
      if (
        is.null(case_id) ||
        !nzchar(case_id)
      ) {
        
        save_message(
          "Please enter or generate a Use Case ID before saving."
        )
        
        return()
        
      }
      
      
      safe_id <-
        gsub(
          "[^A-Za-z0-9_-]",
          "_",
          case_id
        )
      
      
      save_path <-
        file.path(
          SAVED_CASE_DIR,
          paste0(
            safe_id,
            ".rds"
          )
        )
      
      
      saveRDS(
        case_data(),
        save_path
      )
      
      
      updateSelectInput(
        
        session,
        
        "saved_case_select",
        
        choices =
          get_saved_cases(),
        
        selected =
          safe_id
        
      )
      
      
      save_message(
        paste(
          "Assessment saved successfully.",
          "Use Case ID:",
          case_id
        )
      )
      
    }
    
  )
  
  
  # ==========================================================
  # SAVED CASE LIST
  # ==========================================================
  
  observe({
    
    updateSelectInput(
      
      session,
      
      "saved_case_select",
      
      choices =
        get_saved_cases()
      
    )
    
  })
  
  
  observeEvent(
    
    input$refresh_saved_cases,
    
    {
      
      updateSelectInput(
        
        session,
        
        "saved_case_select",
        
        choices =
          get_saved_cases()
        
      )
      
      
      save_message(
        "Saved assessment list refreshed."
      )
      
    }
    
  )
  
  
  # ==========================================================
  # LOAD SAVED CASE
  # ==========================================================
  
  observeEvent(
    
    input$load_case,
    
    {
      
      req(
        input$saved_case_select
      )
      
      
      save_path <-
        file.path(
          
          SAVED_CASE_DIR,
          
          paste0(
            input$saved_case_select,
            ".rds"
          )
          
        )
      
      
      if (
        !file.exists(
          save_path
        )
      ) {
        
        save_message(
          "Saved assessment could not be found."
        )
        
        return()
        
      }
      
      
      saved <-
        readRDS(
          save_path
        )
      
      
      updateTextInput(
        session,
        "use_case_id",
        value =
          saved$use_case_id
      )
      
      updateTextInput(
        session,
        "assessment_owner",
        value =
          saved$assessment_owner
      )
      
      updateTextInput(
        session,
        "project_name",
        value =
          saved$project_name
      )
      
      updateTextAreaInput(
        session,
        "project_description",
        value =
          saved$project_description
      )
      
      updateCheckboxGroupInput(
        session,
        "ai_uses",
        selected =
          saved$ai_uses
      )
      
      updateSelectInput(
        session,
        "lifecycle",
        selected =
          saved$lifecycle
      )
      
      updateSelectInput(
        session,
        "audience",
        selected =
          saved$audience
      )
      
      updateSelectInput(
        session,
        "ai_type",
        selected =
          saved$ai_type
      )
      
      updateSelectInput(
        session,
        "environment",
        selected =
          saved$environment
      )
      
      updateSelectInput(
        session,
        "provider",
        selected =
          saved$provider
      )
      
      updateTextInput(
        session,
        "model_name",
        value =
          saved$model_name
      )
      
      updateTextInput(
        session,
        "model_version",
        value =
          saved$model_version
      )
      
      updateSelectInput(
        session,
        "hosting",
        selected =
          saved$hosting
      )
      
      updateSelectInput(
        session,
        "access_method",
        selected =
          saved$access_method
      )
      
      
      if (
        !is.null(
          saved$monthly_tokens
        )
      ) {
        
        updateNumericInput(
          session,
          "monthly_tokens",
          value =
            saved$monthly_tokens
        )
        
      }
      
      
      if (
        !is.null(
          saved$monthly_cost
        )
      ) {
        
        updateNumericInput(
          session,
          "monthly_cost",
          value =
            saved$monthly_cost
        )
        
      }
      
      
      if (
        !is.null(
          saved$usage_monitoring
        )
      ) {
        
        updateSelectInput(
          session,
          "usage_monitoring",
          selected =
            saved$usage_monitoring
        )
        
      }
      
      
      if (
        !is.null(
          saved$usage_limits
        )
      ) {
        
        updateSelectInput(
          session,
          "usage_limits",
          selected =
            saved$usage_limits
        )
        
      }
      
      
      if (
        !is.null(
          saved$rag_external
        )
      ) {
        
        updateSelectInput(
          session,
          "rag_external",
          selected =
            saved$rag_external
        )
        
      }
      
      
      if (
        !is.null(
          saved$rag_permissions
        )
      ) {
        
        updateSelectInput(
          session,
          "rag_permissions",
          selected =
            saved$rag_permissions
        )
        
      }
      
      
      if (
        !is.null(
          saved$prompt_injection_testing
        )
      ) {
        
        updateSelectInput(
          session,
          "prompt_injection_testing",
          selected =
            saved$prompt_injection_testing
        )
        
      }
      
      
      if (
        !is.null(
          saved$agent_actions
        )
      ) {
        
        updateSelectInput(
          session,
          "agent_actions",
          selected =
            saved$agent_actions
        )
        
      }
      
      
      if (
        !is.null(
          saved$agent_permissions
        )
      ) {
        
        updateSelectInput(
          session,
          "agent_permissions",
          selected =
            saved$agent_permissions
        )
        
      }
      
      
      if (
        !is.null(
          saved$answers
        )
      ) {
        
        for (
          i in seq_len(
            nrow(
              saved$answers
            )
          )
        ) {
          
          question_id <-
            saved$answers$id[i]
          
          answer_value <-
            saved$answers$raw_response[i]
          
          note_value <-
            saved$answers$notes[i]
          
          
          if (
            !is.na(
              answer_value
            )
          ) {
            
            updateRadioButtons(
              session,
              question_id,
              selected =
                answer_value
            )
            
          }
          
          
          updateTextAreaInput(
            session,
            paste0(
              question_id,
              "_notes"
            ),
            value =
              note_value
          )
          
        }
        
      }
      
      
      if (
        !is.null(
          saved$additional_considerations
        )
      ) {
        
        updateTextAreaInput(
          session,
          "additional_considerations",
          value =
            saved$additional_considerations
        )
        
      }
      
      
      save_message(
        paste(
          "Assessment loaded successfully.",
          "Use Case ID:",
          saved$use_case_id
        )
      )
      
    }
    
  )
  
  
  # ==========================================================
  # PROGRESS
  # ==========================================================
  
  completed_questions <- reactive({
    
    sum(
      !is.na(
        answers()$raw_response
      )
    )
    
  })
  
  
  assessment_complete <- reactive({
    
    completed_questions() ==
      TOTAL_ASSESSMENT_QUESTIONS
    
  })
  
  
  output$progress_text <- renderText({
    
    paste0(
      completed_questions(),
      " of ",
      TOTAL_ASSESSMENT_QUESTIONS,
      " questions completed"
    )
    
  })
  
  
  output$progress_bar <- renderUI({
    
    percentage <-
      round(
        
        100 *
          completed_questions() /
          TOTAL_ASSESSMENT_QUESTIONS
        
      )
    
    
    div(
      
      class =
        "progress",
      
      div(
        
        class =
          "progress-bar",
        
        style =
          paste0(
            "width:",
            percentage,
            "%"
          ),
        
        paste0(
          percentage,
          "%"
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # SCORED ANSWERS
  # ==========================================================
  
  scored_answers <- reactive({
    
    answers() %>%
      
      mutate(
        
        adjusted_score = case_when(
          
          not_relevant ~
            NA_real_,
          
          type ==
            "risk" ~
            response,
          
          type ==
            "control" ~
            4 - response,
          
          TRUE ~
            NA_real_
          
        ),
        
        
        weighted_score =
          adjusted_score *
          weight,
        
        
        maximum_score =
          if_else(
            is.na(
              adjusted_score
            ),
            NA_real_,
            4 * weight
          ),
        
        
        response_text =
          map2_chr(
            type,
            raw_response,
            response_label
          ),
        
        
        concern = case_when(
          
          raw_response ==
            "NR" ~
            "Not relevant",
          
          is.na(
            raw_response
          ) ~
            "Not answered",
          
          adjusted_score >=
            4 ~
            "Very high",
          
          adjusted_score >=
            3 ~
            "High",
          
          adjusted_score >=
            2 ~
            "Moderate",
          
          adjusted_score >=
            1 ~
            "Low",
          
          TRUE ~
            "No concern"
          
        )
        
      )
    
  })
  
  
  # ==========================================================
  # INHERENT RISK
  # ==========================================================
  
  inherent_score <- reactive({
    
    dat <-
      answers() %>%
      
      filter(
        type ==
          "risk",
        !is.na(
          response
        )
      )
    
    
    if (
      nrow(dat) ==
      0
    ) {
      
      return(0)
      
    }
    
    
    100 *
      
      sum(
        dat$response *
          dat$weight
      ) /
      
      sum(
        4 *
          dat$weight
      )
    
  })
  
  
  # ==========================================================
  # CONTROL STRENGTH
  # ==========================================================
  
  control_score <- reactive({
    
    dat <-
      answers() %>%
      
      filter(
        type ==
          "control",
        !is.na(
          response
        )
      )
    
    
    if (
      nrow(dat) ==
      0
    ) {
      
      return(0)
      
    }
    
    
    100 *
      
      sum(
        dat$response *
          dat$weight
      ) /
      
      sum(
        4 *
          dat$weight
      )
    
  })
  
  
  # ==========================================================
  # ESCALATION FLAGS
  # ==========================================================
  
  flags <- reactive({
    
    result <-
      tibble(
        severity =
          character(),
        issue =
          character()
      )
    
    
    add_flag <- function(
    severity,
    issue
    ) {
      
      result <<-
        bind_rows(
          
          result,
          
          tibble(
            severity =
              severity,
            issue =
              issue
          )
          
        )
      
    }
    
    
    if (
      !is.null(input$impact_2) &&
      input$impact_2 !=
      "NR" &&
      !is.null(input$human_1) &&
      input$human_1 !=
      "NR" &&
      as.numeric(
        input$impact_2
      ) >=
      4 &&
      as.numeric(
        input$human_1
      ) <=
      1
    ) {
      
      add_flag(
        
        "STOP",
        
        paste(
          "The AI may materially influence consequential decisions",
          "without sufficient meaningful human review."
        )
        
      )
      
    }
    
    
    if (
      !is.null(
        input$impact_3
      ) &&
      input$impact_3 !=
      "NR" &&
      as.numeric(
        input$impact_3
      ) >=
      4
    ) {
      
      add_flag(
        
        "STOP",
        
        paste(
          "The AI may make decisions or trigger consequential actions",
          "without human approval."
        )
        
      )
      
    }
    
    
    if (
      !is.null(input$data_2) &&
      input$data_2 !=
      "NR" &&
      !is.null(input$security_1) &&
      input$security_1 !=
      "NR" &&
      as.numeric(
        input$data_2
      ) >=
      3 &&
      as.numeric(
        input$security_1
      ) <=
      1
    ) {
      
      add_flag(
        
        "STOP",
        
        paste(
          "Sensitive information may be processed through an",
          "insufficiently approved AI environment."
        )
        
      )
      
    }
    
    
    if (
      !is.null(input$impact_1) &&
      input$impact_1 !=
      "NR" &&
      !is.null(input$quality_1) &&
      input$quality_1 !=
      "NR" &&
      as.numeric(
        input$impact_1
      ) >=
      3 &&
      as.numeric(
        input$quality_1
      ) <=
      1
    ) {
      
      add_flag(
        
        "ESCALATE",
        
        paste(
          "A high-impact AI use case does not currently have",
          "sufficient representative testing."
        )
        
      )
      
    }
    
    
    if (
      !is.null(input$impact_1) &&
      input$impact_1 !=
      "NR" &&
      !is.null(input$quality_2) &&
      input$quality_2 !=
      "NR" &&
      as.numeric(
        input$impact_1
      ) >=
      3 &&
      as.numeric(
        input$quality_2
      ) <=
      1
    ) {
      
      add_flag(
        
        "ESCALATE",
        
        paste(
          "A high-impact AI use case does not currently have",
          "sufficient independent verification of AI outputs."
        )
        
      )
      
    }
    
    
    if (
      !is.null(input$ethics_1) &&
      input$ethics_1 !=
      "NR" &&
      as.numeric(
        input$ethics_1
      ) >=
      3
    ) {
      
      add_flag(
        
        "ESCALATE",
        
        paste(
          "The use case may create significant fairness",
          "or differential-impact risks."
        )
        
      )
      
    }
    
    
    if (
      !is.null(input$governance_1) &&
      input$governance_1 !=
      "NR" &&
      as.numeric(
        input$governance_1
      ) <=
      1
    ) {
      
      add_flag(
        
        "ESCALATE",
        
        paste(
          "Ownership, accountability or escalation arrangements",
          "are currently insufficient."
        )
        
      )
      
    }
    
    
    if (
      !is.null(
        input$agent_actions
      ) &&
      input$agent_actions ==
      "Yes - autonomously"
    ) {
      
      add_flag(
        
        "STOP",
        
        paste(
          "The AI profile indicates that the agent",
          "can execute actions autonomously."
        )
        
      )
      
    }
    
    
    distinct(
      result
    )
    
  })
  
  
  # ==========================================================
  # RESIDUAL SCORE
  # ==========================================================
  
  residual_score <- reactive({
    
    score <-
      
      inherent_score() *
      
      (
        1 -
          (
            control_score() /
              100 *
              0.60
          )
      )
    
    
    score <-
      max(
        score,
        0
      )
    
    
    if (
      any(
        flags()$severity ==
        "ESCALATE"
      )
    ) {
      
      score <-
        max(
          score,
          60
        )
      
    }
    
    
    if (
      any(
        flags()$severity ==
        "STOP"
      )
    ) {
      
      score <-
        max(
          score,
          80
        )
      
    }
    
    
    score
    
  })
  
  
  # ==========================================================
  # OVERALL RATING
  # ==========================================================
  
  risk_label <- reactive({
    
    if (
      !assessment_complete()
    ) {
      
      return(
        "INCOMPLETE"
      )
      
    }
    
    
    if (
      any(
        flags()$severity ==
        "STOP"
      )
    ) {
      
      return(
        "DO NOT PROCEED / REVIEW"
      )
      
    }
    
    
    if (
      any(
        flags()$severity ==
        "ESCALATE"
      )
    ) {
      
      return(
        "ESCALATION REQUIRED"
      )
      
    }
    
    
    score <-
      residual_score()
    
    
    case_when(
      
      score <
        20 ~
        "LOW",
      
      score <
        40 ~
        "MODERATE",
      
      score <
        60 ~
        "HIGH",
      
      score <
        75 ~
        "VERY HIGH",
      
      TRUE ~
        "ESCALATION REQUIRED"
      
    )
    
  })
  
  
  # ==========================================================
  # SCORE LABEL HELPERS
  # ==========================================================
  
  risk_description <- function(score) {
    
    case_when(
      
      score <
        20 ~
        "Low",
      
      score <
        40 ~
        "Moderate",
      
      score <
        60 ~
        "High",
      
      score <
        75 ~
        "Very high",
      
      TRUE ~
        "Critical"
      
    )
    
  }
  
  
  control_description <- function(score) {
    
    case_when(
      
      score >=
        80 ~
        "Strong",
      
      score >=
        60 ~
        "Good",
      
      score >=
        40 ~
        "Moderate",
      
      score >=
        20 ~
        "Weak",
      
      TRUE ~
        "Very weak"
      
    )
    
  }
  
  
  # ==========================================================
  # PROJECT HEADING
  # ==========================================================
  
  output$project_heading <- renderUI({
    
    tagList(
      
      if (
        !is.null(input$use_case_id) &&
        nzchar(
          input$use_case_id
        )
      ) {
        
        div(
          
          class =
            "alert alert-light",
          
          strong(
            "Use Case ID: "
          ),
          
          input$use_case_id
          
        )
        
      },
      
      
      if (
        !is.null(input$project_name) &&
        nzchar(
          input$project_name
        )
      ) {
        
        h4(
          paste(
            "Use case:",
            input$project_name
          )
        )
        
      }
      
    )
    
  })
  
  
  # ==========================================================
  # SCORE OUTPUTS
  # ==========================================================
  
  output$inherent_label <- renderText({
    
    if (
      completed_questions() ==
      0
    ) {
      
      return(
        "Not assessed"
      )
      
    }
    
    
    risk_description(
      inherent_score()
    )
    
  })
  
  
  output$control_label <- renderText({
    
    if (
      completed_questions() ==
      0
    ) {
      
      return(
        "Not assessed"
      )
      
    }
    
    
    control_description(
      control_score()
    )
    
  })
  
  
  output$inherent_score_text <- renderText({
    
    if (
      completed_questions() ==
      0
    ) {
      
      return("-")
      
    }
    
    
    paste0(
      round(
        inherent_score(),
        1
      ),
      "%"
    )
    
  })
  
  
  output$control_score_text <- renderText({
    
    if (
      completed_questions() ==
      0
    ) {
      
      return("-")
      
    }
    
    
    paste0(
      round(
        control_score(),
        1
      ),
      "%"
    )
    
  })
  
  
  output$residual_score_text <- renderText({
    
    if (
      !assessment_complete()
    ) {
      
      return(
        
        paste0(
          completed_questions(),
          "/",
          TOTAL_ASSESSMENT_QUESTIONS,
          " completed"
        )
        
      )
      
    }
    
    
    paste0(
      round(
        residual_score(),
        1
      ),
      "%"
    )
    
  })
  
  
  # ==========================================================
  # RISK BADGE
  # ==========================================================
  
  output$risk_badge <- renderUI({
    
    label <-
      risk_label()
    
    
    badge_class <- case_when(
      
      label ==
        "INCOMPLETE" ~
        "secondary",
      
      label ==
        "LOW" ~
        "success",
      
      label ==
        "MODERATE" ~
        "warning",
      
      label ==
        "HIGH" ~
        "warning",
      
      label ==
        "VERY HIGH" ~
        "danger",
      
      TRUE ~
        "danger"
      
    )
    
    
    span(
      
      class =
        paste0(
          "badge bg-",
          badge_class
        ),
      
      style = "
        font-size:20px;
        padding:14px;
        font-weight:800;
      ",
      
      label
      
    )
    
  })
  
  
  # ==========================================================
  # DOMAIN SCORES
  # ==========================================================
  
  domain_scores <- reactive({
    
    scored_answers() %>%
      
      filter(
        !is.na(
          adjusted_score
        )
      ) %>%
      
      group_by(
        domain
      ) %>%
      
      summarise(
        
        risk_score =
          
          100 *
          
          sum(
            weighted_score,
            na.rm =
              TRUE
          ) /
          
          sum(
            maximum_score,
            na.rm =
              TRUE
          ),
        
        .groups =
          "drop"
        
      ) %>%
      
      arrange(
        desc(
          risk_score
        )
      )
    
  })
  
  
  # ==========================================================
  # RISK PLOT
  # ==========================================================
  
  output$risk_plot <- renderPlot({
    
    dat <-
      domain_scores()
    
    
    validate(
      
      need(
        nrow(dat) >
          0,
        "Complete relevant questions to see the risk profile."
      )
      
    )
    
    
    ggplot(
      
      dat,
      
      aes(
        
        x =
          reorder(
            domain,
            risk_score
          ),
        
        y =
          risk_score
        
      )
      
    ) +
      
      geom_col() +
      
      geom_text(
        
        aes(
          label =
            paste0(
              round(
                risk_score
              ),
              "%"
            )
        ),
        
        hjust =
          -0.1,
        
        fontface =
          "bold"
        
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        limits =
          c(
            0,
            110
          )
      ) +
      
      labs(
        x =
          NULL,
        y =
          "Risk / control concern (%)"
      ) +
      
      theme_minimal(
        base_size =
          13
      )
    
  })
  
  
  # ==========================================================
  # ESCALATION OUTPUT
  # ==========================================================
  
  output$flag_output <- renderUI({
    
    if (
      !assessment_complete()
    ) {
      
      return(
        
        div(
          
          class =
            "alert alert-secondary",
          
          paste(
            "Complete all 25 questions before the final",
            "escalation assessment is determined."
          )
          
        )
        
      )
      
    }
    
    
    flag_data <-
      flags()
    
    
    if (
      nrow(flag_data) ==
      0
    ) {
      
      return(
        
        div(
          
          class =
            "alert alert-success",
          
          strong(
            "No automatic escalation or stop conditions were identified."
          )
          
        )
        
      )
      
    }
    
    
    tagList(
      
      lapply(
        
        seq_len(
          nrow(
            flag_data
          )
        ),
        
        function(i) {
          
          css_class <-
            
            if (
              flag_data$severity[i] ==
              "STOP"
            ) {
              
              "alert alert-danger"
              
            } else {
              
              "alert alert-warning"
              
            }
          
          
          div(
            
            class =
              css_class,
            
            strong(
              paste0(
                flag_data$severity[i],
                ": "
              )
            ),
            
            flag_data$issue[i]
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # KEY RISKS
  # ==========================================================
  
  output$key_risks <- renderUI({
    
    dat <-
      
      scored_answers() %>%
      
      filter(
        
        !is.na(
          adjusted_score
        ),
        
        adjusted_score >=
          3
        
      ) %>%
      
      arrange(
        
        desc(
          adjusted_score
        ),
        
        desc(
          weight
        )
        
      ) %>%
      
      slice_head(
        n = 5
      )
    
    
    if (
      nrow(dat) ==
      0
    ) {
      
      return(
        
        div(
          class =
            "alert alert-success",
          "No high-concern responses have currently been identified."
        )
        
      )
      
    }
    
    
    tagList(
      
      lapply(
        
        seq_len(
          nrow(dat)
        ),
        
        function(i) {
          
          div(
            
            class =
              "alert alert-warning",
            
            h5(
              dat$domain[i]
            ),
            
            p(
              dat$question[i]
            ),
            
            p(
              strong(
                "Your response: "
              ),
              dat$response_text[i]
            ),
            
            if (
              nzchar(
                dat$notes[i]
              )
            ) {
              
              p(
                strong(
                  "Your notes: "
                ),
                dat$notes[i]
              )
              
            } else {
              
              NULL
              
            },
            
            p(
              strong(
                "Recommended action: "
              ),
              dat$recommended_action[i]
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # RECOMMENDATIONS
  # ==========================================================
  
  output$recommendations <- renderUI({
    
    dat <-
      
      scored_answers() %>%
      
      filter(
        
        !is.na(
          adjusted_score
        ),
        
        adjusted_score >=
          2
        
      ) %>%
      
      distinct(
        recommended_action
      )
    
    
    if (
      nrow(dat) ==
      0
    ) {
      
      return(
        
        div(
          class =
            "alert alert-success",
          paste(
            "No major additional assurance activities have currently",
            "been identified. Continue to apply proportionate analytical QA."
          )
        )
        
      )
      
    }
    
    
    tags$ul(
      
      lapply(
        dat$recommended_action,
        tags$li
      )
      
    )
    
  })
  
  
  # ==========================================================
  # DECISION SUMMARY
  # ==========================================================
  
  output$decision_summary <- renderUI({
    
    label <-
      risk_label()
    
    
    if (
      label ==
      "INCOMPLETE"
    ) {
      
      return(
        
        div(
          class =
            "alert alert-secondary",
          strong(
            "Assessment incomplete. "
          ),
          "Complete all 25 questions before relying on the overall rating."
        )
        
      )
      
    }
    
    
    if (
      label ==
      "LOW"
    ) {
      
      return(
        
        div(
          class =
            "alert alert-success",
          strong(
            "Proceed with standard assurance. "
          ),
          paste(
            "The use case currently appears suitable to proceed",
            "with proportionate analytical QA and normal controls."
          )
        )
        
      )
      
    }
    
    
    if (
      label ==
      "MODERATE"
    ) {
      
      return(
        
        div(
          class =
            "alert alert-warning",
          strong(
            "Proceed with additional controls. "
          ),
          paste(
            "Address the identified areas requiring attention",
            "and document the assurance undertaken."
          )
        )
        
      )
      
    }
    
    
    if (
      label %in%
      c(
        "HIGH",
        "VERY HIGH"
      )
    ) {
      
      return(
        
        div(
          class =
            "alert alert-danger",
          strong(
            "Enhanced assurance required. "
          ),
          paste(
            "Additional testing, review and specialist assurance",
            "should be completed before operational use."
          )
        )
        
      )
      
    }
    
    
    if (
      label ==
      "ESCALATION REQUIRED"
    ) {
      
      return(
        
        div(
          class =
            "alert alert-danger",
          strong(
            "Escalation required. "
          ),
          paste(
            "One or more material concerns should be reviewed",
            "with the appropriate assurance or specialist team."
          )
        )
        
      )
      
    }
    
    
    div(
      class =
        "alert alert-danger",
      strong(
        "Do not proceed without review. "
      ),
      paste(
        "AI RiskCheck has identified a stop condition.",
        "Resolve or formally review the issue before proceeding."
      )
    )
    
  })
  
  
  # ==========================================================
  # ADDITIONAL CONSIDERATIONS
  # ==========================================================
  
  output$additional_considerations_output <- renderUI({
    
    value <-
      input$additional_considerations
    
    
    if (
      is.null(value) ||
      !nzchar(
        trimws(value)
      )
    ) {
      
      return(
        
        p(
          class =
            "text-muted",
          "No additional considerations were recorded."
        )
        
      )
      
    }
    
    
    div(
      class =
        "alert alert-light",
      p(value)
    )
    
  })
  
  
  # ==========================================================
  # DETAILED TABLE
  # ==========================================================
  
  output$risk_table <- renderDT({
    
    dat <-
      
      scored_answers() %>%
      
      select(
        
        Domain =
          domain,
        
        Question =
          question,
        
        Response =
          response_text,
        
        `User notes / rationale` =
          notes,
        
        Concern =
          concern,
        
        Reference =
          source,
        
        `Recommended action` =
          recommended_action
        
      )
    
    
    datatable(
      
      dat,
      
      rownames =
        FALSE,
      
      options =
        list(
          pageLength =
            10,
          scrollX =
            TRUE,
          autoWidth =
            TRUE
        )
      
    )
    
  })
  
  
  # ==========================================================
  # SYSTEM PROFILE
  # ==========================================================
  
  output$system_profile <- renderUI({
    
    get_value <- function(x) {
      
      if (
        is.null(x) ||
        length(x) ==
        0 ||
        identical(
          x,
          ""
        )
      ) {
        
        return(
          "Not provided"
        )
        
      }
      
      
      paste(
        x,
        collapse =
          "; "
      )
      
    }
    
    
    rows <- list(
      
      c(
        "Use Case ID",
        get_value(
          input$use_case_id
        )
      ),
      
      c(
        "Assessment owner / team",
        get_value(
          input$assessment_owner
        )
      ),
      
      c(
        "Project / use case",
        get_value(
          input$project_name
        )
      ),
      
      c(
        "Description",
        get_value(
          input$project_description
        )
      ),
      
      c(
        "How AI is used",
        get_value(
          input$ai_uses
        )
      ),
      
      c(
        "Lifecycle",
        get_value(
          input$lifecycle
        )
      ),
      
      c(
        "Audience",
        get_value(
          input$audience
        )
      ),
      
      c(
        "AI type",
        get_value(
          input$ai_type
        )
      ),
      
      c(
        "Environment",
        get_value(
          input$environment
        )
      ),
      
      c(
        "Provider",
        get_value(
          input$provider
        )
      ),
      
      c(
        "Model",
        get_value(
          input$model_name
        )
      ),
      
      c(
        "Model version",
        get_value(
          input$model_version
        )
      ),
      
      c(
        "Hosting",
        get_value(
          input$hosting
        )
      ),
      
      c(
        "Access method",
        get_value(
          input$access_method
        )
      )
      
    )
    
    
    tags$table(
      
      class =
        "table table-striped",
      
      tags$tbody(
        
        lapply(
          
          rows,
          
          function(row) {
            
            tags$tr(
              tags$th(
                row[1]
              ),
              tags$td(
                row[2]
              )
            )
            
          }
          
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # REFERENCES
  # ==========================================================
  
  output$reference_cards <- renderUI({
    
    tagList(
      
      map(
        
        seq_len(
          nrow(
            guidance_documents
          )
        ),
        
        function(i) {
          
          ref <-
            guidance_documents[
              i,
            ]
          
          
          card(
            
            class =
              "mb-3",
            
            card_header(
              h4(
                ref$title
              )
            ),
            
            card_body(
              
              p(
                strong(
                  ref$organisation
                ),
                paste0(
                  " — ",
                  ref$date
                )
              ),
              
              p(
                ref$purpose
              ),
              
              tags$a(
                
                href =
                  ref$url,
                
                target =
                  "_blank",
                
                class =
                  "btn btn-outline-primary",
                
                "Open reference"
                
              )
              
            )
            
          )
          
        }
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # GUIDANCE CHAT DISPLAY
  # ==========================================================
  
  output$guidance_chat_history <- renderUI({
    
    chat <-
      guidance_chat()
    
    
    if (
      nrow(chat) ==
      0
    ) {
      
      return(
        
        div(
          
          class =
            "text-muted",
          
          h4(
            "Ask me about the AI guidance"
          ),
          
          p(
            paste(
              "Describe your AI problem and the assistant will explain",
              "what guidance is relevant and which documents it came from."
            )
          ),
          
          p(
            tags$em(
              paste(
                "Example: I am developing an AI tool that generates",
                "analytical commentary from unpublished data.",
                "What guidance should I consider?"
              )
            )
          )
          
        )
        
      )
      
    }
    
    
    tagList(
      
      lapply(
        
        seq_len(
          nrow(chat)
        ),
        
        function(i) {
          
          if (
            chat$role[i] ==
            "user"
          ) {
            
            div(
              
              class =
                "user-message",
              
              strong(
                "You"
              ),
              
              div(
                class =
                  "message-text",
                chat$message[i]
              )
              
            )
            
          } else {
            
            div(
              
              class =
                "assistant-message",
              
              strong(
                "AI Spy: Chat box assistant"
              ),
              
              div(
                class =
                  "message-text",
                chat$message[i]
              ),
              
              
              if (
                nzchar(
                  chat$sources[i]
                )
              ) {
                
                div(
                  
                  class =
                    "source-box",
                  
                  strong(
                    "Guidance documents retrieved: "
                  ),
                  
                  chat$sources[i]
                  
                )
                
              } else {
                
                NULL
                
              }
              
            )
            
          }
          
        }
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # GUIDANCE STATUS
  # ==========================================================
  
  output$guidance_status <- renderUI({
    
    status <-
      guidance_status()
    
    
    if (
      is.null(status)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      class =
        "alert alert-info",
      status
    )
    
  })
  
  
  # ==========================================================
  # ASK GUIDANCE ASSISTANT
  # ==========================================================
  
  observeEvent(
    
    input$ask_guidance,
    
    {
      
      question <-
        trimws(
          input$guidance_question
        )
      
      
      if (
        is.null(question) ||
        !nzchar(question)
      ) {
        
        guidance_status(
          "Please describe your problem or enter a question first."
        )
        
        return()
        
      }
      
      
      guidance_status(
        "Searching the guidance and preparing an answer..."
      )
      
      
      # ------------------------------------------------------
      # Add user message
      # ------------------------------------------------------
      
      guidance_chat(
        
        bind_rows(
          
          guidance_chat(),
          
          tibble(
            
            role =
              "user",
            
            message =
              question,
            
            sources =
              ""
            
          )
          
        )
        
      )
      
      
      # ------------------------------------------------------
      # Retrieve guidance
      # ------------------------------------------------------
      
      retrieved <-
        
        retrieve_guidance(
          
          question =
            question,
          
          top_n =
            10
          
        )
      
      
      # ------------------------------------------------------
      # Identify source documents from retrieval
      # ------------------------------------------------------
      
      source_documents <-
        
        retrieved %>%
        
        distinct(
          title
        ) %>%
        
        pull(
          title
        )
      
      
      source_text <-
        
        paste(
          source_documents,
          collapse =
            "; "
        )
      
      
      # ------------------------------------------------------
      # Build context
      # ------------------------------------------------------
      
      context <-
        build_guidance_context(
          retrieved
        )
      
      
      # ------------------------------------------------------
      # Call Databricks
      # ------------------------------------------------------
      
      answer <-
        
        tryCatch(
          
          {
            
            call_guidance_llm(
              
              question =
                question,
              
              context =
                context
              
            )
            
          },
          
          
          error = function(e) {
            
            paste(
              
              "The Guidance Assistant could not answer this question.",
              
              "\n\nTechnical message:",
              
              conditionMessage(e)
              
            )
            
          }
          
        )
      
      
      # ------------------------------------------------------
      # Add answer
      # ------------------------------------------------------
      
      guidance_chat(
        
        bind_rows(
          
          guidance_chat(),
          
          tibble(
            
            role =
              "assistant",
            
            message =
              answer,
            
            sources =
              source_text
            
          )
          
        )
        
      )
      
      
      # ------------------------------------------------------
      # Clear text input
      # ------------------------------------------------------
      
      updateTextAreaInput(
        
        session,
        
        "guidance_question",
        
        value =
          ""
        
      )
      
      
      guidance_status(
        NULL
      )
      
    }
    
  )
  
  
  # ==========================================================
  # CLEAR GUIDANCE CHAT
  # ==========================================================
  
  observeEvent(
    
    input$clear_guidance_chat,
    
    {
      
      guidance_chat(
        
        tibble(
          
          role =
            character(),
          
          message =
            character(),
          
          sources =
            character()
          
        )
        
      )
      
      
      guidance_status(
        NULL
      )
      
    }
    
  )
  
  
  # ==========================================================
  # FEEDBACK STATUS
  # ==========================================================
  
  output$feedback_status <- renderUI({
    
    msg <-
      feedback_message()
    
    
    if (
      is.null(msg)
    ) {
      
      return(NULL)
      
    }
    
    
    div(
      
      class =
        paste0(
          "alert alert-",
          feedback_message_type()
        ),
      
      msg
      
    )
    
  })
  
  
  # ==========================================================
  # SUBMIT FEEDBACK
  # ==========================================================
  
  observeEvent(
    
    input$submit_feedback,
    
    {
      
      comments <-
        
        if (
          is.null(
            input$feedback_comments
          )
        ) {
          
          ""
          
        } else {
          
          trimws(
            input$feedback_comments
          )
          
        }
      
      
      missing_feedback <-
        
        if (
          is.null(
            input$feedback_missing
          )
        ) {
          
          ""
          
        } else {
          
          trimws(
            input$feedback_missing
          )
          
        }
      
      
      rating_present <-
        
        !is.null(
          input$feedback_rating
        ) &&
        
        length(
          input$feedback_rating
        ) >
        0
      
      
      if (
        !rating_present &&
        !nzchar(comments) &&
        !nzchar(missing_feedback)
      ) {
        
        feedback_message_type(
          "warning"
        )
        
        feedback_message(
          "Please provide a rating or enter some feedback before submitting."
        )
        
        return()
        
      }
      
      
      feedback_row <-
        tibble(
          
          submitted_at =
            as.character(
              Sys.time()
            ),
          
          app_version =
            APP_VERSION,
          
          classification =
            CLASSIFICATION_LABEL,
          
          name_or_team =
            ifelse(
              is.null(
                input$feedback_name
              ),
              "",
              input$feedback_name
            ),
          
          use_case_id =
            ifelse(
              is.null(
                input$feedback_case_id
              ),
              "",
              input$feedback_case_id
            ),
          
          feedback_type =
            input$feedback_type,
          
          usefulness_rating =
            if (
              rating_present
            ) {
              input$feedback_rating
            } else {
              NA_character_
            },
          
          comments =
            comments,
          
          missing_risks_or_guidance =
            missing_feedback
          
        )
      
      
      if (
        file.exists(
          FEEDBACK_FILE
        )
      ) {
        
        existing_feedback <-
          read.csv(
            FEEDBACK_FILE,
            stringsAsFactors =
              FALSE
          )
        
        
        updated_feedback <-
          bind_rows(
            existing_feedback,
            feedback_row
          )
        
      } else {
        
        updated_feedback <-
          feedback_row
        
      }
      
      
      write.csv(
        updated_feedback,
        FEEDBACK_FILE,
        row.names =
          FALSE
      )
      
      
      feedback_message_type(
        "success"
      )
      
      
      feedback_message(
        "Thank you. Your feedback has been submitted successfully."
      )
      
      
      updateTextInput(
        session,
        "feedback_name",
        value =
          ""
      )
      
      updateTextInput(
        session,
        "feedback_case_id",
        value =
          ""
      )
      
      updateSelectInput(
        session,
        "feedback_type",
        selected =
          "Overall experience"
      )
      
      updateRadioButtons(
        session,
        "feedback_rating",
        selected =
          character(0)
      )
      
      updateTextAreaInput(
        session,
        "feedback_comments",
        value =
          ""
      )
      
      updateTextAreaInput(
        session,
        "feedback_missing",
        value =
          ""
      )
      
    }
    
  )
  


  
  # ============================================================
# REPORT HELPERS
# ============================================================

get_report_value <- function(
  value,
  default = "Not provided"
) {

  if (
    is.null(value) ||
    length(value) == 0 ||
    all(is.na(value)) ||
    all(trimws(as.character(value)) == "")
  ) {

    return(default)
  }

  paste(
    value,
    collapse = "; "
  )
}


recommendation_data <- reactive({

  dat <- scored_answers() %>%

    filter(
      !is.na(adjusted_score),
      adjusted_score >= 2
    ) %>%

    arrange(
      desc(adjusted_score),
      desc(weight)
    ) %>%

    distinct(
      recommended_action
    )

  if (nrow(dat) == 0) {

    return(
      tibble(
        recommended_action = paste(
          "No major additional assurance activities have currently",
          "been identified. Continue to apply proportionate analytical QA."
        )
      )
    )
  }

  dat
})


key_risk_data <- reactive({

  scored_answers() %>%

    filter(
      !is.na(adjusted_score),
      adjusted_score >= 3
    ) %>%

    arrange(
      desc(adjusted_score),
      desc(weight)
    ) %>%

    slice_head(
      n = 5
    )
})


decision_summary_text <- reactive({

  label <- risk_label()

  if (label == "INCOMPLETE") {

    return(
      paste(
        "Assessment incomplete.",
        "Complete all 25 questions before relying on the overall rating."
      )
    )
  }

  if (label == "LOW") {

    return(
      paste(
        "Proceed with standard assurance.",
        "The use case currently appears suitable to proceed with",
        "proportionate analytical quality assurance and normal controls."
      )
    )
  }

  if (label == "MODERATE") {

    return(
      paste(
        "Proceed with additional controls.",
        "Address the identified areas requiring attention and",
        "document the assurance undertaken."
      )
    )
  }

  if (
    label %in%
    c(
      "HIGH",
      "VERY HIGH"
    )
  ) {

    return(
      paste(
        "Enhanced assurance required.",
        "Additional testing, review and appropriate specialist assurance",
        "should be completed before operational use."
      )
    )
  }

  if (label == "ESCALATION REQUIRED") {

    return(
      paste(
        "Escalation required.",
        "One or more material concerns should be reviewed with",
        "the appropriate assurance or specialist team."
      )
    )
  }

  paste(
    "Do not proceed without review.",
    "AI RiskCheck has identified a stop condition.",
    "Resolve or formally review the issue before proceeding."
  )
})
  
  
  # ==========================================================
  # DOWNLOAD ASSESSMENT
  # ==========================================================
  
  output$download_assessment <- downloadHandler(
    
    filename = function() {
      
      safe_id <-
        
        if (
          is.null(
            input$use_case_id
          ) ||
          !nzchar(
            input$use_case_id
          )
        ) {
          
          "AI_RiskCheck"
          
        } else {
          
          gsub(
            "[^A-Za-z0-9_-]",
            "_",
            input$use_case_id
          )
          
        }
      
      
      paste0(
        safe_id,
        "_Assessment_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    
    content = function(file) {
      
      results <-
        
        scored_answers() %>%
        
        mutate(
          
          classification =
            CLASSIFICATION_LABEL,
          
          use_case_id =
            input$use_case_id,
          
          assessment_owner =
            input$assessment_owner,
          
          project_name =
            input$project_name,
          
          project_description =
            input$project_description,
          
          ai_uses =
            paste(
              input$ai_uses,
              collapse =
                "; "
            ),
          
          lifecycle =
            input$lifecycle,
          
          audience =
            input$audience,
          
          ai_type =
            input$ai_type,
          
          environment =
            input$environment,
          
          provider =
            input$provider,
          
          model_name =
            input$model_name,
          
          model_version =
            input$model_version,
          
          additional_considerations =
            input$additional_considerations,
          
          assessment_date =
            as.character(
              Sys.Date()
            ),
          
          app_version =
            APP_VERSION,
          
          assessment_complete =
            assessment_complete(),
          
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
            ),
          
          overall_rating =
            risk_label(),
          
          escalation_flags =
            paste(
              flags()$issue,
              collapse =
                " | "
            )
          
        )
      
      
      write.csv(
        results,
        file,
        row.names =
          FALSE
      )
      
    }
    
  )

  
# ============================================================
# HTML REPORT DOWNLOAD
# ============================================================

output$download_report <- downloadHandler(

  filename = function() {

    project_name <- get_report_value(
      input$project_name,
      default = "AI_RiskCheck"
    )

    safe_name <- gsub(
      "[^A-Za-z0-9_-]+",
      "_",
      project_name
    )

    safe_name <- gsub(
      "^_+|_+$",
      "",
      safe_name
    )

    if (safe_name == "") {
      safe_name <- ""
    }

    paste0(
      safe_name,
      "_AI_RiskCheck_Report_",
      Sys.Date(),
      ".html"
    )
  },


  contentType = "text/html",


  content = function(file) {

    # --------------------------------------------------------
    # ASSESSMENT INFORMATION
    # --------------------------------------------------------

    report_information <- tibble(

      field = c(
        "Application",
        "Application version",
        "Classification",
        "Assessment date",
        "Assessment status",
        "Questions completed",
        "App last updated",
        "References last reviewed",
        "Methodology status"
      ),

      value = c(
        APP_NAME,
        APP_VERSION,
        CLASSIFICATION_LABEL,
        format(
          Sys.Date(),
          "%d %B %Y"
        ),
        ifelse(
          assessment_complete(),
          "Complete",
          "Incomplete"
        ),
        paste0(
          completed_questions(),
          " of ",
          TOTAL_ASSESSMENT_QUESTIONS
        ),
        APP_LAST_UPDATED,
        REFERENCES_LAST_REVIEWED,
        "Prototype - scoring and thresholds require internal validation"
      )
    )


    # --------------------------------------------------------
    # AI SYSTEM PROFILE
    # --------------------------------------------------------

    system_profile_data <- tibble(

      field = c(
        "Project / use case",
        "Description",
        "How AI is used",
        "Lifecycle stage",
        "Audience",
        "AI type",
        "Environment",
        "Provider",
        "Model name",
        "Model version",
        "Hosting",
        "Access method",
        "Estimated monthly token usage",
        "Estimated monthly AI cost",
        "Usage monitored",
        "Usage or spending limits",
        "External or untrusted retrieved content",
        "Retrieval permissions restricted",
        "Prompt-injection testing",
        "Agent actions",
        "Agent permissions"
      ),

      value = c(
        get_report_value(input$project_name),
        get_report_value(input$project_description),
        get_report_value(input$ai_uses),
        get_report_value(input$lifecycle),
        get_report_value(input$audience),
        get_report_value(input$ai_type),
        get_report_value(input$environment),
        get_report_value(input$provider),
        get_report_value(input$model_name),
        get_report_value(input$model_version),
        get_report_value(input$hosting),
        get_report_value(input$access_method),
        get_report_value(input$monthly_tokens),
        get_report_value(input$monthly_cost),
        get_report_value(input$usage_monitoring),
        get_report_value(input$usage_limits),
        get_report_value(input$rag_external),
        get_report_value(input$rag_permissions),
        get_report_value(input$prompt_injection_testing),
        get_report_value(input$agent_actions),
        get_report_value(input$agent_permissions)
      )
    )


    # --------------------------------------------------------
    # OVERALL RESULTS
    # --------------------------------------------------------

    results_summary <- tibble(

      measure = c(
        "Overall rating",
        "Inherent risk",
        "Control strength",
        "Residual risk",
        "Assessment complete"
      ),

      result = c(
        risk_label(),

        paste0(
          round(
            inherent_score(),
            1
          ),
          "% - ",
          risk_description(
            inherent_score()
          )
        ),

        paste0(
          round(
            control_score(),
            1
          ),
          "% - ",
          control_description(
            control_score()
          )
        ),

        if (
          assessment_complete()
        ) {
          paste0(
            round(
              residual_score(),
              1
            ),
            "%"
          )
        } else {
          "Not finalised because the assessment is incomplete"
        },

        ifelse(
          assessment_complete(),
          "Yes",
          "No"
        )
      )
    )


    # --------------------------------------------------------
    # QUESTION ANSWERS
    # --------------------------------------------------------

    report_answers <- scored_answers() %>%

      mutate(

        question_number = row_number(),

        question_type = case_when(
          type == "risk" ~ "Risk",
          type == "control" ~ "Control",
          TRUE ~ type
        ),

        raw_rating = case_when(
          raw_response == "NR" ~ "Not relevant",
          is.na(raw_response) ~ "Not answered",
          TRUE ~ paste0(
            raw_response,
            " out of 4"
          )
        ),

        adjusted_rating = case_when(
          is.na(adjusted_score) ~ "Excluded from scoring",
          TRUE ~ paste0(
            round(
              adjusted_score,
              1
            ),
            " out of 4"
          )
        ),

        weighted_rating = case_when(
          is.na(weighted_score) ~ "Excluded from scoring",
          TRUE ~ as.character(
            round(
              weighted_score,
              1
            )
          )
        ),

        report_notes = case_when(
          is.na(notes) | trimws(notes) == "" ~
            "No notes recorded",
          TRUE ~ notes
        )
      ) %>%

      select(
        question_number,
        domain,
        question,
        help_text,
        question_type,
        response_text,
        raw_rating,
        concern,
        report_notes,
        weight,
        adjusted_rating,
        weighted_rating,
        principle,
        source,
        recommended_action
      )


    # --------------------------------------------------------
    # DOMAIN SCORES
    # --------------------------------------------------------

    report_domain_scores <- domain_scores() %>%

      mutate(

        risk_score = round(
          risk_score,
          1
        ),

        rating = case_when(
          risk_score < 20 ~ "Low",
          risk_score < 40 ~ "Moderate",
          risk_score < 60 ~ "High",
          risk_score < 75 ~ "Very high",
          TRUE ~ "Critical"
        )
      )


    # --------------------------------------------------------
    # FLAGS
    # --------------------------------------------------------

    report_flags <- flags()

    if (nrow(report_flags) == 0) {

      report_flags <- tibble(
        severity = "None",
        issue = paste(
          "No automatic escalation or stop conditions",
          "were identified."
        )
      )
    }


    # --------------------------------------------------------
    # KEY RISKS
    # --------------------------------------------------------

    report_key_risks <- key_risk_data() %>%

      transmute(
        domain = domain,
        question = question,
        response = response_text,
        notes = if_else(
          is.na(notes) | trimws(notes) == "",
          "No notes recorded",
          notes
        ),
        concern = concern,
        recommended_action = recommended_action
      )

    if (nrow(report_key_risks) == 0) {

      report_key_risks <- tibble(
        domain = "None identified",
        question = paste(
          "No high-concern responses have currently",
          "been identified."
        ),
        response = "",
        notes = "",
        concern = "No high concern",
        recommended_action = paste(
          "Continue to apply proportionate analytical",
          "quality assurance."
        )
      )
    }


    # --------------------------------------------------------
    # RECOMMENDATIONS
    # --------------------------------------------------------

    report_recommendations <-
      recommendation_data()$recommended_action


    # --------------------------------------------------------
    # ADDITIONAL CONSIDERATIONS
    # --------------------------------------------------------

    report_additional_considerations <-
      get_report_value(
        input$additional_considerations,
        default = "No additional considerations were recorded."
      )


    # --------------------------------------------------------
    # COPY AND RENDER TEMPLATE
    # --------------------------------------------------------

    template_path <- file.path(
      getwd(),
      "AI_RiskCheck_Report.Rmd"
    )

    if (!file.exists(template_path)) {

      stop(
        paste(
          "The report template AI_RiskCheck_Report.Rmd",
          "could not be found in the application directory."
        )
      )
    }

    temporary_report <- file.path(
      tempdir(),
      "AI_RiskCheck_Report.Rmd"
    )

    copied <- file.copy(
      from = template_path,
      to = temporary_report,
      overwrite = TRUE
    )

    if (!copied) {

      stop(
        "The report template could not be copied to the temporary directory."
      )
    }

    rmarkdown::render(

      input = temporary_report,

      output_file = basename(file),

      output_dir = dirname(file),

      params = list(

        app_name =
          APP_NAME,

        app_version =
          APP_VERSION,

        classification =
          CLASSIFICATION_LABEL,

        report_information =
          report_information,

        system_profile =
          system_profile_data,

        results_summary =
          results_summary,

        decision_summary =
          decision_summary_text(),

        domain_scores =
          report_domain_scores,

        flags =
          report_flags,

        key_risks =
          report_key_risks,

        recommendations =
          report_recommendations,

        additional_considerations =
          report_additional_considerations,

        answers =
          report_answers,

        references =
          references
      ),

      envir = new.env(
        parent = globalenv()
      ),

      quiet = TRUE
    )
  }
)



  
}


# ============================================================
# 18. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)