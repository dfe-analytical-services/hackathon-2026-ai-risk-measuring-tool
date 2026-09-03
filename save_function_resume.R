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
library(ggplot2)
library(DT)


# ============================================================
# 2. APP SETTINGS
# ============================================================

APP_NAME <- "AI RiskCheck"
APP_VERSION <- "1.2 Prototype"

APP_LAST_UPDATED <- "3 September 2026"
REFERENCES_LAST_REVIEWED <- "3 September 2026"

TOTAL_ASSESSMENT_QUESTIONS <- 25

CLASSIFICATION_LABEL <-
  "OFFICIAL-SENSITIVE — INTERNAL USE ONLY — DfE EMPLOYEES ONLY"


# ============================================================
# 3. SAVED CASE SETTINGS
# ============================================================

SAVED_CASE_DIR <- "saved_cases"

if (!dir.exists(SAVED_CASE_DIR)) {
  dir.create(SAVED_CASE_DIR, recursive = TRUE)
}


# ============================================================
# 4. RESPONSE OPTIONS
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
# 5. 25-QUESTION ASSESSMENT MATRIX
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
  3,
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
  3,
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
  3,
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
  4,
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
    "This is especially important when analysts use AI to create code,",
    "generate deliverables, summarise evidence or produce quantitative outputs."
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
  4,
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
    "investigation, audit and understanding of how outputs were produced."
  ),
  "control",
  4,
  "UK Government AI Playbook; Data and AI Ethics Framework",
  "Transparency and reproducibility",
  paste(
    "Retain records of models, prompts, data sources, retrieved documents,",
    "important settings and analytical decisions."
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
    "Users should understand material AI involvement so they can",
    "interpret outputs appropriately and avoid over-reliance."
  ),
  "control",
  4,
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
  4,
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
  4,
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
  4,
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
  nrow(questions) == TOTAL_ASSESSMENT_QUESTIONS
)


# ============================================================
# 6. REFERENCES
# ============================================================

references <- tribble(
  
  ~source,
  ~organisation,
  ~date,
  ~purpose,
  ~url,
  
  "Use of artificial intelligence in government",
  "National Audit Office",
  "March 2024",
  "Review of government AI adoption, governance, skills, testing and scaling.",
  "https://www.nao.org.uk/wp-content/uploads/2024/03/use-of-artificial-intelligence-in-government.pdf",
  
  "Good practice guide for organisations using AI",
  "National Audit Office",
  "May 2026",
  "Good-practice guidance covering governance, data, security, evaluation and scaling.",
  "https://www.nao.org.uk/wp-content/uploads/2026/05/good-practice-guide-for-organisations-using-ai.pdf",
  
  "Artificial Intelligence Playbook for the UK Government",
  "UK Government",
  "2025",
  "Government guidance for safe, responsible and effective AI use.",
  "https://www.gov.uk/government/publications/ai-playbook-for-the-uk-government/artificial-intelligence-playbook-for-the-uk-government-html",
  
  "AI Opportunities Action Plan",
  "UK Government",
  "2025",
  "Government approach to identifying, piloting, evaluating and scaling AI opportunities.",
  "https://www.gov.uk/government/publications/ai-opportunities-action-plan/ai-opportunities-action-plan",
  
  "Data and AI Ethics Framework",
  "UK Government",
  "2025",
  "Framework covering transparency, accountability, fairness, privacy and societal impact.",
  "https://www.gov.uk/government/publications/data-ethics-framework/data-and-ai-ethics-framework"
)


# ============================================================
# 7. CSS
# ============================================================

app_css <- HTML("

body {
  background-color:#f4f8fb;
  color:#0b0c0c;
  font-size:16px;
}

.navbar {
  background-color:#003764 !important;
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

.navbar-nav .nav-link.active {
  background:white !important;
  color:#0b0c0c !important;
  border-bottom:5px solid #ffdd00;
}

.classification-banner {
  background:#fff8cc;
  border-bottom:3px solid #ffdd00;
  padding:8px 20px;
  text-align:center;
  font-weight:800;
}

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

.card {
  border:1px solid #cecece;
  border-radius:7px;
  box-shadow:0 2px 6px rgba(0,0,0,.07);
  background:white;
  overflow:visible;
}

.card-header {
  background:#f4f8fb;
  border-bottom:4px solid #347CA9;
  font-weight:800;
}

.card-body {
  padding:22px;
}

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
}

.notes-label {
  font-weight:700;
  margin-top:8px;
}

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

.progress {
  height:25px;
}

.progress-bar {
  background:#1d70b8;
  font-weight:800;
}

.result-card-overall {
  border-top:7px solid #1d70b8;
}

.result-card-inherent {
  border-top:7px solid #f47738;
}

.result-card-controls {
  border-top:7px solid #0f7a52;
}

.alert-info {
  border-left:5px solid #1d70b8;
}

.alert-warning {
  border-left:5px solid #ffdd00;
}

.alert-danger {
  border-left:5px solid #ca3535;
}

.alert-success {
  border-left:5px solid #0f7a52;
}

.save-card {
  border-left:7px solid #158187;
}

.use-case-id-box {
  background:#eef7f7;
  border:2px solid #158187;
  border-radius:5px;
  padding:15px;
}

table.dataTable thead th {
  background:#003764 !important;
  color:white !important;
}

")


# ============================================================
# 8. UI
# ============================================================

ui <- page_navbar(
  
  title = div(
    class = "ai-riskcheck-title",
    
    tags$img(
      src = "dfe-logo.png",
      height = "40px"
    ),
    
    strong(APP_NAME)
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  header = tagList(
    
    tags$head(
      tags$style(app_css)
    ),
    
    div(
      class = "classification-banner",
      CLASSIFICATION_LABEL
    )
  ),
  
  
  # ==========================================================
  # OVERVIEW
  # ==========================================================
  
  nav_panel(
    
    "Overview",
    
    div(
      
      class = "container mt-4",
      
      card(
        
        card_body(
          
          h1(APP_NAME),
          
          h4(
            "Responsible AI Risk Self-Assessment Tool"
          ),
          
          p(
            class = "lead",
            paste(
              "A simple and consistent way for analytical teams to",
              "identify and assess risks associated with AI-assisted work."
            )
          ),
          
          div(
            class = "alert alert-info",
            
            strong("Important: "),
            
            paste(
              "AI RiskCheck supports professional judgement and does not",
              "replace required analytical, security, legal or data-protection assurance."
            )
          ),
          
          div(
            class = "alert alert-secondary",
            
            strong("Questions or support: "),
            
            paste(
              "If you need help completing the assessment or interpreting",
              "the results, contact the AOE Centre of Excellence team."
            )
          )
        )
      ),
      
      br(),
      
      card(
        
        card_header(
          h3("How to use AI RiskCheck")
        ),
        
        card_body(
          
          tags$ol(
            tags$li(
              strong("Overview — "),
              "understand the tool and when it should be used."
            ),
            tags$li(
              strong("AI & Model Profile — "),
              "register, describe and save your use case."
            ),
            tags$li(
              strong("Risk Assessment — "),
              "complete the 25-question assessment."
            ),
            tags$li(
              strong("Results — "),
              "review the risk rating and assurance actions."
            ),
            tags$li(
              strong("References — "),
              "view supporting documents and application information."
            )
          )
        )
      ),
      
      br(),
      
      card(
        
        card_header(
          h3("When should I use AI RiskCheck?")
        ),
        
        card_body(
          
          tags$ul(
            tags$li("Generating or assisting with analytical code"),
            tags$li("Creating analytical commentary or deliverables"),
            tags$li("Summarising evidence or research"),
            tags$li("Analysing or classifying data"),
            tags$li("Generating quantitative outputs"),
            tags$li("Supporting policy or operational decisions"),
            tags$li("Automating analytical workflows"),
            tags$li("Using LLMs, RAG, agents or machine-learning models"),
            tags$li("Moving AI from prototype to operational use")
          )
        )
      ),
      
      br()
    )
  ),
  
  
  # ==========================================================
  # AI & MODEL PROFILE
  # ==========================================================
  
  nav_panel(
    
    "AI & Model Profile",
    
    div(
      
      class = "container mt-4",
      
      h2("AI & Model Profile"),
      
      # ======================================================
      # REGISTER / SAVE / RESUME
      # ======================================================
      
      card(
        
        class = "save-card mb-4",
        
        card_header(
          h3("Register, save or resume your AI use case")
        ),
        
        card_body(
          
          p(
            paste(
              "Each assessment should have a Use Case ID.",
              "You can save your work and return to it later."
            )
          ),
          
          fluidRow(
            
            column(
              6,
              
              textInput(
                "use_case_id",
                "Use Case ID",
                placeholder = "e.g. AIR-2026-0001"
              )
            ),
            
            column(
              6,
              
              textInput(
                "assessment_owner",
                "Assessment owner / team",
                placeholder = "e.g. Teacher Analysis Division"
              )
            )
          ),
          
          fluidRow(
            
            column(
              4,
              
              actionButton(
                "generate_case_id",
                "Generate Use Case ID",
                class = "btn btn-outline-primary"
              )
            ),
            
            column(
              4,
              
              actionButton(
                "save_case",
                "Save work",
                class = "btn btn-primary"
              )
            ),
            
            column(
              4,
              
              actionButton(
                "refresh_saved_cases",
                "Refresh saved cases",
                class = "btn btn-outline-secondary"
              )
            )
          ),
          
          br(),
          
          div(
            class = "use-case-id-box",
            
            selectInput(
              "saved_case_select",
              "Resume a saved assessment",
              choices = character(0)
            ),
            
            actionButton(
              "load_case",
              "Load selected assessment",
              class = "btn btn-success"
            )
          ),
          
          br(),
          
          uiOutput(
            "save_status"
          )
        )
      ),
      
      
      # ======================================================
      # ABOUT USE CASE
      # ======================================================
      
      card(
        
        card_header(
          h3("About your AI use case")
        ),
        
        card_body(
          
          textInput(
            "project_name",
            "Project / use case name"
          ),
          
          textAreaInput(
            "project_description",
            "Describe how AI is being used",
            rows = 5
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
              6,
              
              selectInput(
                "lifecycle",
                "Lifecycle stage",
                c(
                  "Exploring",
                  "Prototype",
                  "Pilot",
                  "Operational",
                  "Scaled operational service"
                )
              )
            ),
            
            column(
              6,
              
              selectInput(
                "audience",
                "Who uses or may be affected by the output?",
                c(
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
          h3("AI technology")
        ),
        
        card_body(
          
          selectInput(
            "ai_type",
            "What type of AI is being used?",
            c(
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
            c(
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
            c(
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
            "Model name"
          ),
          
          textInput(
            "model_version",
            "Model version"
          ),
          
          selectInput(
            "hosting",
            "Where is the model hosted?",
            c(
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
            c(
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
      
      
      conditionalPanel(
        
        condition = "
        input.ai_type == 'Generative AI / LLM' ||
        input.ai_type == 'LLM with RAG' ||
        input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h3("LLM usage")
          ),
          
          card_body(
            
            numericInput(
              "monthly_tokens",
              "Estimated monthly token usage",
              0,
              min = 0
            ),
            
            numericInput(
              "monthly_cost",
              "Estimated monthly AI cost (£)",
              0,
              min = 0
            ),
            
            selectInput(
              "usage_monitoring",
              "Is model/API usage monitored?",
              c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "usage_limits",
              "Are usage or spending limits in place?",
              c(
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
      
      
      conditionalPanel(
        
        condition = "
        input.ai_type == 'LLM with RAG' ||
        input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h3("RAG / retrieval")
          ),
          
          card_body(
            
            selectInput(
              "rag_external",
              "Can retrieved information contain external or untrusted content?",
              c(
                "No",
                "Limited",
                "Yes",
                "Unknown"
              )
            ),
            
            selectInput(
              "rag_permissions",
              "Are retrieval permissions appropriately restricted?",
              c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "prompt_injection_testing",
              "Has prompt-injection behaviour been tested?",
              c(
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
      
      
      conditionalPanel(
        
        condition =
          "input.ai_type == 'AI agent / tool-using LLM'",
        
        card(
          
          card_header(
            h3("AI agent permissions")
          ),
          
          card_body(
            
            selectInput(
              "agent_actions",
              "Can the AI execute actions or use external tools?",
              c(
                "No",
                "Low-impact actions only",
                "Yes - with human approval",
                "Yes - autonomously"
              )
            ),
            
            selectInput(
              "agent_permissions",
              "Are tool permissions restricted using least privilege?",
              c(
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
  # RISK ASSESSMENT
  # ==========================================================
  
  nav_panel(
    
    "Risk Assessment",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Assessment progress"),
        
        textOutput("progress_text"),
        
        br(),
        
        uiOutput("progress_bar"),
        
        hr(),
        
        p(
          "Not relevant responses are excluded from scoring."
        ),
        
        p(
          "Use the notes field to record evidence or rationale."
        )
      ),
      
      div(
        
        class = "p-3",
        
        uiOutput(
          "questions_ui"
        ),
        
        card(
          
          card_header(
            h3("Anything else we should know?")
          ),
          
          card_body(
            
            textAreaInput(
              "additional_considerations",
              label = NULL,
              rows = 6,
              width = "100%",
              placeholder = paste(
                "Record additional risks, assumptions,",
                "dependencies or issues not covered above."
              )
            )
          )
        )
      )
    )
  ),
  
  
  # ==========================================================
  # RESULTS
  # ==========================================================
  
  nav_panel(
    
    "Results",
    
    div(
      
      class = "container-fluid mt-4",
      
      h2("AI RiskCheck Assessment Results"),
      
      uiOutput(
        "project_heading"
      ),
      
      br(),
      
      fluidRow(
        
        column(
          4,
          card(
            class = "result-card-overall",
            card_header("Overall rating"),
            card_body(
              uiOutput("risk_badge"),
              br(),
              h3(textOutput("residual_score_text")),
              p("Residual risk")
            )
          )
        ),
        
        column(
          4,
          card(
            class = "result-card-inherent",
            card_header("Inherent risk"),
            card_body(
              h2(textOutput("inherent_label")),
              h4(textOutput("inherent_score_text"))
            )
          )
        ),
        
        column(
          4,
          card(
            class = "result-card-controls",
            card_header("Control strength"),
            card_body(
              h2(textOutput("control_label")),
              h4(textOutput("control_score_text"))
            )
          )
        )
      ),
      
      br(),
      
      card(
        card_header("Risk profile"),
        plotOutput(
          "risk_plot",
          height = "450px"
        )
      ),
      
      br(),
      
      card(
        card_header("Decision summary"),
        card_body(
          uiOutput("decision_summary")
        )
      ),
      
      br(),
      
      card(
        card_header("Escalation / stop conditions"),
        card_body(
          uiOutput("flag_output")
        )
      ),
      
      br(),
      
      card(
        card_header("Key areas requiring attention"),
        card_body(
          uiOutput("key_risks")
        )
      ),
      
      br(),
      
      card(
        card_header("Recommended assurance activities"),
        card_body(
          uiOutput("recommendations")
        )
      ),
      
      br(),
      
      card(
        card_header("Additional considerations"),
        card_body(
          uiOutput("additional_considerations_output")
        )
      ),
      
      br(),
      
      card(
        card_header("Detailed assessment"),
        DTOutput("risk_table")
      ),
      
      br(),
      
      downloadButton(
        "download_assessment",
        "Download AI RiskCheck assessment"
      ),
      
      br(),
      br()
    )
  ),
  
  
  # ==========================================================
  # REFERENCES
  # ==========================================================
  
  nav_panel(
    
    "References",
    
    div(
      
      class = "container mt-4",
      
      h2("References and supporting documents"),
      
      card(
        
        card_header(
          h3("AI RiskCheck information")
        ),
        
        card_body(
          
          tags$table(
            
            class = "table table-striped",
            
            tags$tbody(
              
              tags$tr(
                tags$th("Application"),
                tags$td(APP_NAME)
              ),
              
              tags$tr(
                tags$th("Version"),
                tags$td(APP_VERSION)
              ),
              
              tags$tr(
                tags$th("Classification"),
                tags$td(CLASSIFICATION_LABEL)
              ),
              
              tags$tr(
                tags$th("Assessment questions"),
                tags$td(TOTAL_ASSESSMENT_QUESTIONS)
              ),
              
              tags$tr(
                tags$th("App last updated"),
                tags$td(APP_LAST_UPDATED)
              ),
              
              tags$tr(
                tags$th("References last reviewed"),
                tags$td(REFERENCES_LAST_REVIEWED)
              )
            )
          )
        )
      ),
      
      br(),
      
      uiOutput(
        "reference_cards"
      )
    )
  )
)


# ============================================================
# 9. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # SAVE STATUS
  # ==========================================================
  
  save_message <- reactiveVal(NULL)
  
  
  output$save_status <- renderUI({
    
    msg <- save_message()
    
    if (is.null(msg)) {
      return(NULL)
    }
    
    div(
      class = "alert alert-info",
      msg
    )
  })
  
  
  # ==========================================================
  # GET SAVED CASES
  # ==========================================================
  
  get_saved_cases <- function() {
    
    files <- list.files(
      SAVED_CASE_DIR,
      pattern = "\\.rds$",
      full.names = FALSE
    )
    
    if (length(files) == 0) {
      return(character(0))
    }
    
    sub(
      "\\.rds$",
      "",
      files
    )
  }
  
  
  # ==========================================================
  # GENERATE USE CASE ID
  # ==========================================================
  
  observeEvent(
    input$generate_case_id,
    {
      
      existing_cases <- get_saved_cases()
      
      current_year <- format(
        Sys.Date(),
        "%Y"
      )
      
      year_cases <- existing_cases[
        grepl(
          paste0("^AIR-", current_year, "-"),
          existing_cases
        )
      ]
      
      if (length(year_cases) == 0) {
        
        next_number <- 1
        
      } else {
        
        numbers <- suppressWarnings(
          as.integer(
            sub(
              paste0("^AIR-", current_year, "-"),
              "",
              year_cases
            )
          )
        )
        
        numbers <- numbers[
          !is.na(numbers)
        ]
        
        if (length(numbers) == 0) {
          next_number <- 1
        } else {
          next_number <- max(numbers) + 1
        }
      }
      
      new_id <- sprintf(
        "AIR-%s-%04d",
        current_year,
        next_number
      )
      
      updateTextInput(
        session,
        "use_case_id",
        value = new_id
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
    
    domains <- unique(
      questions$domain
    )
    
    counter <- 0
    
    
    tagList(
      
      map(
        
        domains,
        
        function(domain_name) {
          
          domain_questions <- questions %>%
            filter(
              domain == domain_name
            )
          
          
          card(
            
            class = "mb-4",
            
            card_header(
              h3(domain_name)
            ),
            
            card_body(
              
              tagList(
                
                map(
                  
                  seq_len(
                    nrow(domain_questions)
                  ),
                  
                  function(i) {
                    
                    counter <<- counter + 1
                    
                    q <- domain_questions[i, ]
                    
                    
                    choices <- if (
                      q$type == "risk"
                    ) {
                      risk_options
                    } else {
                      control_options
                    }
                    
                    
                    div(
                      
                      class = "mb-4",
                      
                      div(
                        
                        style = "
                        display:flex;
                        gap:8px;
                        align-items:flex-start;
                        ",
                        
                        span(
                          class = "question-number",
                          counter
                        ),
                        
                        span(
                          class = "question-title",
                          q$question
                        ),
                        
                        bslib::tooltip(
                          
                          span(
                            class = "ai-info-icon",
                            "i"
                          ),
                          
                          div(
                            
                            style = "
                            max-width:400px;
                            text-align:left;
                            ",
                            
                            p(q$help_text),
                            
                            hr(),
                            
                            p(
                              strong("Reference: "),
                              q$source
                            ),
                            
                            p(
                              strong("Relevant principle: "),
                              q$principle
                            ),
                            
                            p(
                              strong("Recommended action: "),
                              q$recommended_action
                            )
                          ),
                          
                          placement = "right"
                        )
                      ),
                      
                      br(),
                      
                      radioButtons(
                        inputId = q$id,
                        label = NULL,
                        choices = choices,
                        selected = character(0)
                      ),
                      
                      div(
                        class = "notes-label",
                        "Optional notes / rationale"
                      ),
                      
                      textAreaInput(
                        inputId = paste0(
                          q$id,
                          "_notes"
                        ),
                        label = NULL,
                        rows = 2,
                        width = "100%"
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
  
  
  # ==========================================================
  # COLLECT ANSWERS
  # ==========================================================
  
  answers <- reactive({
    
    questions %>%
      
      mutate(
        
        raw_response = map_chr(
          
          id,
          
          function(question_id) {
            
            value <- input[[question_id]]
            
            if (
              is.null(value) ||
              length(value) == 0 ||
              identical(value, "")
            ) {
              return(NA_character_)
            }
            
            as.character(value)
          }
        ),
        
        
        not_relevant =
          !is.na(raw_response) &
          raw_response == "NR",
        
        
        response = map_dbl(
          
          raw_response,
          
          function(value) {
            
            if (
              is.na(value) ||
              value == "NR"
            ) {
              return(NA_real_)
            }
            
            as.numeric(value)
          }
        ),
        
        
        notes = map_chr(
          
          id,
          
          function(question_id) {
            
            value <-
              input[[paste0(
                question_id,
                "_notes"
              )]]
            
            if (
              is.null(value) ||
              length(value) == 0
            ) {
              return("")
            }
            
            as.character(value)
          }
        )
      )
  })
  
  
  # ==========================================================
  # SAVE CASE OBJECT
  # ==========================================================
  
  case_data <- reactive({
    
    list(
      
      use_case_id =
        input$use_case_id,
      
      assessment_owner =
        input$assessment_owner,
      
      saved_date =
        as.character(Sys.time()),
      
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
  # SAVE
  # ==========================================================
  
  observeEvent(
    input$save_case,
    {
      
      case_id <- trimws(
        input$use_case_id
      )
      
      if (
        is.null(case_id) ||
        case_id == ""
      ) {
        
        save_message(
          "Please enter or generate a Use Case ID before saving."
        )
        
        return()
      }
      
      
      safe_id <- gsub(
        "[^A-Za-z0-9_-]",
        "_",
        case_id
      )
      
      
      save_path <- file.path(
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
        choices = get_saved_cases(),
        selected = safe_id
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
  # REFRESH SAVED CASE LIST
  # ==========================================================
  
  observe({
    
    updateSelectInput(
      session,
      "saved_case_select",
      choices = get_saved_cases()
    )
  })
  
  
  observeEvent(
    input$refresh_saved_cases,
    {
      
      updateSelectInput(
        session,
        "saved_case_select",
        choices = get_saved_cases()
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
      
      
      save_path <- file.path(
        SAVED_CASE_DIR,
        paste0(
          input$saved_case_select,
          ".rds"
        )
      )
      
      
      if (!file.exists(save_path)) {
        
        save_message(
          "Saved assessment could not be found."
        )
        
        return()
      }
      
      
      saved <- readRDS(
        save_path
      )
      
      
      updateTextInput(
        session,
        "use_case_id",
        value = saved$use_case_id
      )
      
      
      updateTextInput(
        session,
        "assessment_owner",
        value = saved$assessment_owner
      )
      
      
      updateTextInput(
        session,
        "project_name",
        value = saved$project_name
      )
      
      
      updateTextAreaInput(
        session,
        "project_description",
        value = saved$project_description
      )
      
      
      updateCheckboxGroupInput(
        session,
        "ai_uses",
        selected = saved$ai_uses
      )
      
      
      updateSelectInput(
        session,
        "lifecycle",
        selected = saved$lifecycle
      )
      
      
      updateSelectInput(
        session,
        "audience",
        selected = saved$audience
      )
      
      
      updateSelectInput(
        session,
        "ai_type",
        selected = saved$ai_type
      )
      
      
      updateSelectInput(
        session,
        "environment",
        selected = saved$environment
      )
      
      
      updateSelectInput(
        session,
        "provider",
        selected = saved$provider
      )
      
      
      updateTextInput(
        session,
        "model_name",
        value = saved$model_name
      )
      
      
      updateTextInput(
        session,
        "model_version",
        value = saved$model_version
      )
      
      
      updateSelectInput(
        session,
        "hosting",
        selected = saved$hosting
      )
      
      
      updateSelectInput(
        session,
        "access_method",
        selected = saved$access_method
      )
      
      
      if (!is.null(saved$monthly_tokens)) {
        
        updateNumericInput(
          session,
          "monthly_tokens",
          value = saved$monthly_tokens
        )
      }
      
      
      if (!is.null(saved$monthly_cost)) {
        
        updateNumericInput(
          session,
          "monthly_cost",
          value = saved$monthly_cost
        )
      }
      
      
      if (!is.null(saved$usage_monitoring)) {
        
        updateSelectInput(
          session,
          "usage_monitoring",
          selected = saved$usage_monitoring
        )
      }
      
      
      if (!is.null(saved$usage_limits)) {
        
        updateSelectInput(
          session,
          "usage_limits",
          selected = saved$usage_limits
        )
      }
      
      
      if (!is.null(saved$rag_external)) {
        
        updateSelectInput(
          session,
          "rag_external",
          selected = saved$rag_external
        )
      }
      
      
      if (!is.null(saved$rag_permissions)) {
        
        updateSelectInput(
          session,
          "rag_permissions",
          selected = saved$rag_permissions
        )
      }
      
      
      if (!is.null(saved$prompt_injection_testing)) {
        
        updateSelectInput(
          session,
          "prompt_injection_testing",
          selected = saved$prompt_injection_testing
        )
      }
      
      
      if (!is.null(saved$agent_actions)) {
        
        updateSelectInput(
          session,
          "agent_actions",
          selected = saved$agent_actions
        )
      }
      
      
      if (!is.null(saved$agent_permissions)) {
        
        updateSelectInput(
          session,
          "agent_permissions",
          selected = saved$agent_permissions
        )
      }
      
      
      if (!is.null(saved$answers)) {
        
        for (
          i in seq_len(
            nrow(saved$answers)
          )
        ) {
          
          question_id <-
            saved$answers$id[i]
          
          answer_value <-
            saved$answers$raw_response[i]
          
          note_value <-
            saved$answers$notes[i]
          
          
          if (!is.na(answer_value)) {
            
            updateRadioButtons(
              session,
              question_id,
              selected = answer_value
            )
          }
          
          
          updateTextAreaInput(
            session,
            paste0(
              question_id,
              "_notes"
            ),
            value = note_value
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
    
    percent <- round(
      100 *
        completed_questions() /
        TOTAL_ASSESSMENT_QUESTIONS
    )
    
    
    div(
      class = "progress",
      
      div(
        class = "progress-bar",
        style = paste0(
          "width:",
          percent,
          "%"
        ),
        paste0(
          percent,
          "%"
        )
      )
    )
  })
  
  
  # ==========================================================
  # SCORE ANSWERS
  # ==========================================================
  
  scored_answers <- reactive({
    
    answers() %>%
      
      mutate(
        
        adjusted_score = case_when(
          
          not_relevant ~
            NA_real_,
          
          type == "risk" ~
            response,
          
          type == "control" ~
            4 - response,
          
          TRUE ~
            NA_real_
        ),
        
        weighted_score =
          adjusted_score *
          weight,
        
        maximum_score =
          if_else(
            is.na(adjusted_score),
            NA_real_,
            4 * weight
          )
      )
  })
  
  
  # ==========================================================
  # SCORES
  # ==========================================================
  
  inherent_score <- reactive({
    
    dat <- answers() %>%
      
      filter(
        type == "risk",
        !is.na(response)
      )
    
    
    if (nrow(dat) == 0) {
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
  
  
  control_score <- reactive({
    
    dat <- answers() %>%
      
      filter(
        type == "control",
        !is.na(response)
      )
    
    
    if (nrow(dat) == 0) {
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
  
  
  residual_score <- reactive({
    
    inherent_score() *
      (
        1 -
          (
            control_score() /
              100 *
              0.60
          )
      )
  })
  
  
  risk_label <- reactive({
    
    if (!assessment_complete()) {
      return("INCOMPLETE")
    }
    
    
    score <- residual_score()
    
    
    case_when(
      score < 20 ~ "LOW",
      score < 40 ~ "MODERATE",
      score < 60 ~ "HIGH",
      score < 75 ~ "VERY HIGH",
      TRUE ~ "ESCALATION REQUIRED"
    )
  })
  
  
  # ==========================================================
  # RESULT TEXT
  # ==========================================================
  
  output$project_heading <- renderUI({
    
    tagList(
      
      if (
        !is.null(input$use_case_id) &&
        input$use_case_id != ""
      ) {
        
        div(
          class = "alert alert-light",
          strong("Use Case ID: "),
          input$use_case_id
        )
      },
      
      if (
        !is.null(input$project_name) &&
        input$project_name != ""
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
  
  
  output$inherent_label <- renderText({
    
    case_when(
      inherent_score() < 20 ~ "Low",
      inherent_score() < 40 ~ "Moderate",
      inherent_score() < 60 ~ "High",
      TRUE ~ "Very high"
    )
  })
  
  
  output$control_label <- renderText({
    
    case_when(
      control_score() >= 80 ~ "Strong",
      control_score() >= 60 ~ "Good",
      control_score() >= 40 ~ "Moderate",
      control_score() >= 20 ~ "Weak",
      TRUE ~ "Very weak"
    )
  })
  
  
  output$inherent_score_text <- renderText({
    
    paste0(
      round(
        inherent_score(),
        1
      ),
      "%"
    )
  })
  
  
  output$control_score_text <- renderText({
    
    paste0(
      round(
        control_score(),
        1
      ),
      "%"
    )
  })
  
  
  output$residual_score_text <- renderText({
    
    if (!assessment_complete()) {
      
      paste0(
        completed_questions(),
        "/25 completed"
      )
      
    } else {
      
      paste0(
        round(
          residual_score(),
          1
        ),
        "%"
      )
    }
  })
  
  
  output$risk_badge <- renderUI({
    
    span(
      class = "badge bg-primary",
      style = "
      font-size:20px;
      padding:14px;
      ",
      risk_label()
    )
  })
  
  
  # ==========================================================
  # DOMAIN SCORES
  # ==========================================================
  
  domain_scores <- reactive({
    
    scored_answers() %>%
      
      filter(
        !is.na(adjusted_score)
      ) %>%
      
      group_by(
        domain
      ) %>%
      
      summarise(
        
        risk_score =
          100 *
          sum(
            weighted_score
          ) /
          sum(
            maximum_score
          ),
        
        .groups = "drop"
      )
  })
  
  
  output$risk_plot <- renderPlot({
    
    dat <- domain_scores()
    
    
    validate(
      need(
        nrow(dat) > 0,
        "Complete some assessment questions to see the risk profile."
      )
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
      
      geom_col(
        fill = "#347CA9"
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        limits = c(
          0,
          100
        )
      ) +
      
      labs(
        x = NULL,
        y = "Concern (%)"
      ) +
      
      theme_minimal()
  })
  
  
  # ==========================================================
  # KEY RISKS
  # ==========================================================
  
  output$key_risks <- renderUI({
    
    dat <- scored_answers() %>%
      
      filter(
        !is.na(adjusted_score),
        adjusted_score >= 3
      )
    
    
    if (nrow(dat) == 0) {
      
      return(
        div(
          class = "alert alert-success",
          "No high-concern responses identified."
        )
      )
    }
    
    
    tags$ul(
      lapply(
        dat$question,
        tags$li
      )
    )
  })
  
  
  # ==========================================================
  # RECOMMENDATIONS
  # ==========================================================
  
  output$recommendations <- renderUI({
    
    dat <- scored_answers() %>%
      
      filter(
        !is.na(adjusted_score),
        adjusted_score >= 2
      ) %>%
      
      distinct(
        recommended_action
      )
    
    
    if (nrow(dat) == 0) {
      
      return(
        p(
          "No additional assurance activities currently identified."
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
    
    label <- risk_label()
    
    
    if (label == "INCOMPLETE") {
      
      return(
        p(
          "Complete all 25 questions before relying on the overall rating."
        )
      )
    }
    
    
    if (label == "LOW") {
      
      return(
        div(
          class = "alert alert-success",
          "Proceed with proportionate standard assurance."
        )
      )
    }
    
    
    if (label == "MODERATE") {
      
      return(
        div(
          class = "alert alert-warning",
          "Proceed once identified controls and assurance actions have been addressed."
        )
      )
    }
    
    
    div(
      class = "alert alert-danger",
      paste(
        "Enhanced assurance or specialist review should be completed",
        "before operational use."
      )
    )
  })
  
  
  output$flag_output <- renderUI({
    
    p(
      paste(
        "Use the overall rating, key risks and recommended assurance",
        "activities to determine whether escalation is required."
      )
    )
  })
  
  
  # ==========================================================
  # ADDITIONAL CONSIDERATIONS
  # ==========================================================
  
  output$additional_considerations_output <- renderUI({
    
    if (
      is.null(input$additional_considerations) ||
      input$additional_considerations == ""
    ) {
      
      p(
        class = "text-muted",
        "No additional considerations recorded."
      )
      
    } else {
      
      p(
        input$additional_considerations
      )
    }
  })
  
  
  # ==========================================================
  # DETAILED TABLE
  # ==========================================================
  
  output$risk_table <- renderDT({
    
    dat <- answers() %>%
      
      transmute(
        
        Domain =
          domain,
        
        Question =
          question,
        
        Response =
          case_when(
            raw_response == "NR" ~
              "Not relevant",
            
            is.na(raw_response) ~
              "Not answered",
            
            TRUE ~
              raw_response
          ),
        
        `User notes / rationale` =
          notes,
        
        Reference =
          source,
        
        `Recommended action` =
          recommended_action
      )
    
    
    datatable(
      dat,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
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
          nrow(references)
        ),
        
        function(i) {
          
          ref <- references[i, ]
          
          
          card(
            
            class = "mb-3",
            
            card_header(
              h4(ref$source)
            ),
            
            card_body(
              
              p(
                strong(ref$organisation),
                paste0(
                  " — ",
                  ref$date
                )
              ),
              
              p(
                ref$purpose
              ),
              
              tags$a(
                href = ref$url,
                target = "_blank",
                class = "btn btn-outline-primary",
                "Open reference"
              )
            )
          )
        }
      )
    )
  })
  
  
  # ==========================================================
  # DOWNLOAD
  # ==========================================================
  
  output$download_assessment <- downloadHandler(
    
    filename = function() {
      
      safe_id <- if (
        is.null(input$use_case_id) ||
        input$use_case_id == ""
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
      
      results <- answers() %>%
        
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
          
          lifecycle =
            input$lifecycle,
          
          ai_type =
            input$ai_type,
          
          model_name =
            input$model_name,
          
          model_version =
            input$model_version,
          
          overall_rating =
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
            ),
          
          additional_considerations =
            input$additional_considerations
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
# 10. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)