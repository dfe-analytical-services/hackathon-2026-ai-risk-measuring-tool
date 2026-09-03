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

library(scales)

library(rmarkdown)

library(knitr)
# ============================================================
# 2. APP SETTINGS
# ============================================================

APP_NAME <- "AI RiskCheck"
APP_VERSION <- "1.1 Prototype"

APP_LAST_UPDATED <- "2 September 2026"
REFERENCES_LAST_REVIEWED <- "2 September 2026"

TOTAL_ASSESSMENT_QUESTIONS <- 25

CLASSIFICATION_LABEL <-
  "OFFICIAL-SENSITIVE — INTERNAL USE ONLY — DfE EMPLOYEES ONLY"


# ============================================================
# 3. RESPONSE OPTIONS
# ============================================================

# Risk questions:
# Higher value = greater inherent risk
# NR = Not relevant and excluded from scoring

risk_options <- c(
  "None / negligible" = "0",
  "Low" = "1",
  "Moderate" = "2",
  "High" = "3",
  "Very high" = "4",
  "Not relevant" = "NR"
)


# Control questions:
# Higher value = stronger control

control_options <- c(
  "No" = "0",
  "Mostly no" = "1",
  "Partly" = "2",
  "Mostly yes" = "3",
  "Yes" = "4",
  "Not relevant" = "NR"
)


# ============================================================
# 4. 25-QUESTION ASSESSMENT MATRIX
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
  # 3 QUESTIONS
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
  # 4 QUESTIONS
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
    "with the authority and evidence needed to challenge the AI."
  ),
  
  
  # ==========================================================
  # 3. DATA, PRIVACY & LEGAL
  # 4 QUESTIONS
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
  # 4 QUESTIONS
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
    "edge cases and important known failure modes."
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
    "and conditions for proceeding, revising or stopping the use case."
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
    "Retain appropriate records of models, prompts, data sources,",
    "retrieved documents, important settings and analytical decisions."
  ),
  
  
  # ==========================================================
  # 5. FAIRNESS, TRANSPARENCY & STAKEHOLDERS
  # 3 QUESTIONS
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
    "and circumstances where outputs should not be relied on."
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
    "Identify relevant stakeholders and affected groups and provide",
    "proportionate routes for feedback, challenge and redress."
  ),
  
  
  # ==========================================================
  # 6. SECURITY, PLATFORM & SUPPLIER
  # 3 QUESTIONS
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
    "Confirm that the platform is approved for the intended",
    "use and information before implementation."
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
    "Assess relevant AI-specific threats, restrict access using",
    "least privilege and test important security controls."
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
  # 2 QUESTIONS
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
    "Assign a named accountable owner and document review,",
    "escalation and incident-management arrangements."
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
  # 2 QUESTIONS
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
    "Introduce ongoing monitoring, change control and proportionate",
    "retesting following material changes."
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
    "Complete formal evaluation before operational deployment and",
    "establish clear pause, rollback or decommissioning arrangements."
  )
)


# ============================================================
# CHECK EXACTLY 25 QUESTIONS
# ============================================================

stopifnot(
  nrow(questions) == TOTAL_ASSESSMENT_QUESTIONS
)


# ============================================================
# 5. REFERENCES
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
  "Government guidance for using AI safely, responsibly and effectively.",
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
# 6. CSS
# ============================================================

app_css <- HTML("

/* ==========================================================
   PAGE
   ========================================================== */

body {
  background-color: #f4f8fb;
  color: #0b0c0c;
  font-size: 16px;
}

.container,
.container-fluid {
  padding-top: 8px;
}


/* ==========================================================
   NAVBAR
   ========================================================== */

.navbar {
  background-color: #003764 !important;
  border-bottom: 5px solid #347CA9;
  padding-top: 10px;
  padding-bottom: 10px;
}

.navbar-brand {
  color: white !important;
  font-weight: 800;
}

.ai-riskcheck-title {
  display: flex;
  align-items: center;
  gap: 12px;
  color: white;
  font-size: 21px;
  font-weight: 800;
}


/* ==========================================================
   NAV TABS
   ========================================================== */

.navbar-nav .nav-link {
  color: white !important;
  font-weight: 700;
  margin-right: 6px;
  padding: 10px 15px !important;
  border-radius: 5px 5px 0 0;
}

.navbar-nav .nav-item:nth-child(1) .nav-link {
  background-color: #1d70b8;
}

.navbar-nav .nav-item:nth-child(2) .nav-link {
  background-color: #158187;
}

.navbar-nav .nav-item:nth-child(3) .nav-link {
  background-color: #54319f;
}

.navbar-nav .nav-item:nth-child(4) .nav-link {
  background-color: #0f7a52;
}

.navbar-nav .nav-item:nth-child(5) .nav-link {
  background-color: #347CA9;
}

.navbar-nav .nav-link:hover {
  text-decoration: underline;
  filter: brightness(90%);
}

.navbar-nav .nav-link.active {
  background-color: white !important;
  color: #0b0c0c !important;
  border-bottom: 5px solid #ffdd00;
}


/* ==========================================================
   CLASSIFICATION BANNER
   ========================================================== */

.classification-banner {
  background-color: #fff8cc;
  color: #0b0c0c;
  border-bottom: 3px solid #ffdd00;
  padding: 8px 20px;
  text-align: center;
  font-size: 14px;
  font-weight: 800;
  letter-spacing: 0.3px;
}


/* ==========================================================
   HEADINGS
   ========================================================== */

h1 {
  color: #003764;
  font-weight: 800;
}

h2 {
  color: #003764;
  font-weight: 800;
  border-bottom: 4px solid #347CA9;
  padding-bottom: 8px;
  margin-bottom: 22px;
}

h3 {
  color: #003764;
  font-weight: 800;
}

h4,
h5 {
  color: #0b0c0c;
  font-weight: 700;
}


/* ==========================================================
   CARDS
   ========================================================== */

.card {
  border: 1px solid #cecece;
  border-radius: 7px;
  background-color: white;
  box-shadow: 0 2px 6px rgba(0,0,0,0.07);
  overflow: visible;
}

.card-header {
  background-color: #f4f8fb;
  border-bottom: 4px solid #347CA9;
  padding: 15px 20px;
  font-weight: 800;
}

.card-header h3,
.card-header h4 {
  margin: 0;
  color: #003764;
  font-weight: 800;
}

.card-body {
  padding: 22px;
}


/* ==========================================================
   ASSESSMENT DOMAIN COLOURS
   ========================================================== */

#questions_ui > .card:nth-child(1) {
  border-left: 7px solid #1d70b8;
}

#questions_ui > .card:nth-child(2) {
  border-left: 7px solid #ca357c;
}

#questions_ui > .card:nth-child(3) {
  border-left: 7px solid #158187;
}

#questions_ui > .card:nth-child(4) {
  border-left: 7px solid #54319f;
}

#questions_ui > .card:nth-child(5) {
  border-left: 7px solid #f47738;
}

#questions_ui > .card:nth-child(6) {
  border-left: 7px solid #ca3535;
}

#questions_ui > .card:nth-child(7) {
  border-left: 7px solid #347CA9;
}

#questions_ui > .card:nth-child(8) {
  border-left: 7px solid #0f7a52;
}


/* ==========================================================
   QUESTION STYLING
   ========================================================== */

.question-title {
  font-size: 1.12rem;
  font-weight: 750;
  line-height: 1.4;
  color: #0b0c0c;
}

.question-number {
  display: inline-block;
  background-color: #003764;
  color: white;
  font-weight: 800;
  min-width: 31px;
  height: 31px;
  line-height: 31px;
  text-align: center;
  border-radius: 50%;
  margin-right: 9px;
}


/* ==========================================================
   INFO ICON
   ========================================================== */

.ai-info-icon {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 25px;
  height: 25px;
  border-radius: 50%;
  background-color: #1d70b8;
  color: white;
  font-size: 15px;
  font-weight: 800;
  cursor: help;
  flex-shrink: 0;
}

.ai-info-icon:hover {
  background-color: #0f385c;
}


/* ==========================================================
   FORM CONTROLS
   ========================================================== */

textarea.form-control,
input.form-control,
select.form-select {
  border: 2px solid #0b0c0c;
  border-radius: 3px;
}

textarea.form-control:focus,
input.form-control:focus,
select.form-select:focus {
  border-color: #0b0c0c;
  box-shadow: 0 0 0 4px #ffdd00;
}

.radio label,
.checkbox label {
  padding: 5px 8px;
}

.radio label:hover,
.checkbox label:hover {
  background-color: #f4f8fb;
  border-radius: 4px;
}


/* ==========================================================
   NOTES
   ========================================================== */

.notes-label {
  font-weight: 700;
  color: #484949;
  margin-top: 8px;
}


/* ==========================================================
   SIDEBAR
   ========================================================== */

.bslib-sidebar-layout > .sidebar {
  background-color: #f4f8fb;
  border-right: 4px solid #347CA9;
}


/* ==========================================================
   PROGRESS
   ========================================================== */

.progress {
  height: 25px;
  background-color: #cecece;
}

.progress-bar {
  background-color: #1d70b8;
  font-weight: 800;
}


/* ==========================================================
   ALERTS
   ========================================================== */

.alert-info {
  background-color: #f4f8fb;
  border-left: 5px solid #1d70b8;
  color: #0b0c0c;
}

.alert-warning {
  background-color: #fff8cc;
  border-left: 5px solid #ffdd00;
  color: #0b0c0c;
}

.alert-success {
  background-color: #f3f8f6;
  border-left: 5px solid #0f7a52;
  color: #0b0c0c;
}

.alert-danger {
  background-color: #fcf5f5;
  border-left: 5px solid #ca3535;
  color: #0b0c0c;
}

.alert-secondary {
  border-left: 5px solid #347CA9;
}


/* ==========================================================
   RESULTS CARDS
   ========================================================== */

.result-card-overall {
  border-top: 7px solid #1d70b8;
}

.result-card-inherent {
  border-top: 7px solid #f47738;
}

.result-card-controls {
  border-top: 7px solid #0f7a52;
}

.result-value {
  font-size: 2rem;
  font-weight: 800;
}


/* ==========================================================
   TABLES
   ========================================================== */

table.dataTable thead th {
  background-color: #003764 !important;
  color: white !important;
  font-weight: 800;
}

.table-striped > tbody > tr:nth-of-type(odd) > * {
  background-color: #f4f8fb;
}


/* ==========================================================
   LINKS / BUTTONS
   ========================================================== */

a {
  color: #1a65a6;
  font-weight: 600;
}

a:hover {
  color: #0f385c;
}

.btn-primary,
.btn-default,
.btn-download {
  background-color: #1d70b8;
  border-color: #1d70b8;
  color: white;
  font-weight: 700;
}

.btn-primary:hover,
.btn-default:hover {
  background-color: #0f385c;
  border-color: #0f385c;
}


/* ==========================================================
   ACCESSIBILITY FOCUS
   ========================================================== */

a:focus,
button:focus,
input:focus,
textarea:focus,
select:focus {
  outline: 4px solid #ffdd00 !important;
  outline-offset: 2px;
}


/* ==========================================================
   MOBILE
   ========================================================== */

@media (max-width: 768px) {

  .navbar-nav .nav-link {
    margin-bottom: 5px;
  }

  .card-body {
    padding: 15px;
  }

  .classification-banner {
    font-size: 12px;
  }
}

")


# ============================================================
# 7. USER INTERFACE
# ============================================================

ui <- page_navbar(
  
  title = div(
    
    class = "ai-riskcheck-title",
    
    tags$img(
      src = "dfe-logo.png",
      height = "40px",
      alt = "Department for Education logo"
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
          
          div(
            
            style = "
              display:flex;
              align-items:center;
              gap:15px;
              margin-bottom:18px;
            ",
            
            tags$img(
              src = "dfe-logo.png",
              height = "58px",
              alt = "Department for Education logo"
            ),
            
            div(
              h1(
                style = "margin-bottom:2px;",
                APP_NAME
              ),
              h4(
                "Responsible AI Risk Self-Assessment Tool"
              )
            )
          ),
          
          p(
            class = "lead",
            paste(
              "A simple and consistent way for analytical teams to",
              "identify and assess risks associated with AI-assisted work."
            )
          ),
          
          hr(),
          
          p(
            paste(
              "AI is increasingly used to generate code, create deliverables,",
              "summarise evidence, analyse data and improve analytical processes."
            )
          ),
          
          p(
            paste(
              "AI RiskCheck helps identify quality, ethical, legal, security,",
              "governance and technical risks associated with these uses."
            )
          ),
          
          div(
            class = "alert alert-info",
            
            strong("Important: "),
            
            paste(
              "AI RiskCheck supports professional judgement. It does not replace",
              "analytical QA, information assurance, security, data protection,",
              "legal, commercial or other required departmental approval processes."
            )
          ),
          
          div(
            class = "alert alert-secondary",
            
            strong("Questions or support: "),
            
            paste(
              "If you are unsure how to answer a question, interpret your risk",
              "rating or decide what assurance action may be appropriate,",
              "contact the AOE Centre of Excellence team."
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
          
          p(
            "Work through the five tabs from left to right."
          ),
          
          fluidRow(
            
            column(
              6,
              
              h4("1. Overview"),
              p(
                "Understand what the tool is, when to use it and what it assesses."
              ),
              
              h4("2. AI & Model Profile"),
              p(
                "Describe your use case, model, provider, environment and AI architecture."
              ),
              
              h4("3. Risk Assessment"),
              p(
                "Complete the 25 questions and record optional notes or rationale."
              )
            ),
            
            column(
              6,
              
              h4("4. Results"),
              p(
                "Review risk scores, key concerns, escalation conditions and assurance actions."
              ),
              
              h4("5. References"),
              p(
                "View the supporting documents, application version and review dates."
              )
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
          
          p(
            paste(
              "Consider completing AI RiskCheck when developing, piloting,",
              "using or materially changing an AI-enabled analytical process."
            )
          ),
          
          tags$ul(
            
            tags$li(
              "Generating, explaining or assisting with analytical code"
            ),
            
            tags$li(
              "Producing analytical commentary, reports or deliverables"
            ),
            
            tags$li(
              "Summarising evidence, research or documents"
            ),
            
            tags$li(
              "Analysing, classifying or extracting information from data"
            ),
            
            tags$li(
              "Generating statistics, calculations or quantitative outputs"
            ),
            
            tags$li(
              "Supporting analytical, policy or operational decisions"
            ),
            
            tags$li(
              "Automating analytical processes or workflows"
            ),
            
            tags$li(
              "Using LLMs, RAG, agents or machine-learning models"
            ),
            
            tags$li(
              "Moving from prototype or pilot into operational use"
            )
          ),
          
          div(
            
            class = "alert alert-warning",
            
            strong("Reassess when: "),
            
            paste(
              "the model, prompt, data source, intended use or user group changes;",
              "AI is given more autonomy; the system moves lifecycle stage;",
              "or a material incident or new risk is identified."
            )
          )
        )
      ),
      
      br(),
      
      
      card(
        
        card_header(
          h3("What does AI RiskCheck assess?")
        ),
        
        card_body(
          
          p(
            "The 25 questions cover eight assessment areas:"
          ),
          
          fluidRow(
            
            column(
              6,
              
              tags$ul(
                tags$li("Purpose, value & appropriateness"),
                tags$li("Impact & human oversight"),
                tags$li("Data, privacy & legal"),
                tags$li("Quality, testing & reliability")
              )
            ),
            
            column(
              6,
              
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
  # AI & MODEL PROFILE
  # ==========================================================
  
  nav_panel(
    
    "AI & Model Profile",
    
    div(
      
      class = "container mt-4",
      
      h2("AI & Model Profile"),
      
      p(
        paste(
          "Provide contextual information about the AI use case.",
          "These fields do not directly form part of the 25-question score."
        )
      ),
      
      
      card(
        
        card_header(
          h3("About your AI use case")
        ),
        
        card_body(
          
          textInput(
            "project_name",
            "Project / use case name",
            placeholder = "e.g. AI-assisted publication QA"
          ),
          
          textAreaInput(
            "project_description",
            "Describe how AI is being used",
            rows = 5,
            placeholder = paste(
              "Describe the problem, how AI is used, what it produces",
              "and how the output will be used."
            )
          ),
          
          checkboxGroupInput(
            
            "ai_uses",
            
            "How is AI being used in this project?",
            
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
              6,
              
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
      
      
      card(
        
        card_header(
          h3("AI technology")
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
            "What type of AI environment is being used?",
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
            placeholder = "e.g. Claude, GPT, Gemini, Llama"
          ),
          
          textInput(
            "model_version",
            "Model version",
            placeholder = "Enter version if known"
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
  # RISK ASSESSMENT
  # ==========================================================
  
  nav_panel(
    
    "Risk Assessment",
    
    layout_sidebar(
      
      sidebar = sidebar(
        
        h4("Assessment progress"),
        
        textOutput(
          "progress_text"
        ),
        
        br(),
        
        uiOutput(
          "progress_bar"
        ),
        
        hr(),
        
        p(
          strong("25 questions")
        ),
        
        p(
          paste(
            "Select Not relevant only where a question genuinely does not apply.",
            "Not relevant responses are excluded from scoring."
          )
        ),
        
        p(
          paste(
            "Use the optional notes field to record context, evidence,",
            "assumptions or rationale."
          )
        ),
        
        div(
          class = "alert alert-light",
          "Hover over the blue information icon for more information."
        )
      ),
      
      div(
        
        class = "p-3",
        
        uiOutput(
          "questions_ui"
        ),
        
        br(),
        
        card(
          
          class = "mb-4",
          
          card_header(
            h3("Anything else we should know?")
          ),
          
          card_body(
            
            p(
              paste(
                "Record additional risks, concerns, assumptions, dependencies",
                "or context not covered by the 25 questions."
              )
            ),
            
            textAreaInput(
              "additional_considerations",
              label = NULL,
              placeholder = paste(
                "For example: known limitations, unusual dependencies,",
                "outstanding decisions or additional assurance required."
              ),
              rows = 6,
              width = "100%"
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
            
            card_header(
              "Overall rating"
            ),
            
            card_body(
              uiOutput("risk_badge"),
              br(),
              br(),
              div(
                class = "result-value",
                textOutput("residual_score_text")
              ),
              p("Residual risk")
            )
          )
        ),
        
        column(
          4,
          
          card(
            class = "result-card-inherent",
            
            card_header(
              "Inherent risk"
            ),
            
            card_body(
              h2(textOutput("inherent_label")),
              h4(textOutput("inherent_score_text")),
              p("Risk before safeguards are taken into account.")
            )
          )
        ),
        
        column(
          4,
          
          card(
            class = "result-card-controls",
            
            card_header(
              "Control strength"
            ),
            
            card_body(
              h2(textOutput("control_label")),
              h4(textOutput("control_score_text")),
              p("Strength of safeguards currently in place.")
            )
          )
        )
      ),
      
      br(),
      
      
      fluidRow(
        
        column(
          7,
          
          card(
            full_screen = TRUE,
            card_header("Risk profile"),
            card_body(
              plotOutput(
                "risk_plot",
                height = "450px"
              )
            )
          )
        ),
        
        column(
          5,
          
          card(
            card_header("Decision summary"),
            card_body(
              uiOutput("decision_summary")
            )
          )
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
        card_header("Additional considerations recorded by the user"),
        card_body(
          uiOutput("additional_considerations_output")
        )
      ),
      
      br(),
      
      
      card(
        card_header("Detailed assessment"),
        card_body(
          DTOutput("risk_table")
        )
      ),
      
      br(),
      
      
      card(
        card_header("AI system profile"),
        card_body(
          uiOutput("system_profile")
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
  # REFERENCES
  # ==========================================================
  
  nav_panel(
    
    "References",
    
    div(
      
      class = "container mt-4",
      
      h2(
        "References and supporting documents"
      ),
      
      p(
        paste(
          "The assessment has been developed with reference to",
          "the following government and National Audit Office documents."
        )
      ),
      
      div(
        class = "alert alert-info",
        
        strong("Important: "),
        
        paste(
          "AI RiskCheck summarises themes from these documents for analytical",
          "self-assessment. Consult the original source where detailed guidance",
          "or specialist interpretation is required."
        )
      ),
      
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
              ),
              
              tags$tr(
                tags$th("Primary audience"),
                tags$td("DfE employees / analytical teams")
              ),
              
              tags$tr(
                tags$th("Methodology status"),
                tags$td(
                  "Prototype - scoring and thresholds require internal validation"
                )
              )
            )
          )
        )
      ),
      
      br(),
      
      h3("Reference documents"),
      
      uiOutput(
        "reference_cards"
      ),
      
      br()
    )
  )
)


# ============================================================
# 8. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # QUESTION UI
  # ==========================================================
  
  output$questions_ui <- renderUI({
    
    domains <- unique(
      questions$domain
    )
    
    question_counter <- 0
    
    
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
              
              div(
                
                style = "
                  display:flex;
                  justify-content:space-between;
                  align-items:center;
                ",
                
                h3(
                  style = "margin-bottom:0;",
                  domain_name
                ),
                
                tags$span(
                  paste0(
                    nrow(domain_questions),
                    " questions"
                  )
                )
              )
            ),
            
            card_body(
              
              tagList(
                
                map(
                  
                  seq_len(
                    nrow(domain_questions)
                  ),
                  
                  function(i) {
                    
                    question_counter <<-
                      question_counter + 1
                    
                    
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
                          align-items:flex-start;
                          gap:7px;
                          margin-bottom:12px;
                        ",
                        
                        tags$span(
                          class = "question-number",
                          question_counter
                        ),
                        
                        tags$span(
                          class = "question-title",
                          q$question
                        ),
                        
                        bslib::tooltip(
                          
                          tags$span(
                            class = "ai-info-icon",
                            "i"
                          ),
                          
                          tags$div(
                            
                            style = "
                              max-width:400px;
                              text-align:left;
                            ",
                            
                            tags$p(
                              q$help_text
                            ),
                            
                            tags$hr(),
                            
                            tags$p(
                              tags$strong("Reference: "),
                              q$source
                            ),
                            
                            tags$p(
                              tags$strong("Relevant principle: "),
                              q$principle
                            ),
                            
                            tags$p(
                              tags$strong("Recommended action: "),
                              q$recommended_action
                            )
                          ),
                          
                          placement = "right"
                        )
                      ),
                      
                      
                      radioButtons(
                        
                        inputId = q$id,
                        
                        label = NULL,
                        
                        choices = choices,
                        
                        selected = character(0)
                      ),
                      
                      
                      tags$div(
                        class = "notes-label",
                        "Optional notes / rationale"
                      ),
                      
                      textAreaInput(
                        
                        inputId = paste0(
                          q$id,
                          "_notes"
                        ),
                        
                        label = NULL,
                        
                        placeholder = paste(
                          "Add context, evidence, assumptions or explain",
                          "why you selected Not relevant."
                        ),
                        
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
  # ANSWERS
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
              
              return(
                NA_character_
              )
            }
            
            as.character(
              value
            )
          }
        ),
        
        
        not_relevant = case_when(
          
          is.na(raw_response) ~
            FALSE,
          
          raw_response == "NR" ~
            TRUE,
          
          TRUE ~
            FALSE
        ),
        
        
        response = map_dbl(
          
          raw_response,
          
          function(value) {
            
            if (
              is.na(value) ||
              value == "NR"
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
        
        
        notes = map_chr(
          
          id,
          
          function(question_id) {
            
            note_value <-
              input[[paste0(
                question_id,
                "_notes"
              )]]
            
            if (
              is.null(note_value) ||
              length(note_value) == 0 ||
              identical(note_value, "")
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
    
    percentage <- round(
      100 *
        completed_questions() /
        TOTAL_ASSESSMENT_QUESTIONS
    )
    
    
    div(
      
      class = "progress",
      
      div(
        
        class = "progress-bar",
        
        role = "progressbar",
        
        style = paste0(
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
  # RESPONSE LABELS
  # ==========================================================
  
  response_label <- function(
    type,
    raw_response
  ) {
    
    if (
      is.na(raw_response)
    ) {
      
      return(
        "Not answered"
      )
    }
    
    
    if (
      raw_response == "NR"
    ) {
      
      return(
        "Not relevant"
      )
    }
    
    
    response <- as.numeric(
      raw_response
    )
    
    
    if (
      type == "risk"
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
      response + 1
    ]
  }
  
  
  # ==========================================================
  # SCORING
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
          ),
        
        
        response_text =
          map2_chr(
            type,
            raw_response,
            response_label
          ),
        
        
        concern = case_when(
          
          raw_response == "NR" ~
            "Not relevant",
          
          is.na(raw_response) ~
            "Not answered",
          
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
      )
  })
  
  
  # ==========================================================
  # INHERENT RISK
  # ==========================================================
  
  inherent_score <- reactive({
    
    dat <- answers() %>%
      
      filter(
        type == "risk",
        !is.na(response)
      )
    
    
    if (
      nrow(dat) == 0
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
    
    dat <- answers() %>%
      
      filter(
        type == "control",
        !is.na(response)
      )
    
    
    if (
      nrow(dat) == 0
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
    
    result <- tibble(
      severity = character(),
      issue = character()
    )
    
    
    add_flag <- function(
    severity,
    issue
    ) {
      
      result <<- bind_rows(
        
        result,
        
        tibble(
          severity = severity,
          issue = issue
        )
      )
    }
    
    
    if (
      !is.null(input$impact_2) &&
      input$impact_2 != "NR" &&
      !is.null(input$human_1) &&
      input$human_1 != "NR" &&
      as.numeric(input$impact_2) >= 4 &&
      as.numeric(input$human_1) <= 1
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
      !is.null(input$impact_3) &&
      input$impact_3 != "NR" &&
      as.numeric(input$impact_3) >= 4
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
      input$data_2 != "NR" &&
      !is.null(input$security_1) &&
      input$security_1 != "NR" &&
      as.numeric(input$data_2) >= 3 &&
      as.numeric(input$security_1) <= 1
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
      input$impact_1 != "NR" &&
      !is.null(input$quality_1) &&
      input$quality_1 != "NR" &&
      as.numeric(input$impact_1) >= 3 &&
      as.numeric(input$quality_1) <= 1
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
      input$impact_1 != "NR" &&
      !is.null(input$quality_2) &&
      input$quality_2 != "NR" &&
      as.numeric(input$impact_1) >= 3 &&
      as.numeric(input$quality_2) <= 1
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
      input$ethics_1 != "NR" &&
      as.numeric(input$ethics_1) >= 3
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
      input$governance_1 != "NR" &&
      as.numeric(input$governance_1) <= 1
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
      !is.null(input$agent_actions) &&
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
  # RESIDUAL RISK
  # ==========================================================
  
  residual_score <- reactive({
    
    inherent <- inherent_score()
    controls <- control_score()
    
    
    score <- inherent *
      (
        1 -
          (
            controls /
              100 *
              0.60
          )
      )
    
    
    score <- max(
      score,
      0
    )
    
    
    if (
      any(
        flags()$severity ==
        "ESCALATE"
      )
    ) {
      
      score <- max(
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
      
      score <- max(
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
    
    
    score <- residual_score()
    
    
    case_when(
      
      score < 20 ~
        "LOW",
      
      score < 40 ~
        "MODERATE",
      
      score < 60 ~
        "HIGH",
      
      score < 75 ~
        "VERY HIGH",
      
      TRUE ~
        "ESCALATION REQUIRED"
    )
  })
  
  
  # ==========================================================
  # HELPERS
  # ==========================================================
  
  risk_description <- function(score) {
    
    case_when(
      score < 20 ~ "Low",
      score < 40 ~ "Moderate",
      score < 60 ~ "High",
      score < 75 ~ "Very high",
      TRUE ~ "Critical"
    )
  }
  
  
  control_description <- function(score) {
    
    case_when(
      score >= 80 ~ "Strong",
      score >= 60 ~ "Good",
      score >= 40 ~ "Moderate",
      score >= 20 ~ "Weak",
      TRUE ~ "Very weak"
    )
  }
  
  
  # ==========================================================
  # PROJECT HEADING
  # ==========================================================
  
  output$project_heading <- renderUI({
    
    if (
      is.null(input$project_name) ||
      input$project_name == ""
    ) {
      
      return(NULL)
    }
    
    
    tagList(
      
      h4(
        paste(
          "Use case:",
          input$project_name
        )
      ),
      
      p(
        paste(
          "Lifecycle:",
          input$lifecycle
        )
      )
    )
  })
  
  
  # ==========================================================
  # SCORE OUTPUTS
  # ==========================================================
  
  output$inherent_label <- renderText({
    
    if (
      completed_questions() == 0
    ) {
      return("Not assessed")
    }
    
    risk_description(
      inherent_score()
    )
  })
  
  
  output$control_label <- renderText({
    
    if (
      completed_questions() == 0
    ) {
      return("Not assessed")
    }
    
    control_description(
      control_score()
    )
  })
  
  
  output$inherent_score_text <- renderText({
    
    if (
      completed_questions() == 0
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
      completed_questions() == 0
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
  # BADGE
  # ==========================================================
  
  output$risk_badge <- renderUI({
    
    label <- risk_label()
    
    
    badge_class <- case_when(
      
      label == "INCOMPLETE" ~ "secondary",
      
      label == "LOW" ~ "success",
      
      label == "MODERATE" ~ "warning",
      
      label == "HIGH" ~ "warning",
      
      label == "VERY HIGH" ~ "danger",
      
      TRUE ~ "danger"
    )
    
    
    span(
      
      class = paste0(
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
            na.rm = TRUE
          ) /
          
          sum(
            maximum_score,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) %>%
      
      arrange(
        desc(
          risk_score
        )
      )
  })
  
  
  # ==========================================================
  # RISK PROFILE
  # ==========================================================
  
  output$risk_plot <- renderPlot({
    
    dat <- domain_scores()
    
    
    validate(
      
      need(
        nrow(dat) > 0,
        "Complete relevant assessment questions to see the risk profile."
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
      
      geom_text(
        
        aes(
          label = paste0(
            round(
              risk_score
            ),
            "%"
          )
        ),
        
        hjust = -0.1,
        fontface = "bold"
      ) +
      
      coord_flip() +
      
      scale_y_continuous(
        limits = c(
          0,
          110
        ),
        breaks = seq(
          0,
          100,
          20
        )
      ) +
      
      labs(
        x = NULL,
        y = "Risk / control concern (%)",
        title = "Risk profile by assessment area"
      ) +
      
      theme_minimal(
        base_size = 13
      ) +
      
      theme(
        plot.title = element_text(
          face = "bold"
        ),
        axis.text.y = element_text(
          face = "bold"
        )
      )
  })
  
  
  # ==========================================================
  # FLAGS
  # ==========================================================
  
  output$flag_output <- renderUI({
    
    if (
      !assessment_complete()
    ) {
      
      return(
        
        div(
          class = "alert alert-secondary",
          paste(
            "Complete all 25 questions before the final",
            "escalation assessment is determined."
          )
        )
      )
    }
    
    
    flag_data <- flags()
    
    
    if (
      nrow(flag_data) == 0
    ) {
      
      return(
        
        div(
          class = "alert alert-success",
          strong(
            "No automatic escalation or stop conditions were identified."
          )
        )
      )
    }
    
    
    tagList(
      
      lapply(
        
        seq_len(
          nrow(flag_data)
        ),
        
        function(i) {
          
          alert_class <- if (
            flag_data$severity[i] ==
            "STOP"
          ) {
            
            "alert alert-danger"
            
          } else {
            
            "alert alert-warning"
          }
          
          
          div(
            
            class = alert_class,
            
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
    
    dat <- scored_answers() %>%
      
      filter(
        !is.na(
          adjusted_score
        ),
        adjusted_score >= 3
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
      nrow(dat) == 0
    ) {
      
      return(
        
        div(
          class = "alert alert-success",
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
            
            class = "alert alert-warning",
            
            h5(
              dat$domain[i]
            ),
            
            p(
              dat$question[i]
            ),
            
            p(
              strong("Your response: "),
              dat$response_text[i]
            ),
            
            if (
              dat$notes[i] != ""
            ) {
              
              p(
                strong("Your notes: "),
                dat$notes[i]
              )
              
            } else {
              
              NULL
            },
            
            p(
              strong("Recommended action: "),
              dat$recommended_action[i]
            )
          )
        }
      )
    )
  })
  
  
# ============================================================
# RECOMMENDATIONS
# ============================================================

output$recommendations <- renderUI({

  dat <- recommendation_data()

  tags$ul(
    lapply(
      dat$recommended_action,
      tags$li
    )
  )
})
  
  # ==========================================================
  # ADDITIONAL CONSIDERATIONS
  # ==========================================================
  
  output$additional_considerations_output <- renderUI({
    
    if (
      is.null(
        input$additional_considerations
      ) ||
      identical(
        input$additional_considerations,
        ""
      )
    ) {
      
      return(
        
        p(
          class = "text-muted",
          "No additional considerations were recorded."
        )
      )
    }
    
    
    div(
      class = "alert alert-light",
      p(
        input$additional_considerations
      )
    )
  })
  
  
  # ==========================================================
  # DECISION SUMMARY
  # ==========================================================
  
  output$decision_summary <- renderUI({
    
    label <- risk_label()
    
    
    if (
      label == "INCOMPLETE"
    ) {
      
      return(
        
        tagList(
          h4("Assessment incomplete"),
          p(
            "Complete all 25 questions before relying on the overall rating."
          )
        )
      )
    }
    
    
    if (
      label == "LOW"
    ) {
      
      return(
        
        tagList(
          h4("Proceed with standard assurance"),
          p(
            paste(
              "The use case currently appears suitable to proceed",
              "with proportionate analytical QA and normal controls."
            )
          )
        )
      )
    }
    
    
    if (
      label == "MODERATE"
    ) {
      
      return(
        
        tagList(
          h4("Proceed with additional controls"),
          p(
            paste(
              "Address the identified areas requiring attention",
              "and document the assurance undertaken."
            )
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
        
        tagList(
          h4("Enhanced assurance required"),
          p(
            paste(
              "Additional testing, review and appropriate specialist",
              "assurance should be completed before operational use."
            )
          )
        )
      )
    }
    
    
    if (
      label == "ESCALATION REQUIRED"
    ) {
      
      return(
        
        tagList(
          h4("Escalation required"),
          p(
            paste(
              "One or more material concerns should be reviewed",
              "with the appropriate assurance or specialist team."
            )
          )
        )
      )
    }
    
    
    tagList(
      h4("Do not proceed without review"),
      p(
        paste(
          "AI RiskCheck has identified a stop condition.",
          "Resolve or formally review the issue before proceeding."
        )
      )
    )
  })
  
  
  # ==========================================================
  # DETAILED TABLE
  # ==========================================================
  
  output$risk_table <- renderDT({
    
    dat <- scored_answers() %>%
      
      filter(
        !is.na(
          raw_response
        )
      ) %>%
      
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
      
      rownames = FALSE,
      
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
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
        length(x) == 0 ||
        identical(x, "")
      ) {
        
        return(
          "Not provided"
        )
      }
      
      paste(
        x,
        collapse = "; "
      )
    }
    
    
    profile_rows <- list(
      
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
      
      class = "table table-striped",
      
      tags$tbody(
        
        lapply(
          
          profile_rows,
          
          function(row) {
            
            tags$tr(
              tags$th(row[1]),
              tags$td(row[2])
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
            references
          )
        ),
        
        function(i) {
          
          ref <- references[i, ]
          
          
          card(
            
            class = "mb-3",
            
            card_header(
              h4(
                ref$source
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
  # DOWNLOAD
  # ==========================================================
  


  
  output$download_assessment <- downloadHandler(
    
    filename = function() {
      
      project_name <- input$project_name
      
      
      if (
        is.null(project_name) ||
        project_name == ""
      ) {
        
        project_name <-
          "AI_RiskCheck"
      }
      
      
      safe_name <- gsub(
        "[^A-Za-z0-9]+",
        "_",
        project_name
      )
      
      
      paste0(
        safe_name,
        "_AI_RiskCheck_Assessment_",
        Sys.Date(),
        ".csv"
      )
    },
    
    
    content = function(file) {
      
      results <- scored_answers() %>%
        
        mutate(
          
          classification =
            CLASSIFICATION_LABEL,
          
          project_name =
            input$project_name,
          
          project_description =
            input$project_description,
          
          ai_uses =
            paste(
              input$ai_uses,
              collapse = "; "
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
          
          application_version =
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
              collapse = " | "
            )
        )
      
      
      write.csv(
        results,
        file,
        row.names = FALSE
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
# 9. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)