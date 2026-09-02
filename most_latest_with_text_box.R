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
APP_VERSION <- "1.0 Prototype"

APP_LAST_UPDATED <- "2 September 2026"
REFERENCES_LAST_REVIEWED <- "2 September 2026"

TOTAL_ASSESSMENT_QUESTIONS <- 25


# ============================================================
# 3. RESPONSE OPTIONS
# ============================================================

# Risk questions:
# Higher value = greater inherent risk
#
# "NR" = Not relevant and is excluded from scoring

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
#
# "NR" = Not relevant and is excluded from scoring

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
  paste(
    "Review of government AI adoption including governance,",
    "skills, testing, opportunities and scaling."
  ),
  "https://www.nao.org.uk/wp-content/uploads/2024/03/use-of-artificial-intelligence-in-government.pdf",
  
  "Good practice guide for organisations using AI",
  "National Audit Office",
  "May 2026",
  paste(
    "Good-practice guidance covering governance, data, security,",
    "skills, evaluation, risk management and scaling."
  ),
  "https://www.nao.org.uk/wp-content/uploads/2026/05/good-practice-guide-for-organisations-using-ai.pdf",
  
  "Artificial Intelligence Playbook for the UK Government",
  "UK Government",
  "2025",
  paste(
    "Government guidance for using AI safely, securely,",
    "responsibly and effectively."
  ),
  "https://www.gov.uk/government/publications/ai-playbook-for-the-uk-government/artificial-intelligence-playbook-for-the-uk-government-html",
  
  "AI Opportunities Action Plan",
  "UK Government",
  "2025",
  paste(
    "Government approach to identifying, piloting, evaluating",
    "and scaling AI opportunities."
  ),
  "https://www.gov.uk/government/publications/ai-opportunities-action-plan/ai-opportunities-action-plan",
  
  "Data and AI Ethics Framework",
  "UK Government",
  "2025",
  paste(
    "Framework covering transparency, accountability, fairness,",
    "privacy, societal impact and safety."
  ),
  "https://www.gov.uk/government/publications/data-ethics-framework/data-and-ai-ethics-framework"
)


# ============================================================
# 6. USER INTERFACE
# ============================================================

ui <- page_navbar(
  
  title = div(
    
    style = "
      display:flex;
      align-items:center;
      gap:10px;
    ",
    
    tags$img(
      src = "dfe-logo.png",
      height = "38px",
      alt = "Department for Education logo"
    ),
    
    strong(APP_NAME)
  ),
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  
  # ==========================================================
  # TAB 1 - OVERVIEW
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
              margin-bottom:15px;
            ",
            
            tags$img(
              src = "dfe-logo.png",
              height = "55px",
              alt = "Department for Education logo"
            ),
            
            h1(
              style = "margin:0;",
              APP_NAME
            )
          ),
          
          h3(
            "Responsible AI Risk Self-Assessment Tool"
          ),
          
          p(
            class = "lead",
            paste(
              "A simple and consistent way for analytical teams",
              "to identify and assess risks associated with AI-assisted work."
            )
          ),
          
          hr(),
          
          p(
            paste(
              "AI is increasingly being used to generate code, create",
              "deliverables, summarise evidence, analyse data and improve",
              "the efficiency of analytical processes."
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
              "AI RiskCheck supports professional judgement. It does not",
              "replace analytical QA, information assurance, security,",
              "data protection, legal, commercial or other required",
              "departmental approval processes."
            )
          ),
          
          div(
            
            class = "alert alert-secondary",
            
            strong("Questions or support: "),
            
            paste(
              "If you are unsure how to answer a question, interpret your",
              "risk rating or decide what assurance action may be appropriate,",
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
            paste(
              "AI RiskCheck contains five tabs. For a full assessment,",
              "work through them from left to right."
            )
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              h4("1. Overview"),
              
              p(
                paste(
                  "Understand what AI RiskCheck is, when it should",
                  "be used and what the assessment covers."
                )
              ),
              
              h4("2. AI & Model Profile"),
              
              p(
                paste(
                  "Describe your AI use case and record information",
                  "about the model, provider, environment and architecture."
                )
              ),
              
              h4("3. Risk Assessment"),
              
              p(
                paste(
                  "Complete 25 questions. You can also record notes",
                  "or rationale against individual questions."
                )
              )
            ),
            
            column(
              width = 6,
              
              h4("4. Results"),
              
              p(
                paste(
                  "Review inherent risk, control strength, residual risk,",
                  "key concerns and recommended assurance actions."
                )
              ),
              
              h4("5. References"),
              
              p(
                paste(
                  "View the documents used to develop AI RiskCheck",
                  "and information about the application version."
                )
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
            
            tags$li("Generating, explaining or assisting with analytical code"),
            
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
              "Automating an analytical process or workflow"
            ),
            
            tags$li(
              "Using an LLM, RAG system, AI agent or machine-learning model"
            ),
            
            tags$li(
              "Moving a prototype or pilot into operational or scaled use"
            )
          ),
          
          div(
            
            class = "alert alert-warning",
            
            strong("Reassess when: "),
            
            paste(
              "the model changes, important prompts change, data sources",
              "change, users or intended purpose change, AI is given more",
              "autonomy, the system moves to a new lifecycle stage or",
              "a significant incident or new risk is identified."
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
            "The assessment contains 25 questions across eight areas:"
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
      
      class = "container mt-4",
      
      h2("AI & Model Profile"),
      
      p(
        paste(
          "Provide contextual information about the AI use case.",
          "These fields do not form part of the 25-question score."
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
  # TAB 3 - RISK ASSESSMENT
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
          strong("25 questions")
        ),
        
        p(
          paste(
            "Select Not relevant where a question genuinely does not apply.",
            "Not relevant answers are excluded from the risk calculation."
          )
        ),
        
        p(
          paste(
            "Use the optional notes boxes to record evidence, context,",
            "assumptions or the reason for selecting Not relevant."
          )
        ),
        
        div(
          class = "alert alert-light",
          "Hover over the blue ⓘ icon for further information."
        )
      ),
      
      div(
        
        class = "p-3",
        
        uiOutput(
          "questions_ui"
        ),
        
        br(),
        
        # ------------------------------------------------------
        # FINAL FREE-TEXT FIELD
        # ------------------------------------------------------
        
        card(
          
          class = "mb-4",
          
          card_header(
            h3("Anything else we should know?")
          ),
          
          card_body(
            
            p(
              paste(
                "Record any additional risks, concerns, assumptions,",
                "dependencies or context that are not covered by the 25 questions."
              )
            ),
            
            textAreaInput(
              "additional_considerations",
              label = NULL,
              placeholder = paste(
                "For example: unusual features of the use case, known limitations,",
                "dependencies, outstanding decisions or additional assurance required."
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
  # TAB 4 - RESULTS
  # ==========================================================
  
  nav_panel(
    
    "Results",
    
    div(
      
      class = "container-fluid mt-4",
      
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
                "Risk before safeguards are considered."
              )
            )
          )
        ),
        
        column(
          width = 4,
          
          card(
            
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
      
      fluidRow(
        
        column(
          width = 7,
          
          card(
            
            full_screen = TRUE,
            
            card_header(
              "Risk profile"
            ),
            
            card_body(
              
              plotOutput(
                "risk_plot",
                height = "450px"
              )
            )
          )
        ),
        
        column(
          width = 5,
          
          card(
            
            card_header(
              "Decision summary"
            ),
            
            card_body(
              
              uiOutput(
                "decision_summary"
              )
            )
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
          "Additional considerations recorded by the user"
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
      br()
    )
  ),
  
  
  # ==========================================================
  # TAB 5 - REFERENCES
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
          "The assessment has been developed with reference to the",
          "following government and National Audit Office documents."
        )
      ),
      
      div(
        
        class = "alert alert-info",
        
        strong("Important: "),
        
        paste(
          "AI RiskCheck summarises themes from these documents for",
          "analytical self-assessment. Consult the original source where",
          "detailed guidance or specialist interpretation is required."
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
                tags$td("DfE analytical teams")
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
      
      h3(
        "Reference documents"
      ),
      
      uiOutput(
        "reference_cards"
      ),
      
      br()
    )
  )
)


# ============================================================
# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # DYNAMIC QUESTION UI
  # ==========================================================
  
  output$questions_ui <- renderUI({
    
    domains <- unique(
      questions$domain
    )
    
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
                      
                      # ========================================
                      # QUESTION + HOVER HELP
                      # ========================================
                      
                      div(
                        
                        style = "
                          display:flex;
                          align-items:center;
                          gap:8px;
                          margin-bottom:12px;
                        ",
                        
                        tags$span(
                          
                          style = "
                            font-size:1.15rem;
                            font-weight:500;
                            line-height:1.35;
                          ",
                          
                          q$question
                        ),
                        
                        bslib::tooltip(
                          
                          tags$span(
                            
                            style = "
                              cursor:help;
                              font-size:19px;
                              font-weight:bold;
                              color:#1d70b8;
                              flex-shrink:0;
                            ",
                            
                            "ⓘ"
                          ),
                          
                          tags$div(
                            
                            style = "
                              max-width:380px;
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
                      
                      
                      # ========================================
                      # RESPONSE
                      # ========================================
                      
                      radioButtons(
                        
                        inputId = q$id,
                        
                        label = NULL,
                        
                        choices = choices,
                        
                        selected = character(0)
                      ),
                      
                      
                      # ========================================
                      # OPTIONAL NOTES
                      # ========================================
                      
                      textAreaInput(
                        
                        inputId = paste0(
                          q$id,
                          "_notes"
                        ),
                        
                        label = "Optional notes / rationale",
                        
                        placeholder = paste(
                          "Add context, evidence, assumptions or an explanation",
                          "for your answer. If you selected Not relevant,",
                          "you can explain why here."
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
  # COLLECT ANSWERS
  # ==========================================================
  
  answers <- reactive({
    
    questions %>%
      
      mutate(
        
        # Raw response contains:
        # "0", "1", "2", "3", "4", "NR" or NA
        
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
        
        
        # Explicit Not relevant flag
        
        not_relevant = case_when(
          
          is.na(raw_response) ~
            FALSE,
          
          raw_response == "NR" ~
            TRUE,
          
          TRUE ~
            FALSE
        ),
        
        
        # Numeric score:
        # Not relevant becomes NA and therefore
        # drops out of score calculations
        
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
        
        
        # User notes for each question
        
        notes = map_chr(
          
          id,
          
          function(question_id) {
            
            note_value <- input[[paste0(question_id, "_notes")]]
            
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
  
  # A question counts as completed when the user has selected
  # any answer, including Not relevant.
  
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
  # RESPONSE LABEL
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
          ),
        
        response_text = map2_chr(
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
  # ESCALATION / STOP FLAGS
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
    
    
    # Consequential decisions + weak review
    
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
    
    
    # Autonomous consequential action
    
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
    
    
    # Sensitive data + weak platform approval
    
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
    
    
    # High impact + weak testing
    
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
    
    
    # High impact + weak output QA
    
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
    
    
    # Significant fairness risk
    
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
    
    
    # Weak governance
    
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
    
    
    # Agent autonomy
    
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
  # OVERALL RISK LABEL
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
  # LABEL HELPERS
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
  # RESULT SCORE TEXT
  # ==========================================================
  
  output$inherent_label <- renderText({
    
    if (
      completed_questions() == 0
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
      completed_questions() == 0
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
  # RISK BADGE
  # ==========================================================
  
  output$risk_badge <- renderUI({
    
    label <- risk_label()
    
    
    badge_class <- case_when(
      
      label == "INCOMPLETE" ~
        "secondary",
      
      label == "LOW" ~
        "success",
      
      label == "MODERATE" ~
        "warning",
      
      label == "HIGH" ~
        "warning",
      
      label == "VERY HIGH" ~
        "danger",
      
      TRUE ~
        "danger"
    )
    
    
    span(
      
      class = paste0(
        "badge bg-",
        badge_class
      ),
      
      style = "
        font-size:20px;
        padding:14px;
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
      
      geom_col() +
      
      geom_text(
        
        aes(
          label = paste0(
            round(
              risk_score
            ),
            "%"
          )
        ),
        
        hjust = -0.1
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
        
        y =
          "Risk / control concern (%)",
        
        title =
          "Risk profile by assessment area"
      ) +
      
      theme_minimal(
        base_size = 13
      )
  })
  
  
  # ==========================================================
  # FLAGS OUTPUT
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
  
  
  # ==========================================================
  # RECOMMENDED ASSURANCE
  # ==========================================================
  
  output$recommendations <- renderUI({
    
    dat <- scored_answers() %>%
      
      filter(
        
        !is.na(
          adjusted_score
        ),
        
        adjusted_score >= 2
      ) %>%
      
      arrange(
        
        desc(
          adjusted_score
        ),
        
        desc(
          weight
        )
      ) %>%
      
      distinct(
        recommended_action
      )
    
    
    if (
      nrow(dat) == 0
    ) {
      
      return(
        
        div(
          class = "alert alert-success",
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
          
          h4(
            "Assessment incomplete"
          ),
          
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
          
          h4(
            "Proceed with standard assurance"
          ),
          
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
          
          h4(
            "Proceed with additional controls"
          ),
          
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
          
          h4(
            "Enhanced assurance required"
          ),
          
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
          
          h4(
            "Escalation required"
          ),
          
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
      
      h4(
        "Do not proceed without review"
      ),
      
      p(
        paste(
          "AI RiskCheck has identified a stop condition.",
          "Resolve or formally review the issue before proceeding."
        )
      )
    )
  })
  
  
  # ==========================================================
  # DETAILED ASSESSMENT TABLE
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
  
}


# ============================================================
# 8. RUN APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)