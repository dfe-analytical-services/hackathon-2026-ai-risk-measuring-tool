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

APP_VERSION <- "0.4 Prototype"

GUIDANCE_VERSION <- "September 2026"


# ============================================================
# 3. ASSESSMENT MATRIX
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
  # PURPOSE & APPROPRIATENESS
  # ==========================================================
  
  "purpose_1",
  "Purpose & appropriateness",
  "Is the problem or use case clearly defined?",
  "The AI application should address a clearly defined analytical or operational problem.",
  "control",
  1,
  "UK Government AI Playbook",
  "Understand AI and its limitations",
  "Clearly define the problem, intended users and expected outcome.",
  
  
  "purpose_2",
  "Purpose & appropriateness",
  "Is there a clearly identified user need?",
  "There should be a clear reason why users or stakeholders would benefit from the AI application.",
  "control",
  1,
  "AI Opportunities Action Plan",
  "Identify valuable AI opportunities",
  "Clearly define the user need.",
  
  
  "purpose_3",
  "Purpose & appropriateness",
  "Have non-AI alternatives been considered?",
  "Consider whether conventional analytical, statistical or automation approaches could achieve the same outcome.",
  "control",
  1,
  "UK Government AI Playbook",
  "Use the right tool for the job",
  "Compare the AI approach with simpler non-AI alternatives.",
  
  
  "purpose_4",
  "Purpose & appropriateness",
  "Does using AI provide a clear and measurable benefit?",
  "Benefits could include improved efficiency, quality, accessibility or consistency.",
  "control",
  1,
  "AI Opportunities Action Plan",
  "Evaluate AI opportunities",
  "Define measurable benefits and success criteria.",
  
  
  # ==========================================================
  # EXTENT OF AI USE
  # ==========================================================
  
  "ai_1",
  "Extent of AI use",
  "How much responsibility is delegated to AI?",
  "Consider whether AI assists, generates, analyses, recommends or makes decisions.",
  "risk",
  3,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Ensure human oversight is proportionate to the responsibility delegated to AI.",
  
  
  "ai_2",
  "Extent of AI use",
  "Does the AI produce recommendations that could influence decisions?",
  "AI recommendations may influence analytical, policy or operational decisions.",
  "risk",
  3,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Introduce appropriate human review of AI-generated recommendations.",
  
  
  "ai_3",
  "Extent of AI use",
  "Can the AI trigger actions without human approval?",
  "Examples include sending communications, changing records, updating systems or using tools.",
  "risk",
  4,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Require human approval for consequential AI-triggered actions.",
  
  
  # ==========================================================
  # DATA & PRIVACY
  # ==========================================================
  
  "data_1",
  "Data & privacy",
  "What is the highest sensitivity of information processed by the AI?",
  "Consider public, unpublished official, personal, sensitive or confidential information.",
  "risk",
  4,
  "UK Government AI Playbook",
  "Lawful and responsible use",
  "Confirm that the selected AI service is appropriate for the information being processed.",
  
  
  "data_2",
  "Data & privacy",
  "Is only the minimum necessary information supplied to the AI?",
  "Data minimisation reduces privacy, confidentiality and security risks.",
  "control",
  3,
  "Data and AI Ethics Framework",
  "Privacy",
  "Remove unnecessary information before supplying data to the AI.",
  
  
  "data_3",
  "Data & privacy",
  "Are data retention arrangements understood?",
  "Consider whether prompts, files, retrieved information and outputs are retained.",
  "control",
  3,
  "UK Government AI Playbook",
  "Data protection",
  "Confirm and document data-retention arrangements.",
  
  
  "data_4",
  "Data & privacy",
  "Is it understood whether information supplied to the AI may be used to train the model?",
  "Provider model-training arrangements should be understood before departmental information is processed.",
  "control",
  3,
  "UK Government AI Playbook",
  "Security and data protection",
  "Confirm whether inputs can be used for model training.",
  
  
  "data_5",
  "Data & privacy",
  "Has privacy or data-protection advice been obtained where required?",
  "Additional assessment may be needed where personal information is processed.",
  "control",
  3,
  "ICO AI and data protection guidance",
  "Accountability",
  "Seek data-protection advice and consider whether a DPIA is required.",
  
  
  # ==========================================================
  # IMPACT & CONSEQUENCES
  # ==========================================================
  
  "impact_1",
  "Impact & consequences",
  "How serious would the consequences be if the AI output were materially wrong?",
  "Consider impacts on analysis, policy, funding, services, decisions and individuals.",
  "risk",
  5,
  "UK Government AI Playbook",
  "Understand AI limitations",
  "Increase assurance in proportion to the consequences of incorrect outputs.",
  
  
  "impact_2",
  "Impact & consequences",
  "How widely could an incorrect AI output affect users or stakeholders?",
  "Consider whether an error affects one analyst, a team, DfE, the public or identifiable individuals.",
  "risk",
  4,
  "Data and AI Ethics Framework",
  "Societal impact",
  "Assess who could be affected and the potential scale of harm.",
  
  
  "impact_3",
  "Impact & consequences",
  "Could the AI influence policy, funding or operational decisions?",
  "Consider direct and indirect influence on decision-making.",
  "risk",
  4,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Apply enhanced assurance where AI materially influences decisions.",
  
  
  "impact_4",
  "Impact & consequences",
  "Could the AI affect rights, entitlements, access to services or significant decisions about individuals?",
  "AI systems affecting individuals directly should receive stronger assurance.",
  "risk",
  5,
  "Data and AI Ethics Framework",
  "Fairness and societal impact",
  "Escalate for specialist review and ensure meaningful human decision-making.",
  
  
  # ==========================================================
  # ACCURACY & ANALYTICAL QUALITY
  # ==========================================================
  
  "quality_1",
  "Accuracy & analytical quality",
  "Are AI-generated outputs independently checked?",
  "Important AI-generated analytical outputs should be independently validated.",
  "control",
  4,
  "UK Government AI Playbook",
  "Testing and assurance",
  "Introduce independent QA of AI-generated outputs.",
  
  
  "quality_2",
  "Accuracy & analytical quality",
  "Are numerical claims checked against authoritative source data?",
  "AI-generated statistics and numerical statements should be verified.",
  "control",
  4,
  "UK Government AI Playbook",
  "Testing and assurance",
  "Validate numerical claims against authoritative source data.",
  
  
  "quality_3",
  "Accuracy & analytical quality",
  "Is AI-generated code reviewed and tested before use?",
  "Generated code should be understood and tested by a competent analyst.",
  "control",
  3,
  "UK Government AI Playbook",
  "Understand AI limitations",
  "Introduce code review and appropriate testing.",
  
  
  "quality_4",
  "Accuracy & analytical quality",
  "Has the AI system been evaluated using known examples or a representative test set?",
  "Representative testing can identify systematic errors and unexpected behaviour.",
  "control",
  4,
  "NIST AI Risk Management Framework",
  "Measure",
  "Create a representative evaluation dataset.",
  
  
  "quality_5",
  "Accuracy & analytical quality",
  "Are measurable accuracy or performance criteria defined?",
  "The team should understand what acceptable AI performance looks like.",
  "control",
  3,
  "AI Assurance Guidance",
  "Measure and evaluate",
  "Define measurable acceptance criteria.",
  
  
  "quality_6",
  "Accuracy & analytical quality",
  "Has failure behaviour been tested?",
  "Testing should include difficult, unusual and deliberately problematic inputs.",
  "control",
  3,
  "NIST AI Risk Management Framework",
  "Measure",
  "Test edge cases and known failure scenarios.",
  
  
  "quality_7",
  "Accuracy & analytical quality",
  "Are material model, prompt or configuration changes retested?",
  "Changes can alter AI behaviour and may require reassessment.",
  "control",
  3,
  "UK Government AI Playbook",
  "Lifecycle management",
  "Introduce change control and regression testing.",
  
  
  # ==========================================================
  # FAIRNESS, BIAS & ETHICS
  # ==========================================================
  
  "fairness_1",
  "Bias, fairness & ethics",
  "Could AI outputs affect people or groups differently?",
  "Consider demographic groups, protected characteristics and vulnerable users.",
  "risk",
  4,
  "Data and AI Ethics Framework",
  "Fairness",
  "Identify groups that could experience different outcomes.",
  
  
  "fairness_2",
  "Bias, fairness & ethics",
  "Has potential bias or differential performance been assessed?",
  "Where relevant, compare system performance across different groups.",
  "control",
  4,
  "Data and AI Ethics Framework",
  "Fairness",
  "Evaluate performance and error rates across relevant groups.",
  
  
  "fairness_3",
  "Bias, fairness & ethics",
  "Could false positives or false negatives disproportionately affect a particular group?",
  "Different types of AI errors can have different consequences.",
  "risk",
  3,
  "Data and AI Ethics Framework",
  "Fairness",
  "Assess the distribution and consequences of AI errors.",
  
  
  "fairness_4",
  "Bias, fairness & ethics",
  "Is there a clear public or organisational benefit from the use case?",
  "Benefits should be identifiable and proportionate to potential risks.",
  "control",
  2,
  "Data and AI Ethics Framework",
  "Societal impact",
  "Document the expected benefits and who receives them.",
  
  
  # ==========================================================
  # HUMAN OVERSIGHT
  # ==========================================================
  
  "human_1",
  "Human oversight",
  "What level of human review takes place before AI outputs are used?",
  "Human review should be meaningful rather than a simple approval step.",
  "control",
  5,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Introduce meaningful review by a suitably competent person.",
  
  
  "human_2",
  "Human oversight",
  "Is there a named person accountable for the final output?",
  "Responsibility should remain clear even when AI contributes to the output.",
  "control",
  4,
  "UK Government AI Playbook",
  "Accountability",
  "Assign a named accountable owner.",
  
  
  "human_3",
  "Human oversight",
  "Can the reviewer realistically identify and challenge AI errors?",
  "Consider expertise, workload, time and access to supporting evidence.",
  "control",
  4,
  "UK Government AI Playbook",
  "Meaningful human control",
  "Ensure reviewers have enough expertise and evidence to challenge the AI.",
  
  
  "human_4",
  "Human oversight",
  "Can a human stop or override the AI system where necessary?",
  "Operational systems should allow effective intervention.",
  "control",
  4,
  "NIST AI Risk Management Framework",
  "Manage",
  "Introduce a human override or stop mechanism.",
  
  
  # ==========================================================
  # SECURITY & ROBUSTNESS
  # ==========================================================
  
  "security_1",
  "Security & robustness",
  "Is the AI service approved for the information being processed?",
  "Consider departmental technology, security and information-assurance requirements.",
  "control",
  5,
  "UK Government AI Playbook",
  "Security",
  "Confirm that the AI service is approved for the information being processed.",
  
  
  "security_2",
  "Security & robustness",
  "Are appropriate access controls in place?",
  "Only authorised users should have access to sensitive AI functionality or data.",
  "control",
  3,
  "NCSC Secure AI System Development",
  "Secure deployment",
  "Apply appropriate authentication and least-privilege access.",
  
  
  "security_3",
  "Security & robustness",
  "Could users or external content manipulate the AI system?",
  "LLMs may be vulnerable to prompt injection and malicious instructions.",
  "risk",
  4,
  "NCSC Secure AI System Development",
  "Secure design",
  "Assess prompt-injection and untrusted-input risks.",
  
  
  "security_4",
  "Security & robustness",
  "Can the AI access other systems, tools or data sources?",
  "Tool-enabled AI can create additional security and operational risks.",
  "risk",
  4,
  "NCSC Secure AI System Development",
  "Secure deployment",
  "Restrict permissions and apply least privilege.",
  
  
  "security_5",
  "Security & robustness",
  "Are logs or audit records retained?",
  "Logs support monitoring, assurance and incident investigation.",
  "control",
  3,
  "NCSC Secure AI System Development",
  "Secure operation",
  "Implement appropriate logging and monitoring.",
  
  
  "security_6",
  "Security & robustness",
  "Is there an incident response process for AI-related security issues?",
  "Teams should know how AI-related security incidents will be managed.",
  "control",
  3,
  "NCSC Secure AI System Development",
  "Secure operation",
  "Document an AI-specific incident response process.",
  
  
  # ==========================================================
  # TRANSPARENCY & EXPLAINABILITY
  # ==========================================================
  
  "transparency_1",
  "Transparency & explainability",
  "Is the use of AI documented?",
  "Documentation should describe where AI contributes materially to the process.",
  "control",
  3,
  "Algorithmic Transparency Recording Standard",
  "Transparency",
  "Document how and where AI is used.",
  
  
  "transparency_2",
  "Transparency & explainability",
  "Are users informed where AI materially contributes to outputs?",
  "Disclosure should be proportionate to the role and impact of the AI.",
  "control",
  3,
  "Algorithmic Transparency Recording Standard",
  "Transparency",
  "Clearly communicate material AI involvement.",
  
  
  "transparency_3",
  "Transparency & explainability",
  "Are important limitations and uncertainties communicated?",
  "Users should understand limitations that could affect interpretation.",
  "control",
  3,
  "UK Government AI Playbook",
  "Be open and collaborative",
  "Document and communicate important limitations.",
  
  
  "transparency_4",
  "Transparency & explainability",
  "Can important AI-supported conclusions be traced back to evidence or source information?",
  "Traceability supports analytical assurance and reproducibility.",
  "control",
  4,
  "Algorithmic Transparency Recording Standard",
  "Explainability",
  "Maintain evidence and source traceability.",
  
  
  "transparency_5",
  "Transparency & explainability",
  "Can affected users challenge an AI-supported outcome where appropriate?",
  "Higher-impact use cases may require contestability and redress.",
  "control",
  3,
  "Data and AI Ethics Framework",
  "Accountability",
  "Provide an appropriate route to challenge significant AI-supported outcomes.",
  
  
  # ==========================================================
  # GOVERNANCE & LIFECYCLE
  # ==========================================================
  
  "governance_1",
  "Governance & lifecycle",
  "Is there a named project owner?",
  "Ownership should remain clear throughout development and operation.",
  "control",
  3,
  "UK Government AI Playbook",
  "Organisational assurance",
  "Assign a named project owner.",
  
  
  "governance_2",
  "Governance & lifecycle",
  "Is there a named analytical or technical owner?",
  "Someone should remain responsible for analytical or technical quality.",
  "control",
  3,
  "UK Government AI Playbook",
  "Organisational assurance",
  "Assign an accountable analytical or technical owner.",
  
  
  "governance_3",
  "Governance & lifecycle",
  "Is AI performance monitored after implementation?",
  "Models, prompts, data and user behaviour may change over time.",
  "control",
  4,
  "NIST AI Risk Management Framework",
  "Manage",
  "Introduce ongoing performance monitoring.",
  
  
  "governance_4",
  "Governance & lifecycle",
  "Is there an escalation process for incidents or unexpected behaviour?",
  "Teams should know how significant AI issues will be escalated.",
  "control",
  4,
  "UK Government AI Playbook",
  "Organisational assurance",
  "Define escalation routes and responsibilities.",
  
  
  "governance_5",
  "Governance & lifecycle",
  "Is there a process for stopping or decommissioning the AI system?",
  "Operational AI systems should have a defined exit process.",
  "control",
  3,
  "NIST AI Risk Management Framework",
  "Manage",
  "Create a process for suspension and decommissioning.",
  
  
  "governance_6",
  "Governance & lifecycle",
  "Are changes to models, prompts or data sources controlled?",
  "Material changes should be documented, tested and reassessed.",
  "control",
  4,
  "UK Government AI Playbook",
  "Lifecycle management",
  "Introduce formal change control.",
  
  
  # ==========================================================
  # MODEL, PLATFORM & OPERATIONAL EXPOSURE
  # ==========================================================
  
  "model_1",
  "Model, platform & operational exposure",
  "Is the AI accessed through a DfE-approved or otherwise approved environment?",
  "The platform should be appropriate for the information and use case.",
  "control",
  5,
  "UK Government AI Playbook",
  "Security",
  "Confirm platform approval before processing departmental information.",
  
  
  "model_2",
  "Model, platform & operational exposure",
  "Is the model and model version recorded?",
  "Recording the model version supports reproducibility and change management.",
  "control",
  3,
  "Algorithmic Transparency Recording Standard",
  "Model specification",
  "Record the model name and model version.",
  
  
  "model_3",
  "Model, platform & operational exposure",
  "Are provider data-processing arrangements understood?",
  "Understand where data is processed, retained and potentially reused.",
  "control",
  4,
  "UK Government AI Playbook",
  "Security and data protection",
  "Document provider data-processing arrangements.",
  
  
  "model_4",
  "Model, platform & operational exposure",
  "Is model usage monitored?",
  "Monitoring may include requests, token usage, API activity, errors and abnormal usage.",
  "control",
  2,
  "NCSC Secure AI System Development",
  "Secure operation",
  "Introduce model usage monitoring.",
  
  
  "model_5",
  "Model, platform & operational exposure",
  "Are appropriate usage or spending limits in place?",
  "Limits can help manage unexpected costs and uncontrolled scaling.",
  "control",
  1,
  "AI Opportunities Action Plan",
  "Scale responsibly",
  "Introduce proportionate usage and spending limits.",
  
  
  "model_6",
  "Model, platform & operational exposure",
  "Could the model or provider change without the application being retested?",
  "Provider-side model changes may alter system behaviour.",
  "risk",
  3,
  "UK Government AI Playbook",
  "Lifecycle management",
  "Monitor model changes and retest material updates.",
  
  
  # ==========================================================
  # SKILLS & CAPABILITY
  # ==========================================================
  
  "skills_1",
  "Skills & capability",
  "Does the team have sufficient AI expertise?",
  "Teams should understand the technology, its capabilities and its limitations.",
  "control",
  3,
  "UK Government AI Playbook",
  "Use appropriate skills",
  "Ensure appropriate AI expertise is available.",
  
  
  "skills_2",
  "Skills & capability",
  "Does the team have sufficient analytical or domain expertise?",
  "AI should complement rather than replace relevant subject-matter expertise.",
  "control",
  3,
  "UK Government AI Playbook",
  "Use appropriate skills",
  "Ensure analytical and domain expertise is available.",
  
  
  "skills_3",
  "Skills & capability",
  "Have relevant specialist teams been consulted where required?",
  "Examples may include analytical QA, security, information assurance, legal or data protection.",
  "control",
  3,
  "UK Government AI Playbook",
  "Organisational assurance",
  "Seek specialist advice where required.",
  
  
  # ==========================================================
  # ENVIRONMENTAL & SOCIETAL IMPACT
  # ==========================================================
  
  "society_1",
  "Environmental & societal impact",
  "Could the AI create wider unintended consequences for users or society?",
  "Consider accessibility, exclusion, misinformation, trust, deskilling or behavioural effects.",
  "risk",
  2,
  "Data and AI Ethics Framework",
  "Societal impact",
  "Identify potential wider impacts and appropriate mitigations.",
  
  
  "society_2",
  "Environmental & societal impact",
  "Has the environmental impact of the AI approach been considered?",
  "Consider whether the model size and level of compute are proportionate to the task.",
  "control",
  1,
  "Data and AI Ethics Framework",
  "Environmental sustainability",
  "Consider whether a smaller or more efficient approach could meet the need."
)


# ============================================================
# 4. RESPONSE OPTIONS
# ============================================================

risk_options <- c(
  "None / negligible" = 0,
  "Low" = 1,
  "Moderate" = 2,
  "High" = 3,
  "Very high" = 4
)


control_options <- c(
  "Not implemented / No" = 0,
  "Limited" = 1,
  "Partially implemented" = 2,
  "Mostly implemented" = 3,
  "Fully implemented / Yes" = 4
)


# ============================================================
# 5. GUIDANCE LIBRARY
# ============================================================

guidance <- tribble(
  
  ~source,
  ~organisation,
  ~purpose,
  ~url,
  
  "Generative artificial intelligence in education",
  "Department for Education",
  "DfE-specific guidance on generative AI use in education.",
  "https://www.gov.uk/government/publications/generative-artificial-intelligence-in-education/generative-artificial-intelligence-ai-in-education",
  
  "UK Government AI Playbook",
  "UK Government",
  "Government principles for safe, responsible and effective AI use.",
  "https://www.gov.uk/government/publications/ai-playbook-for-the-uk-government/artificial-intelligence-playbook-for-the-uk-government-html",
  
  "AI Opportunities Action Plan",
  "UK Government",
  "Guidance on identifying, piloting, evaluating and scaling AI opportunities.",
  "https://www.gov.uk/government/publications/ai-opportunities-action-plan/ai-opportunities-action-plan",
  
  "Data and AI Ethics Framework",
  "UK Government",
  "Framework covering transparency, accountability, fairness, privacy, societal impact and sustainability.",
  "https://www.gov.uk/government/publications/data-ethics-framework",
  
  "Algorithmic Transparency Recording Standard",
  "UK Government",
  "Standard for documenting AI and algorithmic systems used by public bodies.",
  "https://www.gov.uk/government/collections/algorithmic-transparency-recording-standard-hub",
  
  "Introduction to AI Assurance",
  "UK Government",
  "Guidance on measuring, evaluating and communicating AI trustworthiness.",
  "https://www.gov.uk/government/publications/introduction-to-ai-assurance",
  
  "Secure AI System Development",
  "National Cyber Security Centre",
  "Security guidance covering AI design, development, deployment and operation.",
  "https://www.ncsc.gov.uk/collection/guidelines-secure-ai-system-development",
  
  "AI Risk Management Framework",
  "NIST",
  "AI risk-management framework covering Govern, Map, Measure and Manage.",
  "https://www.nist.gov/itl/ai-risk-management-framework",
  
  "OECD AI Principles",
  "OECD",
  "International principles for trustworthy and responsible AI.",
  "https://www.oecd.org/en/topics/ai-principles.html"
)


# ============================================================
# 6. UI
# ============================================================

ui <- page_navbar(
  
  # ----------------------------------------------------------
  # NAVBAR TITLE
  # ----------------------------------------------------------
  
  title = div(
    
    style = "
      display:flex;
      align-items:center;
      gap:10px;
    ",
    
    tags$img(
      src = "dfe-logo.png",
      height = "38px"
    ),
    
    strong(APP_NAME)
    
  ),
  
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  
  # ==========================================================
  # START PAGE
  # ==========================================================
  
  nav_panel(
    
    "Start",
    
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
              height = "55px"
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
            "Identify, assess and manage risks when using AI in analytical work."
          ),
          
          hr(),
          
          p(
            "AI RiskCheck helps analytical teams assess potential risks ",
            "associated with the use of artificial intelligence."
          ),
          
          p(
            "The assessment considers the purpose of the AI use case, ",
            "the model and platform being used, data sensitivity, impact, ",
            "analytical quality, fairness, human oversight, security, ",
            "transparency and governance."
          ),
          
          div(
            
            class = "alert alert-info",
            
            strong("Important: "),
            
            "AI RiskCheck supports professional judgement. It does not replace ",
            "analytical QA, information assurance, security, legal, ",
            "data-protection or other required approval processes."
            
          )
          
        )
        
      ),
      
      br(),
      
      card(
        
        card_header(
          h4("About your AI use case")
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
              "Describe the problem, how AI is being used",
              "and what the AI output will be used for."
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
                "Who uses or is affected by the output?",
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
          
        )
        
      )
      
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
        "Record information about the AI technology being assessed."
      ),
      
      card(
        
        card_header(
          h4("Technology")
        ),
        
        card_body(
          
          selectInput(
            "ai_type",
            "What type of AI is being used?",
            choices = c(
              "Generative AI / LLM",
              "LLM with RAG",
              "AI agent / tool-using LLM",
              "Machine learning model",
              "Natural language processing",
              "Computer vision",
              "Recommendation system",
              "Other"
            )
          ),
          
          selectInput(
            "environment",
            "What type of environment is being used?",
            choices = c(
              "DfE-controlled environment",
              "Other government-controlled environment",
              "Approved external service",
              "Open-source model hosted internally",
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
              "Other",
              "Unknown"
            )
          ),
          
          textInput(
            "model_name",
            "Model name",
            placeholder = "e.g. Claude Sonnet, GPT-4.1, Llama"
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
              "Approved cloud environment",
              "On-premise / local",
              "External SaaS",
              "Public web service",
              "Unknown"
            )
          ),
          
          selectInput(
            "access_method",
            "How is the model accessed?",
            choices = c(
              "DfE application",
              "Databricks",
              "API",
              "Web interface",
              "Locally hosted",
              "Other"
            )
          )
          
        )
        
      ),
      
      br(),
      
      
      # --------------------------------------------------------
      # LLM QUESTIONS
      # --------------------------------------------------------
      
      conditionalPanel(
        
        condition = "
          input.ai_type == 'Generative AI / LLM' ||
          input.ai_type == 'LLM with RAG' ||
          input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h4("LLM usage")
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
              "token_monitoring",
              "Is token / API usage monitored?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "cost_limits",
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
      
      
      # --------------------------------------------------------
      # RAG QUESTIONS
      # --------------------------------------------------------
      
      conditionalPanel(
        
        condition = "
          input.ai_type == 'LLM with RAG' ||
          input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h4("RAG / retrieval")
          ),
          
          card_body(
            
            selectInput(
              "rag_external",
              "Can retrieved content contain external or untrusted information?",
              choices = c(
                "No",
                "Limited",
                "Yes",
                "Unknown"
              )
            ),
            
            selectInput(
              "prompt_injection_test",
              "Has prompt injection been tested?",
              choices = c(
                "Yes",
                "Partly",
                "No",
                "Unknown"
              )
            ),
            
            selectInput(
              "retrieval_permissions",
              "Are retrieval permissions appropriately restricted?",
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
      
      
      # --------------------------------------------------------
      # AGENT QUESTIONS
      # --------------------------------------------------------
      
      conditionalPanel(
        
        condition = "
          input.ai_type == 'AI agent / tool-using LLM'
        ",
        
        card(
          
          card_header(
            h4("AI agent permissions")
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
        
      )
      
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
          strong("Risk questions: "),
          "higher responses increase inherent risk."
        ),
        
        p(
          strong("Control questions: "),
          "stronger safeguards reduce residual risk."
        ),
        
        hr(),
        
        p(
          "Use the expandable guidance beneath each question ",
          "to understand why the question is included."
        )
        
      ),
      
      div(
        
        class = "p-3",
        
        uiOutput("questions_ui")
        
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
      
      uiOutput("project_heading"),
      
      br(),
      
      fluidRow(
        
        column(
          
          width = 4,
          
          card(
            
            card_header(
              "Overall risk rating"
            ),
            
            card_body(
              
              uiOutput("risk_badge"),
              
              br(),
              br(),
              
              h3(
                textOutput("residual_score_text")
              ),
              
              p(
                "Residual risk score"
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
                textOutput("inherent_label")
              ),
              
              h4(
                textOutput("inherent_score_text")
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
            
            card_header(
              "Control strength"
            ),
            
            card_body(
              
              h2(
                textOutput("control_label")
              ),
              
              h4(
                textOutput("control_score_text")
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
                height = "500px"
              )
              
            )
            
          )
          
        ),
        
        column(
          
          width = 5,
          
          card(
            
            card_header(
              "What should happen next?"
            ),
            
            card_body(
              uiOutput("overall_action")
            )
            
          )
          
        )
        
      ),
      
      br(),
      
      card(
        
        card_header(
          "🚩 Red flags / escalation"
        ),
        
        card_body(
          uiOutput("red_flag_output")
        )
        
      ),
      
      br(),
      
      card(
        
        card_header(
          "Areas requiring attention"
        ),
        
        card_body(
          DTOutput("risk_table")
        )
        
      ),
      
      br(),
      
      card(
        
        card_header(
          "Recommended assurance activities"
        ),
        
        card_body(
          uiOutput("recommendations")
        )
        
      ),
      
      br(),
      
      card(
        
        card_header(
          "AI system profile"
        ),
        
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
      br()
      
    )
    
  ),
  
  
  # ==========================================================
  # GUIDANCE
  # ==========================================================
  
  nav_panel(
    
    "Guidance",
    
    div(
      
      class = "container mt-4",
      
      h2(
        "AI RiskCheck Guidance Library"
      ),
      
      p(
        "AI RiskCheck draws on DfE, UK Government and recognised ",
        "international guidance on responsible AI."
      ),
      
      div(
        
        class = "alert alert-info",
        
        strong("Guidance hierarchy: "),
        
        "DfE and UK Government guidance should take precedence ",
        "where it applies. International frameworks provide supporting good practice."
        
      ),
      
      br(),
      
      uiOutput(
        "guidance_cards"
      ),
      
      hr(),
      
      h4(
        "Assessment framework"
      ),
      
      p(
        paste(
          "Application version:",
          APP_VERSION
        )
      ),
      
      p(
        paste(
          "Guidance review:",
          GUIDANCE_VERSION
        )
      )
      
    )
    
  )
  
)


# ============================================================
# 7. SERVER
# ============================================================

server <- function(input, output, session) {
  
  
  # ==========================================================
  # GENERATE ASSESSMENT QUESTIONS
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
              
              h3(
                domain_name
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
                      
                      h5(
                        q$question
                      ),
                      
                      p(
                        
                        class = "text-muted",
                        
                        q$help_text
                        
                      ),
                      
                      radioButtons(
                        
                        inputId = q$id,
                        
                        label = NULL,
                        
                        choices = choices,
                        
                        selected = NULL
                        
                      ),
                      
                      
                      # ----------------------------------------
                      # EXPANDABLE GUIDANCE
                      # ----------------------------------------
                      
                      tags$details(
                        
                        style = "
                          background:#f7f7f7;
                          padding:12px;
                          border-radius:6px;
                          margin-top:10px;
                          border:1px solid #e6e6e6;
                        ",
                        
                        tags$summary(
                          
                          style = "
                            cursor:pointer;
                            font-weight:600;
                          ",
                          
                          "Why are we asking this?"
                          
                        ),
                        
                        br(),
                        
                        p(
                          q$help_text
                        ),
                        
                        p(
                          
                          strong(
                            "Guidance: "
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
        
        response = map_dbl(
          
          id,
          
          function(question_id) {
            
            value <- input[[question_id]]
            
            if (
              is.null(value) ||
              length(value) == 0 ||
              identical(value, "")
            ) {
              
              NA_real_
              
            } else {
              
              as.numeric(value)
              
            }
            
          }
          
        )
        
      )
    
  })
  
  
  # ==========================================================
  # ASSESSMENT PROGRESS
  # ==========================================================
  
  output$progress_text <- renderText({
    
    completed <- sum(
      !is.na(
        answers()$response
      )
    )
    
    paste0(
      completed,
      " of ",
      nrow(questions),
      " questions completed"
    )
    
  })
  
  
  output$progress_bar <- renderUI({
    
    completed <- sum(
      !is.na(
        answers()$response
      )
    )
    
    percentage <- round(
      100 *
        completed /
        nrow(questions)
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
  # SCORE ANSWERS
  # ==========================================================
  
  scored_answers <- reactive({
    
    answers() %>%
      
      mutate(
        
        # Risk questions:
        # 0 = negligible
        # 4 = very high risk
        #
        # Control questions:
        # 0 = no control
        # 4 = strong control
        #
        # Reverse controls so weak controls
        # create higher residual concern.
        
        adjusted_score = case_when(
          
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
          4 *
          weight
        
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
    
    
    score <-
      
      100 *
      
      sum(
        dat$response *
          dat$weight
      ) /
      
      sum(
        4 *
          dat$weight
      )
    
    
    score
    
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
    
    
    score <-
      
      100 *
      
      sum(
        dat$response *
          dat$weight
      ) /
      
      sum(
        4 *
          dat$weight
      )
    
    
    score
    
  })
  
  
  # ==========================================================
  # RED FLAG RULES
  # ==========================================================
  
  red_flags <- reactive({
    
    flags <- character(0)
    
    
    # --------------------------------------------------------
    # HIGH IMPACT + WEAK HUMAN OVERSIGHT
    # --------------------------------------------------------
    
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
          
          paste(
            "High-impact AI use has insufficient",
            "meaningful human oversight."
          )
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # SENSITIVE DATA + WEAK PLATFORM APPROVAL
    # --------------------------------------------------------
    
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
          
          paste(
            "Sensitive information may be processed",
            "using an AI service without sufficient approval."
          )
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # AUTONOMOUS ACTION + WEAK OVERSIGHT
    # --------------------------------------------------------
    
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
          
          paste(
            "The AI can trigger significant actions",
            "without sufficient human approval."
          )
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # PUBLIC AI SERVICE + SENSITIVE DATA
    # --------------------------------------------------------
    
    if (
      !is.null(input$environment) &&
      !is.null(input$data_1)
    ) {
      
      if (
        input$environment ==
        "Public AI service" &&
        as.numeric(input$data_1) >= 2
      ) {
        
        flags <- c(
          
          flags,
          
          paste(
            "Potentially sensitive information is being",
            "processed through a public AI service."
          )
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # RIGHTS / ENTITLEMENTS
    # --------------------------------------------------------
    
    if (
      !is.null(input$impact_4)
    ) {
      
      if (
        as.numeric(
          input$impact_4
        ) >= 3
      ) {
        
        flags <- c(
          
          flags,
          
          paste(
            "The AI may materially affect rights,",
            "entitlements or significant decisions about individuals."
          )
          
        )
        
      }
      
    }
    
    
    # --------------------------------------------------------
    # AGENT AUTONOMY
    # --------------------------------------------------------
    
    if (
      !is.null(input$agent_actions)
    ) {
      
      if (
        input$agent_actions ==
        "Yes - autonomously"
      ) {
        
        flags <- c(
          
          flags,
          
          paste(
            "The AI agent can execute actions autonomously.",
            "Specialist review is recommended."
          )
          
        )
        
      }
      
    }
    
    
    unique(flags)
    
  })
  
  
  # ==========================================================
  # RESIDUAL RISK
  # ==========================================================
  
  residual_score <- reactive({
    
    inherent <-
      inherent_score()
    
    controls <-
      control_score()
    
    
    # Controls may reduce,
    # but not eliminate,
    # inherent risk.
    
    score <-
      
      inherent *
      
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
    
    
    # --------------------------------------------------------
    # RED FLAG OVERRIDE
    # --------------------------------------------------------
    
    if (
      length(
        red_flags()
      ) > 0
    ) {
      
      score <- max(
        score,
        75
      )
      
    }
    
    
    score
    
  })
  
  
  # ==========================================================
  # OVERALL RISK LABEL
  # ==========================================================
  
  risk_label <- reactive({
    
    # Any red flag automatically escalates
    
    if (
      length(
        red_flags()
      ) > 0
    ) {
      
      return(
        "STOP / ESCALATE"
      )
      
    }
    
    
    score <-
      residual_score()
    
    
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
        "STOP / ESCALATE"
      
    )
    
  })
  
  
  # ==========================================================
  # LABEL FUNCTIONS
  # ==========================================================
  
  risk_description <- function(score) {
    
    case_when(
      
      score < 20 ~
        "Low",
      
      score < 40 ~
        "Moderate",
      
      score < 60 ~
        "High",
      
      score < 75 ~
        "Very high",
      
      TRUE ~
        "Critical"
      
    )
    
  }
  
  
  control_description <- function(score) {
    
    case_when(
      
      score >= 80 ~
        "Strong",
      
      score >= 60 ~
        "Good",
      
      score >= 40 ~
        "Moderate",
      
      score >= 20 ~
        "Weak",
      
      TRUE ~
        "Very weak"
      
    )
    
  }


# ==========================================================
# RESULTS HEADINGS
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


output$inherent_label <- renderText({
  
  risk_description(
    inherent_score()
  )
  
})


output$control_label <- renderText({
  
  control_description(
    control_score()
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
        font-size:22px;
        padding:14px;
      ",
    
    paste(
      "AI Risk:",
      label
    )
    
  )
  
})


# ==========================================================
# DOMAIN RISK SCORES
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
      
      .groups =
        "drop"
      
    ) %>%
    
    arrange(
      desc(risk_score)
    )
  
})


# ==========================================================
# RISK PROFILE CHART
# ==========================================================

output$risk_plot <- renderPlot({
  
  dat <-
    domain_scores()
  
  
  validate(
    
    need(
      
      nrow(dat) > 0,
      
      paste(
        "Complete some assessment questions",
        "to see the risk profile."
      )
      
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
        by = 20
      )
      
    ) +
    
    labs(
      
      x = NULL,
      
      y =
        "Risk / control concern (%)",
      
      title =
        "AI RiskCheck risk profile"
      
    ) +
    
    theme_minimal(
      base_size = 13
    )
  
})


# ==========================================================
# RED FLAG OUTPUT
# ==========================================================

output$red_flag_output <- renderUI({
  
  flags <-
    red_flags()
  
  
  if (
    length(flags) == 0
  ) {
    
    div(
      
      class =
        "alert alert-success",
      
      strong(
        paste(
          "No mandatory escalation",
          "conditions identified."
        )
      )
      
    )
    
  } else {
    
    div(
      
      class =
        "alert alert-danger",
      
      h5(
        "AI RiskCheck identified the following areas requiring escalation:"
      ),
      
      tags$ul(
        
        lapply(
          
          flags,
          
          tags$li
          
        )
        
      )
      
    )
    
  }
  
})


# ==========================================================
# AREAS REQUIRING ATTENTION
# ==========================================================

output$risk_table <- renderDT({
  
  dat <-
    scored_answers() %>%
    
    filter(
      !is.na(adjusted_score)
    ) %>%
    
    mutate(
      
      Concern = case_when(
        
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
    
    filter(
      adjusted_score >= 2
    ) %>%
    
    arrange(
      desc(adjusted_score),
      desc(weight)
    ) %>%
    
    select(
      
      Domain =
        domain,
      
      Question =
        question,
      
      Concern,
      
      Guidance =
        source,
      
      `Recommended action` =
        recommended_action
      
    )
  
  
  if (
    nrow(dat) == 0
  ) {
    
    dat <- data.frame(
      
      Domain =
        "No major concerns identified",
      
      Question =
        "",
      
      Concern =
        "",
      
      Guidance =
        "",
      
      `Recommended action` =
        ""
      
    )
    
  }
  
  
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
# RECOMMENDED ASSURANCE
# ==========================================================

output$recommendations <- renderUI({
  
  dat <-
    scored_answers() %>%
    
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
  
  
  if (
    nrow(dat) == 0
  ) {
    
    return(
      
      div(
        
        class =
          "alert alert-success",
        
        paste(
          "No major additional assurance activities",
          "have currently been identified.",
          "Continue to apply proportionate analytical QA."
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
# OVERALL ACTION
# ==========================================================

output$overall_action <- renderUI({
  
  label <-
    risk_label()
  
  
  # --------------------------------------------------------
  # LOW
  # --------------------------------------------------------
  
  if (
    label == "LOW"
  ) {
    
    return(
      
      tagList(
        
        h4(
          "Proceed with standard assurance"
        ),
        
        tags$ul(
          
          tags$li(
            "Apply normal analytical QA."
          ),
          
          tags$li(
            "Document where AI has been used."
          ),
          
          tags$li(
            "Maintain appropriate human review."
          ),
          
          tags$li(
            "Reassess if the use case changes."
          )
          
        )
        
      )
      
    )
    
  }
  
  
  # --------------------------------------------------------
  # MODERATE
  # --------------------------------------------------------
  
  if (
    label == "MODERATE"
  ) {
    
    return(
      
      tagList(
        
        h4(
          "Proceed with additional controls"
        ),
        
        tags$ul(
          
          tags$li(
            "Apply enhanced analytical QA."
          ),
          
          tags$li(
            "Document important AI limitations."
          ),
          
          tags$li(
            "Review data handling arrangements."
          ),
          
          tags$li(
            "Confirm appropriate human oversight."
          )
          
        )
        
      )
      
    )
    
  }
  
  
  # --------------------------------------------------------
  # HIGH / VERY HIGH
  # --------------------------------------------------------
  
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
        
        tags$ul(
          
          tags$li(
            "Complete formal analytical QA."
          ),
          
          tags$li(
            "Establish representative evaluation data."
          ),
          
          tags$li(
            "Define measurable acceptance criteria."
          ),
          
          tags$li(
            "Review security and privacy risks."
          ),
          
          tags$li(
            "Confirm meaningful human oversight."
          ),
          
          tags$li(
            "Consider specialist assurance before deployment."
          )
          
        )
        
      )
      
    )
    
  }
  
  
  # --------------------------------------------------------
  # STOP / ESCALATE
  # --------------------------------------------------------
  
  tagList(
    
    h4(
      "⛔ Review required before proceeding"
    ),
    
    p(
      paste(
        "AI RiskCheck has identified one or more",
        "significant escalation conditions."
      )
    ),
    
    tags$ul(
      
      lapply(
        
        red_flags(),
        
        tags$li
        
      )
      
    ),
    
    p(
      
      strong(
        paste(
          "Resolve or formally review these issues",
          "before implementation."
        )
      )
      
    )
    
  )
  
})


# ==========================================================
# AI SYSTEM PROFILE
# ==========================================================

output$system_profile <- renderUI({
  
  profile_rows <- list(
    
    c(
      "AI type",
      input$ai_type
    ),
    
    c(
      "Environment",
      input$environment
    ),
    
    c(
      "Provider",
      input$provider
    ),
    
    c(
      "Model",
      input$model_name
    ),
    
    c(
      "Model version",
      input$model_version
    ),
    
    c(
      "Hosting",
      input$hosting
    ),
    
    c(
      "Access method",
      input$access_method
    ),
    
    c(
      "Lifecycle",
      input$lifecycle
    ),
    
    c(
      "Audience",
      input$audience
    )
    
  )
  
  
  # Add LLM usage if relevant
  
  if (
    input$ai_type %in%
    c(
      "Generative AI / LLM",
      "LLM with RAG",
      "AI agent / tool-using LLM"
    )
  ) {
    
    profile_rows <- append(
      
      profile_rows,
      
      list(
        
        c(
          "Estimated monthly tokens",
          format(
            input$monthly_tokens,
            big.mark = ","
          )
        ),
        
        c(
          "Estimated monthly cost",
          paste0(
            "£",
            format(
              input$monthly_cost,
              big.mark = ","
            )
          )
        ),
        
        c(
          "Usage monitoring",
          input$token_monitoring
        ),
        
        c(
          "Cost / usage limits",
          input$cost_limits
        )
        
      )
      
    )
    
  }
  
  
  tags$table(
    
    class =
      "table table-striped",
    
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
# GUIDANCE LIBRARY
# ==========================================================

output$guidance_cards <- renderUI({
  
  tagList(
    
    map(
      
      seq_len(
        nrow(guidance)
      ),
      
      function(i) {
        
        g <-
          guidance[i, ]
        
        
        card(
          
          class =
            "mb-3",
          
          card_header(
            
            h4(
              g$source
            )
            
          ),
          
          card_body(
            
            p(
              
              strong(
                g$organisation
              )
              
            ),
            
            p(
              g$purpose
            ),
            
            tags$a(
              
              href =
                g$url,
              
              target =
                "_blank",
              
              class =
                "btn btn-outline-primary",
              
              "Open guidance"
              
            )
            
          )
          
        )
        
      }
      
    )
    
  )
  
})


# ==========================================================
# DOWNLOAD ASSESSMENT
# ==========================================================

output$download_assessment <- downloadHandler(
  
  filename = function() {
    
    project_name <-
      input$project_name
    
    
    if (
      is.null(project_name) ||
      project_name == ""
    ) {
      
      project_name <-
        "AI_RiskCheck"
      
    }
    
    
    safe_name <-
      gsub(
        
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
    
    
    # --------------------------------------------------------
    # ADD ASSESSMENT DETAILS TO QUESTION RESULTS
    # --------------------------------------------------------
    
    results <-
      scored_answers() %>%
      
      mutate(
        
        project_name =
          input$project_name,
        
        project_description =
          input$project_description,
        
        assessment_date =
          as.character(
            Sys.Date()
          ),
        
        application_name =
          APP_NAME,
        
        application_version =
          APP_VERSION,
        
        guidance_version =
          GUIDANCE_VERSION,
        
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
        
        red_flags =
          paste(
            red_flags(),
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
# 8. RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
