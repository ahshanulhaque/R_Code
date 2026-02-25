# ---R packages
library(readxl)
library(tidyverse)
library(expss)
library(gtsummary)
library(gt)
library(cards)
library(psych)
library(car)
library(survival)
library(survminer)

mydata <- read_excel('disease.xlsx')

#----------------Data Label--------------------  
dat_s <- mydata %>%
  mutate(
    treatment = factor(treatment,
                       levels = c(0,1),
                       labels = c('Did not Received', 'Received'),
                       exclude = NA),
    csex = factor(csex,
                  levels = c(1,2),
                  labels = c("Male", "Female"),
                  exclude = NA),
    m_edu =factor(m_edu,
                  levels = c(1,2),
                  labels = c("Completed", "Not completed"),
                  exclude = NA),
    anc = factor(anc,
                 levels = c(1,2),
                 labels = c("At least 4 ANC", "Less than 4 ANC"),
                 exclude = NA),
    m_occu = factor(m_occu,
                    levels = c(1,2),
                    labels = c("Working", "Not working"),
                    exclude = NA),
    residence = factor(residence,
                       levels = c(1,2),
                       labels = c("Urban", "Rural"),
                       exclude = NA),
    event = as.numeric(event)           # 0 = censored, 1 = recovered
  ) %>%
  apply_labels(
    case_id = "Study ID",
    disease_days = "Duration of disease",
    event = "Recovery status",
    treatment = "Received treatment",
    csex = "Sex of the child",
    m_edu = "Mother’s education level, Secondary",
    anc = "Four or more ANC visits",
    age_marr = "Mother’s age at marriage",
    m_occu = "Type of residence",
    residence = "Monthly family incom",
    income = "Total family income per month",
    morbi_day = "Child’s sick day"
  )

#---- The Kaplan–Meier graph---------------  

surv_obj <- Surv(time = dat_s$disease_days, event = dat_s$event)
fit_fm <- survfit(surv_obj ~ treatment, data = dat_s)

# --Plot
ggsurvplot(
  fit_fm,
  data = dat_s,
  risk.table = TRUE,
  pval = TRUE,
  conf.int = TRUE,
  xlab = 'Duration of disease',
  ylab = 'Survival probability',
  legend.title = 'Treatment',
  legend.labs = c('No', 'Yes')
)

#----- Cox regression----Hazard Ratio (HR)
coxph(
  Surv(disease_days, event) ~ treatment, data = dat_s
) %>%
  tbl_regression(
    exponentiate = TRUE
  )

#----Multiple Cox regression   
coxph(
  Surv(disease_days, event) ~ treatment + csex + m_edu+ anc+ age_marr+
    m_occu + residence + income + morbi_day, 
  data = dat_s
) %>%
  tbl_regression(
    exponentiate = TRUE
  )


#--Publication ready table for Unadjusted HR  

HR1 <- dat_s %>%
  select(disease_days, event, treatment, csex, m_edu, anc, age_marr, 
         m_occu, residence, income, morbi_day) %>%
  tbl_uvregression(
    method = coxph,
    y=Surv(disease_days, event),
    exponentiate = TRUE,
    pvalue_fun =~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_merge(
    pattern = '{estimate} ({conf.low}, {conf.high})',
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~ '**HR (95% CI)**') %>%
  bold_labels()

print(HR1)

#--Publication ready table for Adjusted HR   

HR2 <- coxph(
  Surv(disease_days, event) ~
    treatment+csex+ m_edu+ anc+ age_marr+ 
    m_occu+ residence+ income+ morbi_day,
  data=dat_s
) %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun =~ style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_merge(
    pattern = '{estimate} ({conf.low}, {conf.high})',
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~ '**HR (95% CI)**') %>%
  bold_labels()

print(HR2)

#--Publication ready table for Unadjusted and Adjusted HR  

HR_Tab <- tbl_merge(
  tbls = list(HR1, HR2),
  tab_spanner = c('**Unadjusted**', '**Adjusted**')
)

print(HR_Tab)

HR_Tab %>%
  as_gt()%>%
  gtsave(filename = "HR_Table.docx",
         path = "C:/Users/mhaque17/Documents/New folder")

