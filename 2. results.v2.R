# Main Results for CxR paper

#Importing Data----

rm(list=setdiff(ls(), lsf.str()))   #removing everything but functions
options(scipen = 999, digits=6)
options(warn=0)

#PROJECT - Importance of CxR--


#loading libraries
foo <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE ) }
  }
}

#  Then try/install packages...
foo(c("ggplot2" , "reshape2", "zoo", "hrbrthemes", "GGally", "ggpubr", 
      "viridis",  "data.table" , "logr", "janitor", "readr", "lubridate", 
      "naniar", "stringdist",  "purrr", "stringr", "mondate", "xlsx",
      "readr", "readxl", "dplyr", "tidyr", "kableExtra", "directlabels", "haven",
      "ggrepel",  "eeptools","gtsummary", "gt", "pROC", "DescTools", "rms", "rpart" , "rpart.plot"))

setwd("~/Desktop/CHAI Box/Research & More/Importance of Chest Xray/data")

# A. Loading data---------

cxr.data.0<-read_csv("cleaned.cxr.csv", guess_max = 30000, skip=0)[-1]%>%
  mutate(diag.qtr=as.yearqtr(diag.date))%>%
  mutate(tb.testing.type=
           case_when(
             is.na(tb.assessment) ~ as.character(NA), 
            tb.assessment==0 ~ as.character(NA), 
             
             TRUE ~ tb.testing.type
           ))%>%
  select(-cxr.consult.yes.no, -cxr.test.result)

#View(cxr.data.0%>%head())
table(cxr.data.0$tb.assessment)
table(cxr.data.0$tb.testing.type)


cxr.data<-cxr.data.0%>%
  filter(!grepl("already", outcome.revised))%>%
  mutate(outcome.binary=
           case_when(
             grepl("not eval|died|tox|TB sugg|lost", outcome) ~ 0, 
             grepl("complet", outcome) ~ 1, 
             TRUE ~ as.numeric(NA)))%>%
  mutate(igra.ind=case_when(
    grepl("confirmed|negative", igra.test.result) ~ 1, 
    TRUE ~ 0))

table(cxr.data$igra.ind)
table(cxr.data$age.category)


# 0. General summary----
t1<-cxr.data%>%
  group_by(symptom.status, cxr.result, tpt.initiated.status, tb.assessment)%>%
  dplyr::summarise(n=n())
clipr::write_clip(t1)

t1<-cxr.data%>%
  group_by(tb.assessment, tpt.initiated.status, cxr.result )%>%
  dplyr::summarise(n=n())
clipr::write_clip(t1)
t1

t1<-cxr.data%>%
  group_by(tb.assessment, tb.diag,tb.positive )%>%
  dplyr::summarise(n=n())
clipr::write_clip(t1)
t1

# 1. Table 1-----
table.a1 <- cxr.data %>% 
  dplyr::select(
    `Symptomatic`=symptom.status, 
    `CxR Screening` = cxr.ind,
    `CxR Result (among screened)` = cxr.result.binary, 
    `TB Assessment / Medical Consultation`=tb.assessment, 
    `TB Positive`=tb.positive, 
    `TPT initiated` = tpt.initiated.status,
    `Drug Regimen (among those TPT initiated, N = XX,XX)*` = regimen.type,
    `Age Category`=age.cat, 
    `Gender` = gender,
    `State` = state, 
    `Consolidated Outcome` = broad.outcome,
    `TPT Outcome (with outcomes declared; N = XX,XX)` = simple.outcome,
    
    `Symptoms (N)` = n.symptoms,
    `Symptoms (categ)`= n.symptoms,
    `Sym Recurrent Cough` =symptom.cough, 
    `Sym Blood` =symptom.blood, 
    `Sym Weight Loss` =symptom.wt, 
    `Sym Night Sweat` =symptom.night.sweat, 
    `Sym Fever` =symptom.fever)

table.a1.out <- table.a1 %>% 
  tbl_summary(
    # statistic = list(
    #   all_continuous() ~ "{mean} (Mean); {median} ({p25}, {p75}) (Median, IQR)"),
    type = list(
      #`Age` ~ 'continuous2', 
      `Symptoms (categ)`~ 'categorical' ,
      `TPT initiated`~ 'categorical' ,
      `Symptoms (N)`~ 'continuous2' ,
      `CxR Screening`~'categorical'),
    statistic = list(
      all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no", 
    # missing = "ifany", 
    # missing_text = "not available"
    )%>%
  bold_labels()
table.a1.out

# S. Table S2------

t2 <- cxr.data %>%
  group_by(state) %>%
  summarise(
    cxr.done = sum(cxr.ind == 1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tpt.initiated = sum(tpt.initiated.status == 1, na.rm = TRUE),
    tb.diagnosed  = sum(tb.positive == 1, na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = mean(cxr.ind == 1, na.rm = TRUE),
    share.tpt          = mean(tpt.initiated.status == 1, na.rm = TRUE),
    `% TB Assessed (from CXR suggestive)`  = tb.assessed / cxr.suggestive,
    .groups = "drop"  )

t2.1 <- cxr.data %>%
  summarise(
    cxr.done = sum(cxr.ind == 1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tpt.initiated = sum(tpt.initiated.status == 1, na.rm = TRUE),
    tb.diagnosed  = sum(tb.positive == 1, na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = mean(cxr.ind == 1, na.rm = TRUE),
    share.tpt          = mean(tpt.initiated.status == 1, na.rm = TRUE),
    `% TB Assessed (from CXR suggestive)`  = tb.assessed / cxr.suggestive,
    .groups = "drop"  )%>%
  mutate(state="Total")

clipr::write_clip(rbind(t2,t2.1))
t2


# S. Table S3----

t3 <- cxr.data %>%
  group_by(diag.qtr) %>%
  summarise(
    cxr.done = sum(cxr.ind == 1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tpt.initiated = sum(tpt.initiated.status == 1, na.rm = TRUE),
    tb.pos  = sum(tb.positive == 1, na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = mean(cxr.ind == 1, na.rm = TRUE),
    share.tpt          = mean(tpt.initiated.status == 1, na.rm = TRUE),
    share.tb.positive  = mean(tb.positive == 1, na.rm = TRUE),
    `% TB Assessed (from CXR suggestive)`  = tb.assessed / cxr.suggestive,
    .groups = "drop"  )
t3
clipr::write_clip(t3)
t3.1 <- cxr.data %>%
  summarise(
    cxr.done = sum(cxr.ind == 1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tpt.initiated = sum(tpt.initiated.status == 1, na.rm = TRUE),
    tb.pos  = sum(tb.positive == 1, na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = mean(cxr.ind == 1, na.rm = TRUE),
    share.tpt          = mean(tpt.initiated.status == 1, na.rm = TRUE),
    share.tb.positive  = mean(tb.positive == 1, na.rm = TRUE),
    `% TB Assessed (from CXR suggestive)`  = tb.assessed / cxr.suggestive,
    
    .groups = "drop"  )%>%
  mutate(diag.qtr="Total")%>%
  select(diag.qtr, everything())
t3.1
clipr::write_clip(t3.1)


# 2. Table 2-----------
t2 <- cxr.data %>%
  group_by(symptom.status) %>%
  summarise(
    cxr.ind = sum(cxr.ind==1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    tb.assessed.cxr.sugg  = sum(tb.assessment == 1 & cxr.result.numeric==1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = cxr.ind/n.hhc,
    share.cxr.sugg          = cxr.suggestive/cxr.ind,
    `% TB Assessed (from CXR suggestive)`  = tb.assessed.cxr.sugg / cxr.suggestive,
    .groups = "drop"  )%>%
  select(symptom.status, n.hhc, share.cxr, share.cxr.sugg, `% TB Assessed (from CXR suggestive)`, cxr.suggestive, tb.assessed)
t2
clipr::write_clip(t2)

t2.1<- cxr.data %>%
  summarise(
    cxr.ind = sum(cxr.ind==1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    tb.assessed.cxr.sugg  = sum(tb.assessment == 1 & cxr.result.numeric==1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = cxr.ind/n.hhc,
    share.cxr.sugg          = cxr.suggestive/cxr.ind,
    `% TB Assessed (from CXR suggestive)`  = tb.assessed.cxr.sugg / cxr.suggestive,
    .groups = "drop"  )%>%
  mutate(symptom.status="totals")%>%
  select( symptom.status, n.hhc, share.cxr, share.cxr.sugg, `% TB Assessed (from CXR suggestive)`, cxr.suggestive, tb.assessed)
t3<-rbind(t2,t2.1)
clipr::write_clip(t3)
clipr::write_clip(t2.1)
t3




# S. Table S4------
# Regression to understand factors affecting CXR Screening
library(lmtest)
glm.1 <- glm(cxr.ind ~   state + gender + age.cat +occ.short + symptom.status + as.factor(diag.qtr), 
             data = cxr.data, family = binomial)

robust_se1 <- coeftest(glm.1, vcov = vcovHC(glm.1, type = "HC1"))

rob_se_2 <- list(sqrt(diag(vcovHC(glm.1, type = "HC1"))))
odds_ratios<-list(round(exp(coef(glm.1)),2))
p_values <- list(robust_se1[, 4])  # Extract p-values from coeftest
stargazer(glm.1,  type = "html",
          coef = odds_ratios, ci= TRUE,
          se = rob_se_2,
          p = p_values, digits = 2,
          apply.coef = NULL,  # Odds Ratios already exponentiated
          #  # Customize the statistics you wish to omit
          title = "Odds Ratios with Robust Standard Errors", out="results/file1.html")



# S. Table S5------
# Age & Screening Coverage

table.a2 <- cxr.data %>% 
  mutate(occ.short=case_when(occ.short=="others1" ~ as.character(NA), 
                             TRUE ~ occ.short))%>%
  dplyr::select(
    `Symptomatic`=symptom.status, 
    `CxR` = cxr.ind,
    `TB assessment`=tb.assessment, 
    `Age`=age.cat)

table.a2.out <- table.a2 %>% 
  tbl_summary(by=`Age`,
              
              statistic = list(
                all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
                all_categorical() ~ "{n} ({p}%)"),
              missing = "ifany", 
              missing_text = "not available")%>%
  add_p(test = everything() ~ "kruskal.test")%>%
  #add_p()%>%
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}") %>%
  bold_labels()
table.a2.out


# S. Table S6------
# Occupation & Screening Coverage

table.a2 <- cxr.data %>% 
  mutate(occ.short=case_when(occ.short=="others1" ~ as.character(NA), 
                             TRUE ~ occ.short))%>%
  dplyr::select(
    `Symptomatic`=symptom.status, 
    `CxR` = cxr.ind,
    `TB assessment`=tb.assessment, 
    `Occ` = occ.short)

table.a2.out <- table.a2 %>% 
  tbl_summary(by=`Occ`,
              
              statistic = list(
                all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
                all_categorical() ~ "{n} ({p}%)"),
              missing = "ifany", 
              missing_text = "not available")%>%
  add_p(test = everything() ~ "kruskal.test")%>%
  #add_p()%>%
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}") %>%
  bold_labels()
table.a2.out


# 3. Table 3 (CXR & TPT Initiations)-----------
table2 <- table.a1 %>% 
  tbl_summary(
    by=`CxR Screening`,
    # statistic = list(
    #   all_continuous() ~ "{mean} (Mean); {median} ({p25}, {p75}) (Median, IQR)"),
    type = list(
      #`Drug Regimen`~'categorical',
      #`Age` ~ 'continuous2', 
      `Symptoms (categ)`~ 'categorical' ,
      `TPT initiated`~ 'categorical' ,
      `Symptoms (N)`~ 'continuous2'),
    statistic = list(
      all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
      all_categorical() ~ "{n} ({p}%)"),
    missing = "no", 
    # missing = "ifany", 
    # missing_text = "not available"
    )%>%
  add_p(test = everything() ~ "kruskal.test")%>%
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}") %>%
  bold_labels()
table2

# Figure 2---

t2 <- cxr.data %>%
  group_by(state) %>%
  summarise(
    tpt.initiated = sum(tpt.initiated.status==1, na.rm = TRUE),
    cxr.ind = sum(cxr.ind==1, na.rm = TRUE),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    share.cxr          = cxr.ind/n.hhc,
    share.cxr.sugg          = cxr.suggestive/cxr.ind,
    tpt.initiated.share          = tpt.initiated/n.hhc,
    
    .groups = "drop"  )
t2
clipr::write_clip(t2)




# S. Table S7------
# Logistic Model --- TPT Inititations

library(boot)
library(lmtest)
library(sandwich)
library(broom)

glm_TPT <- glm(tpt.initiated.status ~ age + gender +occ.short + cxr.ind + n.symptoms + state + 
                 as.factor(diag.qtr), 
               data = cxr.data, family = binomial)

glm_TPT <- glm(tpt.initiated.status ~ cxr.ind + n.symptoms + igra.ind + age + gender +
                 occ.short +  state + 
                 as.factor(diag.qtr), 
               data = cxr.data, family = binomial)

glm_TPT <- glm(tpt.initiated.status ~ cxr.ind*igra.ind + n.symptoms  + age + gender +
                 occ.short +  state + 
                 as.factor(diag.qtr), 
               data = cxr.data, family = binomial)


robust_se1 <- coeftest(glm_TPT, vcov = vcovHC(glm_TPT, type = "HC1"))
rob_se_2 <- list(sqrt(diag(vcovHC(glm_TPT, type = "HC1"))))
odds_ratios<-list(round(exp(coef(glm_TPT)),2))
p_values <- list(robust_se1[, 4])  # Extract p-values from coeftest
stargazer(glm_TPT,  type = "html",
          coef = odds_ratios, ci= TRUE,
          se = rob_se_2,
          p = p_values, digits = 2,
          apply.coef = NULL,  # Odds Ratios already exponentiated
          #  # Customize the statistics you wish to omit
          title = "Odds Ratios with Robust Standard Errors", out="results/file3.html")




# S. Table S8------
# Symptoms * TB Diagnosis


t2 <- cxr.data %>%
  filter(cxr.result.numeric==1)%>%
  group_by(n.symptoms) %>%
  summarise(
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    cxr.suggestive   = n_distinct(hhc.id),
    `% TB Assessed (from CXR suggestive)`  = tb.assessed / cxr.suggestive,
    .groups = "drop"  )

t2
clipr::write_clip(t2)

# S. Table S8------
# Logistic Model * TB Diagnosis
glm.2 <- glm(tb.assessment ~  n.symptoms + age.cat + gender +  as.factor(diag.qtr) +state + occ.short,
             data = cxr.data%>%filter(cxr.result.numeric==1), family = binomial)



robust_se1 <- coeftest(glm.2, vcov = vcovHC(glm.2, type = "HC1"))
rob_se_2 <- list(sqrt(diag(vcovHC(glm.2, type = "HC1"))))
odds_ratios<-list(round(exp(coef(glm.2)),2))
p_values <- list(robust_se1[, 4])  # Extract p-values from coeftest
stargazer(glm.2,  type = "html",
          coef = odds_ratios, ci= TRUE,
          se = rob_se_2,
          p = p_values, digits = 2,
          apply.coef = NULL,  # Odds Ratios already exponentiated
          #  # Customize the statistics you wish to omit
          title = "Odds Ratios with Robust Standard Errors", out="results/file2.html")



# 4. Table 4-----------

t1<-cxr.data%>%
  filter(is.na(tb.testing.type))%>%
  filter(tb.assessment==1)%>%
  group_by(tb.assessment, tb.testing.type, tb.diag, tb.positive, tpt.initiated.status, outcome.revised, cxr.diag.method, cxr.diag.tb, cxr.result.binary, consolidated.outcome)%>%
  dplyr::summarise(n=n())
t2 <- cxr.data %>%
  filter(tb.assessment==1)%>%
  group_by(tb.testing.type) %>%
  summarise(
    n.hhc   = n_distinct(hhc.id),
    share.hhc=n()/sum(n.hhc),
    n.tb.positive=sum(tb.positive == 1, na.rm = TRUE),
    `TB positivity`  = n.tb.positive / n.hhc,
    
    .groups = "drop"  )
t2
clipr::write_clip(t2)



# S. Table S10-------
table.a2 <- cxr.data %>% 
  dplyr::select(
    `TB Testing Type`=tb.testing.type, 
    `Age`=age, 
    `Symptomatic`=symptom.status, 
    `TB Positive`=tb.positive, 
    `Gender` = gender,
    `Symptoms (N)` = n.symptoms,
    `Sym Cough` =symptom.cough, 
    `Sym Blood` =symptom.blood, 
    `Sym Weight Loss` =symptom.wt, 
    `Sym Night Sweat` =symptom.night.sweat, 
    `Sym Fever` =symptom.fever)

table.3 <- table.a2 %>% 
  tbl_summary(
    by=`TB Testing Type`,
    type = list(
      `Gender`~'categorical', 
      `Age`~'continuous2',
      `Symptoms (N)`~'continuous2'),
    statistic = list(
      all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
      all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany", 
    missing_text = "not available")%>%
  add_p(test = everything() ~ "kruskal.test")%>%
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}") %>%
  bold_labels()
table.3


# 5. Table 5------------

# Index patients - MIcrobiological or Clinical Diagnosis

t2 <- cxr.data %>%
  group_by(index.micro.confirmed) %>%
  summarise(
    n.index   = n_distinct(patient.id),
    n.tb.positive=sum(tb.positive == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    
    tb.pos.tb.ass  = paste0(round(n.tb.positive/tb.assessed,2)*100, "%"),
    tb.pos.cxr.sugg  = paste0(round(n.tb.positive/cxr.suggestive,2)*100,"%"),
    tb.pos.all  = paste0(round(n.tb.positive/n.hhc,4)*100,"%"),
    
    
    .groups = "drop"  )%>%
  select(-n.hhc, -cxr.suggestive, -tb.assessed)
t2
clipr::write_clip(t2)


t2.1 <- cxr.data %>%
  summarise(
    n.index   = n_distinct(patient.id),
    n.tb.positive=sum(tb.positive == 1, na.rm = TRUE),
    n.hhc   = n_distinct(hhc.id),
    cxr.suggestive = sum(cxr.result.binary == "tb suggestive", na.rm = TRUE),
    tb.assessed  = sum(tb.assessment == 1, na.rm = TRUE),
    
    tb.pos.tb.ass  = paste0(round(n.tb.positive/tb.assessed,2)*100, "%"),
    tb.pos.cxr.sugg  = paste0(round(n.tb.positive/cxr.suggestive,2)*100,"%"),
    tb.pos.all  = paste0(round(n.tb.positive/n.hhc,4)*100,"%"),
    
    
    .groups = "drop"  )%>%
  select(-n.hhc, -cxr.suggestive, -tb.assessed)%>%
  mutate(index.micro.confirmed="totals")
t2.1
t3<-rbind(t2, t2.1)
clipr::write_clip(t3)
t3


table(cxr.data.0$consolidated.outcome)

# S. Table S11----
table.a3 <- cxr.data %>% 
  dplyr::select(
    `Symptomatic`=symptom.status, 
    `CxR Screening` = cxr.ind,
    `CxR Result (among screened)` = cxr.result.binary, 
    `TB Assessment / Medical Consultation`=tb.assessment, 
    `TB Positive (among those tested)`=tb.positive, 
    `TPT initiated` = tpt.initiated.status,
    `Age Category`=age.cat, 
    `Gender` = gender,
    `Occ` = occ.short,
    `State` = state, 
    `Consolidated Outcome` = broad.outcome,
    `TPT Outcome (among declared)` = simple.outcome)

table.a2.out <- table.a2 %>% 
  tbl_summary(by=`Symptomatic`,
              type = list(
                #`Age` ~ 'continuous2', 
                `TPT initiated`~ 'categorical' ,
                `Symptoms (categ)`~ 'categorical'),
              statistic = list(
                all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
                all_categorical() ~ "{n} ({p}%)"),
              missing = "no", 
              missing_text = "not available")%>%
  add_p(test = everything() ~ "kruskal.test")%>%
  #add_p()%>%
  add_significance_stars(
    hide_p = FALSE,
    pattern = "{p.value}{stars}") %>%
  bold_labels()
table.a2.out


# S. Table S12----
table(cxr.data$cxr.result.numeric)
table.s12 <- cxr.data %>% 
  filter(cxr.ind==0 | (cxr.result.numeric==1 & tb.assessment==0))

table.s12 <- cxr.data %>% 
  filter(cxr.ind==0 | (cxr.result.numeric==1 & tb.assessment==0)) %>%
  dplyr::select(
    `TPT Outcome`=simple.outcome, 
    `TPT Outcome1`=broad.outcome, 
    `Age`=age, 
    `Symptomatic`=symptom.status, 
    `Gender` = gender,
    `Symptoms (N)` = n.symptoms,
  )

table.3 <- table.s12 %>% 
  tbl_summary(
    type = list(
      `Gender`~'categorical', 
      `Age`~'continuous2',
      `Symptoms (N)`~'continuous2'),
    statistic = list(
      all_continuous() ~ c("{median} ({p25}, {p75})", "{mean}"),
      all_categorical() ~ "{n} ({p}%)"),
    missing = "ifany", 
    missing_text = "not available")%>%
  bold_labels()
table.3


# Two Stage Counterfactual Modeling---


cxr.data.counterfactual<-cxr.data%>%
  mutate(elig=
           case_when(
             cxr.result=="cxr suggestive" & tb.assessment==1 ~ 1, 
             cxr.result=="cxr suggestive" & tb.assessment==0 ~ 0, 
             cxr.result=="not suggestive" ~ 2, TRUE ~ 0))%>%
  mutate(tb.positive=case_when(
    tb.positive == 1 ~ 1, 
    TRUE ~ 0))

cxr.data.3<-cxr.data.counterfactual%>%
  filter(cxr.ind==0)


cxr.data.3.1<-cxr.data.counterfactual%>%
  filter(!(cxr.ind==1))%>%
  filter(!(tb.assessment==1))





library(boot)
library(lmtest)
library(sandwich)
library(broom)

#Step 1 — Among those screened by CXR----------

# Model probability of having a suggestive CXR:
#(Pop = All cases who got CXR)

cxr.data.2<-cxr.data.counterfactual%>%
  filter(cxr.ind==1)
glm_CXR <- glm(cxr.result.numeric ~ state +  age + gender + n.symptoms +  as.factor(diag.qtr) + occ.short, 
               data = cxr.data.2, family = binomial)


robust_se1 <- coeftest(glm_CXR, vcov = vcovHC(glm_CXR, type = "HC1"))
rob_se_2 <- list(sqrt(diag(vcovHC(glm_CXR, type = "HC1"))))
odds_ratios<-list(round(exp(coef(glm_CXR)),2))
p_values <- list(robust_se1[, 4])  # Extract p-values from coeftest
# S. S13 Table----
stargazer(glm_CXR,  type = "html",
          coef = odds_ratios, ci= TRUE,
          se = rob_se_2,
          p = p_values, digits = 2,
          apply.coef = NULL,  # Odds Ratios already exponentiated
          #  # Customize the statistics you wish to omit
          title = "Odds Ratios with Robust Standard Errors", out="results/file2.html")


# Calibration


# Align fitted values with data actually used
mf <- model.frame(glm_CXR)
resp <- model.response(mf)
pred <- fitted(glm_CXR)

# Create deciles of predicted probability
breaks <- unique(quantile(pred, probs = seq(0, 1, 0.1), na.rm = TRUE))
decile <- cut(pred, breaks = breaks, include.lowest = TRUE)

# Compute observed vs predicted in each decile
obs <- tapply(resp, decile, mean)
pred_mean <- tapply(pred, decile, mean)

# Plot
plot(pred_mean, obs,
     xlab = "Predicted probability (mean in decile)",
     ylab = "Observed TB positivity (mean in decile)",
     main = "Calibration plot (deciles)",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# Validate the calibration performance of a predictive model
val.prob(fitted(glm_CXR), model.response(model.frame(glm_CXR)))




#AUC
roc_obj <- roc(cxr.data.2$cxr.result.numeric, fitted(glm_CXR))
plot(roc_obj, print.auc=TRUE)
auc(roc_obj)


#Step 2 — Among those with suggestive CXRs who were assessed--------------

#Model probability of testing positive for TB:

#Filtering data where TB assessment was done
cxr.data.2.1<-cxr.data.counterfactual%>%
  filter(cxr.ind==1 & tb.assessment==1)


glm_TB <- glm(tb.positive ~ state +  age + gender + n.symptoms +  as.factor(diag.qtr)+ occ.short, 
              data = cxr.data.2.1, family = binomial)


robust_se1 <- coeftest(glm_TB, vcov = vcovHC(glm_TB, type = "HC1"))
rob_se_2 <- list(sqrt(diag(vcovHC(glm_TB, type = "HC1"))))
odds_ratios<-list(round(exp(coef(glm_TB)),2))
p_values <- list(robust_se1[, 4])  # Extract p-values from coeftest
# S. S14 Table----
stargazer(glm_TB,  type = "html",
          coef = odds_ratios, ci= TRUE,
          se = rob_se_2,
          p = p_values, digits = 2,
          apply.coef = NULL,  # Odds Ratios already exponentiated
          #  # Customize the statistics you wish to omit
          title = "Odds Ratios with Robust Standard Errors", out="results/file2.html")


#AUC
roc_obj <- roc(cxr.data.2.1$tb.positive, fitted(glm_TB))
plot(roc_obj, print.auc=TRUE)
auc(roc_obj)
0.708


# Calibration of TB positivity Model--------
glm_TB <- glm(tb.positive ~ state +  age + gender + n.symptoms +  as.factor(diag.qtr)+ occ.short, 
              data = cxr.data.2.1, family = binomial)
# Align fitted values with data actually used
mf <- model.frame(glm_TB)
resp <- model.response(mf)
pred <- fitted(glm_TB)

# Create deciles of predicted probability
breaks <- unique(quantile(pred, probs = seq(0, 1, 0.1), na.rm = TRUE))
decile <- cut(pred, breaks = breaks, include.lowest = TRUE)

# Compute observed vs predicted in each decile
obs <- tapply(resp, decile, mean)
pred_mean <- tapply(pred, decile, mean)

# Plot
plot(pred_mean, obs,
     xlab = "Predicted probability (mean in decile)",
     ylab = "Observed TB positivity (mean in decile)",
     main = "Calibration plot (deciles)",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2)

# Validate the calibration performance of a predictive model
val.prob(fitted(glm_TB), model.response(model.frame(glm_TB)))




#STEP 3 — Predict suggestive CXR probability (everyone)---------

#Use your first model (glm_CXR) to predict for all individuals, including those without actual CXR results.

# Predict probability of suggestive CXR for everyone (including unscreened)
cxr.data$pred_prob_suggestive <- predict(glm_CXR, newdata = cxr.data, type = "response")

#STEP 4 — Predict TB positivity probability (conditional)

#Now use glm_TB to predict the probability of testing positive if someone had a suggestive CXR and was assessed.

# Predict TB positivity probability for everyone (conditional model)
cxr.data$pred_prob_TB_given_suggestive <- predict(glm_TB, newdata = cxr.data, type = "response")


# Combine to get expected TB 
cxr.data$expected_TB_prob <- cxr.data$pred_prob_suggestive * cxr.data$pred_prob_TB_given_suggestive

# Summed expected cases and prevalence
total_expected_TB_cases <- sum(cxr.data$expected_TB_prob, na.rm = TRUE)
total_N <- nrow(cxr.data)
expected_TB_prev <- total_expected_TB_cases / total_N
expected_TB_prev
total_expected_TB_cases
total_N
cat("Estimated counterfactual TB prevalence (if all screened and assessed):", 
    round(100 * expected_TB_prev, 4), "%\n")

# Replace NAs with 0 (since not tested => no observed TB case)
cxr.data$tb.positive_adj <- ifelse(is.na(cxr.data$tb.positive), 0, cxr.data$tb.positive)

# Observed prevalence (in full cohort)
observed_TB_prev <- mean(cxr.data$tb.positive_adj)
cat("Observed TB prevalence (full cohort):", round(100 * observed_TB_prev, 4), "%\n")



# Bootsrapping


set.seed(1234)   # for reproducibility

# number of bootstrap iterations
B <- 200

boot_estimates <- replicate(B, {
  # sample rows with replacement
  boot_data <- cxr.data[sample(nrow(cxr.data), replace = TRUE), ]
  # compute prevalence within the bootstrap sample
  mean(boot_data$expected_TB_prob, na.rm = TRUE)
})

# 95% CI (percentile method)
boot_CI <- quantile(boot_estimates, probs = c(0.025, 0.975))

cat("Estimated counterfactual TB prevalence:", round(100 * expected_TB_prev, 2), "%\n")
cat("95% CI:", round(100 * boot_CI[1], 4), "–", round(100 * boot_CI[2], 4), "%\n")


observed_TB_prev <- mean(cxr.data$tb.positive_adj, na.rm = TRUE)
obs_CI <- binom.test(sum(cxr.data$tb.positive_adj, na.rm = TRUE),
                     nrow(cxr.data))$conf.int
cat("Observed TB prevalence (95% CI):",
    round(100 * obs_CI[1], 2), "–", round(100 * obs_CI[2], 2), "%\n")



# Figure 1---

length(cxr.data.0)
length(unique(cxr.data$hhc.id))
length(unique(cxr.data.0$patient.id))

table(cxr.data.0$broad.outcome)

t1<-cxr.data%>%
  filter(tb.assessment==1)%>%
  group_by(cxr.result.binary, tb.positive)%>%
  dplyr::summarise(n=n())
t1


# Anonymizing----------

make_id <- function(n = 5) {
  chars <- c(0:9, letters, letters)
  paste0(sample(chars, n, replace = TRUE), collapse = "")
}


make_id.2 <- function(n = 8) {
  chars <- c(0:9, letters, letters)
  paste0(sample(chars, n, replace = TRUE), collapse = "")
}

cxr.data.0$index.basis.of.diag.test.name
cxr.data.anonymized<-cxr.data.0%>%
  group_by(patient.id)%>%
  mutate(uuid.patient.id = make_id())%>%
  ungroup()%>%
  group_by(hhc.id)%>%
  mutate(uuid.hhc.id = make_id.2())%>%
  ungroup()%>%
  dplyr::select(uuid.patient.id, uuid.hhc.id, everything())%>%
  dplyr::select(-patient.id, -hhc.id, -index.basis.of.diag.test.name, -marital.status, -cxr.result)

write.csv(cxr.data.anonymized, "cxr.anonymized.csv", row.names = FALSE, fileEncoding = "UTF-8")


# END_--