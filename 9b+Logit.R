# 
#----Logistic Regression

#  Outcome/ dependent variable will be Binary (0/1) 

# ---R packages
library(readxl)
library(tidyverse)
library(expss)
library(gtsummary)
library(gt)
library(cards)
library(psych)
library(car)

# ----------Import Excel Data-------

mydata<- read_excel('demo.xlsx')



mylab <-list(
id	 = 	"Study ID",
age	 = 	"Age in year",
division	 = 	"Administrative area division",
residence	 = 	"Place of residence",
edu	 = 	"Educational level",
wealth	 = 	"Wealth index",
wt	 = 	"Weight in kilograms",
ht	 = 	"Height in centimeters",
gender	 = 	"Gender of participants",
income	 = 	"Monthly income (Thousand)",
expenditure	 = 	"Monthly expenditure (Thousand)",
infection	 = 	"Pathogen infection",
disease	 = 	"Having a disease"
)



A2 <- mydata %>%
  select(disease, infection, age, wealth, edu, ht, gender) %>%
  filter(complete.cases(.))


#

#---------------------------------------Only one independent variable-----------
OR1<-A2 %>%
  select(disease, infection) %>%
  tbl_uvregression(
    method = glm,
    y=disease,
    method.args = list(family=binomial(link = 'logit')),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3),
    label = mylab
    
  )

#-----------------Unadjusted-----------More independent variables but separate models
OR1<-A2 %>%
  select(disease, infection, age, wealth, edu, ht, gender) %>%
  tbl_uvregression(
    method = glm,
    y=disease,
    method.args = list(family=binomial(link = 'logit')),
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3),
    label = mylab
    
  ) %>%
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~"**OR (95% CI)**") %>%
  bold_labels()


print(OR1)

#--------------Adjusted------------More independent variables in a single model



OR2 <-glm(disease~ infection + age + wealth + edu + ht + gender,
          data = A2,
          family = binomial(link='logit')) %>%
  tbl_regression(
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    label = mylab
  ) %>%
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate ~"**OR (95% CI)**") %>%
  bold_labels()

print(OR2)

#--Publication-ready tables in R--------

OR_Tab <-tbl_merge(
  tbls = list(OR1, OR2),
  tab_spanner = c('**Unadjusted**', '**Adjusted**')
)

print(OR_Tab)

#

OR_Tab %>%
  as_gt()%>%
  gtsave(filename = "OR_Reg_Table.docx",
         path="P:/abc")










#













#







#
# -----Excel data import------------

mydata<-read_excel('P:/abc/demo.xlsx')

mylab <-list(
  id	 = 	"Study ID",
  age	 = 	"Age in year",
  division	 = 	"Administrative area division",
  residence	 = 	"Place of residence",
  edu	 = 	"Educational level",
  wealth	 = 	"Wealth index",
  wt	 = 	"Weight in kilograms",
  ht	 = 	"Height in centimeters",
  gender	 = 	"Gender of participants",
  income	 = 	"Monthly income (Thousand)",
  expenditure	 = 	"Monthly expenditure (Thousand)",
  infection	 = 	"Pathogen infection",
  disease	 = 	"Having a disease"
  )
#

A2<-mydata

OR1<-A2 %>%
  select(disease, infection, age, residence, wealth, edu, ht, gender) %>%
  tbl_uvregression(
    method = glm,
    y=disease,
    label = mylab,
    method.args = list(family=binomial(link = "logit")),
    exponentiate = TRUE,
    pvalue_fun = ~style_pvalue(.x, digits = 3)
  ) %>%
  modify_column_merge(
    patter="{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  ) %>%
  modify_header(estimate~"**OR (95% CI)**") %>%
  bold_labels()


print(OR1)



OR2<-glm(
  disease~  infection + age + residence + wealth+edu + ht + gender,
  data=A2,
  family=binomial(link = "logit")
  
) %>%
  tbl_regression(
    label = mylab,
    exponentiate = TRUE,
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  )%>%
  modify_column_merge(
    pattern = "{estimate} ({conf.low}, {conf.high})",
    rows = !is.na(estimate)
  )%>%
  modify_header(estimate ~ "**OR (95% CI)**")%>%
  bold_labels()

print(OR2)



OR_Tab<-tbl_merge(
  tbls = list(OR1, OR2),
  tab_spanner = c("**Unadjusted**", "**Adjusted**")
)

print(OR_Tab)




OR_Tab %>%
  as_gt()%>%
  gtsave(filename = "OR_Table.docx",
         path = "D:/abc")


