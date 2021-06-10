##########     1) Load required packages/data         ###############
library(purrr);library(tidyverse);library(magrittr);library(randomNames);library(OpenRepGrid);library(readxl);library(caroline);library(stringr);library(bigrquery)
library(randomNames)
set.seed(100)

##########     2) Define parameters         ###############

##########     3) Create Vendors          ###############
n_vendors = 257
ven_s_date = '2008/01/01';ven_e_date = '2021/12/31'
v_status = c("Active", "Blocked" ,"Obsolete","In Preparation"); v_status_probs = c(1, 0.02, 0.2,0.01)
v_onetimeflag = c("Yes", "No"); v_onetimeflag_probs = c(0.015,1)
v_nopaymentflag = c("Yes", "No"); v_nopaymentflag_probs = c(0.015,1)
v_isemployee = c("Yes", "No"); v_isemployee_probs = c(0.015,0.5)
v_cat = c("Medical","Utilities","Health & Safety","Offices Stationery Services","Taxi Services","Data Services","Employees","Associations","Advertising","Legal Services","Subcontractor","Consultancy","IT Service/Supplies","Insurance","Accountancy/Financial Services","Business Travel","Government Department","Marketing Services");v_cat_probs = runif(length(v_cat))
v_paydays = c("0 Days","7 Days","14 Days","21 Days","30 Days","60 Days");v_paydays_probs = c(0.05, 0.15, 0.25, 0.5, 0.4,0.25)


VENDOR_ID = seq(10000,((n_vendors*5)+10000),5)[1:n_vendors]
STATUS = sample(v_status, n_vendors,v_status_probs, replace = T)
VENDOR_CAT = sample(v_cat, n_vendors,v_cat_probs, replace = T)
PAYMENT_TERMS = sample(v_paydays, n_vendors,v_paydays_probs, replace = T)

ONE_TIME_FLAG = sample(v_nopaymentflag, n_vendors,v_nopaymentflag_probs, replace = T)
NO_PAYMENT_FLAG = sample(v_isemployee, n_vendors,v_isemployee_probs, replace = T)







randomNames(5, which.names="both",name.order = "first.last",name.sep = " ",sample.with.replacement=TRUE) 




