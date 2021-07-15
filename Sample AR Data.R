##########     1) Load required packages/data         ###############
library(purrr);library(tidyverse);library(magrittr);library(randomNames);library(OpenRepGrid);library(readxl);library(caroline);library(stringr);library(bigrquery);library(charlatan)
library(randomNames)
set.seed(100)
bq_auth(email = "rixiye2076@relumyx.com")
bqr_auth("rixiye2076@relumyx.com");project = "publicdatasets2021";dataset = "publicdata";billing = "publicdatasets2021"


n_customers = 257
n_users = 1:12; n_users_probs = runif(length(n_users))
cust_s_date = '2008/01/01';cust_e_date = '2021/12/31'
c_create_date       = seq(as.Date(cust_s_date), as.Date(cust_e_date), by="day");c_create_date_probs = seq(1,length(c_create_date),0.0025)[1:length(c_create_date)]
c_is_blocked = c("Yes", "No"); c_is_blocked_probs = c(0.015, 1)
c_onetimeflag = c("Yes", "No"); c_onetimeflag_probs = c(0.015,1)
c_cat = c("Medical","Utilities","Health & Safety","Offices Stationery Services","Taxi Services","Data Services","Employees","Associations","Advertising","Legal Services","Subcontractor","Consultancy","IT Service/Supplies","Insurance","Accountancy/Financial Services","Business Travel","Government Department","Marketing Services");c_cat_probs = runif(length(v_cat))
c_paydays = c(0,7,14,21,30,60);c_paydays_probs = c(0.05, 0.15, 0.25, 0.5, 0.4,0.25)
terms = "Days"
c_ctry = c("GBP", "EUR", "USD"); c_ctry_probs = c(1,0.1,0.05)
c_tx_ctry = c("GBP", "EUR", "USD"); c_tx_ctry_probs = c(1,0.01,0.025)
c_cred_lmt = seq(100000,50000,-1000); c_cred_lmt_probs = seq(1000,500,-10)
c_cred_rating = c("High", "Medium", "Low"); c_cred_rating_probs = c(1,0.05,0.01)


##########     1) CUSTOMERS          ###############

CUST_ID = seq(10000,((n_customers*5)+10000),5)[1:n_customers]
CUST_DESC = ch_company(n = n_customers, locale = "en_US")
ADDRESS   = rep("ADDRESS", n_customers)
POST_CODE =  rep("POSTCODE", n_customers)
CUST_CAT = sample(c_cat, n_customers,c_cat_probs, replace = T)
PAYMENT_TERMS = sample(c_paydays, n_customers,c_paydays_probs, replace = T)
TERMS =  rep("Days", n_customers)
CREATE_DATE = sample(c_create_date, n_customers,c_create_date_probs, replace = T)
COUNTRY = sample(c_ctry, n_customers,c_ctry_probs, replace = T)
TAX_COUNTRY = sample(v_tx_ctry, n_customers,v_tx_ctry_probs, replace = T)
CUST_CREATE_ID = sample(n_users, n_customers, n_users_probs, replace = T)
IBAN = rep("GB0000000000000120", n_customers)
ONE_TIME_FLAG = sample(v_onetimeflag, n_customers,v_onetimeflag_probs, replace = T)
IS_BLOCKED = sample(c_is_blocked, n_customers,c_is_blocked_probs, replace = T)
CREDIT_LIMIT = sample(c_cred_lmt, n_customers,c_cred_lmt_probs, replace = T)
CREDIT_RATING =   sample(c_cred_rating, n_customers,c_cred_rating_probs, replace = T)

CUSTOMERS = data.frame(CUST_ID = CUST_ID,CUST_DESC = CUST_DESC,ADDRESS = ADDRESS,POST_CODE =POST_CODE,CUST_CAT = CUST_CAT,PAYMENT_TERMS = PAYMENT_TERMS,TERMS = TERMS,CREATE_DATE = CREATE_DATE,COUNTRY = COUNTRY,TAX_COUNTRY = TAX_COUNTRY,CUST_CREATE_ID = CUST_CREATE_ID,IBAN = IBAN,ONE_TIME_FLAG = ONE_TIME_FLAG,IS_BLOCKED = IS_BLOCKED,CREDIT_LIMIT = CREDIT_LIMIT, CREDIT_RATING = CREDIT_RATING)


##########     2) Sales Orders       ###############

n_sos = 25700;n_names = 23; n_process_fte = 12
so_s_date = '2020/01/01';so_e_date = '2021/12/31'
so_ord_date = seq(as.Date(so_s_date), as.Date(so_e_date), by="day");
so_ord_date_probs = seq(1,length(so_ord_date),0.005)[1:length(so_ord_date)]
sts = c("Finished","Sent","Rejected","Canceled","In Preparation","Acknowledgement Received","In Revision","In Approval");sts_probs = c(1,0.1,0.1,0.1,0.01,0.01,0.01,0.01)
so_vendors = CUST_ID; so_vendors_probs = runif(length(CUST_ID))
user_names = randomNames(n_names, which.names="both",name.order="first.last",name.sep="  ");user_names_probs = runif(length(user_names))
create_names = randomNames(n_process_fte, which.names="both",name.order="first.last",name.sep="  ");create_names_probs = runif(length(create_names))
n_items = 1:9; n_items_probs = max(n_items):min(n_items)
so_amts = c(seq(100, 50000, 25), seq(100, 15000, 50));so_amts_probs = runif(length(po_amts))
so_tx_rate = 0.2;
dis_chl = c("Direct Sales", "Intercompany") ;dis_chl_probs = c( 0.9,0.05)

SO_ID = seq(11100000,((n_sos*1)+11100000),1)[1:n_sos]
SO_DT = sample(so_ord_date, n_sos,so_ord_date_probs, replace = T)
SO_CURR = rep("GBP", n_sos)
SO_CUST_ID = sample(so_vendors, n_sos,so_vendors_probs, replace = T) 
DIST_CHL = sample(dis_chl, n_sos,dis_chl_probs, replace = T)
NO_ITEMS = sample(n_items, n_sos,n_items_probs, replace = T)
SO_GROSS_AMT = sample(so_amts, n_sos,so_amts_probs, replace = T)
SO_TAX = SO_GROSS_AMT * so_tx_rate
SO_NET_AMT = SO_GROSS_AMT - SO_TAX


SOs = data.frame(SO_ID= SO_ID,SO_DT= SO_DT,SO_CURR=SO_CURR,SO_CUST_ID=SO_CUST_ID,DIST_CHL=DIST_CHL,NO_ITEMS=NO_ITEMS ,SO_GROSS_AMT=SO_GROSS_AMT,SO_TAX=SO_TAX,SO_NET_AMT=SO_NET_AMT)

head(SOs)


##########     3) Invoices       ###############




n_invs = 30421;n_products = 1:10;n_products_probs = runif(length(n_products))
tax_rate = 0.2
inv_s_date = '2020/01/01';inv_e_date = '2021/12/31'
inv_date = seq(as.Date(inv_s_date), as.Date(inv_e_date), by="day");inv_date_probs = seq(1,length(inv_date),0.005)[1:length(inv_date)]
inv_create_date = -2:6;inv_create_date_probs = c(c(0.015,0.025),rep(1,length(inv_create_date)-2))

inv_status = c("Paid","Partially Paid","Not Paid","Voided","Canceled");inv_status_probs = c(1, 0.15, 0.01, 0.01, 0.05)
inv_doc_status = c("Delivered","Partially Delivered","Not Delivered");inv_doc_status_probs = c(1, 0.0025, 0.01)
inv_doc_type = c("Invoice","Credit Memo");inv_doc_type_probs = c(1, 0.005)
inv_descrip = " "
inv_line_descrip = " "
inv_comp_codes = c("1000","1005","1010","1020"); inv_comp_codes_probs = c(1,0.8,0.25, 0.4)
inv_cc_codes = c("200000","210000","220000","230000","240000","250000"); inv_cc_codes_probs = c(1,0.8,0.9, 0.1,0.25, 0.15)
inv_gl_code = c("50000","50050","50100","50125","50150","50300","50400","50500","51000","52000","60100","60150","60200","60225","60250");inv_gl_code_probs = runif(length(inv_gl_code))
inv_customers = CUST_ID; inv_customers_probs = runif(length(inv_customers))
inv_pay_terms = c(0,7,14,21,28,35,60,90); inv_pay_terms_probs = seq(10,length(inv_pay_terms),-(10-length(inv_pay_terms))/length(inv_pay_terms))[1:length(inv_pay_terms)]
inv_so = c(1,0);inv_so_probs = c(0.6,0.4)
inv_qty = 1:10; inv_qty_probs = seq(max(inv_qty),min(inv_qty),-1 )
inv_gross_amt = c(seq(5,15000, 50), seq(50,25000, 150),seq(500,50000, 500));inv_gross_amt_probs =runif(inv_gross_amt)


INV_ID = seq(100,((n_invs*1)+100),1)[1:n_invs]
INV_STS = sample(inv_status, n_invs,inv_status_probs, replace = T)
DOC_STS = sample(inv_doc_status, n_invs,inv_doc_status_probs, replace = T)
DOC_TYPE = sample(inv_doc_type, n_invs,inv_doc_type_probs, replace = T)
INV_DT = sample(inv_date, n_invs,inv_date_probs, replace = T)
INV_CREATE_DT = INV_DT - sample(inv_create_date, n_invs,inv_create_date_probs, replace = T)
INV_DESC =   rep(inv_descrip, n_invs)
INV_LINE_DESC =   rep(inv_descrip, n_invs)
INV_LINE_ID = rep(1,n_invs)
INV_EXT_DOC_ID = rep(" ", n_invs)
PRODUCT = sample(n_products, n_invs,n_products_probs, replace = T)
COMP_CODE = sample(inv_comp_codes, n_invs,inv_comp_codes_probs, replace = T)
COST_CNTR = sample(inv_cc_codes, n_invs,inv_cc_codes, replace = T)
GL_CODE = sample(inv_gl_code, n_invs,inv_gl_code_probs, replace = T)
PAYMENT_TERMS = sample(inv_pay_terms, n_invs,inv_pay_terms_probs, replace = T)
DUE_DATE = INV_DT+PAYMENT_TERMS
PAYMENT_TYPE = rep("CARD", n_invs)
INV_CUSTOMER_ID = sample(inv_customers, n_invs,inv_customers_probs, replace = T)
INV_SO_ID_T = sample(inv_so, n_invs,inv_so_probs, replace = T);INV_SO_ID = ifelse(INV_SO_ID_T == 1, sample(SO_ID,sum(INV_SO_ID_T)), " " );rm(INV_SO_ID_T)
QTY = sample(inv_qty, n_invs,inv_qty_probs, replace = T)
GROSS_AMT = sample(inv_gross_amt, n_invs,inv_gross_amt_probs, replace = T)
TAX_AMT = GROSS_AMT * tax_rate 
NET_AMT = GROSS_AMT *(1 - tax_rate)
UNIT_PRICE = round(GROSS_AMT/QTY ,2)
UNIT = rep("Amount",n_invs)
n_payments = length(INV_STS[INV_STS == "Paid"])
pay_days = c(seq(1,21,3),seq(1,21,1),seq(1,14,1),seq(1,7,1));pay_days_probs = runif(pay_days)

SO_DT= SOs %>% select(INV_SO_ID = SO_ID,SO_DT) %>% mutate(INV_SO_ID = as.character(INV_SO_ID))
SO_DAYS_DIFF = c(seq(1,21,1),seq(1,15,3),seq(1,20,4),seq(1,7,1),seq(1,7,1));SO_DAYS_DIFF_PROBS = runif(length(SO_DAYS_DIFF))


INVOICES_T = data.frame(INV_ID =INV_ID,INV_STS =INV_STS,DOC_STS =DOC_STS,DOC_TYPE =DOC_TYPE,INV_DT =INV_DT,INV_CREATE_DT =INV_CREATE_DT,INV_DESC =INV_DESC,INV_LINE_DESC =INV_LINE_DESC,
                        INV_LINE_ID =INV_LINE_ID,INV_EXT_DOC_ID =INV_EXT_DOC_ID,PRODUCT =PRODUCT,COMP_CODE =COMP_CODE,COST_CNTR =COST_CNTR,GL_CODE =GL_CODE,PAYMENT_TERMS =PAYMENT_TERMS,
                        DUE_DATE =DUE_DATE,PAYMENT_TYPE =PAYMENT_TYPE,INV_CUSTOMER_ID =INV_CUSTOMER_ID,INV_SO_ID =INV_SO_ID,QTY =QTY,GROSS_AMT =GROSS_AMT,TAX_AMT = TAX_AMT,NET_AMT =NET_AMT,UNIT_PRICE =UNIT_PRICE,UNIT =UNIT) %>% 
  mutate(days_pay = ifelse(INV_STS == "Paid", sample(pay_days, n_payments,pay_days_probs, replace = T), 0)) %>% left_join(SO_DT) %>% 
  mutate(INV_DT_ADD = ifelse(is.na(SO_DT), INV_DT , as.Date(SO_DT + sample(SO_DAYS_DIFF,n(), SO_DAYS_DIFF_PROBS, replace = T)))) %>% 
  mutate(INV_DT = as.Date(INV_DT_ADD, origin = "1970-01-01"),PAY_DATE = INV_DT + days_pay)   ## %>% select(-INV_DT_ADD, -ORD_DT,-days_pay) 


head(INVOICES_T)

##########     8) Create tests for exceptions ###############
options(digits=15)
INV_SUMM = INVOICES_T %>%  group_by(INV_ID) %>% 
  summarise(GROSS_AMT = round(sum(GROSS_AMT),2), N_INV = n()) %>% data.frame()

##########     9) Test Layer ###############




## 1) Round sum invoices

## 2) Incomplete customer master data - Customers that have missing bank account, address

## 3) Duplicate Invoices - Identify any duplicate invoices	

## 4) Excessive Refunds - Identify customers with high volume of refunds	

## 5) Blank credit limits - Flags customers without a credit limit

## 6) Sale greater than credit limit






