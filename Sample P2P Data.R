##########     1) Load required packages/data         ###############
library(purrr);library(tidyverse);library(magrittr);library(randomNames);library(OpenRepGrid);library(readxl);library(caroline);library(stringr);library(bigrquery);library(charlatan)
library(randomNames)
set.seed(100)
bq_auth(email = "rixiye2076@relumyx.com")
bqr_auth("rixiye2076@relumyx.com");project = "publicdatasets2021";dataset = "publicdata";billing = "publicdatasets2021"

##########     2) Define parameters         ###############

##########     3) Create Vendors          ###############
n_vendors = 257
n_users = 1:12; n_users_probs = runif(length(n_users))
ven_s_date = '2008/01/01';ven_e_date = '2021/12/31'
v_create_date       = seq(as.Date(ven_s_date), as.Date(ven_e_date), by="day");v_create_date_probs = seq(1,length(v_create_date),0.0025)[1:length(v_create_date)]
v_status = c("Active", "Blocked" ,"Obsolete","In Preparation"); v_status_probs = c(1, 0.02, 0.2,0.01)
v_onetimeflag = c("Yes", "No"); v_onetimeflag_probs = c(0.015,1)
v_nopaymentflag = c("Yes", "No"); v_nopaymentflag_probs = c(0.015,1)
v_isemployee = c("Yes", "No"); v_isemployee_probs = c(0.015,0.5)
v_cat = c("Medical","Utilities","Health & Safety","Offices Stationery Services","Taxi Services","Data Services","Employees","Associations","Advertising","Legal Services","Subcontractor","Consultancy","IT Service/Supplies","Insurance","Accountancy/Financial Services","Business Travel","Government Department","Marketing Services");v_cat_probs = runif(length(v_cat))
v_paydays = c(0,7,14,21,30,60);v_paydays_probs = c(0.05, 0.15, 0.25, 0.5, 0.4,0.25)
terms = "Days"
v_ctry = c("GBP", "EUR", "USD"); v_ctry_probs = c(1,0.1,0.05)
v_tx_ctry = c("GBP", "EUR", "USD"); v_tx_ctry_probs = c(1,0.01,0.025)


VENDOR_ID = seq(10000,((n_vendors*5)+10000),5)[1:n_vendors]
VENDOR_DESC = ch_company(n = n_vendors, locale = "en_US")
STATUS = sample(v_status, n_vendors,v_status_probs, replace = T)
ADDRESS   = rep("ADDRESS", n_vendors)
POST_CODE =  rep("POSTCODE", n_vendors)
VENDOR_CAT = sample(v_cat, n_vendors,v_cat_probs, replace = T)
PAYMENT_TERMS = sample(v_paydays, n_vendors,v_paydays_probs, replace = T)
CREATE_DATE = sample(v_create_date, n_vendors,v_create_date_probs, replace = T)
COUNTRY = sample(v_ctry, n_vendors,v_ctry_probs, replace = T)
TAX_COUNTRY = sample(v_tx_ctry, n_vendors,v_tx_ctry_probs, replace = T)
VEN_CREATE_ID = sample(n_users, n_vendors, n_users_probs, replace = T)
IBAN = rep("GB0000000000000120", n_vendors)
ONE_TIME_FLAG = sample(v_onetimeflag, n_vendors,v_onetimeflag_probs, replace = T)
NO_PAYMENT_FLAG = sample(v_nopaymentflag, n_vendors,v_nopaymentflag_probs, replace = T)
IS_EMPLOYEE = sample(v_isemployee, n_vendors,v_isemployee_probs, replace = T)

VENDORS = data.frame(VENDOR_ID = VENDOR_ID,VENDOR_DESC = VENDOR_DESC,STATUS = STATUS, ADDRESS = ADDRESS,POST_CODE =POST_CODE,VENDOR_CAT = VENDOR_CAT,PAYMENT_TERMS = PAYMENT_TERMS,TERMS = terms, CREATE_DATE = CREATE_DATE,COUNTRY = COUNTRY,TAX_COUNTRY = TAX_COUNTRY,VEN_CREATE_ID = VEN_CREATE_ID,IBAN = IBAN,ONE_TIME_FLAG = ONE_TIME_FLAG,NO_PAYMENT_FLAG = NO_PAYMENT_FLAG,IS_EMPLOYEE = IS_EMPLOYEE)

write.delim(VENDORS, "Vendors.txt", sep = "|")

##########     4) POs          ###############

n_pos = 25700;n_names = 23; n_process_fte = 12
po_s_date = '2020/01/01';po_e_date = '2021/12/31'
po_ord_date = seq(as.Date(po_s_date), as.Date(po_e_date), by="day");
po_ord_date_probs = seq(1,length(po_ord_date),0.005)[1:length(po_ord_date)]
sts = c("Finished","Sent","Rejected","Canceled","In Preparation","Acknowledgement Received","In Revision","In Approval");sts_probs = c(1,0.1,0.1,0.1,0.01,0.01,0.01,0.01)
order_status = c("Ordered","Not Ordered");order_status_probs = c(1, 0.01)
delivery_status = c("Delivered","Partially Delivered","Not Delivered");delivery_status_probs = c(1, 0.0025, 0.01)
invoice_status = c("Delivered","Partially Invoiced","Not Invoiced");invoice_status_probs = c(1, 0.0025, 0.01)
po_vendors = VENDOR_ID; po_vendors_probs = runif(length(VENDOR_ID))
user_names = randomNames(n_names, which.names="both",name.order="first.last",name.sep="  ");user_names_probs = runif(length(user_names))
create_names = randomNames(n_process_fte, which.names="both",name.order="first.last",name.sep="  ");create_names_probs = runif(length(create_names))
n_items = 1:9; n_items_probs = max(n_items):min(n_items)
po_amts = c(seq(100, 50000, 25), seq(100, 15000, 50));po_amts_probs = runif(length(po_amts))



PO_ID = seq(11100000,((n_pos*1)+11100000),1)[1:n_pos]
ORD_DT = sample(po_ord_date, n_pos,po_ord_date_probs, replace = T)
PO_STS = sample(sts, n_pos,sts_probs, replace = T)
ORD_STS = sample(order_status, n_pos,order_status_probs, replace = T)
DEL_STS = sample(delivery_status, n_pos,delivery_status_probs, replace = T)
INV_STS = sample(invoice_status, n_pos,invoice_status_probs, replace = T)
NO_ITEMS = sample(n_items, n_pos,n_items_probs, replace = T)
PO_AMT = sample(po_amts, n_pos,po_amts_probs, replace = T)

PO_CURR = "GBP"
PO_SUPPLIER_ID = sample(po_vendors, n_pos,po_vendors_probs, replace = T)
BUYER = sample(user_names,n_pos,user_names_probs, replace = T)
RECEIPIENT = sample(user_names,n_pos,user_names_probs, replace = T)
PO_CREATE_ID = sample(create_names,n_pos,create_names_probs, replace = T)
PO_APPROVER_ID = sample(create_names,n_pos,create_names_probs, replace = T)

POs = data.frame(PO_ID= PO_ID,ORD_DT= ORD_DT,PO_STS=PO_STS,ORD_STS=ORD_STS,DEL_STS=DEL_STS,INV_STS=INV_STS,NO_ITEMS=NO_ITEMS,PO_AMT=PO_AMT,PO_CURR=PO_CURR,PO_SUPPLIER_ID=PO_SUPPLIER_ID,BUYER=BUYER,RECEIPIENT=RECEIPIENT,PO_CREATE_ID=PO_CREATE_ID,PO_APPROVER_ID =PO_APPROVER_ID)

write.delim(POs, "POs.txt", sep = "|")

##########     5) Invoices          ###############

n_invs = 30421;n_products = 1:10;n_products_probs = runif(length(n_products))
tax_rate = 0.2
inv_s_date = '2020/01/01';inv_e_date = '2021/12/31'
inv_date = seq(as.Date(inv_s_date), as.Date(inv_e_date), by="day");inv_date_probs = seq(1,length(inv_date),0.005)[1:length(inv_date)]
inv_create_date = -2:6;inv_create_date_probs = c(c(0.015,0.025),rep(1,length(inv_create_date)-2))

inv_status = c("Paid","Partially Paid","Not Paid","Voided","Canceled");inv_status_probs = c(1, 0.15, 0.01, 0.01, 0.05)
inv_doc_status = c("Delivered","Partially Delivered","Not Delivered");inv_doc_status_probs = c(1, 0.0025, 0.01)
inv_doc_type = c("Invoice","Credit Memo");inv_doc_type_probs = c(1, 0.005)
inv_descrip = c("Fuel","Expenses","Printer expenses","Card Fees","Freight Charge-Courier Services","New office chair","Card","Mileage","Trip overseas","Hotel","Flights","Breakfast");inv_descrip_probs = runif(length(inv_descrip))
inv_line_descrip = c("Fuel","Expenses","Printer expenses","Card Fees","Freight Charge-Courier Services","New office chair","Card","Mileage","Trip overseas","Hotel","Flights","Breakfast");inv_line_descrip_probs = runif(length(inv_line_descrip))
inv_comp_codes = c("1000","1005","1010","1020"); inv_comp_codes_probs = c(1,0.8,0.25, 0.4)
inv_cc_codes = c("200000","210000","220000","230000","240000","250000"); inv_cc_codes_probs = c(1,0.8,0.9, 0.1,0.25, 0.15)
inv_gl_code = c("50000","50050","50100","50125","50150","50300","50400","50500","51000","52000","60100","60150","60200","60225","60250");inv_gl_code_probs = runif(length(inv_gl_code))
inv_vendors = VENDOR_ID; inv_vendors_probs = runif(length(inv_vendors))
inv_pay_terms = c(0,7,14,21,28,35,60,90); inv_pay_terms_probs = seq(10,length(inv_pay_terms),-(10-length(inv_pay_terms))/length(inv_pay_terms))[1:length(inv_pay_terms)]
inv_po = c(1,0);inv_po_probs = c(0.6,0.4)
inv_qty = 1:10; inv_qty_probs = seq(max(inv_qty),min(inv_qty),-1 )
inv_gross_amt = c(seq(5,15000, 50), seq(50,25000, 150),seq(500,50000, 500));inv_gross_amt_probs =runif(inv_gross_amt)




INV_ID = seq(100,((n_invs*1)+100),1)[1:n_invs]
INV_STS = sample(inv_status, n_invs,inv_status_probs, replace = T)
DOC_STS = sample(inv_doc_status, n_invs,inv_doc_status_probs, replace = T)
DOC_TYPE = sample(inv_doc_type, n_invs,inv_doc_type_probs, replace = T)
INV_DT = sample(inv_date, n_invs,inv_date_probs, replace = T)
INV_CREATE_DT = INV_DT - sample(inv_create_date, n_invs,inv_create_date_probs, replace = T)
INV_DESC =   sample(inv_descrip, n_invs,inv_descrip_probs, replace = T)
INV_LINE_DESC =   sample(inv_line_descrip, n_invs,inv_line_descrip_probs, replace = T)
INV_LINE_ID = rep(1,n_invs)
INV_EXT_DOC_ID = rep(" ", n_invs)
PRODUCT = sample(n_products, n_invs,n_products_probs, replace = T)
COMP_CODE = sample(inv_comp_codes, n_invs,inv_comp_codes_probs, replace = T)
COST_CNTR = sample(inv_cc_codes, n_invs,inv_cc_codes, replace = T)
GL_CODE = sample(inv_gl_code, n_invs,inv_gl_code_probs, replace = T)
PAYMENT_TERMS = sample(inv_pay_terms, n_invs,inv_pay_terms_probs, replace = T)
DUE_DATE = INV_DT+PAYMENT_TERMS
PAYMENT_TYPE = rep("CARD", n_invs)
INV_SUPPLIER_ID = sample(inv_vendors, n_invs,inv_vendors_probs, replace = T)
INV_PO_ID_T = sample(inv_po, n_invs,inv_po_probs, replace = T);INV_PO_ID = ifelse(INV_PO_ID_T == 1, sample(PO_ID,sum(INV_PO_ID_T)), " " );rm(INV_PO_ID_T)
QTY = sample(inv_qty, n_invs,inv_qty_probs, replace = T)
GROSS_AMT = sample(inv_gross_amt, n_invs,inv_gross_amt_probs, replace = T)
TAX_AMT = GROSS_AMT * tax_rate 
NET_AMT = GROSS_AMT *(1 - tax_rate)
UNIT_PRICE = round(GROSS_AMT/QTY ,2)
UNIT = rep("Amount",n_invs)
n_payments = length(INV_STS[INV_STS == "Paid"])
pay_days = c(seq(1,21,3),seq(1,21,1),seq(1,14,1),seq(1,7,1));pay_days_probs = runif(pay_days)
PO_DT= POs %>% select(INV_PO_ID = PO_ID,ORD_DT) %>% mutate(INV_PO_ID = as.character(INV_PO_ID))
PO_DAYS_DIFF = c(seq(1,21,1),seq(1,15,3),seq(1,20,4),seq(1,7,1),seq(1,7,1));PO_DAYS_DIFF_PROBS = runif(length(PO_DAYS_DIFF))


INVOICES_T = data.frame(INV_ID =INV_ID,INV_STS =INV_STS,DOC_STS =DOC_STS,DOC_TYPE =DOC_TYPE,INV_DT =INV_DT,INV_CREATE_DT =INV_CREATE_DT,INV_DESC =INV_DESC,INV_LINE_DESC =INV_LINE_DESC,
                      INV_LINE_ID =INV_LINE_ID,INV_EXT_DOC_ID =INV_EXT_DOC_ID,PRODUCT =PRODUCT,COMP_CODE =COMP_CODE,COST_CNTR =COST_CNTR,GL_CODE =GL_CODE,PAYMENT_TERMS =PAYMENT_TERMS,
                      DUE_DATE =DUE_DATE,PAYMENT_TYPE =PAYMENT_TYPE,INV_SUPPLIER_ID =INV_SUPPLIER_ID,INV_PO_ID =INV_PO_ID,QTY =QTY,GROSS_AMT =GROSS_AMT,TAX_AMT = TAX_AMT,NET_AMT =NET_AMT,UNIT_PRICE =UNIT_PRICE,UNIT =UNIT) %>% 
                      mutate(days_pay = ifelse(INV_STS == "Paid", sample(pay_days, n_payments,pay_days_probs, replace = T), 0)) %>% left_join(PO_DT) %>% 
                      mutate(INV_DT_ADD = ifelse(is.na(ORD_DT), INV_DT , as.Date(ORD_DT + sample(PO_DAYS_DIFF,n(), PO_DAYS_DIFF_PROBS, replace = T)))) %>% 
                      mutate(INV_DT = as.Date(INV_DT_ADD, origin = "1970-01-01"),PAY_DATE = INV_DT + days_pay) %>% select(-INV_DT_ADD, -ORD_DT,-days_pay) 

head(INVOICES_T)

##########     8) Create tests for exceptions ###############
options(digits=15)
INV_SUMM = INVOICES_T %>%  group_by(INV_ID) %>% 
  summarise(GROSS_AMT = round(sum(GROSS_AMT),2), N_INV = n()) %>% data.frame()

##########     9) Test Layer ###############

#### I_01_DUPE - duplicate invoices ####
I_01_DUPE = INV_SUMM %>% filter(N_INV > 1) %>% select(INV_ID, I_01_DUPE = N_INV)


#### I_02_BLKDVEN - Blocked Vendors ####
INV_VENDOR_SUMM = INVOICES_T %>%  group_by(INV_ID,INV_SUPPLIER_ID ) %>% 
  summarise(GROSS_AMT = round(sum(GROSS_AMT),2), N_INV = n()) %>% data.frame()
VEN_STS = VENDORS %>% select(INV_SUPPLIER_ID  = VENDOR_ID,STATUS)

I_02_BLKDVEN = left_join(INV_VENDOR_SUMM,VEN_STS) %>% filter(STATUS == "Blocked") %>% 
  select(INV_ID, STATUS )


#### I_03_PAYDATE_INVDATE - Blocked Vendors ####
I_03_PAYDATE_INVDATE = INVOICES_T %>% filter(INV_CREATE_DT > INV_DT) %>% select(INV_ID, INV_GA =GROSS_AMT ) 

#### I_04_LRG_INV_WO_PO - Blocked Vendors ####
I_04_LRG_INV_WO_PO = INVOICES_T %>% filter(INV_PO_ID == " " & GROSS_AMT > 15000) %>% select(INV_ID, INV_GA =GROSS_AMT )


#### Consolidate tests ####

INV_TEST_RESULTS =  left_join(INV_SUMM, I_01_DUPE, by = "INV_ID") %>% mutate(I_01_DUPE = ifelse(is.na(I_01_DUPE) ,0,1))  %>% 
  left_join(I_02_BLKDVEN) %>% mutate(I_02_BLKDVEN = ifelse(is.na(STATUS) ,0,1)) %>% select(-STATUS) %>% 
  left_join(I_03_PAYDATE_INVDATE) %>% mutate(I_03_PAYDATE_INVDATE = ifelse(is.na(INV_GA) ,0,1)) %>% select(-INV_GA) %>%
  left_join(I_04_LRG_INV_WO_PO) %>% mutate(I_04_LRG_INV_WO_PO = ifelse(is.na(INV_GA) ,0,1)) %>% select(-INV_GA) %>%
  filter(I_01_DUPE != 0 | I_02_BLKDVEN != 0 | I_03_PAYDATE_INVDATE != 0 | I_04_LRG_INV_WO_PO != 0) %>% 
  select(INV_ID, I_01_DUPE, I_02_BLKDVEN, I_03_PAYDATE_INVDATE, I_04_LRG_INV_WO_PO)

INVOICES =  left_join(INVOICES_T,INV_TEST_RESULTS) %>% 
    mutate(I_01_DUPE = ifelse(is.na(I_01_DUPE),0,I_01_DUPE),I_02_BLKDVEN = ifelse(is.na(I_02_BLKDVEN),0,I_02_BLKDVEN),
           I_03_PAYDATE_INVDATE = ifelse(is.na(I_03_PAYDATE_INVDATE),0,I_03_PAYDATE_INVDATE),I_04_LRG_INV_WO_PO = ifelse(is.na(I_04_LRG_INV_WO_PO),0,I_04_LRG_INV_WO_PO)
      ) 

write.delim(INVOICES, "Invoices.txt", sep = "|")









## Remove vendor tables ##
rm(n_vendors);rm(n_users); rm(n_users_probs); rm(ven_s_date);rm(ven_e_date);rm(v_create_date);rm(v_create_date_probs);rm(v_status); rm(v_status_probs);rm(v_onetimeflag); rm(v_onetimeflag_probs);
rm(v_nopaymentflag); rm(v_nopaymentflag_probs);rm(v_isemployee); rm(v_isemployee_probs);rm(v_cat);rm(v_cat_probs);rm(v_paydays);rm(v_paydays_probs);rm(v_ctry); rm(v_ctry_probs);rm(v_tx_ctry); rm(v_tx_ctry_probs)

## Remove PO tables ##
rm(n_pos);rm(n_names);rm(n_process_fte);rm(po_s_date);rm(po_e_date);rm(po_ord_date);rm(po_ord_date_probs);rm(sts);rm(sts_probs);rm(order_status);rm(order_status_probs);rm(delivery_status);rm(delivery_status_probs);
rm(invoice_status);rm(invoice_status_probs);rm(po_vendors); rm(po_vendors_probs);rm(user_names);rm(user_names_probs);rm(create_names);rm(create_names_probs);rm(n_items); rm(n_items_probs);rm(po_amts);rm(po_amts_probs)

## Remove invoice tables ##
rm(INV_ID);rm(INV_STS);rm(DOC_STS);rm(DOC_TYPE);rm(INV_DT);rm(INV_CREATE_DT);rm(INV_DESC);rm(INV_LINE_DESC);rm(INV_LINE_ID);rm(INV_EXT_DOC_ID);rm(PRODUCT);rm(COMP_CODE);rm(INVOICES_T);
rm(COST_CNTR);rm(GL_CODE);rm(PAYMENT_TERMS);rm(DUE_DATE);rm(PAYMENT_TYPE);rm(INV_SUPPLIER_ID);rm(INV_PO_ID_T);rm(QTY);rm(GROSS_AMT);rm(TAX_AMT);rm(NET_AMT);rm(UNIT_PRICE);rm(UNIT);rm(n_payments);rm(pay_days);rm(I_01_DUPE);rm(I_02_BLKDVEN);rm(I_03_PAYDATE_INVDATE);rm(I_04_LRG_INV_WO_PO)





bqr_auth("rixiye2076@relumyx.com")
project = "publicdatasets2021"
dataset = "publicdata"
billing = "publicdatasets2021"
bq_conn <-  dbConnect(bigquery(), 
                      project = project,
                      dataset = dataset)
Top10Rows = tbl(bq_conn, "CompaniesHouse") %>%select(CompanyName) %>%  data.frame() 



sql_query = "SELECT * FROM `publicdatasets2021.publicdata.PostCodes` limit 20;"
TestQuery = bq_project_query(project, sql_query)
TestDF    = bq_table_download(TestQuery)

### Upload ###
TempTable = bq_table(project = project,  dataset = dataset,table = "SampleTable")
bq_table_upload(TempTable, DataFrame)



path = "https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/986561/UK_sanctions_list.ods"




