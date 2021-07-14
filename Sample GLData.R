##########     1) Load required packages/data         ###############
library(purrr);library(tidyverse);library(magrittr);library(randomNames);library(OpenRepGrid);library(readxl);library(caroline);library(stringr)
setwd("C:/Users/czucche/Documents/R/AMLSandpit")
coa = read_excel("Dummy CoA.xlsx") %>% data.frame()


setwd("C:/Users/czucche/Desktop/Use Case Development/Augmented Restructuring/2) Data")

##########     2) Define parameters         ###############
n_lines = 2500000;set.seed(100)
n_users = 57
n_prods = 15
n_vendors = 27
n_customers = 12
start_date = '2019/01/01'; end_date = '2021/12/31'


##########     2) Define input fields data    ###############
comp_codes      =seq(1000,1020,5)
profit_cntr_codes  =seq(1,657,1);
cost_cntr_codes  =seq(25000,26000,100);
rpt_days_diff = c(0,1,2,3,-10,-30)
users = seq(1,n_users,1); products =  seq(1,n_prods,1); vendors = seq(1,n_vendors,1); 

customers = c("Family first high spenders", "Just for me working young professionals","Empty nest elderlies","Environmentally conscious entreprenuers","Asset rich adults","Budget focused baby boomers",  "Meme focused millenials", "Mission enabled metropolitans","Respendent retirees","Rural retirees", "Income heavy independents","Capital endowed cosmopolitans")
customers = data.frame(CUST_ID = 1:12,CUST_DESC = customers )
write.delim(customers, "CUSTOMER_DETAIL.txt" ,sep = "|")


customers = seq(1,n_customers,1)
doc_types = c("Bank Statement","Cash Disbursement","Cash Receipt","Cash Transfer","Check Deposit","Clearing","Customer Credit Memo","Customer Invoice","Expense Assignment to Sales Documents","External Service Confirmation","Goods Receipt/Invoice Receipt Clearing","Internal Service Confirmation","Journal Entry Voucher","Other Charge/Credit","Revenue Recognition","Settlement","Supplier Credit Memo","Supplier Invoice")
jnl_lines = c(2,3,4,5,6,7,8,9,10)
rpt_curr = c("GBP")
lcl_curr = c("GBP", "USD", "EUR")
jnl_amts = c(seq( 100, 750000,50 ), rep(seq( 100, 7500,50 ),100))
v_GBP_EUR = 1.1634; v_GBP_USD = 1.4180 
gl_codes = coa$GL_CODE


##########     3) Define probabilities for fields    ###############
comp_probs = c( 0.5, 0.35, 0.2,0.1,0.05)
cost_cntr_probs = runif(length(cost_cntr_codes));profit_cntr_code_probs = runif(length(profit_cntr_codes))
rpt_probs = c( 2, 0.5, 0.25,0.1,0.05,0.025)
user_probs = c(runif(length(users)-2),0.0015,0.0015)  ; products_probs = runif(length(products)); vendors_probs = runif(length(vendors)); cust_probs = runif(length(customers))
doc_types_probs = runif(length(doc_types))
jnl_lines_probs = c(1,0.9,0.8,0.5,0.4,0.3,0.05,0.05,0.025)
rpt_curr_probs = 1; lcl_curr_probs = c(1, 0.1,0.1)
jnl_amt_probs = runif(length(jnl_amts))
gl_dbt_probs = coa$DBT; gl_cdt_probs = coa$CDT; 


##########     4) Create fields from input and probs  ###############
jnl_lines_t = sample(jnl_lines, n_lines,jnl_lines_probs, replace = T) # 1
jnl_df_t = data.frame(jnl_lines_t, agg_sum = cumsum(jnl_lines_t), line_index = 1:length(jnl_lines_t)) %>% filter(agg_sum < n_lines) 
n_jnls = nrow(jnl_df_t)
v_desc_length = 2

DOC_TYPE = sample(doc_types, n_jnls,doc_types_probs, replace = T)
POST_DATE = sample(seq(as.Date(start_date), as.Date(end_date), by="day"), n_jnls, replace = T)
RPT_DATE  = POST_DATE + sample(rpt_days_diff, n_jnls,rpt_probs, replace = T)
RPT_CURR = sample(rpt_curr, n_jnls,rpt_curr_probs, replace = T)
LCL_CURR = sample(lcl_curr, n_jnls,lcl_curr_probs, replace = T) 
COMP_CODE = sample(comp_codes, n_jnls,comp_probs, replace = T)
PROFIT_CTR = sample(profit_cntr_codes, n_jnls,profit_cntr_code_probs, replace = T)
COST_CNTR = sample(cost_cntr_codes, n_jnls,cost_cntr_probs, replace = T)
USER_ID   = sample(users, n_jnls,user_probs, replace = T) 
JNL_AMT   = sample(jnl_amts, n_jnls,jnl_amt_probs, replace = T) 
MAN_FLAG  = ifelse(runif(n_jnls) > .68,1,0)
JNL_DESC  = rep(" ", n_jnls)  # randomSentences(n_jnls, v_desc_length)
ADD_FIELD_1 = rep(" ", n_jnls) 
ADD_FIELD_2 = rep(" ", n_jnls) 

##########     5) Create data structure for journals   ###############

journal_id_df = data.frame(jnl_id = as.character(seq(1000000001,(1000000000+nrow(jnl_df_t)),1)),
                           RPT_CURR_T = RPT_CURR, LCL_CURR_T = LCL_CURR,DOC_TYPE_T = DOC_TYPE,POST_DATE_T = POST_DATE,RPT_DATE_T =RPT_DATE,
                           COMP_CODE_T = COMP_CODE,PROFIT_CTR_T =PROFIT_CTR, COST_CNTR_T = COST_CNTR,USER_ID_T = USER_ID, MAN_FLAG_T = MAN_FLAG,JNL_AMT_T = JNL_AMT, JNL_DESC_T = JNL_DESC,
                           ADD_FIELD_1_T = ADD_FIELD_1, ADD_FIELD_2_T = ADD_FIELD_2
                           )
journal_id = data.frame(jnl_id = as.character(seq(1000000001,(1000000000+nrow(jnl_df_t)),1)))%>% split(.$jnl_id)


for(i in 1:nrow(journal_id_df)){ 
  journal_id[[i]] = data.frame(JNL_ID = rep(journal_id_df$jnl_id[i], jnl_df_t$jnl_lines_t[i]),
                               LINE_ID = 1:jnl_df_t$jnl_lines_t[i],
                               
                               
                               RPT_CURR  = rep(journal_id_df$RPT_CURR_T[i], jnl_df_t$jnl_lines_t[i]),LCL_CURR  = rep(journal_id_df$LCL_CURR_T[i], jnl_df_t$jnl_lines_t[i]),
                               DOC_TYPE  = rep(journal_id_df$DOC_TYPE_T[i], jnl_df_t$jnl_lines_t[i]),POST_DATE  = rep(journal_id_df$POST_DATE_T[i], jnl_df_t$jnl_lines_t[i]),
                               RPT_DATE  = rep(journal_id_df$RPT_DATE_T[i], jnl_df_t$jnl_lines_t[i]),COMP_CODE  = rep(journal_id_df$COMP_CODE_T[i], jnl_df_t$jnl_lines_t[i]),JNL_DESC  = rep(journal_id_df$JNL_DESC_T[i], jnl_df_t$jnl_lines_t[i]),
                               COST_CNTR  = rep(journal_id_df$COST_CNTR_T[i], jnl_df_t$jnl_lines_t[i]),USER_ID  = rep(journal_id_df$USER_ID_T[i], jnl_df_t$jnl_lines_t[i]) ,
                               PROFIT_CTR  = rep(journal_id_df$PROFIT_CTR_T[i], jnl_df_t$jnl_lines_t[i]),ADD_FIELD_1  = rep(journal_id_df$ADD_FIELD_1_T[i], jnl_df_t$jnl_lines_t[i]),ADD_FIELD_2  = rep(journal_id_df$ADD_FIELD_2_T[i], jnl_df_t$jnl_lines_t[i]),
                               MAN_FLAG = rep(journal_id_df$MAN_FLAG_T[i], jnl_df_t$jnl_lines_t[i]),   N_LINES = jnl_df_t$jnl_lines_t[i], JNL_AMT = rep(journal_id_df$JNL_AMT_T[i], jnl_df_t$jnl_lines_t[i]),
                               DBT_LINES = sample(jnl_df_t$jnl_lines_t[i],1), DBT_SIDE = round(journal_id_df$JNL_AMT[i]/2,2)
                                                              )
              }

v_round_jnls = 4
gl_temp =  bind_rows(journal_id) %>% mutate(DBT_LINES = ifelse(DBT_LINES == N_LINES, DBT_LINES - 1,DBT_LINES),CDT_LINES = N_LINES - DBT_LINES, 
                                            DBT_DIST = round(runif(length(DBT_LINES))+0.01,v_round_jnls),CDT_DIST = round(runif(length(CDT_LINES))+0.01, v_round_jnls))  %>% 
                                      mutate(DBT_PR = ifelse(LINE_ID <= DBT_LINES, DBT_DIST, 0),CDT_PR = ifelse(LINE_ID > DBT_LINES, CDT_DIST, 0)
                                             ) %>% select(-DBT_DIST,-CDT_DIST)%>% split(.$JNL_ID)

for(i in 1:length(gl_temp)){
  gl_temp[[i]]$RPT_DBT = round(gl_temp[[i]]$DBT_PR/sum(gl_temp[[i]]$DBT_PR)*gl_temp[[i]]$DBT_SIDE,2)
  gl_temp[[i]]$RPT_CDT = round(gl_temp[[i]]$CDT_PR/sum(gl_temp[[i]]$CDT_PR)*gl_temp[[i]]$DBT_SIDE,2)
    }

##########     6) Add lines data into journals and create ###############
##########      other fields for GL detail                ###############

GL_DETAIL =  bind_rows(gl_temp) %>%   select(COMP_CODE, COST_CNTR,PROFIT_CTR,  JNL_ID, LINE_ID,DOC_TYPE, POST_DATE, RPT_DATE,RPT_CURR,LCL_CURR, RPT_DBT,RPT_CDT,JNL_DESC,  USER_ID, MAN_FLAG,ADD_FIELD_1,ADD_FIELD_2) %>% 
              mutate(LCL_DBT = ifelse(LCL_CURR == "EUR", RPT_DBT * v_GBP_EUR, ifelse(LCL_CURR == "USD", RPT_DBT * v_GBP_EUR,RPT_DBT) ),
                     LCL_CDT = ifelse(LCL_CURR == "EUR", RPT_CDT * v_GBP_EUR, ifelse(LCL_CURR == "USD", RPT_CDT * v_GBP_EUR, RPT_CDT) ),LINE_DESC = rep(" ",n()), #randomSentences(n(), v_desc_length),  
                     CUST_ID   = sample(customers, n(),cust_probs, replace = T), VEND_ID   = sample(vendors, n(),vendors_probs, replace = T), PRODUCT_ID   = sample(products, n(),products_probs, replace = T),
                     GL_CODE = ifelse(RPT_DBT > 0, sample(gl_codes, n(), gl_dbt_probs, replace = T),sample(gl_codes,n(), gl_cdt_probs, replace = T))) %>% 
              mutate(GL_CODE = format(GL_CODE, scientific = FALSE)) %>%
              filter(as.numeric(GL_CODE) >= 400000)  ### use logic if doing restructuring demo data

##########     7) Export data for dashboard  ###############

write.delim(GL_DETAIL, "GL_DETAIL.txt", sep = "|")

##########     8) Create tests for exceptions ###############
options(digits=15)
JNL_SUMM = GL_DETAIL %>%  group_by(JNL_ID, RPT_DATE, POST_DATE,USER_ID) %>% 
  summarise(JNL_AMT = round(sum(RPT_DBT),2)) %>% data.frame()

JNL_CONSOL = GL_DETAIL %>%  group_by(JNL_ID) %>% 
  summarise(JNL_AMT = round(sum(RPT_DBT),2)) %>% data.frame()

##########     9) Test Layer ###############
 
#### T_01_RS - round sum journals ####
T_01_RS = JNL_SUMM %>% select(-RPT_DATE, -POST_DATE, -USER_ID) %>%   mutate(JNL_AMT = format(JNL_AMT, nsmall = 2)) %>% 
  filter(str_sub(JNL_AMT,-6,-1 ) %in% c("000.00","999.00","999.00")) %>% rename( T_01_IS_RS = JNL_AMT)


#### T_02_BDPs - back posted jnls ####
T_02_BDPs = JNL_SUMM %>% mutate(POST_DIFF = as.numeric(RPT_DATE - POST_DATE)) %>% 
  filter(POST_DIFF <= -30) %>% select(JNL_ID, T_02_IS_BDPs = POST_DIFF )


#### T_03_LPUs - low posting users ####
LOW_USER_POSTS = GL_DETAIL %>%  group_by(USER_ID) %>% summarise(N_POSTS = n_distinct(JNL_ID)) %>% data.frame() %>% 
        filter(N_POSTS < 25) 

T_03_LPUs = JNL_SUMM %>% filter(USER_ID %in% LOW_USER_POSTS$USER_ID) %>% 
        select(JNL_ID,  T_03_IS_LPUs = USER_ID ); rm(LOW_USER_POSTS)

#### T_04_MRJs - manual journals posted to revenue accounts ####
T_04_MRJs =  GL_DETAIL %>% filter(MAN_FLAG == 1 & str_sub(GL_CODE,1,1) == "4") %>% select(JNL_ID, T_04_IS_MREV = GL_CODE) %>% distinct()

#### Consolidate tests ####

TEST_RESULTS =  left_join(JNL_CONSOL, T_01_RS, by = "JNL_ID") %>% mutate(T_01_IS_RS = ifelse(is.na(T_01_IS_RS) ,0,1)) %>% 
                  left_join(T_02_BDPs) %>% mutate(T_02_IS_BDPs = ifelse(is.na(T_02_IS_BDPs) ,0,1)) %>% 
                  left_join(T_03_LPUs) %>% mutate(T_03_IS_LPUs = ifelse(is.na(T_03_IS_LPUs) ,0,1)) %>%
                  left_join(T_04_MRJs) %>% mutate(T_04_IS_MREV = ifelse(is.na(T_04_IS_MREV) ,0,1)) %>% 
                  filter(T_01_IS_RS != 0 | T_02_IS_BDPs != 0 | T_03_IS_LPUs != 0 | T_04_IS_MREV != 0) %>% 
                  select(JNL_ID,T_01_IS_RS, T_02_IS_BDPs, T_03_IS_LPUs, T_04_IS_MREV)

write.delim(TEST_RESULTS, "TEST_RESULTS.txt", sep = "|")


##########     10) Completeness tests ###############

#### Unbalanced journals ####

UNBAL_JNLS = GL_DETAIL %>%  group_by(JNL_ID) %>% 
  summarise(JNL_AMT = round(sum(RPT_DBT - RPT_CDT),2)) %>% data.frame() %>% 
  filter(JNL_AMT != 0.00) %>% mutate(IS_UNBAL = 1) %>% select(-JNL_AMT)

write.delim(UNBAL_JNLS, "UNBAL_JNLS.txt", sep = "|")




##########     11) Store data ###############
### Get UK Postcodes - txt file already produced, no need to run twice ###
UKPostCodes = tbl(bq_conn, "PostCodes") %>% filter(In_Use_ == TRUE & Latitude != 0 & County != "") %>% 
  select(Postcode, Latitude, Longitude, County, District, Ward, Country, Constituency, Rural_urban, Region, Local_authority,Index_of_Multiple_Deprivation  ) %>%  data.frame() 
write.delim(UKPostCodes, "UKPostCodes.txt", sep = "|" )




### Read UK Postcodes ###
UKPostCodes = read_delim("UKPostCodes.txt", delim = "|") %>% data.frame()

set.seed(1234)
n_stores = length(profit_cntr_codes)
square_ft = c(seq(6000, 13500,5),seq(6000, 8500,1));square_ft_probs = runif(length(square_ft))
st_srt_t1 ='2000/01/01';st_srt_t2 ='2010/01/01';st_end_dt = '2021/12/31'; ls_ear_dt = '2022/06/30'  ; ls_end_dt = '2026/12/31'
lease_duration = seq(as.Date(ls_ear_dt), as.Date(ls_end_dt), by = "month");lease_duration_probs = runif(length(lease_duration))
desc = c("High Street", "Shopping Center","Freehold shop"); desc_probs = c(0.3,0.25,0.35)

STORE_ID =  profit_cntr_codes
AREA = UKPostCodes[sample(1:nrow(UKPostCodes),length(STORE_ID),replace = T ),]
OPEN_DATE = sample(c(seq(as.Date(st_srt_t1), as.Date(st_end_dt), 5),seq(as.Date(st_srt_t2), as.Date(st_end_dt), 2)), n_stores, replace = T)
LEASE_END_DATE = sample(lease_duration, n_stores, replace = T)
SQ_FT = sample(square_ft, n_stores, square_ft_probs, replace = T)
STORE_TYPE = sample(desc, n_stores, desc_probs, replace = T)

STORE_DETAIL = data.frame( STORE_ID= STORE_ID,STORE_TYPE = STORE_TYPE, AREA, OPEN_DATE = OPEN_DATE,LEASE_END_DATE = LEASE_END_DATE, SQ_FT = SQ_FT, REGION_COUNTRY = paste0(AREA$County, ", ", AREA$Country ))

write.delim(STORE_DETAIL, "STORE_DETAIL.txt", sep = "|" )




