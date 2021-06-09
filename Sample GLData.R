library(purrr);library(tidyverse);library(magrittr);library(randomNames);library(OpenRepGrid)


randomNames(5, which.names="both",name.order="first.last", name.sep = " ")

##### create GL data #####

n_lines = 50000;set.seed(100)
n_users = 12
n_prods = 15
n_vendors = 27
n_customers = 863
start_date = '2020/01/01'; end_date = '2021/12/31'


#### Input data      ####  
comp_codes      =seq(1000,1020,5)
cost_cntr_codes  =seq(25000,26000,100);
rpt_days_diff = c(0,1,2,3,-10,-30)
users = seq(1,n_users,1); products =  seq(1,n_prods,1); vendors = seq(1,n_vendors,1); customers = seq(1,n_customers,1)
doc_types = c("Bank Statement","Cash Disbursement","Cash Receipt","Cash Transfer","Check Deposit","Clearing","Customer Credit Memo","Customer Invoice","Expense Assignment to Sales Documents","External Service Confirmation","Goods Receipt/Invoice Receipt Clearing","Internal Service Confirmation","Journal Entry Voucher","Other Charge/Credit","Revenue Recognition","Settlement","Supplier Credit Memo","Supplier Invoice")
jnl_lines = c(2,3,4,5,6,7,8,9,10)
rpt_curr = c("GBP")
lcl_curr = c("GBP", "USD", "EUR")
jnl_amts = c(seq( 100, 750000,50 ), rep(seq( 100, 7500,50 ),100))


#### Input probs     ####  
comp_probs = c( 0.5, 0.35, 0.2,0.1,0.05)
cost_cntr_probs = runif(length(cost_cntr_codes))
rpt_probs = c( 2, 0.5, 0.25,0.1,0.05,0.025)
user_probs = runif(length(users)); products_probs = runif(length(products)); vendors_probs = runif(length(vendors)); cust_probs = runif(length(customers))
doc_types_probs = runif(length(doc_types))
jnl_lines_probs = c(1,0.9,0.8,0.5,0.4,0.3,0.05,0.05,0.025)
rpt_curr_probs = 1; lcl_curr_probs = c(1, 0.1,0.1)
jnl_amt_probs = runif(length(jnl_amts))

#### Intermediary calcs     ####  
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
COST_CNTR = sample(cost_cntr_codes, n_jnls,cost_cntr_probs, replace = T) 
USER_ID   = sample(users, n_jnls,user_probs, replace = T) 
JNL_AMT   = sample(jnl_amts, n_jnls,jnl_amt_probs, replace = T) 
MAN_FLAG  = ifelse(runif(n_jnls) > .68,1,0)
JNL_DESC  = randomSentences(n_jnls, v_desc_length)


journal_id_df = data.frame(jnl_id = as.character(seq(1000000001,(1000000000+nrow(jnl_df_t)),1)),
                           RPT_CURR_T = RPT_CURR, LCL_CURR_T = LCL_CURR,DOC_TYPE_T = DOC_TYPE,POST_DATE_T = POST_DATE,RPT_DATE_T =RPT_DATE,
                           COMP_CODE_T = COMP_CODE,COST_CNTR_T = COST_CNTR,USER_ID_T = USER_ID, MAN_FLAG_T = MAN_FLAG,JNL_AMT_T = JNL_AMT, JNL_DESC_T = JNL_DESC
                           )
journal_id = data.frame(jnl_id = as.character(seq(1000000001,(1000000000+nrow(jnl_df_t)),1)))%>% split(.$jnl_id)


for(i in 1:nrow(journal_id_df)){ 
  journal_id[[i]] = data.frame(JNL_ID = rep(journal_id_df$jnl_id[i], jnl_df_t$jnl_lines_t[i]),
                               LINE_ID = 1:jnl_df_t$jnl_lines_t[i],
                               RPT_CURR  = rep(journal_id_df$RPT_CURR_T[i], jnl_df_t$jnl_lines_t[i]),LCL_CURR  = rep(journal_id_df$LCL_CURR_T[i], jnl_df_t$jnl_lines_t[i]),
                               DOC_TYPE  = rep(journal_id_df$DOC_TYPE_T[i], jnl_df_t$jnl_lines_t[i]),POST_DATE  = rep(journal_id_df$POST_DATE_T[i], jnl_df_t$jnl_lines_t[i]),
                               RPT_DATE  = rep(journal_id_df$RPT_DATE_T[i], jnl_df_t$jnl_lines_t[i]),COMP_CODE  = rep(journal_id_df$COMP_CODE_T[i], jnl_df_t$jnl_lines_t[i]),JNL_DESC  = rep(journal_id_df$JNL_DESC_T[i], jnl_df_t$jnl_lines_t[i]),
                               COST_CNTR  = rep(journal_id_df$COST_CNTR_T[i], jnl_df_t$jnl_lines_t[i]),USER_ID  = rep(journal_id_df$USER_ID_T[i], jnl_df_t$jnl_lines_t[i]) ,
                               MAN_FLAG = rep(journal_id_df$MAN_FLAG_T[i], jnl_df_t$jnl_lines_t[i]),   N_LINES = jnl_df_t$jnl_lines_t[i], JNL_AMT = rep(journal_id_df$JNL_AMT_T[i], jnl_df_t$jnl_lines_t[i]),
                               DBT_LINES = sample(jnl_df_t$jnl_lines_t[i],1), DBT_SIDE = round(journal_id_df$JNL_AMT[i]/2,2)
                                                              )
              }

v_round_jnls = 1
gl_temp =  bind_rows(journal_id) %>% mutate(DBT_LINES = ifelse(DBT_LINES == N_LINES, DBT_LINES - 1,DBT_LINES),CDT_LINES = N_LINES - DBT_LINES, 
                                            DBT_DIST = round(runif(length(DBT_LINES)),v_round_jnls),CDT_DIST = round(runif(length(CDT_LINES)),v_round_jnls))  %>% 
                                      mutate(DBT_PR = ifelse(LINE_ID <= DBT_LINES, DBT_DIST, 0),CDT_PR = ifelse(LINE_ID > DBT_LINES, CDT_DIST, 0)
                                             ) %>% select(-DBT_DIST,-CDT_DIST)%>% split(.$JNL_ID)

for(i in 1:length(gl_temp)){
  gl_temp[[i]]$RPT_DBT = round(gl_temp[[i]]$DBT_PR/sum(gl_temp[[i]]$DBT_PR)*gl_temp[[i]]$DBT_SIDE,2)
  gl_temp[[i]]$RPT_CDT = round(gl_temp[[i]]$CDT_PR/sum(gl_temp[[i]]$CDT_PR)*gl_temp[[i]]$DBT_SIDE,2)
    }
    
gl_temp_2 =  bind_rows(gl_temp) %>% select(COMP_CODE, COST_CNTR, JNL_ID, LINE_ID,DOC_TYPE, POST_DATE, 
                                        RPT_DATE, RPT_DBT,RPT_CDT,JNL_DESC,  USER_ID, MAN_FLAG) %>% mutate(LINE_DESC = randomSentences(n(), v_desc_length) )





#### Create data    ####  
CUST_ID   = sample(customers, nrow(gl_temp),cust_probs, replace = T) # 18
VEND_ID   = sample(vendors, nrow(gl_temp),vendors_probs, replace = T) # 19
PRODUCT_ID   = sample(products, nrow(gl_temp),products_probs, replace = T) # 20











