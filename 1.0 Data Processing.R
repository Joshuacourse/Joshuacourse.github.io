

kable(head(transaction_cleaned)) %>% kable_styling()


library(readr) # Read Rectangular Text Data
library(dplyr) # A Grammar of Data Manipulation
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(tidyr) # Tidy Messy Data
library(lubridate) # Make Dealing with Dates a Little Easier
library(kableExtra) # Construct Complex Table with 'kable' and Pipe Syntax
library(factoextra) # Extract and Visualize the Results of Multivariate Data Analyses
library(FactoMineR) # Multivariate Exploratory Data Analysis and Data Mining
library(gridExtra) # Miscellaneous Functions for "Grid" Graphics
library(float)    # 32-Bit Floats
library(recommenderlab) # Lab for Developing and Testing Recommender Algorithms

# 【1】Read Raw Data
app <- read_csv("app_dat.csv")  
category_ref <- read_csv("category_ref.csv")
device_ref <- read_csv("device_ref.csv")
in_app <- read_csv("in-app_dat.csv")
transaction <- read_csv("transaction_dat.csv")
account <- read.csv("account_dat.csv")

# 【2】Data Cleaning

# (1)Transaction Table: Remove duplicates in the transaction tables
transaction_cleaned <- transaction %>% 
  distinct() %>% 
  rename("transaction_date"=create_dt) %>% 
  mutate(device_id=as.character(device_id))
remove(transaction)

# (2) App Table & In App Table:  Remove wrongly label app_name ("#NAME?") 
# Create parent_app_content_id and type columns so it could be combined with in_app table
app_cleaned <- app %>% 
  filter(app_name!="#NAME?") %>% 
  mutate(type=NA,
         parent_app_content_id=content_id,
         device_id=as.character(device_id)) 

app_and_in_app_cleaned <- in_app %>% 
  left_join(app_cleaned,by=c("parent_app_content_id"="content_id")) %>% 
  rename("type"=type.x) %>% 
  select(names(app_cleaned)) %>% 
  rbind.data.frame(app_cleaned)
remove(app)
remove(app_cleaned)
remove(in_app)



# (3) Account Table: Fix the issue of mixed date format
account_cleaned <- account %>% 
  mutate(helper=as.character(map(create_dt,~str_split(.x,"/")[[1]][1]))) %>% 
  mutate(account_creation_date=if_else(
    str_length(helper)==4, ymd(create_dt), mdy(create_dt)
  )) %>% 
  select(-create_dt,-helper)
remove(account)

# (4) Device Table
device_ref_cleaned <- device_ref %>% 
  mutate(device_id=as.character(device_id))
remove(device_ref)



#【3】Join the fact tables with dimension tables to create a transaction master table for downstream analysis
# Filter out transactions without account information
# Filter out transactions without app information
# Filter out transactions where account_creation_date > transaction_date

transaction_master_table <- transaction_cleaned %>% 
  left_join(account_cleaned,by=c("acct_id"="acct_id")) %>% 
  left_join(app_and_in_app_cleaned,by=c("content_id"="content_id")) %>% 
  rename("device_id_from_transaction"=device_id.x,"device_id_from_app"=device_id.y) %>% 
  left_join(device_ref_cleaned,by=c("device_id_from_transaction"="device_id")) %>% 
  left_join(category_ref,by=c("category_id"="category_id")) %>% 
  filter(!is.na(account_creation_date) ) %>% 
  filter(!is.na(app_name)) %>% 
  filter(account_creation_date<=transaction_date) 

remove(account_cleaned)
remove(app_and_in_app_cleaned)
remove(category_ref)
remove(device_ref_cleaned)
remove(transaction_cleaned)

# write_csv(transaction_master_table,"transaction_master_table.csv")


#【4】Add a column called "revenue_model" to the transaction_master_table (transaction level)
# (1) If an app never charge users, it's a free app
# (2) If an app charge users only when user download it, its a paid app
# (3) If an app charge users in-app by consumable, its a freemium consumable app 
# (4) If an app charge users in-app by subscription, its a freemium subscription app 

app_with_revenue_model <- transaction_master_table %>% 
  group_by(app_name,category_name,type) %>% 
  summarise(total_sales=sum(price)) %>% 
  mutate(revenue_model=
           case_when(
             is.na(type)  & total_sales==0  ~ "free app",
             is.na(type) & total_sales>0 ~ "paid app",
             type=="consumable" ~ "freemium consumable app",
             type=="subscription" ~ "freemium subscription app "
             
           )
  ) %>% 
  arrange(app_name,category_name,type,desc(total_sales)) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  select(app_name,revenue_model)

transaction_master_table <- transaction_master_table %>% 
  left_join(app_with_revenue_model,by=c("app_name"="app_name"))

remove(app_with_revenue_model)


# 【5】 # 2016-08-22， 2016-08-23， 2016-08-24 were missings
transaction_date_diagnosis <- transaction_master_table %>%
  group_by(transaction_date) %>%
  summarise(total_price=sum(price)) %>%
  arrange(transaction_date) %>%
  mutate(helper=lag(transaction_date)) %>%
  filter(transaction_date-helper!=1)
remove(transaction_date_diagnosis)


#【6】Summary: in the downstream analysis

sapply(transaction_master_table, function(x){length(unique(x))})

# (1) There are 5 categories of app. 
# (2) There are 2 devices. 
# (3) There are 996 apps categorized into 4 types. 
# (4) There are  28456 accounts 
# (5) There are 2 payment types. 
# (6) There are 110 days of data (2016-06-01 to 2016-09-21)
# (7) There are 3454546 transactions (purchase + download)



# 【7】split transaction_master_table into downloads and purchases

download_master_table <- transaction_master_table %>% 
  filter(price==0)

purchase_master_table <- transaction_master_table %>% 
  filter(price>0)

remove(transaction_master_table)
