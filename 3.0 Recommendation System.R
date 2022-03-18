

# rm(list = ls(all.names = TRUE))


# 【1】Prepare for the data
apps_matrix <- purchase_master_table %>% 
  group_by(acct_id,app_name) %>% 
  summarise(if_purchased=n()) %>% 
  ungroup() %>% 
  spread(app_name, if_purchased)




# 【2】Split the dataset into training set (80%) and testing set (20%):
set.seed(80)
train_set <- sample(x = c(TRUE, FALSE), size = nrow(apps_matrix), replace = TRUE, 
                    prob = c(0.8, 0.2))

account_id <- apps_matrix$acct_id  
apps_matrix <- apps_matrix %>% select(-acct_id)
rownames(apps_matrix) <- account_id

Train_account_id <- account_id[train_set] # 12034
Test_account_id <- account_id[!train_set] # 2959


apps_matrix <- as(as.matrix(apps_matrix), "realRatingMatrix")
apps_Train <- apps_matrix[train_set, ] 
# 12025 x 772
apps_Test <- apps_matrix[!train_set, ]
# 2957 x 772


# 【3】Training the model
recommender <- Recommender(apps_Train, method = "IBCF", parameter = list(k = 30))



# 【4】Top Apps based on co-purchase pattern
similarityMatrix <- getModel(recommender)$sim
# similarity <- as.matrix(recommender@model$sim)
which_max <- order(colSums(similarityMatrix > 0), decreasing = TRUE)[1:10]
topApps <- as.data.frame((rownames(similarityMatrix)[which_max]))
colnames(topApps) ="Top Apps"


kable((topApps)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = T, color = "white", background = "#fc5e5e") %>%
  scroll_box(width = "100%", height = "600px")


# 【5】Recommendations using test set
pred <- predict(recommender, newdata = apps_Test, n = 6)


prediction_table<- as(pred,"list") %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column( "account_id") %>% 
  rename("app1"=V1,
         "app2"=V2,
         "app3"=V3,
         "app4"=V4,
         "app5"=V5,
         "app6"=V6,
  ) 
kable((prediction_table)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(0, bold = T, color = "white", background = "#fc5e5e") %>%
  scroll_box(width = "100%", height = "800px")

