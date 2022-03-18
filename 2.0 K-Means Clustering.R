
# 【1】Prepare Data for the K-Means Clustering Algorithm
#  use the customer’s spending behavior, their products of interest and some basic 
# information about their activity to perform segmentation.


# (1)  customers' spending behavior
current_date <- max(purchase_master_table$transaction_date)

customer_spending_behavior <- purchase_master_table %>%
  group_by(acct_id) %>%
  summarise(n_purchase = n(),
            min_purchase = min(price),
            avg_purchase = mean(price),
            max_purchase = max(price),
            total_purchase = sum(price),
            first_purchase_date = min(transaction_date),
            last_purchase_date = max(transaction_date)
  ) %>%
  mutate(days_since_first_purchase = as.integer(current_date - first_purchase_date),
         days_since_last_purchase = as.integer(current_date - last_purchase_date)) %>% 
  select(-first_purchase_date,-last_purchase_date)

# (2) customers' product interest 
customer_product_interest = purchase_master_table %>%
  group_by(acct_id) %>%
  spread(category_name, price, fill = 0, convert = TRUE) %>%
  dplyr::select(-transaction_date,-content_id,-device_id_from_transaction,-payment_type,
                -account_creation_date,-app_name,-category_id,-device_id_from_app,
                -type,-parent_app_content_id,-device_name,-revenue_model) %>%
  group_by(acct_id) %>% summarise_all(.funs = sum)



# (3) join the two tables and remove acct_id
customer_order_summary = customer_spending_behavior %>% left_join(customer_product_interest,
                                                                  by=c("acct_id"="acct_id")) 
account_id_cached <- customer_order_summary$acct_id

customer_order_summary <- customer_order_summary %>% 
  select(-acct_id)

remove(customer_product_interest)
remove(customer_spending_behavior)
remove(purchase_master_table)




# 【2】Determine the number of clusters
# (1) scale the data
scaled_cutomer_order_summary = as.data.frame(scale(customer_order_summary))
# (2) Find optimal number of clusters for k-means
fviz_nbclust(scaled_cutomer_order_summary, kmeans, method='silhouette')

# As you can see above, the optimal number of clusters is 2 hands-down. So let’s choose k=2.


# 【3】K-Means Clustering
set.seed(123)
# 3. Compute k-means
km_model = kmeans(scaled_cutomer_order_summary, 2, nstart = 25)

customer_order_summary$Cluster = km_model$cluster

# 【4】verify if the clusters were extracted correctly

pca <- PCA(scaled_cutomer_order_summary,  graph = FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 50))


fviz_cluster(km_model, data = scaled_cutomer_order_summary,
             axes = c(1,2),
             geom = "point",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot Dim1 vs. Dim2")



# 【5】detect which indicators along 40 variables distinguish our customers



cluster_diff = customer_order_summary %>% group_by(Cluster) %>%
  summarise('Number of Customers' = n(),
            'Recency Mean (Day)' = round(mean(days_since_last_purchase),2),
            'Frequency Mean' = scales::comma(round(mean(n_purchase))),
            'Monetary Value Mean ($)' = scales::comma(round(mean(total_purchase))),
            'Cluster Revenue' = scales::comma(sum(total_purchase)))

kable(cluster_diff, caption = " Diffreence between the two clusters") %>% kable_styling()



# 【6】RFM Analysis
customer_order_summary$Cluster = as.factor(customer_order_summary$Cluster)

r = customer_order_summary %>%
  ggplot(aes(x = Cluster, y = days_since_last_purchase, fill = Cluster)) +
  geom_boxplot(fill = c("steelblue1", "gold3")) +
  labs(x = "Cluster", y = "Number of Days",
       title = "Recency: Distribution of Days since Last Order") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()


f = customer_order_summary %>%
  ggplot(aes(x = Cluster, y = n_purchase, fill = Cluster)) +
  geom_boxplot(fill = c("steelblue1", "gold3")) +
  labs(x = "Cluster", y = "Number of Transactions",
       title = "Frequency: Distribution of Transactions") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()


m = customer_order_summary %>%
  ggplot(aes(x = Cluster, y = total_purchase, fill = Cluster)) +
  geom_boxplot(fill = c("steelblue1", "gold3")) +
  labs(x = "Cluster", y = "Order Value (£)",
       title = "Monetary: Distribution of Order Value") +
  scale_fill_brewer(palette="RdBu") + theme_minimal()

grid.arrange(r, f, m, nrow = 3)
remove(r,f,m)
            

# 【7】 Products of Interest within each Cluster

product_stats_cluster =  customer_order_summary %>%
  dplyr::select( -n_purchase, -min_purchase, -avg_purchase,
                 -max_purchase, -total_purchase, -days_since_first_purchase, -days_since_last_purchase)

product_stats_cluster = 
  product_stats_cluster %>% gather(key = "ProductCategory", value = "BasketValue", -Cluster) 

product_stats_cluster %>% 
  filter(ProductCategory %in% c("Entertainment", "Games", 
                                "Photos & Videos", "Social Networking",
                                "Utilities")) %>%
  ggplot(aes(x = ProductCategory, BasketValue)) +
  stat_summary(fun.y=sum,geom="bar",fill="#CC6666",colour="black") +
  labs(x = "Product Category", y = "Sales Revenue (£)",
       title = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Cluster, scales = "free") 



