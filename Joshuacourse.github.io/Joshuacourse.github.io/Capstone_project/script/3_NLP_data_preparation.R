##### Now we are starting from NLP_table

### We need to get a average stars of each of the categories
library(dplyr)
load("NLP.RData")
Intermediate_table=NLP_table %>%
  group_by(categories) %>%
  summarise(AVG_stars_category=mean(stars_of_business))
#### round to nearest half(0.5)
Intermediate_table$AVG_stars_category_round=round(Intermediate_table$AVG_stars_category/0.5)*0.5
Revise_NLP_table=NLP_table %>%
  left_join(Intermediate_table,by=c("categories"="categories"))

#### Get the Index of self-defined positive reviews
Index_Positive=which(Revise_NLP_table$stars_of_individual_review>Revise_NLP_table$AVG_stars_category_round)
Revise_NLP_table_positive=Revise_NLP_table[Index_Positive,]
#### Get the Index of self-defined negative reviews
Index_Negative=which(Revise_NLP_table$stars_of_individual_review<Revise_NLP_table$AVG_stars_category_round)
Revise_NLP_table_negative=Revise_NLP_table[Index_Negative,]


##### Strings Contatenation(concatenation collapse by business_id)
Revise_NLP_table_positive=Revise_NLP_table_positive[,-which(colnames(Revise_NLP_table_positive)=="stars_of_individual_review")]
NLP_positive_business_not_filter=Revise_NLP_table_positive %>%
    group_by(business_id,categories,stars_of_business,AVG_stars_category,AVG_stars_category_round) %>%
    summarise(text=paste(text,collapse = ''),name=first(name))
write.csv(NLP_positive_business_not_filter,"NLP_positive_business_not_filter.csv")


Revise_NLP_table_negative=Revise_NLP_table_negative[,-which(colnames(Revise_NLP_table_negative)=="stars_of_individual_review")]
NLP_negative_business_not_filter=Revise_NLP_table_negative %>%
  group_by(business_id,categories,stars_of_business,AVG_stars_category,AVG_stars_category_round) %>%
  summarise(text=paste(text,collapse = ''),name=first(name))
write.csv(NLP_negative_business_not_filter,"NLP_negative_business_not_filter.csv")
