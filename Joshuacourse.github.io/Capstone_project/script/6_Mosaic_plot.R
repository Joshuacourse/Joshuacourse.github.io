#### Mosaic plot for Price Range and stars_of_business
Final_table_for_predition=read.csv("Final_table_for_predition_done.csv",stringsAsFactors = F)
chisq.test(Final_table_for_predition$stars_of_business,Final_table_for_predition$attributes.Price.Range)
mosaicplot(table(Final_table_for_predition$stars_of_business,Final_table_for_predition$attributes.Price.Range),shade = T)

#### Mosaic plot for Drive.Thru and stars_of_business
mosaicplot(table(Final_table_for_predition$stars_of_business,Final_table_for_predition$attributes.Drive.Thru),shade = T)

#### Mosaic plot for Noise.Level and stars_of_business
Table_for_noise=subset(Final_table_for_predition,!Final_table_for_predition$attributes.Noise.Level=="")
index=which(Table_for_noise$attributes.Noise.Level=="very_loud")
Table_for_noise$attributes.Noise.Level[index]="loud"
Table_for_noise$attributes.Noise.Level <- factor(Table_for_noise$attributes.Noise.Level, levels = c("loud", "average", "quiet"))
mosaicplot(table(Table_for_noise$stars_of_business,Table_for_noise$attributes.Noise.Level),shade = T)

