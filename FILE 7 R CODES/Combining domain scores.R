#####################################################  THE BELGIAN INDEX OF MULTIPLE DEPRIVATION ###################################################################

# combination of indicators 
# possible to combine the domain scores without the health domain 

# THE BELGIAN INDEX OF MULTIPLE DEPRIVATION 2001 
bimd2001 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%201%20BIMD2001%20DOMAINS%20(SCORE%2C%20RANKS%2C%20DECILES)/bimd2001_domains_score_ranks_deciles.csv")


bimd2001$BIMD2001_score <- bimd2001$exp_income_score_2001*0.20 + bimd2001$exp_employment_score_2001*0.20 + bimd2001$exp_education_score_2001*0.25 + bimd2001$exp_housing_score_2001*0.15 +
  bimd2001$exp_crime_score_2001*0.05 + bimd2001$exp_health_score_2001*0.15

bimd2001$BIMD2001_rank <- rank(-bimd2001$BIMD2001_score , na.last = 'NA', ties.method = "random") 

bimd2001$BIMD2001_deciles <- decile(vector=bimd2001$BIMD2001_rank, decreasing = F) 

write.table(bimd2001, "file1", quote = F, sep ="\t", row.names = F, col.names = T)




# THE BELGIAN INDEX OF MULTIPLE DEPRIVATION 2011 


bimd2011 <-read.csv("https://raw.githubusercontent.com/bimd-project/Belgian-Indices-of-Multiple-Deprivation/main/FILE%202%20BIMD2011%20DOMAINS%20(SCORES%2C%20RANKS%2C%20DECILES)/bimd2011_domains_score_ranks_deciles.csv")


bimd2011$BIMD2011_score <- bimd2011$exp_income_score_2011*0.20 + bimd2011$exp_employment_score_2011*0.20 + bimd2011$exp_education_score_2011*0.25 + bimd2011$exp_housing_score_2011*0.15 +
  bimd2011$exp_crime_score_2011*0.05 + bimd2011$exp_health_score_2011*0.15

bimd2011$BIMD2011_rank <- rank(-bimd2011$BIMD2011_score , na.last = 'NA', ties.method = "random") 

bimd2011$BIMD2011_deciles <- decile(vector=bimd2011$BIMD2011_rank, decreasing = F) 

write.table(bimd2011, "file", quote = F, sep ="\t", row.names = F, col.names = T)

