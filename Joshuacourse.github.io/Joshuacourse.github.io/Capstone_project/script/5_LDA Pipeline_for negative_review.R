######### Now we got two text-aggregated master tables for NLP 
#### One is about negative reviews, the other is about positive reviews
### The standard of positive/negative reviews is different in terms of restaurant category
### For example, 3.5 stars-Fast food restaurant is good in the standard of fast food 
### However, 3.5 stars is the standard of average Thai food restaurant (the average stars for Thai is 4)
#### The following codes is for negative reviews


## Read in the data
NLP_negative_business_not_filter=read.csv("NLP_negative_business_not_filter.csv",stringsAsFactors = F)  
#### But those businesses that I wouldn't pay attention to
## Those businesses that has average ranking as they should have
Index_remove_negative=which(NLP_negative_business_not_filter$stars_of_business==NLP_negative_business_not_filter$AVG_stars_category_round)
NLP_negative_business_filtered=NLP_negative_business_not_filter[-Index_remove_negative,]

### Let's choose a specific category of restaurant
### The following list is all the restaurant
cuisine_list=c("Afghan",
               "African",
               "American",
               "Arabian",
               "Argentine",
               "Armenian",
               "Asian Fusion",
               "Australian",
               "Austrian",
               "Bangladeshi",
               "Barbeque",
               "Basque",
               "Belgian",
               "Brasseries",
               "Brazilian",
               "Breakfast & Brunch",
               "British",
               "Buffets",
               "Burgers",
               "Burmese",
               "Cafes",
               "Cafeteria",
               "Cajun/Creole",
               "Cambodian",
               "Caribbean",
               "Catalan",
               "Cheesesteaks",
               "Chicken Shop",
               "Chicken Wings",
               "Chinese",
               "Comfort Food",
               "Creperies",
               "Cuban",
               "Czech",
               "Delis",
               "Diners",
               "Dinner Theater",
               "Ethiopian",
               "Fast Food",
               "Filipino",
               "Fish & Chips",
               "Fondue",
               "Food Court",
               "Food Stands",
               "French",
               "Gastropubs",
               "German",
               "Gluten-Free",
               "Greek",
               "Halal",
               "Hawaiian",
               "Himalayan/Nepalese",
               "Hong Kong Style Cafe",
               "Hot Dogs",
               "Hot Pot",
               "Hungarian",
               "Iberian",
               "Indian",
               "Indonesian",
               "Irish",
               "Italian",
               "Japanese",
               "Korean",
               "Kosher",
               "Laotian",
               "Latin American",
               "Live/Raw Food",
               "Malaysian",
               "Mediterranean",
               "Mexican",
               "Middle Eastern",
               "Modern European",
               "Mongolian",
               "Moroccan",
               "New Mexican Cuisine",
               "Nicaraguan",
               "Noodles",
               "Pakistani",
               "Persian/Iranian",
               "Peruvian",
               "Pizza",
               "Polish",
               "Pop-Up Restaurants",
               "Portuguese",
               "Poutineries",
               "Russian",
               "Salad",
               "Sandwiches",
               "Scandinavian",
               "Scottish",
               "Seafood",
               "Singaporean",
               "Slovakian",
               "Soul Food",
               "Soup",
               "Southern",
               "Spanish",
               "Sri Lankan",
               "Steakhouses",
               "Supper Clubs",
               "Sushi Bars",
               "Syrian",
               "Taiwanese",
               "Tapas Bars",
               "Tapas/Small Plates",
               "Tex-Mex",
               "Thai",
               "Turkish",
               "Ukrainian",
               "Uzbek",
               "Vegan",
               "Vegetarian",
               "Vietnamese",
               "Waffles",
               "Bars")

##### Choose a category of Restaurant
NLP_negative_business_filtered_category=subset(NLP_negative_business_filtered,categories=="American")
### See how many restaurants are there for the text mining
nrow(NLP_negative_business_filtered_category)

########## LDA
library(LDAvis)
library(devtools)
library(servr)
library(SnowballC)
library(ngram)
library(tau)
library(stringr)
library(tm)


##### For positive reviews Pre-processing
reviews = NLP_negative_business_filtered_category$text

# read in some stopwords:
stop_words = stopwords("SMART")
# pre-processing:
reviews = gsub("'", "", reviews)  # remove apostrophes
reviews = gsub("[[:punct:]]", " ", reviews)  # replace punctuation with space
reviews = gsub("[[:cntrl:]]", " ", reviews)  # replace control characters with space
reviews = gsub("^[[:space:]]+", "", reviews) # remove whitespace at beginning of documents
reviews = gsub("[[:space:]]+$", "", reviews) # remove whitespace at end of documents
reviews = tolower(reviews)  # force to lowercase
reviews = removeNumbers(reviews)


###ASCII
reviews=remove_stopwords(reviews, stop_words, lines = T)
reviews= stripWhitespace(reviews)
reviews=iconv(reviews,to="ASCII",sub="")
# doc.list = strsplit(reviews, "[[:space:]]+")


#### After removing stopwords_ I need to fill in some words for the algorithm to run

##### Define a new function to count the words
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  str_count(string, pattern)
}
### For those reviews that have less than 3 words, replace it "not very good"
reviews[which(nwords(reviews)<3)]="not very good"
doc.list=list()
for (i in 1:nrow(NLP_negative_business_filtered_category)){
  
  a=ngram::ngram_asweka(reviews[i], min=2, max=3)
  doc.list[[length(doc.list)+1]] =a
}

# compute the table of terms:
term.table = table(unlist(doc.list))
term.table = sort(term.table, decreasing = TRUE)
# remove terms that are stop words or occur fewer than 7 times:
del = names(term.table) %in% stop_words | term.table < 7
term.table = term.table[!del]
vocab = names(term.table)

# now put the documents into the format required by the lda package:
get.terms = function(x) {
  index = match(x, vocab)
  index = index[!is.na(index)]
  rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
}
documents = lapply(doc.list, get.terms)

# Compute some statistics related to the data set:
D = length(documents)  # number of documents
W = length(vocab)  # number of terms in the vocab 
doc.length = sapply(documents, function(x) sum(x[2, ]))  # number of tokens per document 
N = sum(doc.length)  # total number of tokens in the data 
term.frequency = as.integer(term.table)  # frequencies of terms in the corpus

# MCMC and model tuning parameters:
K = 7
G = 5000
alpha = 0.02
eta = 0.02

# Fit the model:
library(lda)
set.seed(357)
t1 = Sys.time()
fit_American_Negative = lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab, 
                                   num.iterations = G, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 = Sys.time()
t2 - t1 

## Visualizing the fitted model with LDAvis
theta = t(apply(fit_American_Negative$document_sums + alpha, 2, function(x) x/sum(x)))
phi = t(apply(t(fit_American_Negative$topics) + eta, 2, function(x) x/sum(x)))


RestaurantReview_American_Negative = list(phi = phi,
                         theta = theta,
                         doc.length = doc.length,
                         vocab = vocab,
                         term.frequency = term.frequency)


library(LDAvis)
RestaurantReview_American_Negative$vocab[136]="Restaurant"
# create the JSON object to feed the visualization:
json_American_Negative = createJSON(phi = RestaurantReview_American_Negative$phi, 
                   theta = RestaurantReview_American_Negative$theta, 
                   doc.length = RestaurantReview_American_Negative$doc.length, 
                   vocab = RestaurantReview_American_Negative$vocab, 
                   term.frequency = RestaurantReview_American_Negative$term.frequency)

serVis(json_American_Negative, out.dir = 'vis', open.browser = T)

save("fit_American_Negative","RestaurantReview_American_Negative","json_American_Negative",file="American_topic_model_negative_7.Rdata")


