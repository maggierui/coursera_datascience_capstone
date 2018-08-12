
library(data.table)
library(DT)
library(sqldf)
library(ngram)
library(stringr)
library(RTextTools)

#### Function to create ngram tables, including the ngram, the frequency, and each word in the ngram, for each ngram.
create_ngram_table<-function(ngram,n){
  temp_ngram<-ngram[[n]]
  words_split<-strsplit(temp_ngram$ngram,"_")
  length(words_split)
  phrase_lengths<-table(lengths(words_split))
  while(length(phrase_lengths)>1){
    i<-1
    while(i<=length(words_split)){
      if (length(words_split[[i]])>n){
        words_split<-words_split[-i]
        temp_ngram<-temp_ngram[-i]
      }
      i<-i+1
    }
    phrase_lengths<-table(lengths(words_split))
  }
  
  words_table<-do.call(rbind,words_split)
  words_table<-as.data.table(words_table)
  word_ngram_table<-cbind(temp_ngram,words_table)
  return(word_ngram_table)
}

#### Creating searchable tables with ngram, the frequency, and each word in the ngram.
blog_qua_table<-create_ngram_table(data_blog,4)
blog_tri_table<-create_ngram_table(data_blog,3)
blog_bi_table<-create_ngram_table(data_blog,2)
blog_uni_table<-create_ngram_table(data_blog,1)


#### Type in a phrase
typed<-"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
n<-wordcount(typed)
typed_word<-vector(mode="character",length=n)
for (i in 1:n)
  typed_word[i]<-word(typed,i)
typed_word<-wordStem(typed_word)

#### Function to search in bigram 
search_bigram<-function(bi,uni,input){
  matching_dt<-bi[V1==input[1]] 
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1:50,V2]
    print("used bigram to predict")
  }else {
    sorted_dt<-uni[order(-frequency)]
    predicted_word<-sorted_dt[1:50,V1]
    print("used unigram to predict")
  }
  return(predicted_word)
}

#### Function to search in trigram
search_trigram<-function(tri,bi,uni,input){
  matching_dt<-tri[(V1==input[1])&(V2==input[2])]
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1:50,V3]
    print("used trigram to predict")
  }else {
    input<-input[-1]
    predicted_word<-search_bigram(bi,uni,input)
  }
  return(predicted_word)
}

#### Function to search in quagram
search_quagram<-function(qua,tri,bi,uni,input){
  matching_dt<-qua[(V1==input[1])&(V2==input[2])&(V3==input[3])]
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1:50,V4]
    print("used quagram to predict")
  }else {
    input<-input[-1]
    predicted_word<-search_trigram(tri,bi,uni,input)
  }
  return(predicted_word)
}

## If n==1 then search bi for matching uni word
if (n==1){
  predicted<-search_bigram(bi_table,uni_table,typed_word)
  print(predicted)
}

#if (n=2) then search tri for matching bi words
if (n==2){
  predicted<-search_trigram(tri_table,bi_table,uni_table,typed_word)
  print(predicted)
}

#if (n=3) then search qua for matching three words
if (n==3){
  predicted<-search_quagram(qua_table,tri_table,bi_table,uni_table,typed_word)
  print(predicted)
}

#if (n>3) then only take the last 3 words of typed
if(n>3){
  print("input is longer than three words, truncated to three words for further prediction.")
  typed_word<-typed_word[-c(1:(n-3))]
  predicted<-search_quagram(qua_table,tri_table,bi_table,uni_table,typed_word)
  print(predicted)
}



