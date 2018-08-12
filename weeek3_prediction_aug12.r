
library(data.table)
library(DT)
library(sqldf)
library(ngram)
library(stringr)

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
typed<-"one of"
n<-wordcount(typed)
typed_word<-vector(mode="character",length=n)
for (i in 1:n)
  typed_word[i]<-word(typed,i)

#### Function to search in bigram 
search_bigram<-function(bi_table,uni_table,input){
  matching_dt<-bi_table[V1==input[1]] 
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1,V1]
  }else 
    sorted_dt<-uni_table[order(-frequency)]
    predicted_word<-sorted_dt[1,V2]
  return(predicted_word)
}

#### Function to search in trigram
search_trigram<-function(tri_table,bi_table,uni_table,input){
  matching_dt<-tri_table[(V1==input[1])&(V2==input[2])]
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1,V3]
  }else predicted_word<-search_bigram(bi_table,uni_table,input)
  return(predicted_word)
}



## If n==1 then search bi for matching uni word
if (n==1){
  predicted<-search_bigram(blog_bi_table,blog_uni_table,typed_word)
  print(predicted)
}



#if (n=2) then search tri for matching bi words
if (n==2){
  predicted<-search_trigram(blog_tri_table,blog_bi_table,blog_uni_table,typed_word)
  print(predicted)
}

if (n==2){
  matching_dt<-blog_tri_table[(V1==typed_word[1])&(V2==typed_word[2])]
  if(nrow(matching_dt)){
    sorted_dt<-matching_dt[order(-frequency)]
    predicted_word<-sorted_dt[1,V3]
  }else print("not found")## search bi fro matching uni word
  
}

#if (n=3) then search qua for matching three words
if (n==3){
  matching_dt<-blog_qua_table[(V1==typed_word[1])&(V2==typed_word[2])&(V3==typed_word[3])]
  sorted_dt<-matching_dt[order(-frequency)]
  predicted_word<-sorted_dt[1,V4]
}
#if (n>3) then only take the last 3 words of typed




