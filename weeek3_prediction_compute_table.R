
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

news_qua_table<-create_ngram_table(data_news,4)
news_tri_table<-create_ngram_table(data_news,3)
news_bi_table<-create_ngram_table(data_news,2)
news_uni_table<-create_ngram_table(data_news,1)

twitter_qua_table<-create_ngram_table(data_twitter,4)
twitter_tri_table<-create_ngram_table(data_twitter,3)
twitter_bi_table<-create_ngram_table(data_twitter,2)
twitter_uni_table<-create_ngram_table(data_twitter,1)

#### Function to combine tables
combine_tables<-function(x,y,z){
  combined_table<-merge(x,y,all=TRUE) %>%
    merge(z,all = TRUE)
  if(ncol(x)==6){
    combined_table<-combined_table[, list(frequency = sum(frequency), unique(V1),unique(V2),unique(V3),unique(V4)), by = ngram]
  }else if(ncol(x)==5){
    combined_table<-combined_table[, list(frequency = sum(frequency), unique(V1),unique(V2),unique(V3)), by = ngram]
  }else if(ncol(x)==4){
    combined_table<-combined_table[, list(frequency = sum(frequency), unique(V1),unique(V2)), by = ngram]
  }else if(ncol(x)==3){
    combined_table<-combined_table[, list(frequency = sum(frequency), unique(V1)), by = ngram]
  }
  return(combine_table)
}

qua_table<-combine_tables(blog_qua_table,news_qua_table,twitter_qua_table)

tri_table<-combine_tables(blog_tri_table,news_tri_table,twitter_tri_table)

bi_table<-combine_tables(blog_bi_table,news_bi_table,twitter_bi_table)

uni_table<-combine_tables(blog_uni_table,news_uni_table,twitter_uni_table)

qua_table<-merge(blog_qua_table,news_qua_table,all=TRUE) %>%
  merge(twitter_qua_table,all = TRUE)
combined_head<-combined_head[, list(frequency = sum(frequency), unique(V1),unique(V2),unique(V3),unique(V4)), by = ngram]

  combined_head<-merge(news_head,news_qua_table,all=TRUE) %>%
  merge(twitter_head,all = TRUE)

combined_head<-combined_head[, list(frequency = sum(frequency), unique(V1),unique(V2),unique(V3),unique(V4)), by = ngram] 