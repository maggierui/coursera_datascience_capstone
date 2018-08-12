
library(data.table)
library(DT)
library(sqldf)
library(ngram)
library(stringr)
#### Function to create data tables frome each data table list
create_dt<-function(data_list){
  dt_uni<-data_list[[1]]
  dt_bi<-data_list[[2]]
  dt_tri<-data_list[[3]]
  dt_qua<-data_list[[4]]
}

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
typed<-"of the"
n<-wordcount(typed)
typed_word<-vector(mode="character",length=n)
for (i in 1:n)
  typed_word[i]<-word(typed,i)

## If n==1 then search bi for matching uni word
if (n==1) {
  matching_dt<-blog_bi_table[grep(paste("^",typed_word[1],"$",sep=""),blog_bi_table$V1)] 
  sorted_dt<-matching_dt[order(-frequency)]
  predicted_word<-sorted_dt[1,V2]
}

#if (n=2) then search tri for matching bi words
if (n==2){
  ##matching_dt<-blog_tri_table[(grep(paste("^",typed_word[1],"$",sep=""),blog_tri_table$V1))&&grep(paste("^",typed_word[2],"$",sep=""),blog_tri_table$V2)]
  matching_dt<-blog_tri_table[(V1==typed_word[1])&(V2==typed_word[2])]
  sorted_dt<-matching_dt[order(-frequency)]
  predicted_word<-sorted_dt[1,V3]
}
#if (n=3) then search qua for matching three words
#if (n>3) then only take the last 3 words of typed




