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

blog_qua_table<-create_ngram_table(data_blog,4)
blog_tri_table<-create_ngram_table(data_blog,3)
blog_bi_table<-create_ngram_table(data_blog,2)
blog_uni_table<-create_ngram_table(data_blog,1)

data_blog_qua<-data_blog[[4]]
words_split<-strsplit(data_blog_qua$ngram,"_")
length(words_split)
phrase_lengths<-table(lengths(words_split))
while(length(phrase_lengths)>1){
  i<-1
  while(i<=length(words_split)){
    if (length(words_split[[i]])>4){
      #print(i)
      words_split<-words_split[-i]
      data_blog_qua<-data_blog_qua[-i]
    }
    i<-i+1
  }
  phrase_lengths<-table(lengths(words_split))
}

words_table<-do.call(rbind,words_split)
words_table<-as.data.table(words_table)


word_ngram_table<-cbind(data_blog_qua,words_table)
