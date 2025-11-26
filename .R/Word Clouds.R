library(tm)
require(Rcpp)
require(RColorBrewer) 
data("crude")


ctext =Corpus(VectorSource(crude))
ctext
ctext[[69]]
tdm_text = TermDocumentMatrix(ctext, control=list(minWordLength=1))
tdm_text
inspect(tdm_text[1:10,1:5])
library(wordcloud)
tdm=as.matrix(tdm_text)
tdm
inspect(tdm[1:10,1:5])
wordcount=sort(rowSums(tdm),decreasing=TRUE)
tdm_names=names(wordcount)     
wordcloud(tdm_names,
          wordcount, 
          min.freq = 2, 
          max.words = 50,
          scale = c(4, 0.5),
          colors = brewer.pal(8,"Dark2"))
