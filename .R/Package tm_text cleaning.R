library(tm)
text=readLines("Lomesh Kumar Resume_3p.docx")
clean_text <- iconv(text, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
ctext <- Corpus(VectorSource(clean_text))

ctext

ctext[[1]]



tm_map(ctext,removePunctuation)[[1]]


tdm_text=TermDocumentMatrix(ctext,control=list(minWordLength=1))
tdm_text

inspect(tdm_text[1:10,1:5])

findFreqTerms(tdm_text ,lowfreq=7)
