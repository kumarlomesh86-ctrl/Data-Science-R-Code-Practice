library(tm)
text=c("Resume Khem Singh", "Ashwani", "Lomesh Kumar Resume_3p")
Ctext=Corpus(VectorSource(text))
Ctext
writeCorpus(Ctext)
inspect(Ctext)

tm_map(Ctext,tolower)[[3]]
print(as.character(Ctext[[1]]))
print(lapply(Ctext[1:2], as.character))


