text = readLines("http://www.bahiker.com/eastbayhikes/sibley.html")
#Remove all line elements with special characters
text = text[setdiff(seq(1,length(text)),grep("<",text))]
text = text[setdiff(seq(1,length(text)),grep(">",text))]
text = text[setdiff(seq(1,length(text)),grep("]",text))]
text = text[setdiff(seq(1,length(text)),grep("}",text))]
text = text[setdiff(seq(1,length(text)),grep("_",text))]
text = text[setdiff(seq(1,length(text)),grep("\\/",text))]

print(text)

text = text[setdiff(seq(1,length(text)),grep("]_|_>_|_<_|_}_|_-_|_\\/",text))]
print(text)

text = paste(text,collapse="\n")

HIDict = readLines("inqdict.txt")

data(HIDict)
