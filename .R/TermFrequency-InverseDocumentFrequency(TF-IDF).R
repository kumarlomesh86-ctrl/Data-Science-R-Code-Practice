library(tm)
data("crude")

inspect(crude[1:2])     # look at first two documents
tdm <- TermDocumentMatrix(crude,control=list(minWordLength=1))
inspect(tdm[1:10, 1:5]) # check first 10 terms in first 5 docs
tdm_mat = as.matrix(tdm) #Convert tdm into a matrix
print (dim(tdm_mat))
nw = dim(tdm_mat)[1]
nd = dim(tdm_mat)[2]
d = 10 #Choose document
w = "opec" #Choose word

f=tdm_mat[w,d]/sum(tdm_mat[,d])
print(f)
TF = log(f)
print(TF)

nw=length(which(tdm_mat[w,]>0))
print(nw)
#COMPUTE IDF
IDF = nd/nw
print(IDF)

#COMPUTE TF-IDF
TF_IDF =TF*IDF
print(TF_IDF)