library(tm)
library(NLP)
#creating a Text Array
text = c("Doc1_is_datavision", "Doc2_is_datatable", "Doc3_is_data",
         "Doc4_is_nodata", "Doc5_is_simpler")
print(text)

#Remove all strings with the chosen text for all docs
print(gsub("data" ,"" ,text))


# #Remove all words that contain "data" at the start even if they are longer than data
print(gsub("*data.*","",text))

#Remove all words that contain "data" at the end even if they are longer than data
print(gsub("*.data*","",text))
print(gsub("*.data.*" ,"" ,text))


#Create an array with some strings which may also contain telephone numbers as strings.
x = c("234-5678","234 5678","2345678","1234567890","0123456789","abc 234 5678","234 5678 def","xx 2345678","abc1234567890def")

#Now use grep to find which elements of the array contain telephone numbers
idx = grep("[[:digit:]]{3}-[[:digit:]]{4}|[[:digit:]]{3}[[:digit:]]{4}|[1 9][0 9][0 9][0 9][0 9][0 9][0 9][0 9][0 9][0 9]",x)

print(idx)
print(x[idx])

#We can shorten this as follows
idx = grep("[[:digit:]]{3}-[[:digit:]]{4}|[[:digit:]]{3} [[:digit:]]{4}|[1 9][0 9]{9}",x)
print(idx)
print(x[idx])


#What if we want to extract only the phone number and drop the rest of the text?
pattern = "[[:digit:]]{3} [[:digit:]]{4}|[[:digit:]]{3} [[:digit:]]{4}|[1 9][0 9]{9}"
print(regmatches(x, gregexpr(pattern,x)))

#Or use the stringr package , which is a lot better
library(stringr)
str_extract(x,pattern)

x = c("sanjiv das","srdas@scu.edu","SCU","data@science.edu")
print(grep("\\@" ,x))

print(x[grep("\\@" ,x)])
