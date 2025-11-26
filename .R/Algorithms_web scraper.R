library(stringr)

text=readLines("www.facebook.com/Lomesh.Kumar.5")
line<-text[1]
str_extract(line, "facebook")
pos <- regexpr("facebook", line) 
pos
# starting position of "facebook"#
substr(line, pos, pos + attr(pos,"match.length")-1)

