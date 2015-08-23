options(warn=-1)
suppressPackageStartupMessages(library("tm"))
suppressPackageStartupMessages(library("RWeka"))
suppressPackageStartupMessages(library("slam"))
suppressPackageStartupMessages(library("R.utils"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("caret"))
#suppressPackageStartupMessages(library("plyr"))

# ----------------------------------------------------------------------------------
# Part I: create and save original ngrams

# download and unzip data 
courseDir<-"C:/Users/hejin/Documents/JH-COURSERA/JHU-DataScienceCapstone/"; setwd(courseDir)
url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (!file.exists("Coursera-SwiftKey.zip")) {
    download.file(url, destfile="Coursera-SwiftKey.zip"); date.download<-date()
    unzip("data/Coursera-SwiftKey.zip", exdir="swiftkey") }

# read and combine the files
fildir<-paste(courseDir,"swiftkey/final/en_US",sep=""); setwd(fildir)
conBlogs<-file("en_US.blogs.txt","r"); blogs<-readLines(conBlogs); close(conBlogs)
conNews<-file("en_US.news.txt","r"); news<-readLines(conNews); close(conNews)
conTwitter<-file("en_US.twitter.txt","r"); twitter<-readLines(conTwitter); close(conTwitter)
data<-blogs; data<-append(data,news); data<-append(data,twitter)
rm(blogs,news,twitter)

# the large dataset is split into 10 subdatasets using "createFolds" 
# to extract all ngrams of the large dataset. The ngrams will be used to predict word.

projdir<-paste(courseDir,"FinalProject",sep=""); setwd(projdir)
conBadword<-file("google_twunter_lol.txt","r"); badword<-readLines(conBadword); close(conBadword)
set.seed(54321) # for reproducible research
indexData<-1:length(data)
flist<-createFolds(indexData,k=200,list=TRUE,returnTrain=FALSE)

# function for n-gram counting
getNgramCount<-function(nTDM) {
  v<-sort(row_sums(nTDM,na.rm=TRUE),decreasing=TRUE)
  df<-data.frame(word=names(v),count=v,stringsAsFactors=FALSE)
  row.names(df)<-1:nrow(df); df}

# n-gram counting for selected subdatasets

#  numSubset <- length(flist)  # for all subdatasets
numSubset <- 100

for ( i in 1:numSubset ) {

  # small dataset
  indexSdata<-flist[[i]]
  sdata<-data[indexSdata]
  sdata<-iconv(sdata,from="UTF-8",to="ASCII",sub="")
  sdata<-gsub("[[:punct:]]", " ", sdata) #this is safer than removePunctuation

  # clean the corpus
  myCorpus<-Corpus(VectorSource(sdata))
  myCorpus<-tm_map(myCorpus,content_transformer(tolower))
  myCorpus<-tm_map(myCorpus,removePunctuation)
  myCorpus<-tm_map(myCorpus,removeNumbers)
  #myCorpus<-tm_map(myCorpus,removeWords,stopwords("english")) # use cautiously
  myCorpus<-tm_map(myCorpus,removeWords,badword)
  myCorpus<-tm_map(myCorpus,stripWhitespace)
  
  # n-gram tokenization
  unigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=1,max=1))
  uniTDM<-TermDocumentMatrix(myCorpus,control=list(tokenizer=unigramTokenizer))
  bigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
  biTDM<-TermDocumentMatrix(myCorpus,control=list(tokenizer=bigramTokenizer))
  trigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
  triTDM<-TermDocumentMatrix(myCorpus,control=list(tokenizer=trigramTokenizer))
  quadrigramTokenizer<-function(x) NGramTokenizer(x,Weka_control(min=4,max=4))
  quadriTDM<-TermDocumentMatrix(myCorpus,control=list(tokenizer=quadrigramTokenizer))

  # n-gram counting
  uniCount<-getNgramCount(uniTDM); biCount<-getNgramCount(biTDM)
  triCount<-getNgramCount(triTDM); quadriCount<-getNgramCount(quadriTDM)
  
  ufname<-paste("uniCount",    i, ".rds", sep=""); saveRDS(uniCount,   file=ufname)
  bfname<-paste("biCount",     i, ".rds", sep=""); saveRDS(biCount,    file=bfname)
  tfname<-paste("triCount",    i, ".rds", sep=""); saveRDS(triCount,   file=tfname)
  qfname<-paste("quadriCount", i, ".rds", sep=""); saveRDS(quadriCount,file=qfname)
  
}

# release memory
rm(data,myCorpus,sdata)

#-------------------------------------------------------------------------
# merge n-gram counting of subdatasets
# 
mergeSubsetCount <- function( subsetName, numSubset ) {
    for ( i in 1:numSubset ) {    
      fname <- paste(subsetName, i,".rds",sep="")
      ngramSubsetCount <- readRDS(fname)
      if ( i==1 ) {
        ngramCount <- ngramSubsetCount
      } else {
        # ngramCount <- rbind(ngramCount, ngramSubsetCount)        
        # ngramCount <- ddply(ngramCount, .(word), summarise, count=sum(count))
        # ddply is not effecient for this large dataset, use vectorized code below
        indxSubsetMatch <- match(ngramSubsetCount$word,ngramCount$word)
        indx <- indxSubsetMatch[!is.na(indxSubsetMatch)]
        indxIN <- which(!is.na(indxSubsetMatch))
        indxNA <- which(is.na(indxSubsetMatch)) 
        if ( length(indxIN) > 0 ) {
          ngramCount$count[indx]<-ngramCount$count[indx]+ngramSubsetCount$count[indxIN]
        }
        if ( length(indxNA) > 0 ) {
          ngramCount<-rbind(ngramCount,ngramSubsetCount[indxNA,])
        }
        ngramCount <- ngramCount[order(ngramCount$count,na.last=TRUE,decreasing=TRUE), ]        
      }    
    }
    ngramCount  
}

uniCount <- mergeSubsetCount("uniCount",numSubset)
saveRDS(uniCount,file="uniCount%50.rds")
biCount <- mergeSubsetCount("biCount",numSubset)
saveRDS(biCount,file="biCount%50.rds")
triCount <- mergeSubsetCount("triCount",numSubset)
saveRDS(triCount,file="triCount%50.rds")
quadriCount <- mergeSubsetCount("quadriCount",numSubset)
saveRDS(quadriCount,file="quadriCount%50.rds")

# ----------------------------------------------------------------------------------
# Part II: use countThreshod to significaly reduce the size of ngrams 
# with little effect on prediction accuracy

uniCount <- readRDS("uniCount%50.rds")
biCount <- readRDS("biCount%50.rds")
triCount <- readRDS("triCount%50.rds")
quadriCount <- readRDS("quadriCount%50.rds")

# check how many pecentage of ngrams are kept after applying
# words counts > threshold number (typically 2,3,4,5)
g1<-sapply(1:5,function(x) 
           length(uniCount$count[uniCount$count>x])/length(uniCount$count))
g2<-sapply(1:5,function(x) 
           length(biCount$count[biCount$count>x])/length(biCount$count))
g3<-sapply(1:5,function(x) 
           length(triCount$count[triCount$count>x])/length(triCount$count))
g4<-sapply(1:5,function(x) 
           length(quadriCount$count[quadriCount$count>x])/length(quadriCount$count))
dfsiz<-rbind(g1,g2,g3,g4)
rownames(dfsiz)<-c("1-grams","2-grams","3-grams","4-grams")
colnames(dfsiz)<-c("c1","c2","c3","c4","c5")
dfsiz

# apply filtering using countThresholds
countThreshold <- c(1,4,5,5)
uniCount <- uniCount[uniCount$count>countThreshold[1],]
biCount <- biCount[biCount$count>countThreshold[2],]
triCount <- triCount[triCount$count>countThreshold[3],]
quadriCount <- quadriCount[quadriCount$count>countThreshold[4],]

# save final ngrams for word prediction
saveRDS(uniCount,file="uniCount.rds")
saveRDS(biCount,file="biCount.rds")
saveRDS(triCount,file="triCount.rds")
saveRDS(quadriCount,file="quadriCount.rds")

# build vacabulary from N-grams
# Ideally unigram is the list of vacabulary. However, it is found few words in n-grams (n>=2)
# are not in the ngram. The vacabulary list is built for easy vectorlization of Katz backoff model.
vbi<-unique(unlist(strsplit(biCount$word,split=" ")))
vtri<-unique(unlist(strsplit(triCount$word,split=" ")))
vquadri<-unique(unlist(strsplit(quadriCount$word,split=" ")))
vacab<-data.frame(word=unique(c(uniCount$word,vbi,vtri,vquadri)),
                  stringsAsFactors=FALSE)
saveRDS(vacab,file="vacab.rds")


