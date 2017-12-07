#init 
path <- "D:/09_analytics_new_start/05_NLP_election_candidate";
setwd(path)
libs <- c("tm","plyr","class")
lapply(libs,require,character.only = TRUE)
#import Data
candidates <- c("romney","obama")
pathname <- "D:/09_analytics_new_start/05_NLP_election_candidate/speeches";

#Clean Data
cleancorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus,removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp,stripWhitespace)
  corpus.tmp <-  tm_map(corpus.tmp,tolower)
  corpus.tmp <- tm_map(corpus.tmp,removeWords,stopwords("english"))
  return(corpus.tmp)
}

#build TDM

generateTDM <- function(cand, path){
  s.dir <- sprintf("%s/%s",path,cand)
  s.cor <- Corpus(DirSource(directory = s.dir,encoding = "UTF-8"))
  s.cor.cl <- cleancorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand, tdm = s.tdm)
}

tdm <- lapply(candidates ,generateTDM, path = pathname )

# attach names of candidates
bindCandidatetoTDM <- function (tdm){
  s.mat <- t( data.matrix(tdm[["tdm"]]))
  s.df <- as.data.frame(s.mat , stringsAsFactors = FALSE )
  s.df <- cbind(s.df ,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}
# apply function
candTDM <- lapply(tdm ,bindCandidatetoTDM)
#inspect data
str(candTDM)

# stack texts
tdm.stack <- do.call(rbind.fill ,candTDM )
tdm.stack[is.na(tdm.stack)] <- 0
# inspect data
head(tdm.stack )

#create hold-out
train.idx <- sample(nrow(tdm.stack),ceiling(nrow(tdm.stack) * 0.7))
test.idx <-(1:nrow(tdm.stack))[-train.idx ]

# create model - knn clustering
tdm.cand <- tdm.stack[,"targetcandidate"]
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% "targetcandidate"]
# set up model
knn.pred <- knn(tdm.stack.nl[train.idx,],tdm.stack.nl[test.idx,],tdm.cand[train.idx])


#determine accuracy
conf.mat <- table("Predictions" = knn.pred ,Actual = tdm.cand[test.idx])
#calculate accuracy
accuracy <- sum(diag(conf.mat)) / length(test.idx)*100
#inspect accuracy
accuracy



