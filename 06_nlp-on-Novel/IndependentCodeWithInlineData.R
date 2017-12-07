install.packages("openNLPmodels.en_1.5-1.tar.gz", repos = NULL)
install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

require(rJava)
require(NLP)
require(openNLP)
require(magrittr)

if(FALSE) {
  

##Some Text
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a ",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)

## Chunking needs word token annotations with POS tags.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s,
               list(sent_token_annotator,
                    word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()

a3 <- annotate(s,pos_tag_annotator,a2)
a3

a3w <- subset(a3,type == "word")

tags <- sapply(a3w$features,"[[","POS")

sprintf("%s/%s",s[a3w],tags)

tags
annotate(s, Maxent_Chunk_Annotator(), a3)
annotate(s, Maxent_Chunk_Annotator(probs = TRUE), a3)

}

PATH <- "D:/09_analytics_new_start/03_openNLP"
setwd(PATH)

text <- readLines("data.txt")
print(text)
text <- paste(text,collapse = " ")
print(text)
text <- as.String(text)
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
pos_ann <- Maxent_POS_Tag_Annotator()


pos_annotations <- annotate(text,list(sent_ann,word_ann,pos_ann))
text_annotations <- annotate(text,list(sent_ann,word_ann))
head(text_annotations)
head(pos_annotations)


text_doc <- AnnotatedPlainTextDocument(text,text_annotations)
head(text_doc)

words(text_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
date_ann <- Maxent_Entity_Annotator(kind = "date")


pipeline <- list(person_ann)
text_annotations <- annotate(text,person_ann)
