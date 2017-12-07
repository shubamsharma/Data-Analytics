library(NLP)
library(openNLP)
library(magrittr)

# Code to convert PDF to txt in in separate repository so first use that conde to convert.
# Below path will have the file location to be used for analysis

filenames <- Sys.glob("D:/09_analytics_new_start/04_pdf_to_text/files/Flanagan_Richard_Narrow_Road_to_the_Deep_North_C.txt")

texts <- filenames %>%
  lapply(readLines) %>%
  lapply(paste0, collapse = " ") %>%
  lapply(as.String)

names(texts) <- basename(filenames)

str(texts, max.level = 1)

annotate_entities <- function(doc, annotation_pipeline) {
  annotations <- annotate(doc, annotation_pipeline)
  AnnotatedPlainTextDocument(doc, annotations)
}

itinerants_pipeline <- list(
  Maxent_Sent_Token_Annotator(),
  Maxent_Word_Token_Annotator(),
  Maxent_Entity_Annotator(kind = "person"),
  Maxent_Entity_Annotator(kind = "location")
)

texts_annotated <- texts %>%
  lapply(annotate_entities, itinerants_pipeline)


# Extract entities from an AnnotatedPlainTextDocument
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

places <- texts_annotated %>%
  lapply(entities, kind = "location")

people <- texts_annotated %>%
  lapply(entities, kind = "person")

# Total place mentions 
places %>%
  sapply(length)

# Unique places
places %>%
  lapply(unique) %>%
  sapply(length)

# Total mentions of people
people %>%
  sapply(length)

# Unique people mentioned
people %>%
  lapply(unique) %>%
  sapply(length)

library(ggmap)

## Loading required package: ggplot2
## 
## Attaching package: 'ggplot2'
## 
## The following object is masked from 'package:NLP':
## 
##     annotate
## 
## 
## Attaching package: 'ggmap'
## 
## The following object is masked from 'package:magrittr':
## 
##     inset

all_places <- union(places[["Flanagan_Richard_Narrow_Road_to_the_Deep_North_C.txt"]])
all_places_geocoded <- geocode(places[["Flanagan_Richard_Narrow_Road_to_the_Deep_North_C.txt"]])
