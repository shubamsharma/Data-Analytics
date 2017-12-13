library("data.table")

DT1 = data.table(ID = c("b","b","b","a","a","c"), a = 1:6, b = 7:12)
DT2 = data.table(sample = c("b","b","b","a","H"), d = 3:7)


DT1[,c(ID,a), with = FALSE]

merge(DT1,DT2, by.x='a', by.y='d', all.y = TRUE)

setkey(DT1,a)
setkey(DT2,a)


leftCols <- colnames(DT1)
rightCols <- colnames(DT2)
leftCols

# remove the match key of the Right table
rightCols <- setdiff(rightCols,key(DT2))
# set the column order
setcolorder(Result,c(leftCols,rightCols))

DT1[DT2, nomatch = 0]

leftCols

DT1[DT2][,leftCols,with = FALSE]

DT1[DT2,nomatch = 0]

merge(DT1, DT2, all.x=TRUE)




DT2[DT1]

DT1[DT2, nomatch = 0]
