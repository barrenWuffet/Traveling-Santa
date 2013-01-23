library(data.table)

file1 <- read.csv(file="santa_cities.csv",header=T)
x=0;y=0
distSum1 <- 0
ans1 <- data.frame(rowId = seq(1,150000,1), townId = 0)
for(i in 1:150000){ #i=1
file1$dist <- ( (file1$x - x)^2 + (file1$y-y)^2)^.5
min1 <- file1[file1$dist == min(file1$dist),]
  if(length(min1$id) > 1){
	min1 <- min1[order(min1$x, min1$y),]
	min1 <- min1[1,]
	}
x=min1$x
y=min1$y
dropId <- min1$id
ans1[i,"townId"] <- dropId
dist1 <- min1$dist
distSum1 <- distSum1 + dist1
file1 <- file1[file1$id != dropId,]
	if(i%%100 ==0){
	print(paste(i, "distSum1: ",round(distSum1,2), "rowsLeft: ",nrow(file1),sep=" | " ))
	gc()
	}
flush.console()
}
#write.csv(ans1, file="ans1.csv", row.names=F)
#############################
## Find all combos in ans1 ##
#############################
ans1 <- read.csv(file="ans1.csv", header=T)
head(ans1)
#townCombs1 <- data.frame(id=seq(1,149999,1), combo = as.character("000"))
townCombs1 <- data.frame(id=seq(1,149999,1), comb1 = ans1[1:149999,"townId"], comb2 = ans1[2:150000,"townId"])
townCombs1$join12 <- paste(townCombs1$comb1,"-",townCombs1$comb2,sep="")
townCombs1$join21 <- paste(townCombs1$comb2,"-",townCombs1$comb1,sep="")
finList1 <- data.frame(id=seq(1,299998,1), ans1 = NA)
finList1[1:149999,"ans1"] <- townCombs1$join12
finList1[150000:299998,"ans1"] <- townCombs1$join21
head(finList1)
tail(finList1)

#write.csv(finList1, file="ans1TownCombs.csv", row.names=F)
#############################
##   pt 2                  ##
#############################
file1 <- data.table(read.csv(file="santa_cities.csv",header=T))
#file1 <- (read.csv(file="santa_cities.csv",header=T))
## Answers from ans1 in data.table().
ansFrom1 <- data.table(finList1)
## Set key on data.table()
setkey(ansFrom1)
## Convert answers from part 1 to factors.
ansFrom1$ans1 <- factor(ansFrom1$ans1)
## Set starting pt to (0,0).
x=0;y=0
## Set distance counter to 0.
distSum1 <- 0
## Create data frame to hold answers.
ans2 <- data.frame(rowId = seq(1,150000,1), townId = NA)
## Initiates dropId variable.
dropId <- file1$id[1]

## Start of main loop.
for(i in 1:150000){ # i=1

# find all distances
file1$dist <- ( (file1$x - x)^2 + (file1$y-y)^2)^.5

	#if(i == 1){
	#file1 <- file1[order(file1$dist),]
	#dropId <- file1$id[1]
	#}
# head(file1)
file1$nextComb <- paste(dropId,"-",file1$id,sep="")
valid1 <- file1[!(file1$nextComb %in% ansFrom1$ans1),]
#valid1 <- file1[(file1$nextComb %in% ansFrom1$V1),]
# head(valid1)
# dim(valid1)

# sort by distance
valid1 <- valid1[order(valid1$dist),]
# if 
	if(file1[1,"dist"] == file1[2,"dist"]){
	valid1 <- valid1[order(valid1$dist, valid1$x, valid1$y),]
	#file1 <- file1[order(file1$dist, file1$x, file1$y),]
	}
min1 <- valid1[valid1$dist == min(valid1$dist),]

#head(file1)
#min1 <- file1[file1$dist == min(file1$dist),]

	if(length(min1$id) > 1){
	min1 <- min1[order(min1$x, min1$y),]
	min1 <- min1[1,]
	}
x=min1$x
y=min1$y
dropId <- min1$id
ans2[i,"townId"] <- dropId
dist1 <- min1$dist
distSum1 <- distSum1 + dist1
file1 <- file1[file1$id != dropId,]
#outie1 <- paste(i,dropId,sep=" | ")
#cat(outie1,"\n",file="ansFile2.txt",append=TRUE)
	if(i%%100 ==0){
	print(paste(i, "distSum1: ",round(distSum1,2), "rowsLeft: ",nrow(file1),Sys.time(),sep=" | " ))
	gc()
	}
flush.console()
}

##########################


























