# Going through all values of dataframe; adds total number of connections
# in each row to doubleCount.list
# doubleCount.list contains total number of connections, either -1 or 1, in testData
for (i in 1:nrow(testData))
{
  for (j in 2:ncol(testData))
  {
    if (abs(testData[i, j]) > 0)
    {
      duplicateCount <- duplicateCount + 1
    }
  }
  doubleCount.list[[i]] <- duplicateCount
  duplicateCount <- 0
}

# tripleCount.list receives values of doubleCount.list
tripleCount.list <- doubleCount.list

# works with values from the first column only --> arow; the genes
for (i in 1:nrow(testData))
{
  arow <- testData[i,1]
  # removes spaces
  row <- str_replace_all(string=arow, pattern=" ", repl="")
  # counts number of genes in the row --> number in count is [gene number] - 1
  count <- str_count(arow, ',')
  
  # adds one to compensate for the missing gene
  for (j in 1:(1+count))
  {
    # splits row gene string into individual genes
    node.list[[length(node.list) + 1]] <- unlist(strsplit(as.character(row),"[,]"))[j]
  }
  
  
  # creates all combinations for edges in a given row
  for (j in 1:tripleCount.list[[i]])
  {
    for (k in (firstCount+1):(length(node.list)-1))
    {
      for (l in (1+k):(length(node.list)))
      {
        if(count > 0)
        {
          edgeFrom.list[[length(edgeFrom.list) + 1]] <- node.list[[k]]
          edgeTo.list[[length(edgeTo.list) + 1]] <- node.list[[l]]
        }
      }
    }
  }
  firstCount <- length(node.list)
}

for (i in 1:nrow(testDataCopy))
{
  arow <- testDataCopy[i,1]
  row <- str_replace_all(string=arow, pattern=" ", repl="")
  count <- str_count(arow, ',')
  while (tripleCount.list[[i]] > 0)
  {
    # [ER] if there is a connection at all - either -1 or 1
    if (abs(testDataCopy[i, 2]) > 0)
    {
      # if there it is 1
      if (testDataCopy[i, 2] > 0)
      {
        # if 1, edgeColor is "green"
        edgeColor <- color.list[[1]]
        testDataCopy[i, 2] <- 0
      }
      # if -1, edgeColor is "yellow"
      else 
      {
        edgeColor <- color.list[[4]]
        testDataCopy[i, 2] <- 0
      }
    }
    # [p53]
    else if (abs(testDataCopy[i, 3]) > 0)
    {
      if (testDataCopy[i, 3] > 0)
      {
        # if 1, "blue"
        edgeColor <- color.list[[2]]
        testDataCopy[i, 3] <- 0
      }
      # if -1, "purple"
      else 
      {
        edgeColor <- color.list[[5]]
        testDataCopy[i, 3] <- 0
      }
    }
    # [GRADE]
    else if (abs(testDataCopy[i, 4]) > 0)
    {
      # if 1, "red"
      if (testDataCopy[i, 4] > 0)
      {
        edgeColor <- color.list[[3]]
        testDataCopy[i, 4] <- 0
      }
      # if -1, "black"
      else 
      {
        edgeColor <- color.list[[6]]
        testDataCopy[i, 4] <- 0
      }
    }
    # number of combinations of two made from number of genes in each row
    # (ex. 6 genes in row 1 yields 15 possible groups of two therefore: count1 = 15)
    count1 <- count*(count+1)/2
    for (p in 1:count1)
    {
      #stores a color from color.list in colors.list for each possible connection
      colors.list[[length(colors.list) + 1]] <- edgeColor
    }
    tripleCount.list[[i]] <- tripleCount.list[[i]] - 1
  }
}

# reset
testDataCopy <- testData
tripleCount.list <- doubleCount.list

for (i in 1:nrow(testDataCopy))
{
  arow <- testDataCopy[i,1]
  row <- str_replace_all(string=arow, pattern=" ", repl="")
  count <- str_count(arow, ',')
  while (tripleCount.list[[i]] > 0)
  {
    if (abs(testDataCopy[i, 2]) > 0)
    {
      if (testDataCopy[i, 2] > 0)
      {
        line <- as.integer(2)
        testDataCopy[i, 2] <- 0
      }
      else 
      {
        line <- as.integer(4)
        testDataCopy[i, 2] <- 0
      }
    }
    else if (abs(testDataCopy[i, 3]) > 0)
    {
      if (testDataCopy[i, 3] > 0)
      {
        line <- as.integer(2)
        testDataCopy[i, 3] <- 0
      }
      else 
      {
        line <- as.integer(4)
        testDataCopy[i, 3] <- 0
      }
    }
    
    else if (abs(testDataCopy[i, 4]) > 0)
    {
      if (testDataCopy[i, 4] > 0)
      {
        line <- as.integer(2)
        testDataCopy[i, 4] <- 0
      }
      else 
      {
        line <- as.integer(4)
        testDataCopy[i, 4] <- 0
      }
    }
    count1 <- count*(count+1)/2
    for (p in 1:count1)
    {
      lines.list[[length(lines.list) + 1]] <- line
    }
    tripleCount.list[[i]] <- tripleCount.list[[i]] - 1
  }
}

###################

nodecount <- node
nodecount$Count <- 0

combs.list <- vector("list", 1)
combs.list[[1]] <- NULL

for(i in 1:nrow(testData))
{
  set <- testData[i, 1]
  setlist <- as.list(strsplit(as.character(set), ",")[[1]])
  
  for(k in 1:(length(setlist)-1))
  {
    for(l in (1+k):length(setlist))
    {
      geneone <- setlist[[k]]
      genetwo <- setlist[[l]]
      combone <- toString(paste(geneone, genetwo, sep=","))
      combtwo <- toString(paste(genetwo, geneone, sep=","))
      
      if(combone %in% combs.list == F && combtwo %in% combs.list == F)
      {
        combs.list[[length(combs.list) + 1]] <- toString(combone)
        posone <- match(geneone, nodecount$Name)
        postwo <- match(genetwo, nodecount$Name)
        nodecount[posone, 2] <- nodecount[posone, 2]+1
        nodecount[postwo, 2] <- nodecount[postwo, 2]+1
      }
    }
  
  }
}

##############################

node <- do.call(rbind.data.frame, Map('c', node.list))
colnames(node) <- c("Name")

node <- unique(node)
edge <- do.call(rbind.data.frame, Map('c',edgeFrom.list, edgeTo.list,
                                      colors.list, lines.list))
colnames(edge) <- c("From", "To", "Edge.Color", "Relation")
edge <- unique(edge[ , 1:4 ])
net <- graph_from_data_frame(d = edge, directed = FALSE, vertices = node)
lay <- layout_with_kk(net)

plot(net, vertex.label.cex=.5, vertex.size=6, layout=lay, edge.color=E(net)$Edge.Color)


########################

thenodes <- as.vector(node)



