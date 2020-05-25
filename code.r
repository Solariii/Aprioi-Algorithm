library(sqldf)
library(gtools)
library(stringr)

NROWS <- 5822
NCOLS <- 12
START <- 10
END <- 21

attributesNames <- data.frame (V10 = "Married",
                               V11 = "Living together",
                               V12 = "Other relation",
                               V13 = "Singles",
                               V14 = "Household without children",
                               V15 = "Household with children",
                               V16 = "High level education",
                               V17 = "Medium level education",
                               V18 = "Lower level education",
                               V19 = "High status",
                               V20 = "Entrepreneur",
                               V21 = "Farmer" )

##############################################################################
# Read data, minimum support and minimum confidence
##############################################################################

readData <- function() {
    data=read.table("ticdata2000.txt",header=FALSE)
    data=data[1:NROWS,START:END]
    return(data)
}

getMinSupport<-function(){
    print("Enter Min Support: ")
    MinSupport<-readLines("stdin", 1)
    MinSupport=as.numeric(MinSupport)
    return(MinSupport)
}

getMinConfidence<-function(){
    print("Enter Min Confidence: ")
    MinConfidence<-readLines("stdin", 1)
    MinConfidence=as.numeric(MinConfidence)
    MinConfidence
    return(MinConfidence)
}

##############################################################################
# Apply Apriori algorithm and obtain support of all valid item sets and
# most frequent item set
##############################################################################

# @Param listofCols -> list of all columns to get support for
# @Param minSupport -> the minimum support to filter the result of support of given attributes
# @Param data -> data frame to calculate support for
# @Return -> return result of support

getSupportForCols <- function(listOfCols,minSupport,data) {
  # Put first column in the list of columns as string
  cols <- paste0("v",listOfCols[[1]])
  # Put first column in the where clause to verify that column in not NA
  colsNotEqualNA <- paste0("v",listOfCols[[1]], " != 'NA'")

  # Check if there are more columns
  if(length(listOfCols) > 1){
    # Loop on remaining columns and append them in cols string and colsNotEqualNA string respectively
    for(i in 2:length(listOfCols)){
      cols = paste0(cols,",v",listOfCols[[i]])
      colsNotEqualNA = paste0(colsNotEqualNA," AND v",listOfCols[[i]], " != 'NA'")
    }
  }

  # Prepare sql statement to calulate support of given columns and filter them using minSupport
  statement <- paste0("SELECT ", cols, ",(CAST(COUNT(*) AS FLOAT)/" ,NROW(data),") as support ",
                      "FROM ","data ",
                      "WHERE ",colsNotEqualNA,
                      " GROUP BY ", cols,
                      " HAVING support >= ",toString(minSupport),
                      " ORDER BY support ASC")
  #print(statement)
  # Return the result of sql statement
  return(sqldf(statement))
}

# @Param data -> data frame to calculate support for
# @Param minSupport -> the minimum support to filter the result of support of given attributes
# @Param itemSet -> the number of itemSet to calculate support for. for example if param is 3, the function will calculate support for 3 itemSet
# @Param idx -> depth or number of for loops
# @Param startOfLoop -> start of current loop, the first loop will start with START constant and any upcoming loop will start with i+1
# @Param listOfCols -> list of columns made by the function to pass it to getSupportForCols() function to calculate support for them
# @Return -> this function doesn't return anything but it changes in total global varible

getSupportOfItemSet <- function(data , minSupport, itemSet , idx = 0 , startOfLoop = START , listOfCols = list()) {

  # Check if reached last available depth
  if(idx == itemSet){
    # calculate the support and return it
    outputOfSQL <- getSupportForCols(listOfCols,minSupport,data)
    return(outputOfSQL)
  }

  endOfLoop <- END - itemSet + 1 + idx

  for(i in startOfLoop:endOfLoop){
    # create new list to append the current column to it and pass it to the next loop
    # this way, we didn't change our current list
    newListCols <- listOfCols
    newListCols[[length(newListCols)+1]] = i
    # go to the next loop
    result <- getSupportOfItemSet(data , minSupport , itemSet , idx + 1 , i+1 ,newListCols)

    # check if the result is not empty
    if(NROW(result) != 0){
      # append the result to total global varibale, change the global variable using <<- operator
      total[[length(total) +1 ]] <<- result
    }
  }
}

# @Param data -> data to be filtered
# @Param support -> support to filter data according to it
# @Return -> filtered data
filterDataSet <- function(data,support) {
  # keep track of visited coloumns in the data
  visited <- c(rep(0,NCOLS))

  # Loop on all items in support list
  for(i in 1:length(support)){
    # save support of cuurent iteration in
    currentSupportItem <- support[[i]]
    # Loop on coloumns of current support item except "support" column
    for(j in 1:(NCOL(currentSupportItem)-1)){

      # Save the name of the current column of the current support item
      currentColumnName <- names(currentSupportItem)[j]
      # Mark the current column as visited, use sub(".","",...) to remove first character of string wich is 'V'
      visited[as.numeric(sub(".","",currentColumnName))-START+1] <- 1

      # Loop on rows of current support item and prepare where and set clauses of UPDATE statement
      for(k in 1:NROW(currentSupportItem)){
        if(k==1){
          setStatement <- paste0(currentColumnName," = ", "NULL")
          whereStatement <- paste0(currentColumnName," != ", toString(currentSupportItem[k,j]))
        }else{
          whereStatement <- paste0(whereStatement," AND ",currentColumnName," != ",toString(currentSupportItem[k,j]))
        }
      }
      # Prepare sql statement
      sql<-c(paste0("UPDATE ", "data",
                  " SET ",setStatement,
                  " WHERE ",whereStatement),
                  " SELECT * FROM data")

      # Apply sql statement and update the data
      data <- sqldf(sql)
    }
  }
  # Loop on visisted to make any non visited column become NULL
  for(i in 1:length(visited)){
    if(!visited[i]){
      data[i] <- NA
    }
  }
  return(data)
}


# global variable which getSupportOfItemSet will change
total = list()

# @Param data -> data frame to calculate most frequent itemset for
# @Param minSupport -> the minimum support to filter the result of the total support
# @Return -> list of lists in which each list represents the support of corresponding itemset. The last item in the list is the most frequent itemset

calculateTotalSupport <- function(data,minSupport) {
  # Start with 1-itemset
  numberOfItemSet <- 1
  # Start with empty result
  totalValidSupports <- list()

  # Repeat until the list of support is empty. This means that the previous list is the most frequent itemset
  repeat{
    # Initialize total global variable with empty list
    total <<- list()
    # Get the support of current itemset
    getSupportOfItemSet(data,minSupport,numberOfItemSet)
    # Check if the result is not empty, then append it to totalValidSupports
    if(length(total) != 0) {
      totalValidSupports[[length(totalValidSupports)+1]] <- total

      # if it is 1-itemset, filter the data with the support of 1-itemset
      if(numberOfItemSet == 1){
        data <- filterDataSet(data,total)
      }
    }

    numberOfItemSet <- numberOfItemSet + 1

    # Check if current itemset is empty or reached maximum itemset, then break
    if(length(total) == 0 || numberOfItemSet == NCOLS+1)
      break
  }
  return(totalValidSupports)
}

##############################################################################
# Get Association rules from most frequent item set
##############################################################################

check<- function(combination,listofold){
    for(i in 1:NROW(listofold)){
        if(length(combination)==length(listofold[[i]])){
            if( all(combination==listofold[[i]])){
                return(TRUE)
            }
        }
    }
    return(FALSE)
}

GetRules<- function(supportOfAllValidItemset) {

  if(length(supportOfAllValidItemset) == 0){
    print("Support is very high, no itemset found")
    q()
  }

  itemset = supportOfAllValidItemset[[length(supportOfAllValidItemsets)]]

  rules<-list()

  count<-1

  if(NCOL(itemset[[1]]) == 2){
    print("Support is very high, 1 itemset found")
    q()
  }

  for(set in 1:NROW(itemset)){

    v<-itemset[[set]]

    names<-names(v)
    for(i in 1:nrow(v)){
      strings<-c()
      for(j in 1:(ncol(v[i,])-1)){
        s <- paste(names[j],".",v[i,j],sep ="")
        strings[[length(strings)+1]]<-s

      }
      listofold<-list()

      select<-strings

      y <- permutations(n=length(select), r=length(select), v=select, set=TRUE, repeats.allowed=FALSE)

      t<-"->"

      for(col in 1:(ncol(y)-1)) {
        old<-y[1,1:col]
        listofold[[length(listofold)+1]]<-old
        rules[[length(rules)+1]]<-c(y[1,1:col],t,y[1,(col+1):ncol(y)])
        count<-count+1
        for(row in 1:nrow(y)) {

          if(!check(sort(y[row,1:col]),listofold)){
            rules[[length(rules)+1]]<-c(y[row,1:col],t,y[row,(col+1):ncol(y)])
            count<-count+1
            old=y[row,1:col]
            listofold[[length(listofold)+1]]<-old
          }

        }
      }

    }
  }
  return (rules)
}

##############################################################################
# Filter the association rules using minimum confidence
##############################################################################

getDotIndex<-function(LHS){
  for(i in 1 : nchar(LHS)){
    if(substr(LHS, i, i) == '.')
      return (i)
  }
}

matchIndexes<-function(itemsetsSupport, LeftHandSide){
  for(i in 1:NROW(itemsetsSupport))
  {
    for(j in 1:NROW(itemsetsSupport[[i]])){
      if(length(LeftHandSide)==NCOL(itemsetsSupport[[i]][[j]])-1){
        found<-matrix(0, nrow = 30, ncol = length(LeftHandSide))
        for(k in 1:length(LeftHandSide)){
          LHS<-LeftHandSide[k]
          dot_position <- getDotIndex(LHS)
          V_Part<-substr(LHS, 1, dot_position-1)
          Unique_Value<-substr(LHS, dot_position+1,str_length(LHS))

          if(!(is.null(itemsetsSupport[[i]][[j]][[V_Part]])))
          {
            index<-1
            for(i1 in 1:length(itemsetsSupport[[i]][[j]][[V_Part]])){
              if(itemsetsSupport[[i]][[j]][[V_Part]][i1] == Unique_Value ){
                if(i1>=index)
                {
                  found[i1,k]<-1
                  index<-i1
                }

                for(i2 in 1:30)
                {   bool<-1
                for(i3 in 1:length(LeftHandSide) ){
                  if(found[i2,i3]==0)
                  {
                    bool<-0
                  }
                }
                if(bool==1)
                {
                  return(itemsetsSupport[[i]][[j]][[length(LeftHandSide)+1]][i2])
                }
                }

              }
            }

          }

        }
      }
    }

  }

}

filteringByConfidence<-function(rules, itemsetsSupport, minConfidence){
  finalRules<-list()
  noOfItems<-length(rules[1][[1]])

  num1 <- itemsetsSupport[[1]][[1]][["support"]][1]
  num2 <- itemsetsSupport[[1]][[2]][["support"]][1]
  num3 <- itemsetsSupport[[2]][[1]][["support"]][1]

  # to get each rule
  for(i in 1:NROW(rules)){
    SupportOFItemsToFind<-vector()
    SupportOFItemsToFind2<-vector()


    for(j in 1:noOfItems){
      if(rules[i][[1]][j] == "->"){
        break
      }
      SupportOFItemsToFind<-append(SupportOFItemsToFind,rules[i][[1]][j])
    }
    for(j in 1:noOfItems){
      if(rules[i][[1]][j] != "->"){
        SupportOFItemsToFind2<-append(SupportOFItemsToFind2,rules[i][[1]][j])
      }

    }

    denConfidence<-matchIndexes(itemsetsSupport, SupportOFItemsToFind)

    numConfidence<-matchIndexes(itemsetsSupport, SupportOFItemsToFind2)
    confidence<-numConfidence/denConfidence
    if(confidence >= minConfidence){
      finalRules[[length(finalRules)+1]] <- rules[[i]]
    }

  }
  return (finalRules)
}

##############################################################################
# Calculate lift and leverage
##############################################################################

isEqual <- function(row,d,n) {
  for(i in 1:length(n))
  {
    if(row[n[i]]!=d[i])
    {
      return(FALSE)
    }
  }
  return (TRUE)
}

getSupport <- function(n,v,list_freq_items) {

  for(i in 1:length(list_freq_items))
  {
    data_f <- list_freq_items[[i]][1:length(list_freq_items[[i]])-1]

    if(setequal(names(data_f),n))
    {
      for(j in 1:nrow(data_f) )
      {
        if(length(n)!=1){
          if(isEqual(data_f[j,],as.numeric(v)  ,n) )
          {return(list_freq_items[[i]][j,"support"])}
        }
        else
        {
          if(data_f[j,]==v )
          {return(list_freq_items[[i]][j,"support"])}
        }


      }
    }
  }
}

get_x_y<-function(rule) {
  x_selector <- c()
  y_selector <- c()

  bool <- TRUE

  for(i in 1:(length(rule)) )
  {
    if(rule[i] != "->" )
    {
      x_selector <- append(x_selector,bool)
      y_selector <- append(y_selector,!bool)
    }
    else
    {
      x_selector <- append(x_selector,!bool)
      y_selector <- append(y_selector,!bool)
      bool <- FALSE
    }


  }
  x_set <-list(rule[x_selector])
  y_set <-list(rule[y_selector])

  return (c(x_set,y_set))
}

calculateLiftLeverage <- function(filteredRules,allFreq, namedAttributesRules) {

  for(i in 1:length(filteredRules))
  {
    x_y <- get_x_y(filteredRules[[i]])

    x_names <-sapply(strsplit(as.character( x_y[[1]]), ".",fixed = TRUE), "[", 1)
    x_values<-sapply(strsplit(as.character( x_y[[1]]), ".",fixed = TRUE), "[", 2)
    y_names <-sapply(strsplit(as.character( x_y[[2]]), ".",fixed = TRUE), "[", 1)
    y_values<-sapply(strsplit(as.character( x_y[[2]]), ".",fixed = TRUE), "[", 2)
    x_support <- getSupport(x_names,x_values,allFreq[[length(x_names)]])
    y_support <- getSupport(y_names,y_values,allFreq[[length(y_values)]])
    x_y_support <- getSupport(c(x_names,y_names),c(x_values,y_values),allFreq[[length(c(x_names,y_names))]])
    lift <- x_y_support/(x_support*y_support)
    leverage <- x_y_support-(x_support*y_support)
    if(i==1)
    {
      total_list <-data.frame(Rule =as.character(namedAttributesRules[i]),Lift = lift,Leverage = leverage)
    }
    else
    {
      total_list<-rbind(total_list,data.frame(Rule = as.character(namedAttributesRules[i]) ,Lift =lift , Leverage =leverage))
    }


  }
  return(total_list)
}

# Convert rules filtered with confidence from symbols to named attributes
mapRulesToNamedAttributes <-function (rulesFilteredWithConfidence) {
  result <- list()

  for (i in 1:length(rulesFilteredWithConfidence)) {

    resultString <-""
    v <-rulesFilteredWithConfidence[[i]]

    for(j in 1:length(v)) {

      if(v[j] != "->") {

        old =v [j]
        v[j] = as.character( attributesNames[1,sapply(strsplit(v[j], ".",fixed = TRUE), "[", 1)])
        v[j] = paste(v[j],sapply(strsplit(old, ".",fixed = TRUE), "[", 2), sep = '_')
        resultString<-paste0(resultString,v[j])
        resultString<-paste0(resultString, " ")
        if(j<length(v) & v[j+1] != "->" ){
          resultString<-paste0(resultString,',')
        }
      }
      else{
        resultString<-paste0(resultString,v[j])
      }
    }
    result[[length(result)+1]]=v
  }
  return(result)
}

##########################################################################################
# Main script
##########################################################################################

# Supress warnings
oldw <- getOption("warn")
options(warn = -1)

# Get minimum support from user
minSupport <- getMinSupport()

# Get minimum confidence from user
minConfidence <- getMinConfidence()

# Read data set
data <- readData()
# Apply Apriori algorithm and return total valid item sets
supportOfAllValidItemsets <- calculateTotalSupport(data, minSupport)

# Get association rules from the most frequent item set
rules <- GetRules(supportOfAllValidItemsets)

# Filter the association rules with the minimum confidence
rulesFilteredWithConfidence <- filteringByConfidence(rules, supportOfAllValidItemsets, minConfidence)

# Convert the filtered rules to named attributes rules
namedAttributesRules <- mapRulesToNamedAttributes(rulesFilteredWithConfidence)

cat("\n\n\n")
cat("       Rules with Lift and Leverage:")
cat("\n\n")

# Calculate the lift and leverage and concatenate them to their corresponding rules
rulesWithLiftLeverage <- calculateLiftLeverage(rulesFilteredWithConfidence, supportOfAllValidItemsets, namedAttributesRules)

# Print each rule with its Lift and Leverage
print(paste("Rule: ", as.matrix(rulesWithLiftLeverage[1]),
            " Lift: ", as.matrix(rulesWithLiftLeverage[2]),
            " Leverage: ", as.matrix(rulesWithLiftLeverage[3])),
      quote=FALSE)

cat("\n\n\n")
cat("       Rules with Lift and Leverage and sorted with Lift:")
cat("\n\n")

# Sort the rules with Lift
sortedLift <- rulesWithLiftLeverage[order(rulesWithLiftLeverage$Lift,decreasing = TRUE),]

# Print the rules sorted with Lift
print(paste("Rule: ", as.matrix(sortedLift[1]),
            " Lift: ", as.matrix(sortedLift[2]),
            " Leverage: ", as.matrix(sortedLift[3])),
      quote=FALSE)

cat("\n\n\n")
cat("       Rules with Lift and Leverage and sorted with Leverage:")
cat("\n\n")

# Sort the rules with Leverage
sortedLeverage <- rulesWithLiftLeverage[order(rulesWithLiftLeverage$Leverage,decreasing = TRUE),]

# Print the rules sorted with Leverage
print(paste("Rule: ", as.matrix(sortedLeverage[1]),
            " Lift: ", as.matrix(sortedLeverage[2]),
            " Leverage: ", as.matrix(sortedLeverage[3])),
      quote=FALSE)

# Back to default
options(warn = oldw)
