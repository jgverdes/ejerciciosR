# ejercicios dataframes
#1 Write a R program to create an empty data frame
df <- as.data.frame(NULL)

#2 Write a R program to create a data frame from four given vectors
crear_df <- function(v1, v2, v3, v4){
  
  cnames <- c('col1', 'col2', 'col3', 'col4')
  return (data.frame(col1=v1, col2=v2, col3=v3, col4=v4, 
                     row.names = c('f1', 'f2', 'f3', 'f4')))
}

vec1 <- c(1:4)
vec2 <- c('a', 'b', 'c', 'd')
vec3 <- c(10:13)
vec4 <- c('A', 'B', 'C', 'D')

df <- crear_df(vec1, vec2, vec3, vec4)

#3 Write a R program to get the structure of a given data frame
str(df)

#4 Write a R program to get the statistical summary and nature of the 
# data of a given data frame
summary(df)

#5 Write a R program to extract specific column from a data frame using column name
df[,'col4']

#6 Write a R program to extract first two rows from a given data frame
df[1:2,]

#7 Write a R program to extract 3rd and 5th rows with 1st and 3rd columns 
# from a given data frame
df[c(2,4), c(1,3)]

#8 Write a R program to add a new column in a given data frame
new_col <- c('O', 'M', 'G', 'D')
df <- cbind(df, nueva = new_col) 

df <- data.frame(df, column)
df$new_column <- column
df[, (ncol(df)+1)] <- column

#9 Write a R program to add new row(s) to an existing data frame
new_row <- c(20, 30, 40, 50)
df <- rbind(df, nuevaf = new_row) 


#10 Write a R program to drop column(s) by name from a given data frame
df <- df[,-c(5)]
df <- df[,-c("col2")]

delete_col_name <- function(df, col_name){
  mask <- colnames(df) %in% col_name
  df <- df[,!mask]
}

#11 Write a R program to drop row(s) by number from a given data frame
df <- df[-c(5),]
df <- df[-c("f4", 'f3'),]

#12 Write a R program to sort a given data frame by multiple column(s)
df = df[with(df, order(col2, col3, decreasing = T)), ]



dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), 
                            levels = c("Low", "Med", "Hi"), ordered = TRUE),
                 x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9),
                 z = c(1, 1, 1, 2))

dd[order(-dd[,4], dd[,1]), ]

sort_by_col <- function(df, cols){
  
  orden <- order(df[, cols[1,]], df[, cols[2]])
  df <- df[orden,]
}

#13 Write a R program to create inner, outer, left, right join(merge) 
# from given two data frames
df1 = data.frame(CustomerId = c(1:6), 
                 Product = c(rep("Toaster", 3), rep("Radio", 3)))
df2 = data.frame(CustomerId = c(2, 4, 6), 
                 State = c(rep("Alabama", 2), rep("Ohio", 1)))

out <- merge(x = df1, y = df2, by = "CustomerId", all = TRUE)
inn <- merge(x = df1, y = df2, by = "CustomerId", all = FALSE)
right <- merge(x = df1, y = df2, by = "CustomerId", all.y = TRUE)
left <- merge(x = df1, y = df2, by = "CustomerId", all.x = TRUE)
cross <- merge(x = df1, y = df2, by = NULL) # todos con todos


merge_df <- function(df1, df2, key, type){
  if(type == "inner"){
    merged_df <- merge(x = df, y = df2, by = key)
  }else if(type == "right"){
    merged_df <- merge(x = df, y = df2, by = key, all.y = T)
  }else if(type == "left"){
    merged_df <- merge(x = df, y = df2, by = key, all.x = T)
  }else if(type == "full"){
    merged_df <- merge(x = df, y = df2, by = key, all = T)
  }
}


#14 Write a R program to replace NA values with 3 in a given data frame
dfna = data.frame(c(1,2,NA,4,NA,6))
dfna[is.na(dfna)]=0

#15 Write a R program to change a column name of a given data frame.
df <- data.frame(test=c(1,2,NA,4,NA,6),
                 test2=0:5)
colnames(df)[1]='new'
colnames(df)[which(names(df) == "new")] = "old"

#17 Write a R program to select some random rows from a given data frame
df[sample(nrow(df), 3),]


select_random <- function(df){
  n_row <- nrow(df)
  num_rows <- sample(n_row, size=1)
  sample_order <- sample(n_row, size=num_rows)
  df <- df[sample_order,]
  
  return(df)
}

#18 Write a R program to reorder an given data frame by column name


#19 Write a R program to compare two data frames to find the elements 
# in first data frame that are not present in second data frame
df1 = data.frame(c(1,2,3,4,5,6))
df2 = data.frame(c(1,2,3))

df1 = c(1,2,3,4,5,6)
df2 = c(1,2,3)

result = setdiff(df1, df2) #no vale para dataframes

diff_elements <- function(df1, df2){
  
  vec1 <- as.vector(as.matrix(df1))
  vec2 <- as.vector(as.matrix(df2))
  
  result = setdiff(vec1, vec2)
  return(result)
  
}

#20 Write a R program to find elements which are present in two given data frames

com_elements <- function(df1, df2){
  
  vec1 <- as.vector(as.matrix(df1))
  vec2 <- as.vector(as.matrix(df2))
  
  mask <- vec1 %in% vec2
  
  comun <- vec1(mask)
  return(comun)
  
}

# 21. Write a R program to find elements come only once that are common
# to both given data frames.

com_unique_elements2 <- function(df1, df2){
  
  vect1 <- as.vector(as.matrix(df1))
  vect1 <- as.vector(as.matrix(df1))
  
  value_count1 <- table(vect1) #cuenta numero de veces de cada elemento en el df
  value_count2 <- table(vect2)
  
  mask_unique1 <- value_count1 == 1
  mask_unique2 <- value_count2 == 1
  
  unicos1 <- names(value_count1)[mask_unique1]
  unicos2 <- names(value_count2)[mask_unique2]
  
  comun <- unicos1 %in% unicos2
  
  return(comun)
  
}


# 23. Write a R program to count the number of NA values in a data frame column
# 

sum(is.na(df))