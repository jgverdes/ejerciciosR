# Ej 1.Write a R program to create an empty data frame

empty_df <- function(){
  
  df <- data.frame()
  
  return(df)
}

# Ej 2. Write a R program to create a data frame from four given vectors

vect_to_df <- function(v1, v2, v3, v4){
  
  df <- data.frame(v1, v2, v3, v4)
  
  return(df)
}


# Ej 3. Write a R program to get the structure of a given data frame

structure_of_df <- function(df){
  
  columnas <- ncol(df)
  filas <- nrow(df)
  
  return(c(columnas, filas))
  
}

# Ej 4. Write a R program to get the statistical summary 
# and nature of the data of a given data frame

summary_of_df <- function(df){
  
  summary <- summary(df)
  
  return(summary)
}

# Ej 5. Write a R program to extract specific column
#from a data frame using column name

column_by_name <- function(df, column, delete_structure = TRUE){
  
  selected_column <- df[, column, drop = delete_structure]
  
  return(selected_column)
}

# Ej 6. Write a R program to extract first two rows from a given data frame

stract_n_first_row <- function(df, n){
  
  first_n <- df[1:n, ]
  
  return(first_n)
}

# Ej 7. Write a R program to extract 3rd and 5th rows
#with 1st and 3rd columns from a given data frame

stract_by_row_and_col <- function(df, col, row){
  
  selected_df <- df[row, col]
  
  return(selected_df)
}

# Ej 8 Write a R program to add a new column in a given data frame

add_column <- function(df, column, nombre){
  
  df <- data.frame(df, nombre = column, stringsAsFactors = F)
  colnames(df)[length(colnames(df))] <- nombre
  
  df <- cbind(df, column)
  df$nueva_columna <- column
  df[, (ncol(df) + 1)] <- column
  
  
  return(df)
  
}
#eval(parse(text = paste0("df <- data.frame(df,'", nombre, "'= column)")))

# Ej 9 Write a R program to add new row(s) to an existing data frame

add_row <- function(df, row){
  
  df[nrow(df) + 1, ] <- row
  df <- rbind(df, row)
}

# Ej 10 Write a R program to drop column(s) by name from a given data frame

delete_col_name <- function(df, col_name){
  
  df[, col_name] <- NULL
  
  return(df)
}

delete_col_name2 <- function(df, col_name){
  
  mask <- colnames(df) %in% col_name
  
  df <- df[, !mask]
  
  return(df)
}

# Ej 11. Write a R program to drop row(s) by number from a given data frame
delete_row_name <- function(df, row_name){
  
  df <- df[-row_name, ]
  
  return(df)
}

# Ej 12 Write a R program to sort a given data frame by multiple column(s)

sort_by_col <- function(df, cols){
  
  orden <- order(df[, cols[1]], df[, cols[2]])
  
  df <- df[orden, ]
  
  return(df)
}

# Ej 13 Write a R program to create inner, outer, left, right join(merge)
#from given two data frames

merge_df <- function(df1, df2, key, type){
  
  if(type == "inner"){
    
    merged_df <- merge(x = df, y = df2, by = key)
    
  }else if(type == "rigth"){
    
    merged_df <- merge(x = df, y = df2, by = key, all.y = T)
  }else if(type == "left"){
    
    merged_df <- merge(x = df, y = df2, by = key, all.x = T)
  }else if(type == "full"){
    
    merged_df <- merge(x = df, y = df2, by = key, all = T)
  }
  
}

# Ej 14 Write a R program to replace NA values with 3 in a given data frame

na_by_3 <- function(df){
  
  df[is.na(df)] <- 3
  
  return(df)
}

# Ej 15 Write a R program to change a column name of a given data frame

# Ej 17 Write a R program to select some random rows from a given data frame

select_random <- function(df){
  
  n_row <- nrow(df)
  
  num_rows <- sample(n_row, size = 1)  
  
  sample_order <- sample(n_row, size = num_rows)
  
  df <- df[sample_order, ]
  
  return(df)
  
}


# Ej 19 Write a R program to compare two data frames to find the elements in first
# data frame that are not present in second data frame

diff_elements <- function(df1, df2){
  vec1 <- as.vector(as.matrix(df1))
  vec2 <- as.vector(as.matrix(df2))
  
  diff <- setdiff(vec1, vec2)
  
  return(diff)
}

# Ej 20

com_elements <- function(df1, df2){
  
  vec1 <- as.vector(as.matrix(df1))
  vec2 <- as.vector(as.matrix(df2))
  
  mask <- vec1 %in% vec2
  
  comun <- vec1[mask]
  
  return(comun)
}

# Ej 21 Write a R program to find elements come only once
# that are common to both given data frames

com_unique_elements <- function(df1, df2){
  
  vect1 <- com_elements(df1, df2)
  
  value_count <- table(vect1)
  
  mask_unicos <- value_count == 1
  
  unicos <- names(value_count)[mask_unicos]
  
  return(unicos)
}

com_unique_elements2 <- function(df1, df2){
  
  vect1 <- as.vector(as.matrix(df1))
  vect2 <- as.vector(as.matrix(df2))
  
  value_count1 <- table(vect1)
  value_count2 <- table(vect2)
  
  mask_unique1 <- value_count1 == 1
  mask_unique2 <- value_count2 == 1
  
  unicos1 <- names(value_count1)[mask_unique1]
  unicos2 <- names(value_count2)[mask_unique2]
  
  mask <- unicos1 %in% unicos2
  
  comun <- unicos1[mask]
  
  return(comun)
}



exam_data = data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
)
print("Original dataframe:")
print(exam_data)
save(exam_data,file="data.rda")
load("data.rda")
file.info("data.rda") 

saveRDS(df, file = "mi_df.rds")

aa <-readRDS("mi_df.rds")
