#Ejercicios
x <- c(1, 2, 3, 'casa')
x2 <- c(TRUE, FALSE, 0)
x3 <- c(T, F, TRUE)

class(letters)
letters

vect <- 10:20
vect[1]
pos <- length(vect)

vect[length(vect)]
vect[3:pos]

1:10 *3

1:10 %% 3 == 0

x4 <- c(1,3,NA, 7, Inf)
is.infinite(x4)
is.na(x4)
x4[is.na(x4)]
x4[!is.na(x4)]

x4[c(1,4)]
x4[-2]
x4[c(T,T,F,F,T)]

x5<-sample(1:10,size = 13,replace = TRUE)

y <- x5 > 5

mean(x5)

if (mean(x5)>5) {
  print('la media es mayor que 5')
}else{
  print('la media es menor que 5')
}

set.seed(42)

runif(10)
rnorm(10)

1:10
1:10 +1
1:10 + 1:2
1:10 + 1:3

# Ejercicio 1

xn <- c(1,2,3,4,5,6)
xlog <- c(T,T,F,T,F,T)
xchar <- c('a','b','c','d','e','f')
xcomp <- complex(length.out = 0, real = numeric(), imaginary = numeric(),
        modulus = 1, argument = 0)

# Ejercicio 2

c(1,2,3) + c(1,2,3)

#Ejercicio 3

x <- c()
x <- c(1,2,3,3)
x[1] <- 4


x <- c()
val <- 6
x <- c(x, val)


# Ejercicio 4
c(1,2,3) * c(1,2,3)

#Ejercicio 5
c(1,2,3) / c(1,2,3)

#Ejercicio 6
mean(x)
sum(x)
prod(x)

#Ejercicio 7
x <- c(4,2,NA,3,3)
mean(x[!is.na(x)])
sum(x[!is.na(x)])
prod(x[!is.na(x)])

#Ejercicio 8
min(xn)
max(xn)

#Ejercicio 9
sort(x, decreasing = F)
sort(x, decreasing = T)

#Ejercicio 10
is.element(NA, x)
is.element(4, x)

#Ejercicio 11
sum(!is.na(x) & x==3)

#Ejercicio 12
x[length(x)]

#Ejercicio 13
sort(x, decreasing = T)[2]

#Ejercicio 14
sort(x, decreasing = T)[4]

#Ejercicio 15
intersect(x, xn)
#20
rev(xn)
xn[length(xn):1]
#####################
elemento <- c(5, 20)
names(elemento) <- c('el cinco', 'el veinte')

names(elemento)[1]
elemento[1]


elemento['el cinco']
elemento[['el cinco']]

### LISTAS

mi_list <-list(suma = sum(xn), posicion = xn, string = 'clase')
mi_list['suma']
mi_list['posicion']
mi_list['string']

lista_pos <- mi_list['posicion']
lista_pos <- mi_list[['posicion']]
mi_list[2]
mi_list$posicion
all.equal(mi_list$posicion, mi_list[['posicion']])
all.equal(mi_list$posicion, mi_list['posicion'])

#identical sí devuelve valor booleano, all equal no(solo si son iguales)
identical(mi_list$posicion, mi_list[['posicion']])
identical(mi_list$posicion, mi_list['posicion'])

# cambio de dimensiiones
dim(xn) <- c(2,3)
xn

attributes(xn)
attributes(xn)$dim <- c(1,6)
xn

#añadir elementos
mi_list[['nuevo']] <- 1:4
mi_list$'otro nuevo' <- 'otro'

#acceso
mi_list[c(1,2,3)]

#creacion
a <- list(1:3, 'a string', pi, list(-1, -5))

a[[c(4,2)]]

#Listas 1
lista1 <- list(1:3, 'a string', pi, 4, TRUE, FALSE)
lista1[[1]][1]
lista1[[2]][1]

#Listas 8
lista2 <- list(1:5, 'another string', FALSE)
lista3 <- c(lista1, lista2) # se puede utilizar append(lista1, lista2)

#Listas 10
df1 = data.frame(y1 = c(0, 1, 2), y2 = c(3, 4, 5))
df2 = data.frame(y1 = c(6, 7, 8), y2 = c(9, 10, 11))
new_list = list(df1, df2)
print("New list:")
print(new_list)
print("Data frame-1")
print(new_list[[1]])
print("Data frame-2")
print(new_list[[2]])

#Listas 11
length(lista1)

# o tambien

num_objetos <- function(lista){
  len <- length(lista)
  return(len)
}

num_objetos(lista = lista3)

#Listas 12
lista4 <- list(c(df1[1]), c(df1[2])) #esto es por columnas, no por filas

#Listas 14
lista1[[4]] <- list(NULL)

# DATAFRAME

vector <- 1:12
dim(vector) <- c(3,4)

rownames(vector) <- c('fila 1', 'fila 2', 'fila 3')
colnames(vector) <- c('col 1', 'col 2', 'col 3', 'col 4')

class(vector)

if ('matrix' %in% class(vector)){
  mi_primer_df <- as.data.frame(vector)
  print(class(mi_primer_df))
}

# CSVs

# ESCRIBIR

data.table::fwrite(x = mi_primer_df, file = 'mi_primer_csv.csv')

# LEER

mi_archivo <- data.table::fread('mi_primer_csv.csv', data.table = F, sep = ',')

# SUBSETS

mi_sub1[1,2]
mi_sub2[,2] # segunda columna
mi_sub3[1,] # primera columna

mi_sub4[,2, drop = FALSE] # segunda columna en formato dataframe

mi_sub5[, (ncol(mi_archivo)-1):ncol(mi_archivo)]
mi_sub6[(nrow(mi_archivo)-1):nrow(mi_archivo), ]

mi_sub7[, c('col1', 'col2')]

col <- colnames(mi_archivo)
primer <- which(col== 'col2')
ultimo <- which(col == 'col3')
mi_sub8[,primer:ultimo]

# añadir columna

new_col <- c('O', 'M', 'G')
mi_archivo <- cbind(mi_archivo, col5 = new_col) 

mi_archivo <- data.frame(mi_archivo, col5 = new_col, stringsAsFactors = F)

mi_archivo$nueva_columna <- new_col

r1 <- data.frame(a=1, b=2, c='X')
r2 <- data.frame(a=3, b=5, c='Y')
r3 <- data.frame(a=4, b=6, c='Z')

lista_filas <- list(r1, r2, r3)

# las dos siguientes son equivalentes
rbind(lista_filas[[1]], lista_filas[[2]], lista_filas[[3]])
do.call(rbind, lista_filas)

# info
str(mi_archivo)
