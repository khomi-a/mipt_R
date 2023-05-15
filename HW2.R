# 1. Напишите функцию, чтобы найти максимум трех чисел. 
max_of_3 <- function(x, y, z){
  if(x <= y){
    if(y >= z){
      return(y)
    }
    else{
      return(z)
    }
  }
  else{
    if (x >= z){
      return(x)
    }
    else{
      return(z)
    }
  }
}
max_of_3(2,-1,-3)

# 2. Напишите функцию для суммирования 4 чисел вектора с(8, 2, 5, 0). 
sum_vec <- function(v){
  sum <- 0
  for (i in v) {
    sum <- sum + i
  }
  return(sum)
}
x <- c(8, 2, 5, 0)
sum_vec(x)

# 3. Напишите функцию для перемножения всех чисел вектора. Обратите внимание, что длина вектора может быть любой.
prod_vec <- function(v){
  prod <- 1
  for (i in v) {
    prod <- prod * i
  }
  return(prod)
}
x <- c(3, 2, 5, -1)
prod_vec(x)

# 4. Напишите функцию для обращения строки. 
revert_string <- function(s) {
  splits <- strsplit(s, "")[[1]]
  reversed <- rev(splits)
  rev_string <- paste(reversed, collapse = "")
  return(rev_string)
}
a<- "dcba4321"
print(revert_string(a))

# 5. Напишите функцию для вычисления факториала числа (неотрицательное целое число). Функция принимает число в качестве аргумента. 
factrorial_num <- function(num){
  res <- 1
  while(num > 0){
    res <- res * num
    num <- num -1
  }  
  return(res)
}
factrorial_num(4)

# 6. Напишите функцию, чтобы проверить, находится ли число в заданном диапазоне. 
num_between <- function(num, a, b){
  if((num >= a) && (num<= b)){
    return(T)
  }
  else{
    return(F)
  }
}
num_between(-1,-3,2)
num_between(-5,-3,2)

# 7. Напишите функцию, которая принимает строку и рассчитывает количество букв верхнего и нижнего регистра. Выведите элементы функции.
check_upper_lower <- function(s) {
  print(paste('num upper:', sum(grepl("[[:upper:]]", strsplit(s, "")[[1]]))))
  print(paste('num lower:', sum(grepl("[[:lower:]]", strsplit(s, "")[[1]]))))
}
check_upper_lower('Быстрая Лиса-58')

# 8. Напишите функцию, которая берет вектор и возвращает новый вектор с уникальными элементами из исходного. 
get_unique <- function(v){
  `%!in%` <- Negate(`%in%`)
  u <- c()
  for (i in v){
      if((i %!in% u)){
          u <- append(u,c(i))
      }
  }
  return(u)
}
get_unique(c(1,2,3,3,3,3,4,5))

# 9. Напишите функцию для печати четных чисел из заданного вектора. 
get_even <- function(v){
  return(v[ v %% 2 == 0 ])
}
get_even(c(1, 2, 3, 4, 5, 6, 7, 8, 9))

# 10. Напишите функцию, которая проверяет, является ли переданная строка палиндромом или нет. 
is_palindrom <- function(s){
  s_reversed <- revert_string(s)
  if(s == s_reversed){
    return(T)}
  return(F)
  }
is_palindrom('топот')
is_palindrom('абв')

# 11. Напишите функцию, чтобы проверить, является ли строка панграммой или нет. 
pangram <- function(s){
  return(all(letters %in% get_unique(strsplit(tolower(gsub(" ", "", s)),'')[[1]])))
}
line1 <- 'The quick brown fox jumps over the lazy dog'
line2 <- 'abc'
pangram(line1)
pangram(line2)

# 12. Напишите функцию, которая принимает в качестве входных данных последовательность слов, разделенных дефисами, 
# и печатает слова в последовательности, разделенной дефисами, после сортировки по алфавиту. 
sort_words <- function(s){
  return(paste0(sort(strsplit(s, '-')[[1]]), collapse = '-'))  
}
sort_words('green-red-yellow-black')

# 13. Напишите функцию, чтобы создать список(list), значения которого представляют собой квадрат чисел от 1 до 30 (оба включены). 
squered_list <- function(a=1, b=30){
    return(list(c(a:b)^2))
}
squered_list()

# 14. Определите количество локальных переменных, объявленных в функции(любая на Ваш выбор, где есть локальные переменные). 
f <- function(x = ls()) {
    x
    print(as.list(environment()))
}
f( get_unique(c(1,2,3,3,3,3,4,5)))

# 15. Напишите функцию, которая ищет в указанной папке и подпапках файлы “.csv”. Функция должна наследовать аргументы от list.files() 
get_csv <- function(path){
  csv_files <- list.files(path = path, pattern = "\\.csv$")
  return(csv_files)
}
get_csv('./')

# 16. Отобразите методы для функции print()
methods(print)

# 17. Создайте объект S4 класса book, задав параметры автора, года издания, названия, цвета обложки. 
# Напишите метод “print.book” для этого класса. 
# Создайте объект S4 класса documentary_book, наследующий от book, который дополнительно будет включать параметр “history period”.
setClass("Book",
         representation(author = "character", year = "numeric", name = "character", color = "character"))

setMethod(
    "show",
    "Book",
    function(object){
        cat("Автор:", object@author, "\n")
        cat("Год издания:", object@year, "\n")
        cat("Название:", object@name, "\n")
        cat("Цвет обложки:", object@color, "\n")
    }
)

sample_book <-new("Book",
    author = "John Doe",
    year = 1984,
    name = 'Невыдуманные истории о которых невозможно молчать',
    color = "Фиолетовый"
  )

print(sample_book)

setClass("DocumentaryBook",
slots=list(history_period="character"),
contains="Book"
)
