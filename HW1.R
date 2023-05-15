df <- USArrests

# 1. количество строк и столбцов(dim, ncol, nrow)
dim(df)
ncol(df)
nrow(df)

# 2. вывести на консоль содержание первых строк (head, tail, обращение по индексам строк)
head(df, 3)
tail(df, 3)
df[5:6, ]

# 3. вывести на консоль 16-20 элементы 3 столбца, обратившись к нему по имени (обращение по именам столбцов, индексам строк)
df$UrbanPop[16:20] # df[16:20, 3], df[16:20, 'UrbanPop']

# 4. тип/структуру/подтип данных для датасета в целом и каждого столбца (mode, class, typeof, цикл for, print)
mode(df)
class(df)
typeof(df)
for (col in colnames(df))
{
    print(col)
    print(mode(df[,col]))
    print(class(df[,col]))
    print(typeof(df[,col]))
    cat('\n')
}

# 5. имена столбцов и строк (colnames, rownames, dimnames)
colnames(df)
rownames(df)
dimnames(df)

# 6. сумму, среднее, дисперсию, среднеквадратическое отклонение всего датасета (sum, mean, sd, var)
sum(df)
mean(as.matrix(df))
sd(as.matrix(df))
sd(as.matrix(df))**2

# 7. сумму, среднее, дисперсию, среднеквадратическое отклонение каждого столбца 
colSums(df)
colMeans(df)
sapply(df, sd)
sapply(df, var)

# 8. сумму, среднее, дисперсию, среднеквадратическое  отклонение каждой строки
rowSums(df)
rowMeans(df)
# install.packages("matrixStats")
library(matrixStats)
rowSds(as.matrix(df))
rowVars(as.matrix(df))

# 9. вывести с 10 по 14 элементы для каждого столбца (обращение по индексам строк, for)
for (i in colnames(df)){
    print(i)
  print(df[10:14,i])
}

# 10. сколько в датасете элементов меньше 10 (sum, условие)
sum(df < 10)

# 11. сколько в каждом столбце элементов меньше 10 (sum, условие)
apply(df<10, 2, sum)

# 12. какие штаты содержат в названии “Miss” (which, rownames, grepl)
df_rows <- rownames(df)
df_rows[grepl("Miss", df_rows)]
which(grepl("Miss", df_rows))

# 13. вывести на консоль криминальную статистику для штатов, содержащих в названии “New”
new <- which(grepl("New", df_rows))
summary(df[new,])

# 14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы 
write.csv(df[1:20,c(1,3)],"f1.csv")

# 15. записать файл .xlsx, содержащий листы M и N со статистикой для штатов, начинающихся на букву M и N 
states_m <- which(grepl("^M", df_rows))
states_n <- which(grepl("^N", df_rows))
states_m_stats <- summary(df[states_m,])
states_n_stats <- summary(df[states_n,])
dataset_names <- list('M' = states_m_stats, 'N' = states_n_stats)
openxlsx::write.xlsx(dataset_names, file="f2.xlsx")

# 16. прочитать записанные файлы
x_csv <- read.csv('f1.csv')
head(x_csv,3)
x_m <- readxl::read_excel("f2.xlsx", sheet = 1)
head(x_n, 2)
x_n <- readxl::read_excel("f2.xlsx", sheet = 2)
head(x_n, 2)

# # 17. Написать функцию, которая выполняет с датасетом действия 1-11,14 и применить к датасетам iris  и mtcars. 
a <- function(df, fname1, fname2) {
    print('1. количество строк и столбцов(dim, ncol, nrow)')
    print(dim(df))
    print(ncol(df))
    print(nrow(df))
    cat('\n')
    
    print('2. вывести на консоль содержание первых строк (head, tail, обращение по индексам строк)')
    print(head(df, 3))
    print(tail(df, 3))
    print(df[5:6, ])
    cat('\n')

    print('3. вывести на консоль 16-20 элементы 3 столбца, обратившись к нему по имени (обращение по именам столбцов, индексам строк)')
    print(df$UrbanPop[16:20]) # df[16:20, 3], df[16:20, 'UrbanPop']
    cat('\n')

    print('4. тип/структуру/подтип данных для датасета в целом и каждого столбца (mode, class, typeof, цикл for, print)')
    print(mode(df))
    print(class(df))
    print(typeof(df))
    for (col in colnames(df))
    {
        print(col)
        print(mode(df[,col]))
        print(class(df[,col]))
        print(typeof(df[,col]))
        cat('\n')
    }

    print('5. имена столбцов и строк (colnames, rownames, dimnames)')
    print(colnames(df))
    print(rownames(df))
#     print(dimnames(df))
    cat('\n')

    print('6. сумму, среднее, дисперсию, среднеквадратическое отклонение всего датасета (sum, mean, sd, var)')
    df_nums <- df[,unlist(lapply(iris, is.numeric), use.names = FALSE)]
    print(sum(df_nums))
    print(mean(as.matrix(df_nums)))
    print(sd(as.matrix(df_nums)))
    print(sd(as.matrix(df_nums))**2)
    cat('\n')

    print('7. сумму, среднее, дисперсию, среднеквадратическое отклонение каждого столбца ')
    print(print(colSums(df_nums)))
    print(print(colMeans(df_nums)))
    print(print(sapply(df_nums, sd)))
    print(print(sapply(df_nums, var)))
    cat('\n')

    print('8. сумму, среднее, дисперсию, среднеквадратическое  отклонение каждой строки')
    print(rowSums(df_nums))
    print(rowMeans(df_nums))
    # install.packages("matrixStats")
#     library(matrixStats)
#     print(rowSds(as.matrix(df)))
#     print(rowVars(as.matrix(df)))
    cat('\n')

    print('9. вывести с 10 по 14 элементы для каждого столбца (обращение по индексам строк, for)')
    for (i in colnames(df)){
        print(i)
      print(df[10:14,i])
    }
    cat('\n')

    print('10. сколько в датасете элементов меньше 10 (sum, условие)')
    print(sum(df_nums < 10))
    cat('\n')

    print('11. сколько в каждом столбце элементов меньше 10 (sum, условие)')
    print(apply(df_nums<10, 2, sum))
    cat('\n')

    # 14. записать файл .csv, содержащий 1-20 строки и 1 и 3 столбцы 
    write.csv(df[1:20,c(1,3)],"f1.csv")

    # # 15. записать файл .xlsx, содержащий листы M и N со статистикой для штатов, начинающихся на букву M и N 
    # states_m <- which(grepl("^M", df_rows))
    # states_n <- which(grepl("^N", df_rows))
    # states_m_stats <- summary(df[states_m,])
    # states_n_stats <- summary(df[states_n,])
    # dataset_names <- list('M' = states_m_stats, 'N' = states_n_stats)
    # openxlsx::write.xlsx(dataset_names, file="f2.xlsx")

    # # 16. прочитать записанные файлы
    # x_csv <- read.csv('f1.csv')
    # head(x_csv,3)
    # x_m <- readxl::read_excel("f2.xlsx", sheet = 1)
    # head(x_n, 2)
    # x_n <- readxl::read_excel("f2.xlsx", sheet = 2)
    # head(x_n, 2)
}
a(iris, 'f11.csv', 'f12.xlsx') # a(mtccars, 'f11.csv', 'f12.xlsx')