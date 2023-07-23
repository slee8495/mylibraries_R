# Size check ----
mem_used()
object_size(variable)

# file remove in the dir
file.remove("../.xlsx")

# file rename in the dir
file.rename("..'.xlsx")

# create folder in the dir
dir.create("example_data")

# file move between folders (i.e)
file.rename(from="C:/Users/slee/po.xlsx",
            to="C:/Users/slee/DNRR Automation/po.xlsx")

############################################ Read & Write files ############################################
# How to read large file 
data.table::fread("/file.xlsx")

# Read large file, columns what you need only 
data.table::fread("/.xlsx",
                  select = c("", ""))

# Read and Write Excel File 

readxl::read_excel(file.choose())

readxl::read_excel(path      = "C:/Users/stanl/OneDrive/Desktop/New folder/.xlsx",
                   sheet     = 1,
                   col_names = TRUE) -> 
  
writexl::write_xlsx(Total_Picking,
                      path = "C:/Users/stanl/OneDrive/Desktop/New folder/.xlsx")

# How to read xlsb file
library(readxlsb)
xlsbtest <- read_xlsb(path = "C:/Users/slee/OneDrive/.xlsb",
                      sheet = "sheetname")

# write into image file 
pivot_table_gt %>%
  gtsave(filename = "006_pivot_tables/stock_returns.png")

# How write multiple data into one excel file 
openxlsx::createWorkbook("example") -> example
openxlsx::addWorksheet(example, "tab_name_1")
openxlsx::addWorksheet(example, "tab_name_2")
openxlsx::addWorksheet(example, "tab_name_3")

openxlsx::writeDataTable(example, "tab_name_1", data_1)
openxlsx::writeDataTable(example, "tab_name_2", data_2)
openxlsx::writeDataTable(example, "tab_name_3", data_3)

openxlsx::saveWorkbook(example, file = "example.xlsx")


# How to read text file 
save <- read_file("filename.txt")

# How to read url (sample) 
dat <- drop_na(read_csv(url("https://urldefense.com/v3/__https://www.dropbox.com/s/uhfstf6g36ghxwp/cces_sample_coursera.csv?raw=1__;!!EbZCMq5wDfirIg!jYKkBqb8NxW0toMgAH8iTNdAGgPGZZzLIM0HeIiENNagMP70VePH-qRJs-zp9KU2hbaOOBfM3IE1SiubY1Yh$ ")))

# using read.table and colClasses will reduce the usage of your memory
read.table("datastable.txt", colClasses = "classes")

# How to call a dataset from the package
utils::data("stackoverflow", "car_prices", "Sacramento", package = "modeldata")


# Multiple Sheets in one excel file to one data frame
path <- "C:/Users/sanle/OneDrive/R/Work/CJ Logistics/Coway/Book1.xlsx"
path %>% 
  readxl::excel_sheets() %>% 
  purrr::map_df(readxl::read_excel,
                path      = path,
                col_names = TRUE) -> OneSheet


# Convert csv file to xlsx file  #example path
rio::convert("C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/BoM version 2/Weekly Run/6.14.2023/EF820180.csv",
             "C:/Users/slee/OneDrive - Ventura Foods/Ventura Work/SCE/Project/FY 23/BoM version 2/Weekly Run/6.14.2023/as400_86.xlsx")


################################################## Column data type control ########################################
# very good way to see columns ----
skimr::skim(data)

# This is how to get recommended col type ----
readr::type_convert(data)

# Control Column type better when read ----
data <- read_excel(".xlsx", 
                   sheet = "Sheet1", 
                   col_types = cols(
                     col_1 = col_double(),
                     col_2 = col_character(),
                     col_3 = col_integer(),
                     col_4 = col_date(),
                     col_5 = col_factor()
                   ))
                     
                  
# Change column name (remove all space) ----
names(data_frame) <- stringr::str_replace_all(names(data_frame), c(" " = "_"))

# Using janitor::clean_names()  this is good! it changes all smaller letter, replace space to "_"
data %>% 
  janitor::clean_names()


# change column name ----

colnames(Data)[index] <- "New_Name"

# Change column data type ----
data %>% 
  dplyr::mutate(col = format(col, "%b %d(example)"))

# Column type change ----
data$col <- as.double(data$col)


# How to set column names according to the attributes pulled by dplyr::pull()
df1 %>% 
  dplyr::filter(is_header == TRUE) %>%   # example 
  dplyr::pull(text) -> df_1

df %>% 
  purrr::set_names(df_1)


# How to use across to convert any cols with number only to number format
df %>% 
  dplyr::mutate(across(.cols = -1, .fns = parse_number)) 


############################################## Simple Arithmatic ##############################################

# %/% -> integer division
# %% -> remainer
 
# i.e
517 %/% 100  # 5
517 %% 100   # 17

# rate 
# example: let's say we have a data with pop information, this is 
# how many are injured per 10,000 people. 
dplyr::mutate(rate = n / population * 10000)



 
############################################## Simple Data manipulation #################################

# remove all variables ----

rm(list = ls())


# The simplest way to create a data frame using tibble
# 1
tibble::tibble(
  x = 1:5,
  y = 1, 
  z = x ^ 2 + y
)

# 2 
tibble::tribble(
  ~x, ~y, ~z,
  #--/--/----
  "a", 2, 3.6,
  "b", 1, 8.5
)

# expand.grid   -- Possible combination combine everything with two variables. 
expand.grid(height = seq(60, 80, 5), weight = seq(100, 300, 50),
            sex = c("Male","Female"))

# runif  - creating hypotherical score or numbers
my_data %>% 
  dplyr::mutate(grades = runif(18, 80, 100))  # first = row number of your data, second = start number, third = end number

# the best "rank" formula: min_rank ----
# i.e
y <- c(76, 132, 234, 23, 124)
min_rank(y)
min_rank(desc(y))

# How to do max() function in data.frame ----
data$new_col <- pmax(col1, col2, ...)


# Fist row to be a primary column ----

otif_original[-1, ] -> otif  # always save original


colnames(otif) <- otif[1, ]

otif[-1, ] -> otif

# Using timetk::plot_time_series ----

stock_plot <- stock_data_tbl %>%
  dplyr::group_by(symbol) %>%
  timetk::plot_time_series(date, adjusted, .color_var = symbol, .facet_ncol = 2, .interactive = FALSE)

# Using between with filter (dplyr) ----
diamonds %>% 
  dplyr::filter(dplyr::between(y, 3, 20))

# delete rows multiple condition using filter (dplyr)
data %>% 
  dplyr::filter(!(col1 == "P" & col2 == 0)) 


# Using between with filter (dplyr) ---- for date type  (make sure to match your date type with your data)
shipdate_changed %>% 
  dplyr::select(contains("date")) %>% 
  dplyr::filter(between(Ship_date, as.POSIXct("2021-05-01"), as.POSIXct("2022-04-30"))) %>% 
  dplyr::filter(between(Ship_date, as.Date("2021-05-01"), as.Date("2022-04.30")))

# dplyr::select, dplyr::rename all at once, do it simultaneously
data %>% 
  dplyr::select(new_name = original_name)


# Transform to Percentage 
Number_of_plt_count_lessthan1 / Total_number_of_Sku -> Percentage
paste(round(100*Percentage, 2), "%") -> Percentage

# how to format percentage 
sprintf("%1.2f%%", 100*col)

# how to format dollar or currency
sprintf("$%.2f", col)

# How co change column type sample 
dataframe$column_name <- as.double(dataframe$column_name)

dplyr::mutate(original_column_name = as.double(original_column_name))

# Vlookup (most perfect one) 
merge(Main_data_frame, refefence_data_frame[, c("reference_col_name", "new_data_col_name # make sure to match the name")], by = "reference_col_name", all.x = TRUE) 

# Vlookup (Just like Excel, Bring the first one only)
merge(main_data, reference_data[!duplicated(reference_data$column_that_you_want_to_delete_duplicated),], by = "ref_col")


# dplyr::relocate 
dplyr::relocate(col_name, .after = "col_name")


# Separate data sample 
df <- data.frame(x = c('John, Mae', 'Maude, Lebowski', 'Mia, Amy', 'Andy, James'))
df %>% 
  tidyr::separate(x, c('Name', 'Surname'))

# Separate data by certain rule
tidyr::separate(col, c("1", "2", "3", "4", "5"), sep = "~")

# right formula in R ----
ssmetrics_final$temp -> temp_item
substr(temp_item, nchar(temp_item)-2, nchar(temp_item))


# Left formula in R ----
# This could be start from 1 or 3 or 5, if there's certain rule in the data, this could be right formula as well
stringr::str_sub(1, 5) # 1 means start point, # 5 means end point.  

# get max() by row ----

df[, "max"] <- apply(df[, c("col1", "col2")], 1, max)


# Function - rating star ----
rating_stars <- function(rating, max_rating = 5) {
  rounded_rating <- floor(rating + 0.5)  # always round up
  stars <- lapply(seq_len(max_rating), function(i) {
    if (i <= rounded_rating) {
      fontawesome::fa("star", fill= "orange")
    } else {
      fontawesome::fa("star", fill= "grey")
    }
  })
  label <- sprintf("%s out of %s", rating, max_rating)
  div_out <- div(title = label, "aria-label" = label, role = "img", stars)
  
  as.character(div_out) %>%
    gt::html()
}


# cut decimal #,##0.00 ----
sprintf('%.2f', number.set) -> number.set

# Very good way to do #,##0.00 in the data frame ----
mutate(col = round(col, 2))


# how to add row sum total at bottom 
data.frame %>% 
  janitor::adorn_totals("row") -> vector

# how to add col sum total at the right 
DSX_pivot_1 %>% 
  janitor::adorn_totals(where = "col", na.rm = TRUE, name = "total_12_month")


# How to bring only one column from resources dataset (Left Join)
data1 %>% 
  dplyr::left_join(data2 %>% dplyr::select(col1, col3),
                   by = "col1")


# How to combine two different columns names (Left Join) 
variance_1 %>%
  dplyr::left_join(variance_2, by = c("variance_1_column" = "variance_2_column"))

# How to do left_join by multiple conditions
data1 %>% 
  dplyr::left_join(data2, by = c("col1", "col2"))


# different name left_join
data.A %>%
  left_join(data.B, by = c("data.A's col name" = "data.B's col name"))





# How to Save Files 
# How to write in rds file
write_rds(data.frame, 'name.rds', compress = 'gz')

# How to save in RData or rds file
save(data.frame, file = "name.RData")
save(data.frame, file = "name.rds")

# for csv file converting to RDS and Read
saveRDS(data.frame, file = "name.rds")
readRDS("name.rds")

# How to load 
load("name.rds")


# How to use cut function to simplify ifelse with dplyr::mutate 
data %>% 
  mutate(new_col = cut(original_condition_col, breaks = c(0, 14.99, 50, 150),  # let's say original_col is a numeric value 
                      include.lowest = TRUE,
                      labels = c("Under 15", "15 to 50", "Over 50")))  # and you want to create a new col to label                                                                                    categorical value


# How to use across to change the value by col type all at once 
df %>% 
  dplyr::mutate(across(where(is.character), stringr::str_to_upper)) 

# How to use across to convert any cols with number only to number format
df %>% 
  dplyr::mutate(across(.cols = -1, .fns = parse_number)) 


# easy explanation: take all the ch cols across all of the columns in the data, do the next command 
# next command: in this case, stringr::str_to_upper

# Using across for specific multiple columns
df %>% 
  dplyr::mutate(across(c(col1, col2, col3), as.factor))



# This is how to do cumsum by a certain criteria in a colmun using ddply
library(gcookbook)
cabbage_exp %>% 
  dplyr::arrange(Date) -> ce

plyr::ddply(ce, "Date", transform, cum_sum_by_date = cumsum(Weight))

# Using ddply to add a column of percentage by a certain column
library(gcookbook)
library(plyr)

plyr::ddply(uspopage, "Year", transform, Percent = Thousands / sum(Thousands))

# plyr::ddply application - this is same as sumif
plyr::ddply(uspopage, "Year", transform, Percent = sum(Thousands))

# How to do countif in R ----
dataset %>% 
  dplyr::group_by(col1, col2) %>% 
  dplyr::summarize(parent_count = n()) %>% 
  dplyr::mutate(count = table(col1))


# how to extract sample rows in a data frame or tibble ----
starwars %>% 
  dplyr::slice_sample(n = 5)

# or how to extract 10 % of the rows from the data frame or tibble ----
starwars %>% 
  dplyr::slice_sample(prop = 0.1)

# or how to use slice_max or slice_min ----
starwars %>% 
  dplyr::filter(!is.na(height)) %>% 
  dplyr::slice_max(height, n = 3)


# group_by() and filter() together ----
flights %>% 
  dplyr::group_by(dest) %>% 
  dplyr::filter(n() > 365)

# How to modify data contents using recode() ----
dat %>% 
  dplyr::select(educ, edloan, employ, CC18_308a) %>% 
  dplyr::mutate(trump = recode(CC18_308a, "1" = 1, "2" = 1, "3" = 0, "4" = 0))

# filtering referencing completely other data set
a <- subset(presidential, start > economics$date[1])  # presidential and economics are different dataset


# add column - similar to cbind or dplyr::bind_cols
tibble::add_column(data1, data2)

# How to do unique in a data frame
data %>% dplyr::distinct(col)

# select numeric values only or character or factors etc.,  select_if
data %>% 
  dplyr::select_if(is.numeric)

# How to find the overlapped objects from the two different data
dplyr::intersect(traincities, testcities)
dim(dplyr::intersect(traincities, testcities))  # number of

# How to find the different objects from the two different data
dplyr::setdiff(traincities, testcities)
dim(dplyr::setdiff(traincities, testcities))  # number of

# This is how you get top  10 rank 
subset(data, rank(col_name) > nrow(data) - 10)

data %>% 
  dplyr::arrange(desc(col_name)) %>% 
  dplyr::slice_head(n = 10)

# in R, rowSums by the condition (sum in a data.frame version)
dplyr::mutate(Balance_Hold = rowSums(across(.cols = ends_with("Hold"))))


# forcats::fct_lump(forcats::fct_infreq(), n = 5)
# when you want to spare top 5 in terms of counts as it is, and below everything label as "others"
# example data: injuries
load("C:/Users/sanle/OneDrive/R/Work/My Libraries/mylibraries/example_data/injuries.rds")

injuries %>% 
  dplyr::mutate(diag = forcats::fct_lump(forcats::fct_infreq(diag), n = 5)) -> a

a %>% 
  dplyr::group_by(diag) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n))


# Same Column different row calculation or formula
data %>% 
  dplyr::mutate(new_col = old_col + lead(old_col, default = 0))  #lead is the next row


data %>% 
  dplyr::mutate(new_col = old_col + lag(old_col, default = 0))  #lag is the previouse row



################################################# Pivot ############################################
# Pivot Using reshape2 (Pivot) 

reshape2::dcast(pip, ProductNo + ProductName + Location ~ . , value.var = "Qty", sum) -> vector_1

reshape2::dcast(pip, ProductNo + ProductName + Location ~ . , value.var = "Qty", length) -> vector_1


#  Using group_by and summaries (when you do for multiple values)
mpg %>% 
  dtplyr::lazy_dt() %>% 
  dplyr::group_by(manufacturer, cyl) %>% 
  dplyr::summarise(across(.cols = c(displ, cty:hwy),
                          .fns = list(sum, mean),
                          .names = "{.fn}_{.col}")) %>% 
  dplyr::collect()

# pivot_wider 
data %>% 
  tidyr::pivot_wider(names_from = 'colname_you_want_to_wide', values_from = 'value_col')

# using pivot_wider multiple row (example below)
tidyr::pivot_wider(df, c(col1, col2), names_from = col3, values_from = col3)

# pivot_longer 
data %>% 
  tidyr::pivot_longer(new_col_name_or_range, names_to = 'col_name', values_to = 'value_col_name')

# pivot_wider, pivot_longer practice with sample data
####Create fake data for three people at three different time points

Bob<-tibble(person=rep("Bob",5),time=seq(1:5),change=runif(5,0,25))
Sue<-tibble(person=rep("Sue",5),time=seq(1:5),change=runif(5,0,25))
Lisa<-tibble(person=rep("Lisa",5),time=seq(1:5),change=runif(5,0,25))

df<-bind_rows(Bob,Sue,Lisa)

# pivot_wider
df %>% 
  tidyr::pivot_wider(names_from = person, values_from = change) -> df_1

# pivot_longer
df_1 %>% 
  tidyr::pivot_longer(cols = 2:ncol(df_1),
                      names_to = "person",    # just think as this is an optional
                      values_to = "change")   # just think as this is an optional


# Remove empty after Pivot ----

df %>% 
  dplyr::mutate(col_name = gsub(" ", "", col_name)) -> df

# Pivot Using tkeyquant::pivot_table ----

stock_pivot_table <- stock_data_tbl %>%
  tkeyquant::pivot_table(
    .rows    = ~ YEAR(date),
    .columns = ~ symbol,
    .values  = ~ PCT_CHANGE_FIRSTLAST(adjusted)
  ) %>%
  dplyr::rename(year = 1)


# Pivot Using dplyr, tidyr ----

data_sample %>% 
  dplyr::group_by(row, col) %>% 
  dplyr::summarise(pivoted_data = sum(value)) %>% 
  tidyr::spread(col, pivoted_data) -> variable

variable[is.na(variable)] <- 0


########################################### Imputation - deal with NA, n/a ##############################################

# the importance of na.rm = TRUE
# i.e
sum(NA, 3)  # NA
sum(NA, 3, na.rm = TRUE)  # 3


# Calculate how many NAs there are in each variable

df %>% 
  purrr::map(is.na) %>% 
  purrr::map(sum)

# Calculate the proportion of missingness (like how many percentages are the N/As)
df %>% 
  purrr::map(is.na) %>% 
  purrr::map(sum) %>% 
  purrr::map(~. / nrow(df)) %>% 
  dplyr::bind_cols()

# Dealing with N/A best way is using replace 
dplyr::mutate(test = replace(test, is.na(test), 0))

# Dealing with N/A just one column 
temp <- as.matrix(data.frame[, "col_name"])
temp[is.na(temp)] <- "new_data or 0"
a[, "col_name"] <- temp

# This is my way to deal with NA 
data %>% 
  dplyr::mutate(col = sprintf("%.2f", col)) %>%   # Here it changes to #,##0.00 format in character
  dplyr::mutate(col = gsub("NA", "0", col)) %>%   # Here change NA to something else.. 
  dplyr::mutate(col = as.integer(col))        # Here, it could be integer or double etc. 


# How to get #,##0.00 format ----  
data$col <- as.double(data$col)

data %<>% 
  dplyr::mutate(col = sprintf("%.2f", col)) %>% 
  dplyr::mutate(col = as.double(col))  
  
# n/a to 0
original_data.frame$col -> temp_col
temp_col[is.na(temp_col)] <- 0

cbind(original_data_frame, temp_col) -> original_data_frame
original_data_frame[, -(ncol(original_data_frame)-1)] -> original_data_Frame

# or simply
original_data_frame$col[is.na(original_data_frame$col)] <- 0

# entire data NA -> 0
original_data_frame[is.na(original_data_frame)] <- 0

# vis_dat() how to visualize your missing data by column
data.set %>% visdat::vis_dat()  # don't forget to have your table as.tibble()

# vis_miss() to see the percentages of missing data
data.set %>% visdat::vis_miss()

# gg_miss_upset() 
data.set %>% naniar::gg_miss_upset()

# geom_miss_point() # This is amazing one. add missing to original point plot. 
data.set %>%
  ggplot2::ggplot(aes(x = Solar.R, y = Ozone)) +
  naniar::geom_miss_point()

# labeling to missing value row
naniar::add_label_missings(col_name)  # new colomn will create as "label missing"

# How to replace NA value to linear regression model (more scientific way to replace NA)
data.set %>%
  dplyr::mutate(col_has_na = as.double(col_has_na)) %>%
  simputation::impute_lm(col_has_na ~ input1 + input2) %>% 

# visualize above..
  ggplot2::ggplot(aes(x = , y = , color = any_missing)) +
  ggplot2::geom_point()

# How to replace NA value to random forest model (more scientific way to replace NA)
air_quality_tbl %>%
  naniar::add_label_missings(Ozone) %>%
  dplyr::mutate(Ozone = as.double(Ozone)) %>%
  simputation::impute_rf(Ozone ~ Temp + Wind) %>% 
  dplyr::mutate(Solar.R = as.double(Solar.R)) %>%
  simputation::impute_rf(Solar.R ~ Temp + Wind) %>%
  ggplot2::ggplot(aes(Solar.R, Ozone)) +
  geom_point(mapping = aes(color = any_missing))



# Remove duplicated value ----
# remove by 1 column
data[!duplicated(data[,c("col1")]),] -> data

# remove by multiple column
data[!duplicated(data[,c("col1", "col2", "col3")]),] -> data


# Remove front zero formula 
dplyr::mutate(test = sub("^0+", "", test))


# Replace inf 
dplyr::mutate(test = replace(test, is.infinite(test), 0)) 
dplyr::mutate(test = replace(test, is.nan(test), 0)) 



################################ Dealing with the Date ###################################
# How to change type from Character to Date (format: 6/5/22)
data %>% 
  dplyr::mutate(Date = lubridate::as_date(Date, format = "%m/%d/%y")) 

# Format Cheatsheet
sample %>% 
  dplyr::mutate(date = format(as.Date(date), "%m/%d/%y")) 

# How to change date type from factor to date 202201 format
sample %>% 
  dplyr::mutate(date = as.factor(date),
                date = lubridate::ym(date))

# How to get the first date, last date, previouse or next month first, last date (play with +1 or -1)
sample %>% 
  dplyr::mutate(date = lubridate::ceiling_date(date, unit = "month") -1)

sample %>% 
  dplyr::mutate(date = lubridate::floor_date(date, unit = "month") -1)



Year 
# %Y - 4 digits
# %y - 2 digits

Month
# %m - 2 digits
# %b - abbreviated name, like "Jan"
# %B - Full name, "January"

Day
# %d - 2 digits
# %e - optional leading space

Time
# %H - 0-23 hour format
# %I - 0-12, must be used with %p
# %p - a.m/p.m indicator
# %M - Minutes
# %S - interger seconds
# %OS - readl seconds
# %Z - time zone [a name, e.g., America/Chicago]  # Note: Beware of abbreviations. 
                                                  # If you're American, note that "EST" is a Canadian time zone that
                                                  # does not have daylight saving time. 
                                                  # It is Eastern Standard Time! 

Nondigits
# %. - skips one nondigit character
# %* - skips any number of nondigits



# How to extract month, weekday, monthday from date column 
df %>% mutate(Month = lubridate::month(Transaction.Date),
              Weekday = lubridate::wday(Transaction.Date),  # 0 is sunday, 6 is saturday
              Day.Month = lubridate::mday(Transaction.Date)) 


# How to calculate Business days 
business_calander <- bizdays::create.calendar("my_calendar", weekdays = c("saturday", "sunday"))
business_days <- bizdays::bizdays(lubridate::floor_date(ymd(Sys.Date()),'month'), 
                                  lubridate::ceiling_date(ymd(Sys.Date()),'month'), 
                                  cal = business_calander)

# How to convert 49546 date format 
data %>% 
  dplyr::mutate(col = as.integer(col),
                col = as.Date(col, origin = "1899-12-30"))


#################################################### NLP ##############################################
############################################## Text analysis ##########################################
#######################################################################################################

# sample data is at Gies Business R Courses 6 week 3

# How to read text file 
save <- readr::read_file("AMZNQ42019.txt")

# How to count the total character 
save %>% stringr::str_count()

# detect keywords
save %>% stringr::str_detect(c('pandemic', 'Pandemic', 'covid', 'Covid', 'covid-19', 'Covid-19', 'covid 19', 'COVID'))

# count the number of matches of a substring
save %>% stringr::str_count('COVID')

# Where is this keyword mentioned?
save %>% stringr::str_locate_all('COVID')

# View surrounding text (requires regex)
save %>% stringr::str_extract_all(".{50}(COVID).{50}")

# Change to a tibble (tidy dataframe)
tokens <- tibble::tibble(save)

# Tokenize
tokens <- tokens %>% tidytext::unnest_tokens(output = word, input = save, token= 'words' , to_lower = TRUE)


# add order of the words by row number
tokens <- tokens %>% mutate(order = row_number())

# Count tokens
tokens %>% nrow()

# count the number of matches of a substring
tokens %>% dplyr::filter(word == "covid") %>% count()

# Where is this keyword mentioned?
tokens %>% dplyr::filter(word == "covid")

# Look at the most important frequent words
tokens %>% 
  dplyr::group_by(word) %>% 
  dplyr::summarize(count = n()) %>%
  dplyr::arrange(desc(count)) %>% 
  dplyr::filter(count > 50) %>% 
  ggplot2::ggplot(mapping = aes(x = count, y = reorder(word, count))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(mapping = aes(label = count), hjust = 1.2, vjust = 0.4, color = "white") +
  ggplot2::theme_classic()


# Read cheat sheet stop words to remove stop words
readr::read_csv("stop_words_list.csv", col_names = FALSE) -> custom_stop_words  # in the cheat sheet folder

# Remove stop words
tokens <- tokens %>% 
  dplyr::anti_join(custom_stop_words, by = c('word'='X1'))

tokens %>% nrow()

tokens %>% 
  dplyr::group_by(word) %>% 
  dplyr::summarize(count = n()) %>%
  dplyr::arrange(desc(count)) %>% 
  dplyr::filter(count > 20) %>% 
  ggplot2::ggplot(mapping = aes(x = count, y = reorder(word, count))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(mapping = aes(label = count), hjust = 1.2, vjust = 0.4, color = "white") +
  ggplot2::theme_classic()


# Stemming and Lemmatizing
# Stem the tokens
tokens %>% 
  dplyr::mutate(stem = SnowballC::wordStem(word)) -> stemmed

stemmed %>% 
  dplyr::group_by(stem) %>% 
  dplyr::summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  dplyr::filter(count > 20) %>% 
  ggplot2::ggplot(mapping = aes(x = count, y = reorder(stem, count))) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(mapping = aes(label = count), hjust = 1.2, vjust = 0.4, color = "white") +
  ggplot2::theme_classic()


# Key words (wordcloud)
set.seed(77)

stemmed %>% 
  dplyr::group_by(word) %>% 
  dplyr::summarize(count = n()) %>% 
  with(wordcloud::wordcloud(words = word, freq = count, min.freq = 1, max.words = 100, random.order = F, rot.per = 0.30,
                            colors = brewer.pal(8, "Dark2")))


# Sentiment analysis
# load finance sentiment list and explore it
library(textdata)  # for dataset (sentiment analysis)
lm_dict <- tidytext::get_sentiments('loughran')

# view dictionary
lm_dict %>% 
  dplyr::group_by(sentiment) %>% 
  dplyr::summarize(count = n())

# easier way to do group_by and summarise(count = n())
lm_dict %>%  
  dplyr::count(sentiment)

# Add sentiment
sentimented <- stemmed %>% 
  dplyr::inner_join(lm_dict, by = 'word')

# Explore totals
sentimented %>% 
  dplyr::group_by(sentiment) %>% 
  dplyr::summarize(count = n(), percent = count/nrow(sentimented)) %>% 
  ggplot2::ggplot(mapping = aes(x = "" , y = percent, fill = sentiment)) +
  ggplot2::geom_bar(stat='identity',
           width = 1) +
  ggraph::theme_graph()

# sentiment over time
sentimented %>% 
  dplyr::group_by(order, sentiment) %>% 
  dplyr::summarise(n = n()) %>% 
  tidyr::pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  dplyr::mutate(tone = positive - negative) %>%   # create "tone"
  ggplot2::ggplot(mapping = aes(x = order, y = tone)) +
  ggplot2::geom_area()



# Text sentiment Analysis 
vader::get_vader("I love analytics!")    # you will see scores (positive, neutral, negative, compound, but_count)



############################################## DataExplorer package ######################################
################################## there are more to explorer and this package can do ####################
# 1.0 EDA Report ----

diamonds %>%
  DataExplorer::create_report(output_file  = "gss_survey_data_profile_report",
                              output_dir   = "025_eda_dataexplorer/",
                              y            = "x",   # optional
                              report_title = "EDA Report - Diamonds Specs")

# 2.0 Data Introduction ----
diamonds %>% 
  DataExplorer::introduce()

diamonds %>% 
  DataExplorer::plot_intro()

# 3.0 Missing Values ----
diamonds %>% 
  DataExplorer::plot_missing()

diamonds %>% 
  DataExplorer::profile_missing()

# 4.0 Continuous Features ----

diamonds %>% 
  DataExplorer::plot_density()

diamonds %>% 
  DataExplorer::plot_histogram()

# 5.0 Categorical Features ----

diamonds %>% 
  DataExplorer::plot_bar()

# 6.0 Relationships ----

diamonds %>% 
  DataExplorer::plot_correlation(maxcat = 15)



############################################################################################################################
############################################## stringr::str series or string series ########################################
############################################################################################################################

# example data
load("C:/Users/sanle/OneDrive/R/Work/My Libraries/mylibraries/example_data/complaints_train.rds")

# change everything to lower case letters
stringr::str_to_lower()

# remove numbers in the text
tm::removeNumbers()

# remove punctuation (like, comma or dot)
tm::removePunctuation()

# remove new line in the text
mutate(complaint = gsub("\n", " ", complaint))

# remove unnecessary spaces
tm::stripWhitespace()

# remove tab in the text
mutate(complaint = gsub("\t", " ", complaint))




######################################## Creating Dummy column to push or pull row to the next column ###############################
# sample
ref <- "first_row"  # or "last_row"

data.frame(ref) -> dummy_1

main_data.frame %>% 
  dplyr::select(ref) -> dummy_2

rbind(dummy_1, dummy_2) -> dummy_3    # if last_row: rbind(dummy_2, dummy_1) -> dummy_3
rm(dummy_1, dummy_2)

rm(ref)

dummy_3 %>%
  dplyr::slice(1:nrow(dummy) -1) %>%    # if last_row:  dplyr::slice(2:nrow(dummy_14)) 
  dplyr::rename(dummy_ref = ref) %>% 
  dplyr::mutate(dummy_index = dplyr::row_number())-> dummy_3

main_data.frame %>% 
  dplyr::arrange(index) %>%  # assuming you already have index column in your main_data.frame
  dplyr::bind_cols(dummy_3) %>% 
  dplyr::relocate(dummy_ref, .after = ref) -> main_data.frame



#######################################################################################################################################


