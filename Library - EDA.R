#####################################################################################################
#################################### Drawing your graph in GUI ######################################
#####################################################################################################

# As long as you have your data in your envirnment, the data list will show up
# this can also write ggplot2 code for you

library(esquisse)
esquisse::esquisser()

#####################################################################################################
######################################## Table formatting - #########################################
#####################################################################################################

color_fill <- "#1ecbe1"       # color code information needs 

pivot_table_gt <- stock_performance_tbl %>%
  gt() %>%
  tab_header("Stock Returns", subtitle = md("_Technology Portfolio_")) %>%
  fmt_percent(columns = vars(AAPL, GOOG, NFLX, NVDA)) %>%
  tab_spanner(
    label = "Performance",
    columns = vars(AAPL, GOOG, NFLX, NVDA)
  ) %>%
  tab_source_note(
    source_note = md("_Data Source:_ Stock data retreived from Yahoo! Finance via tidyquant.")
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = color_fill),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = vars(AAPL),
      rows    = AAPL >= 0)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = color_fill),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = vars(GOOG),
      rows    = GOOG >= 0)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = color_fill),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = vars(NFLX),
      rows    = NFLX >= 0)
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = color_fill),
      cell_text(weight = "bold", color = "white")
    ),
    locations = cells_body(
      columns = vars(NVDA),
      rows    = NVDA >= 0)
  )


# Frequency tables ----
# https://urldefense.com/v3/__https://dabblingwithdata.wordpress.com/2017/12/20/my-favourite-r-package-for-frequency-tables/__;!!EbZCMq5wDfirIg!iTWROOhR1FIFpGtjTQg6cazQ-cPHmveCSvAgtMrFsstDAyc_S38xY4V3DJxFWhEON__DWGp221T0CZZeHZ1k$ 


# Range (cut formula) sample ----

head(Label_1_hist_main) -> test
test1 <- test$Sum_of_Gross_lbs
cut(test1, plyr::round_any(ceiling(max(Label_1_hist_main$Sum_of_Gross_lbs)),1000) / 1000,
    labels = seq(from = 1000, to = plyr::round_any(ceiling(max(Label_1_hist_main$Sum_of_Gross_lbs)),1000), by = 1000))

# Freq Table ----
janitor::tabyl(Label_1_hist_main$bins, sort = TRUE) -> cum


# Table visualiziging using gt() + 5 rating star (bonus) ----
data_set %>%
  dplyr::mutate(rating = cut_number(col_1, n = 5) %>% as.numeric()) %>%
  dplyr::mutate(rating = purrr::map(rating, rating_stars)) %>%
  dplyr::arrange(desc(col_1)) %>%
  gt::gt() %>%
  gt::tab_header(title = gt::md("__Main Title__")) %>%
  gt::tab_spanner(
    label = gt::html("<small>Relationship Strength (Sub Title)</small>"),
    columns = gt::vars(col_2, col_1, col_3, col_4)
  ) %>%
  gt::fmt_number(columns = gt::vars(col_1, col_1.1)) %>%
  gt::fmt_number(columns = gt::vars(col_4), decimals = 3) %>%
  gt::tab_style(
    style = gt::cell_text(size = px(12)),
    locations = gt::cells_body(
      columns = gt::vars(col_1, col_1.1, col_4))
  ) %>%
  cols_label(
    manufacturer = gt::md("__New_col_Name__"),
    nobs = gt::md("__New_col_Name__"),
    r.squared = gt::html(glue::glue("<strong>New_col_Name ", fontawesome::fa("arrow-down", fill = "orange"), "</strong>")),
    adj.r.squared = gt::md("__New_col_Name__"),
    p.value = gt::md("__New_col_Name__"),
    rating  = gt::md("__New_col_Name__")
  )


#########################################################################################################
############################################### Graph ###################################################
#########################################################################################################

# How to call a dataset from the package
utils::data(gapminder, package = "gapminder")
utils::data("stackoverflow", "car_prices", "Sacramento", package = "modeldata")

# How to set view section divide by ----
par(mfrow = c(2,2))  # first 2 is nrow, second 2 is ncol

# How to tilt your xaxis or yaxis label ----
ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Point plot, shape and size ----
# sample
ggplot2::ggplot(data, mapping = aes(x = xxx, y = yyy)) +
  ggplot2::geom_point(shape = 21, size = 1.5)

# Using scale_shape_manual, scale_color_brewer ----
# sample
ggplot2::ggplot(data, mapping = aes(x = xxx, y = yyy)) +
  ggplot2::geom_point(shape = 21, size = 1.5) +
  ggplot2::scale_shape_manual(values = c(1, 2)) +
  ggplot2::scale_color_brewer(palette = "Set1")

# Using scale_color_gradient
# sample
ggplot2::ggplot(data, mapping = aes(x = xxx, y = yyy)) +
  ggplot2::geom_point(shape = 21, size = 1.5) +
  ggplot2::scale_color_gradient(low="gray",high="purple")


# Box plot ----
  ggplot2::geom_boxplot() +
  ggplot2::coord_cartesian(ylim=c(-10, 10)) + 
  ggplot2::labs(title = 'Box Plot-revenue by year', caption = "https://urldefense.com/v3/__http://www.data.com__;!!EbZCMq5wDfirIg!jYKkBqb8NxW0toMgAH8iTNdAGgPGZZzLIM0HeIiENNagMP70VePH-qRJs-zp9KU2hbaOOBfM3IE1SqCyd-ej$ ")

# box plot grouping ----
ggplot2::ggplot(data = data, mapping = aes(x = xx, y = yy)) +
  ggplot2::geom_boxplot(mapping = aes(group = zz))

# box plot, outlier
ggplot2::ggplot(data = data, mapping = aes(x = xx, y = yy)) + 
  ggplot2::geom_boxplot(outlier.size = 1.5, outlier.alpha = .5, outlier.fill = "blue", outlier.shape = 21,
                        outlier.color = "red")

# boxplot, single group (use x = 1)
ggplot2::ggplot(data = data, mapping = aes(x = 1, y = yy)) +
  ggplot2::geom_boxplot()

# boxplot reorder 
ggplot2::ggplot(data = mpg, mapping = aes(x = reorder(class, hwy, FUN = median),
                                          y = hwy)) +
  ggplot2::geom_boxplot()

# Violin plot ----
ggplot2::ggplot(df1, aes(x=factor(year), y=revenue)) + 
  ggplot2::geom_violin() + 
  ggplot2::labs(title = 'Violin Plot-revenue by year') + 
  ggplot2::coord_cartesian(ylim=c(-10,10)) +
  ggplot2::stat_summary(fun = median, geom ='point', size = 2, color = 'red') + 
  ggplot2::stat_summary(fun = mean, geom ="point", shape = 23, size = 2)

# Violin plot + box plot
ggplot2::ggplot(data = data, mapping = aes(x = x, y = y)) +
  ggplot2::geom_violin() +
  ggplot2::geom_boxplot(width = 0.1, fill = "black", outlier.color = NA) +
  ggplot2::stat_summary(fun.y = median, geom = "point", fill = "white", shape = 21, size = 2.5)


# Histogram ----
ggplot(df1, aes(x=revenue)) + 
  geom_histogram(binwidth = .25) +
  coord_cartesian(xlim = c(-50, 50), ylim = c(0, 5000)) +
  labs(y = quote(f(x) == x^3), title = 'Histogram of Revenue', subtitle = "this is subtitle")  
# quote function displays mathmatical sign


# Histogram with Density Line and vline for mean or median
ggplot2::ggplot(data = mpg, mapping = aes(x = displ)) +
  ggplot2::geom_vline(mapping = aes(xintercept = mean(displ)), 
                      color = "blue",
                      linetype = "dashed", size = 1.5) +
  ggplot2::geom_vline(mapping = aes(xintercept = median(displ)), 
                      color = "red",
                      linetype = "dashed", size = 1.5) +
  ggplot2::geom_histogram(binwidth = 1, 
                          mapping = aes(y = ..density..), 
                          color = "black", fill = "white") +
  ggplot2::geom_density(alpha = 0.4, fill = "red") +
  ggplot2::labs(title = "Histogram to Show Density of displ",
                caption = "blue dotted: avg
                          red dotted: median") +
  ggplot2::theme_minimal()



# Histogram documentation ----
diamonds %>% 
  dplyr::count(cut_width(carat, 0.5))

# Histsogram with two or more groups in one plot
ggplot2::ggplot(data = diamonds, mapping = aes(x = carat, fill = cut, color = cut)) +
  ggplot2::geom_histogram(position = "identity", alpha = 0.5) +
  ggplot2::theme_bw()
  

# multiple histogram with frepoly (lines) ----
diamonds %>% 
  ggplot2::ggplot(mapping  = aes(x = carat)) +
  ggplot2::geom_freqpoly(mapping = aes(color = cut),
                         binwidth = 0.1) +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::theme_bw()

# another way to show multiple histogram ----
# feel free to add scale_color_brewer
diamonds %>% 
  ggplot2::ggplot(mapping = aes(x = carat)) +
  ggplot2::geom_histogram(mapping = aes(fill = cut),
                          position = "identity",
                          alpha = 0.4)


# Density Curve
ggplot2::ggplot(data = data, mapping = aes(x = x)) +
  ggplot2::geom_line(stat = "density")

# advanced density curve with adjust (more detail)
ggplot2::ggplot(data = data, mapping = aes(x = x)) +
  ggplot2::geom_line(stat = "density") +
  ggplot2::geom_line(stat = "density", adjust = .25, color = "red") +
  ggplot2::geom_line(stat = "density", adjust = 2, color = "blue")

# advanced density curve with fill
ggplot2::ggplot(data = diamonds, mapping = aes(x = table)) +
  ggplot2::geom_density(fill = "blue", alpha = 0.2, color = NA)


# Histogram with density  # y = ..density..
ggplot2::ggplot(data = diamonds, mapping = aes(x = price, y = ..density..)) +
  ggplot2::geom_histogram(binwidth = 1000, fill = "cornsilk", color = "grey60", size = 0.2) +
  ggplot2::geom_density()

# Scatter chart ----
ggplot(df1, aes(x=revenue, y=gp_margin)) + 
  geom_point() + 
  coord_cartesian(xlim=c(0, 30), ylim=c(-1,1)) + 
  labs(title='Scatter Plot of revenue & gross profit margin')


# Scatter plot combination with library(hexbin)
ggplot2::ggplot(df1, mapping = aes(x = x, y = y)) +
  ggplot2::stat_binhex() +
  ggplot2::scale_fill_gradient(low = "lightblue", high = "red", limits = c(0, 8000))

# Col chart 1 ----
ggplot2::ggplot(df1, aes(x=factor(year), y=revenue)) + 
  ggplot2::geom_col() + 
  ggplot2::labs(title = 'Bar Plot-revenue by year')

# Col chart 2 ----
ggplot2::ggplot(month, aes(x=month, y=sum_revenue)) + 
  ggplot2::geom_col(aes(fill = month)) +
  ggplot2::scale_y_continuous(breaks = seq(0, 700000, 100000)) +
  ggplot2::labs(title = "Sum of Revenue by Month")

# Line chart ----
# It's important to have group in line plot
ggplot2::ggplot(year_month, aes(x = month, y = sum_revenue, color = factor(year))) +
  ggplot2::geom_line(aes(group = factor(year))) +
  ggplot2::geom_point()


# Line chart with factor ----

# Don't forget to use (group = 1) in aes. 
ggplot2::ggplot(data, mapping = aes(x = Time, y = demand, group = 1)) +
  ggplot2::geom_line()


# Line type set ----
# data: library(plyr)  
plyr::ddply(ToothGrowth, c("supp", "dose"), summarise, length = mean(len))

ToothGrowth %>%
  dplyr::group_by(supp, dose) %>% 
  dplyr::summarise(length = mean(len)) %>% 
  ggplot2::ggplot(mapping = aes(x = dose, y = length)) +
  ggplot2::geom_line(mapping = aes(linetype = supp, color = supp)) 



# Time Series with group_by ----
raw_data %>%
  dplyr::group_by(whatever_you_want_to_group) %>%
  timetk::plot_time_series(
    .date_var    = Date(example),
    .value       = Weekly_Sales(example),
    .color_var   = id(recommended_you_grouped),
    .smooth      = TRUE(trend_line),
    .facet_ncol  = 2(recommended),
    .interactive = FALSE(recommended)
  ) +
  ggplot2::theme_minimal() 

# Comparison Line Graph & Time Series group by facet ----
raw_data %>%
  ggplot2::ggplot(aes(x = Date(example), y = Weekly_Sales(example), color = id(example), group = id(example))) +
  ggplot2::geom_line() +
  # up to here.. comparison line graph
  ggplot2::geom_smooth(
    aes(x = Date, y = Weekly_Sales),
    inherit.aes = FALSE,
    se = FALSE
  ) +
  ggplot2::facet_wrap(
    facets = ~ id,
    scales = "free_y",
    ncol   = 2
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")

# Line graph
# How to avoid overlapping

ggplot2::ggplot(data = data, mapping = aes(x = x, y = y, shale = z)) +
  ggplot2::geom_line(position = position_dodge(0.2)) +           # dodge lines by 0.2
  ggplot2::geom_point(position = position_dodge(0.2), size = 4)  # dodge points by 0.2


# Line graph
# how to format your line in style
ggplot2::ggplot(data = data, mapping = aes(x = x, y = y)) +
  ggplot2::geom_line(linetype = "dashed", size = 1, color = "blue")

# Stacked bar graph ----
# simply..

diamonds %>% 
  ggplot2::ggplot() +
  ggplot2::geom_bar(mapping = aes(x = cut, fill = clarity))

# Dodged bar graph ----
diamonds %>% 
  ggplot2::ggplot() +
  ggplot2::geom_bar(mapping = aes(x = cut, fill = clarity),
                    position = "dodge")

# Fill stacked bar graph ----
diamonds %>% 
  ggplot2::ggplot() +
  ggplot2::geom_bar(mapping = aes(x = cut, fill = clarity),
                    position = "fill")

# bar graph turn into sized pie graph using coord_polar() ----
ggplot2::ggplot(data = diamonds) +
  ggplot2::geom_bar(mapping = aes(x = cut, fill = cut),
                    show.legend = FALSE,
                    width = 1) +
  ggplot2::theme(aspect.ratio = 1) +   # how to adjust the ratio
  ggplot2::labs(x = NULL, y = NULL) +   # how to clear x and y lab
  ggplot2::coord_polar()

# How to use reorder in Bar chart ----
data %>% 
  ggplot2::ggplot(mapping = aes(x = reorder(x, y), y = y)) +   # Reorder. how to use
  ggplot2::geom_bar(stat = "identity",
                    mapping = aes(fill = x2),
                    scale_fill_manual(values = c("#669933", "#FFCC66")))  # Use R Color Cheat Sheet


# Bar chart width adjust (especially for dodge chart) ----
ggplot2::ggplot(data = cabbage_exp) +
  ggplot2::geom_bar(mapping = aes(x = Date, y = Weight, fill = Cultivar),
                    stat = "identity",
                    position = position_dodge(0.7),
                    width = 0.5)

# (Example)
data %>%
  dplyr::select(-percent_of_below_ss) %>% 
  tidyr::pivot_longer(2:4, names_to = "Attribute", values_to = "value") %>% 
  ggplot2::ggplot(aes(x = Planner_Category, y = value, fill = Attribute)) +
  geom_bar(stat = "identity", position = "dodge") +  # position could be "stack"
  theme_classic()


# labeling to the bar graph ----
ggplot(data=dat, aes(x=Types, y=Number, fill=sample)) +
  geom_bar(position = 'dodge', stat='identity') +
  geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25)

# Two graphs display together ----
gridExtra::grid.arrange(figure1, figure2, ncol = 1)

# Same result only different data ----
# (example)

a <- ggplot2::ggplot(data1, aes(x = aaa, y = bbb)) +
  ggplot2::geom_bar(stat = "identity") + xlab ("")

b <- a %+% data2

# ggplot, continouse color & title format (size) ----
g1 <- mtcars %>%
  ggplot2::ggplot(aes(x = disp, y = mpg, color = cyl)) +
  ggplot2::geom_point(size=2) +
  ggplot2::scale_color_continuous(limits=c(0,8)) +
  ggplot2::ggtitle("mtcars: Displacement vs mpg vs # of cylinders") +
  ggplot2::theme(title = element_text(size=8),
                 text = element_text(size=12))


# How to color using scale_fill_brewer (R Cheatsheet for "RColorBrewer") ----
ggplot2::ggplot(data = diamonds) +
  ggplot2::geom_bar(mapping = aes(x = cut, fill = clarity),
                    position = "stack") +
  ggplot2::scale_fill_brewer(palette = "Pastel1")  # also look at R Cheatsheet for "RColorBrewer"


# Using geom_smooth ----
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = factor(cyl))) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm", se = FALSE)

# How to filp the graph ----
ggplot2::ggplot(data = mpg) +
  ggplot2::geom_boxplot(mapping = aes(x = class, y = hwy)) +
  ggplot2::coord_flip()

# How to flip the legend ----
ggplot2::ggplot(data = data) +
  ggplot2::geom_bar(mapping = aes(x = x, y = y, fill = fill),
                    stat = "identity",
                    guides(fill = guide_legend(reverse = TRUE)))

# How to flip stacked bar graph order ----
ggplot2::ggplot(data = data) +
  ggplot2::geom_bar(mapping = aes(x = x, y = y, fill = fill, order = desc(fill)),
                    stat = "identity")

# How to draw map using geom_polygon ----

map_data("state") -> us

ggplot2::ggplot(data = us) +
  ggplot2::geom_polygon(mapping = aes(x = long, y = lat, group = group),
                        fill = "white", color = "black") +
  ggplot2::coord_quickmap() +  # ratio adjustment
  ggraph::theme_graph()

# color by states 
ggplot2::ggplot(data = us) +
  ggplot2::geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = region),
                        show.legend = FALSE) +
  ggraph::theme_graph()
  

# How to get abline with geom ----
ggplot2::ggplot(data = mpg, mapping = aes(x = cty, y = hwy)) +
  ggplot2::geom_point() +
  ggplot2::geom_abline() +
  ggplot2::coord_fixed()  # this is important to have with abline()


# How to get two criteria combined x axis ----
library(gcookbook)
ggplot2::ggplot(data = cabbage_exp, 
                mapping = aes(x = interaction(Date, Cultivar), y = Weight)) +
  ggplot2::geom_bar(stat = "identity")


# How to label to a bar chart ----
library(gcookbook)
ggplot2::ggplot(data = cabbage_exp,
                mapping = aes(x = interaction(Date, Cultivar), y = Weight)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_text(mapping = aes(label = Weight), vjust = 1.5, color = "white")


# How to get your graph skin looking more like.. bold (good for line graph) ----
ggplot2::ggplot(data = dataset, mapping = aes(x = x, y = y)) +
  ggplot2::geom_line() +
  ggplot2::geom_hline(yintercept = 0, color = "gray50") +
  ggplot2::theme_minimal() -> g

plotly::ggplotly(g)


# Dot rank plot ----
# data - library(gcookbook)
tophitters2001 %>% 
  dplyr::slice_head(n = 25) %>% 
  ggplot2::ggplot(mapping = aes(x = avg, y = reorder(name, avg))) +
  ggplot2::geom_point(size = 3) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(color = "grey60", linetype = "dashed")) +
  geom_text(mapping = aes(label = avg),
            color = "black",
            size = 3.5,
            hjust = -0.2) +
  coord_cartesian(xlim = c(0.31, 0.354))


# Dot rank plot #2 ----
# library(gcookbook)
tophitters2001 %>% 
  dplyr::slice_head(n = 25) %>% 
  dplyr::select(name, lg, avg) -> tophitter

nameorder <- tophitter$name[order(tophitter$lg, tophitter$avg)]
tophitter$name <- factor(tophitter$name, levels = nameorder)

tophitter %>% 
  ggplot2::ggplot(mapping = aes(x = avg, y = name)) +
  ggplot2::geom_segment(mapping = aes(yend = name), xend = 0, color = "grey50") +
  ggplot2::geom_point(size = 3, mapping = aes(color = lg)) +
  ggplot2::scale_color_brewer(palette = "Set1", limits = c("NL", "AL")) +
  ggplot2::theme_bw() +
  ggplot2::theme(panel.grid.major.y = element_blank(),
                 legend.position = c(1, 0.55),
                 legend.justification = c(1, 0.5)) +
  geom_text(mapping = aes(label = avg),
            color = "black",
            size = 3.5,
            hjust = -0.2) +
  coord_cartesian(xlim = c(0.31, 0.354))



tophitter %>% 
  ggplot2::ggplot(mapping = aes(x = avg, y = name)) +
  ggplot2::geom_segment(mapping = aes(yend = name), xend = 0, color = "grey50") +
  ggplot2::geom_point(size = 3, mapping = aes(color = lg)) +
  ggplot2::scale_color_brewer(palette = "Set1", limits = c("NL", "AL"), guide = FALSE) +
  ggthemes::theme_igray() +
  ggplot2::theme(panel.grid.major.y = element_blank()) +
  ggplot2::facet_grid(lg ~ ., scales = "free_y", space = "free_y") +
  geom_text(mapping = aes(label = avg),
            color = "black",
            size = 3.5,
            hjust = -0.2) +
  coord_cartesian(xlim = c(0.31, 0.354))



# geom_path plot 
mtcars %>% 
  ggplot2::ggplot(mapping = aes(x = mpg, y = wt)) +
  ggplot2::geom_path() +
  ggplot2::theme_bw()-> a

plotly::ggplotly(a)


# scale_color_manual sample, laps sample
ggplot2::ggplot(fig115, mapping = aes(x = seniority, y = all_pass))+
  geom_jitter(mapping = aes(color = party)) +
  labs(x = "Seniority", y = "Bills Passed", title = "Seniority and Bills Passed in the 115th Congress") +
  scale_color_manual(values = c("blue","red"))


# scale_fill_grey
ggplot2::scale_fill_grey(start = 0.25, end = 0.75)

# scale_fill_manual
ggplot2::scale_fill_manual(values = c("red", "green", "blue"))

# scale_fill_hue (This is pretty good color set, try different numbers)
ggplot2::scale_fill_hue(c = 40)


# How to change your axis title when your axis is grouped (fill or color) 
# if the legend is set by fill.. you should do with fill in labs
cel %>% 
  dplyr::filter(congress == 115) %>% 
  dplyr::mutate(female = recode(female, "1" = "female", "0" = "male")) %>% 
  ggplot2::ggplot(mapping = aes(x = dwnom1, y = all_pass)) +
  ggplot2::geom_point(mapping = aes(color = female)) +
  ggplot2::labs(x = "Ideology", y = "Bills Passed", color = "Gender")   # here, you changed the axis title

# control your legend position
g +
  ggplot2::theme(legend.position = c(0.7, 0.4))

g +
  ggplot2::theme(legend.position = "none")  # or "bottom", "right", "left", "right", "top" 

# control your legend color, background color
g +
  ggplot2::theme(legend.background = element_blank()) +
  ggplot2::theme(legend.key = element_blank())


# removing x axis or y axis using labs
g +
  ggplot2::labs(x = NULL, y = NULL)    # x = "" would do it but still space remained

# Legend title change
ggplot2::ggplot(data = cces_3, mapping = aes(x = income, fill = race_2)) +
  ggplot2::geom_histogram(stat = "count", position = "dodge") +
  ggthemes::theme_stata() +
  ggplot2::scale_fill_hue("Race")  # <- here. if your legend is colored, not filled, use scale_color_hue("new title")

# tile plot
diamonds %>% 
  dplyr::count(color, cut) %>% 
  ggplot2::ggplot(mapping = aes(x = color, y = cut)) +
  ggplot2::geom_tile(mapping = aes(fill = n))


# How to work on percentage
# for labeling - create another column in your data 
data %>% 
  dplyr::mutate(percentage_2 = sprintf("%1.0f%%", 100*percentage)) %>% 
  ggplot2::ggplot() + #do whatever you need 
    ggplot2::geom_text(mapping = aes(label = percentage_2))

# How to layout % in x axis or y axis
g +
  ggplot2::scale_y_continuous(labels = scales::percent) # or
  ggplot2::scale_x_continuous(labels = scales::percent) # or
  ggplot2::scale_x_continuous(labels = scales::dollar)


  
## Correlation Plot
###########  Using GGally::ggpairs  : Creating Correlation chart at once  (Customizing options included below)
  ## All of your data should be numeric
GGally::ggpairs(df)
  
my_scatter <- function(data, mapping){
  ggplot(data = data, mapping = mapping)+
    geom_jitter(color = "lightblue")
}
#### Write your own function for the density plot
my_density<- function(data, mapping){
  ggplot(data = data, mapping = mapping)+
    geom_density(alpha = 0.05,
                   fill="lightblue")
}
##### substitute your functions for the functions that ggpairs() uses to draw the figures
GGally::ggpairs(df,
        lower=list(continuous = my_scatter),
        diag=list(continuous = my_density))


# correlation plot using ggcorrplot::ggcorrplot
df<- cces %>% select("educ","pid7","pew_religimp")
r<-cor(df,use="complete.obs")
ggcorrplot::ggcorrplot(r)

#### modify some visual elements
ggcorrplot(r,
           type="lower",
           title="Correlations",
           ggtheme=theme_igray())


######## Area plot
# sample data: gcookbook
ggplot2::ggplot(data = uspopage, mapping = aes(x = Year, y = Thousands, fill = AgeGroup)) +
  ggplot2::geom_area(color = "black") +
  ggplot2::scale_fill_brewer(palette = "Blues")

# Area chart reverse stack 
# sample data: gcookbook
ggplot2::ggplot(data = uspopage, mapping = aes(x = Year, y = Thousands, fill = forcats::fct_rev(AgeGroup))) +
  ggplot2::geom_area(color = "black") +
  ggplot2::scale_fill_brewer(palette = "Greens", "New_Legend_Name") # How to change legend name  


# legend flip anytime 
mapping = aes(fill = forcats::fct_rev())



### dumbbell chart  (This is basically, showing min and max between two time frame)
####Create fake sample data
utils::data(gapminder, package = "gapminder")

gapminder %>% 
  dplyr::filter(continent == "Americas" &
                  year %in% c(1952, 2007)) %>% 
  dplyr::select(country, year, lifeExp) %>% 
  tidyr::spread(year, lifeExp) %>% 
  dplyr::rename(y1952 = "1952",
                y2007 = "2007") -> gapminder_tidy

# dumbbell plot with ggalt
ggplot2::ggplot(data = gapminder_tidy, mapping = aes(y = reorder(country, y1952), 
                                                     x = y1952, 
                                                     xend = y2007)) +
  ggalt::geom_dumbbell(size = 1.2,
                       size_x = 3,
                       size_xend = 3,
                       color = "grey",
                       colour_x = "blue",
                       colour_xend = "red") +
  ggplot2::labs(title = "Change in Life Expectancy",  
                subtitle = "1952 to 2007",
                x = "Life Expectancy (years)", 
                y = "Person") +
  ggthemes::theme_igray() 
  


# Waterfall plot using waterfalls
# sample data
category <- c("Sales", "Services", "Fixed Costs", "Variable Costs", "Taxex")
amount <- c(101000, 52000, -23000, -15000, -10000)
income <- tibble::tibble(category, amount)

income %>% 
  waterfalls::waterfall(calc_total = TRUE,
                        total_axis_text = "Net",
                        total_rect_text_color = "black",
                        total_rect_color = "goldenrod1") +
  ggplot2::scale_y_continuous(label = scales::dollar) +
  ggplot2::labs(title = "West Coast Profit and Loss",
                subtitle = "Year 2017",
                y = NULL,
                x = NULL) +
  ggthemes::theme_igray()


# Packed circle plot
# Use congress data as an example
# Sample a number of members from the 114th Congress
# Sample Data: cel - Data Visualization course
cel %>%
  dplyr::filter(congress ==114) %>% 
  dplyr::sample_n(25) -> members

# circleProgressiveLayout automatically creates a data frame with xaxis and yaxis centers for circles and radius, based on the values for the data you feed it.
packcircles::circleProgressiveLayout(members$all_pass, sizetype = 'area') -> packing

cbind(members, packing) -> members

####Provides more points for ggplot to draw the perimeters of the circles
packcircles::circleLayoutVertices(packing, npoints = 50) -> dat.gg

####combine the circle data with text data
ggplot2::ggplot() + 
  ggplot2::geom_polygon(data = dat.gg,
                        aes(x = x,
                            y = y,
                            group = id,
                            fill = as.factor(id),
                            alpha=0.6)) +
  ggplot2::geom_text(data = members,
                     aes(x=x,
                         y=y,
                         size = all_pass,
                         label = thomas_name)) +
  ggplot2::theme(legend.position = "none") +  # How to remove the legend
  ggplot2::coord_equal()  # How to do square ratio.


  
## Survival plot
utils::data(lung, package = "survival")

sfit <- survival::survfit(Surv(time, status) ~ sex, data = lung)
survminer::ggsurvplot(sfit,
                      conf.int = TRUE,
                      pval = TRUE)



# Slope plot (ranking by time change)
# sample data library(CGPfunctions)
gapminder %>% 
  dplyr::filter(year %in% c(1992, 1997, 2002, 2007) & 
                  country %in% c("Panama", "Costa Rica", "Nicaragua", "Honduras",
                                 "El Salvador", "Guatemala", "Belize")) %>% 
  dplyr::mutate(year = factor(year),
                lifeExp = round(lifeExp)) -> slope


CGPfunctions::newggslopegraph(slope, year, lifeExp, country) +
  ggplot2::labs(title = "Life Expectancy by Country",
                subtitle = "Central America",
                caption = "source: gapminder")



# Tree map / Heat map 
library(treemap)
treemap::treemap(diamonds,
                 # data
                 index = "cut",    # what do you want to measure?
                 vSize = "carat",  # value has to be numeric
                 type = "index",
                 
                 # Main
                 title = "",
                 palette = "Dark2",
                 
                 # Borders
                 border.col = c("black"),
                 border.lwds = 1,
                 
                 # Labels
                 fontsize.labels = 5,
                 fontcolor.labels = "white",
                 fontface.labels = 2,
                 align.labels = c("left", "top"),
                 overlap.labels = 0.5,
                 inflate.labels = TRUE)  # font size being affected by the rectangles size


# geom_rug  (How to elaborate your x, y axis (barcode looking frequency))
ggplot2::ggplot(data = mtcars, mapping = aes(x = mpg, y = disp)) +
  ggplot2::geom_point() +
  ggplot2::geom_rug(position = "jitter", size = 0.2)


# How to style dollar in the label and x or y axis
# sample data
load("C:/Users/sanle/OneDrive/R/Work/My Libraries/mylibraries/example_data/largest_companies_dt.rds")

largest_companies_dt %>%
  dplyr::mutate(V4 = readr::parse_number(V4),
                V2 = as_factor(V2) %>% forcats::fct_rev()) %>%
  data.frame() %>%
  ggplot2::ggplot(mapping = aes(x = V4, y = V2)) +
  ggplot2::geom_col(mapping = aes(fill = V4)) +
  ggplot2::geom_label(mapping = aes(label = scales::dollar_format()(V4)), hjust = 1) +     # Here, dollar_format
  ggplot2::theme_minimal() +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +   # Here again, dollar_format
  ggplot2::labs(title = "Revenue (Millions) for Largest Companies") +
  ggplot2::theme(legend.position = 'none')



# Ridgeline Plot
# sample data
load("C:/Users/sanle/OneDrive/R/Work/My Libraries/mylibraries/example_data/txhousing_tbl.rds")

txhousing_tbl %>%
  tidyr::drop_na() %>%
  dplyr::mutate(city = factor(city) %>% forcats::fct_reorder(median) %>% forcats::fct_rev()) %>%  # how to reorder your plot by the value
  dplyr::filter(as.numeric(city) %in% (1:10)) %>%   # how many county do you want? like top 10 (how to do top 10)
  
  ggplot2::ggplot(mapping = aes(x = median, y = fct_rev(city))) +
  ggridges::geom_density_ridges(color = "#18BC9C",        #  this plot is really good to compare the value
                               fill  = "gray10",
                               alpha = 0.75,
                               size  = 1) +
  ggplot2::scale_x_continuous(labels = scales::dollar_format()) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "Median Home Price", y = "", title = "Top 10 Cities by Median Home Price")


# Change Legend Name
# use labs(fill = , color = , size = )

# Changing the Apperance of a Legend Title
# example

p + ggplot2::theme(legend.title = element_text(face = "italic",
                                               family = "Times",
                                               color = "red",
                                               size = 14))


# ggside sample
load("nobel.rds")

nobel %>% 
  ggplot2::ggplot(mapping = aes(x = year, y = age)) +
  ggplot2::geom_point(mapping = aes(color = gender, shape = category),
                      size = 2,
                      alpha = 0.5) +
  ggplot2::annotate("rect", xmin = 2012, xmax = 2020, ymin = 20, ymax = 30, alpha = 0.2, fill = "red") +
  ggplot2::annotate(geom = "curve", x = 2000, xend = 2010, y = 15, yend = 20, size = 1.5,
                    curvature = 0.2, arrow = arrow(length = unit(2, "mm"))) +
  ggplot2::annotate(geom = "text", x = 1990, y = 15, label = "First female 20's Nobel Prize in peace category", color = "red") +
  ggplot2::geom_rug(position = "jitter", size = 0.2, alpha = 0.3) +
  ggside::geom_xsidedensity(mapping = aes(y = ggplot2::after_stat(density), fill = category),
                            alpha = 0.5,
                            position = "stack") +
  ggside::geom_ysidefreqpoly(color = "red") +
  ggside::geom_ysidehistogram(fill = "blue",
                              alpha = 0.5) +
  tidyquant::scale_color_tq() +
  tidyquant::scale_fill_tq() +
  tidyquant::theme_tq() +
  ggplot2::theme(ggside.panel.scale.x = 0.4,
                 ggside.panel.scale.y = 0.4) +
  ggplot2::scale_color_manual(values = c("red","blue")) +
  ggplot2::labs(title = "Main Plot: Correlation between the Year and Age",
                subtitle = "X - Side Plot: Density Plot by Category\nY - Side Plot: Histogram by Age of Nobel Winners",
                x = "Year",
                y = "Age") +
  ggplot2::theme(plot.title = element_text(size = 12))


# How to use ggtext::geom_richtext
load("C:/Users/slee/OneDrive - Ventura Foods/Stan/R Codes/mylibraries/mylibraries/example_data/business_science_doc_to_R.rds")

business_science_doc_to_R %>%
  ggplot2::ggplot(mapping = aes(x = students, y = lectures_completed)) +
  ggplot2::geom_point(mapping = aes(size = activity_ratio)) +
  ggplot2::geom_smooth(method = "loess") +
  ggtext::geom_richtext(aes(label = str_glue("___Course: {course}___<br>Ratio: {round(activity_ratio)}")),
                        vjust = "inward", 
                        hjust = "inward", 
                        size = 3.5) +  # How to put label pulling from the data table
  ggplot2::labs(title = "Lessons Completed Vs Students", 
                x = "No. of Students", 
                y = "No. of Lessons Completed") +
  ggplot2::scale_y_continuous(label = scales::comma) +  # How to put comma in x or y axis in ggplot graph
  ggplot2::expand_limits(y = 0) +
  ggplot2::theme_minimal() 

###################################################################################################################
######################################################### map series ##############################################
###################################################################################################################

# theme: either ggplot2::theme_void() or ggraph::theme_graph() 

# Map function using leaflet 
leaflet::leaflet() %>% 
  leaflet::setView(lng = -117.8230, lat = 33.6846, zoom = 16) %>% 
  leaflet::addTiles() %>% 
  leaflet::addMarkers(lng = -117.8230, lat = 33.6846, popup = "Irvine")

###################### ggplot2::geom_polygon() version ##################

# data
maps::world.cities
map_data("world") 
map_data("state")

# For point color, use this function
ggplot2::scale_color_distiller(palette = 7)


# basic world map using geom_polygon
# sample data  library(maps)
my_world_map <- map_data("world")

ggplot2::ggplot(data = my_world_map, mapping = aes(x= long, y = lat, group = group)) +
  ggplot2::geom_polygon(fill= "white", color = "black") +
  ggplot2::theme_void()

ggplot2::ggplot(data = my_world_map, mapping = aes(x= long, y = lat, group = group)) +
  ggplot2::geom_polygon(mapping = aes(fill = region),
                        show.legend = FALSE,
                        color = "black") +
  ggplot2::theme_void()


# USA, Canada, South Korea
my_world_map %>% 
  dplyr::filter(region %in% c("USA", "Canada", "South Korea")) %>% 
  ggplot2::ggplot(mapping = aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(mapping = aes(fill = region), color = "black") +
  ggplot2::theme_void()

# region by long & lat
my_world_map %>% 
  dplyr::filter(long < -50 & long > -200 & lat < 90 & lat > 20) %>% 
  ggplot2::ggplot(mapping = aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(mapping = aes(fill = region), color = "black",
                        show.legend = FALSE) +
  ggplot2::theme_void()

# Drawing US Main Land map
library(maps)
usa <- map_data("state")

ggplot2::ggplot(data = usa, mapping = aes(x= long, y= lat, group = group)) +
  ggplot2::geom_polygon(fill = "white", color = "black") +
  ggraph::theme_graph() +
  ggplot2::labs(title="United States")
# or
ggplot2::ggplot(data = usa, mapping = aes(x= long, y= lat, group = group)) +
  ggplot2::geom_polygon(mapping = aes(fill = region),
                        show.legend = FALSE,
                        color = "black") +
  ggraph::theme_graph() +
  ggplot2::labs(title="United States")

# Drawing other country map
world <- map_data("world")
world %>% 
  dplyr::filter(region == "Japan") -> japan
 
ggplot2::ggplot(data = japan, mapping = aes(x = long, y = lat, group = group)) +
ggplot2::geom_polygon(color = "black", fill = "white") +
ggraph::theme_graph() 

# adding cities with point
maps::world.cities %>% 
  dplyr::filter(country.etc == "Japan") -> japan.cities

ggplot2::ggplot(data = japan, mapping = aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(color = "black", fill = "white") +
  ggplot2::geom_point(data = japan.cities,
                      mapping = aes(x = long, y = lat, group = NULL), 
                      alpha = 0.2) +
  ggplot2::scale_color_distiller(palette = 7) +           # this is usually used point color in conjunction with polygon
  ggraph::theme_graph() 

# let's pick just the big cities
japan.cities %>% 
  dplyr::filter(country.etc == "Japan" & pop > 500000) -> japan.big.cities

ggplot2::ggplot(data = japan, mapping = aes(x= long, y= lat, group = group)) +
  ggplot2::geom_polygon(color = "black", fill = "white") +
  ggplot2::geom_point(data = japan.big.cities, 
                      mapping = aes(x = long, y = lat, group = NULL),
                      color="blue",
                      alpha = 0.3,
                      size = 5)

# vary size of point by city size
japan.big.cities %>% 
  dplyr::mutate(qual = sample(LETTERS[1:5], nrow(japan_big_cities), replace=TRUE)) -> japan.big.cities_2 # 5 different color

ggplot2::ggplot(data = japan, 
                mapping = aes(x= long, y= lat, group = group)) +
  ggplot2::geom_polygon(color = "black", fill = "white") +
  ggplot2::geom_point(data = japan.big.cities_2,
                      mapping = aes(x = long, y = lat, group = NULL, size = pop, color = qual),
                      alpha = 0.8) +
  ggplot2::scale_size_continuous(label = comma) #library(scales) 
# this is how to get your legend from scientific number to normal number

# Labeling to the map according to your data properly

ggplot2::ggplot() +
  ggplot2::geom_polygon(data = japan, 
                        mapping = aes(x = long, y = lat, group = group), 
                        fill="grey", 
                        alpha = 1.2) +
  ggplot2::geom_point(data = japan.big.cities_2, 
                      mapping = aes(x = long, y = lat, alpha = pop)) +
  ggrepel::geom_text_repel(data = japan.big.cities_2 %>% arrange(pop) %>% tail(10), 
                           mapping = aes(x = long, y = lat, label = name), 
                           size = 5) +
  ggplot2::geom_point(data = japan.big.cities_2 %>% arrange(pop) %>% tail(10), 
                      mapping = aes(x = long, y = lat), 
                      color = "lightblue", 
                      size = 3) +
  ggplot2::theme_void() + 
  ggplot2::coord_map() +
  ggplot2::theme(legend.position = "none")



### United States Sample ###

# 1
map_data("state") -> state

ggplot2::ggplot(data = state) +
  ggplot2::geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = region),
                        fill = "white", 
                        color = "black",
                        show.legend = FALSE) +
  ggplot2::theme_void()


# 2
maps::world.cities -> city
city %>% filter(country.etc == "USA" & name %in% c("Los Angeles", "Tucson", "Portland", "Irvine", "San Diego")) %>% 
  filter(lat < 50 & lat > 0 & long < -100 & long > -130) -> cities


state %>% filter(region %in% c("california", "oregon", "arizona")) -> sample_states

ggplot2::ggplot(data = sample_states, mapping = aes(x = long, y = lat, group = group)) +
  ggplot2::geom_polygon(mapping = aes(fill = region),
                        show.legend = FALSE,
                        fill = "white",
                        color = "black") +
  ggplot2::coord_map() +
  ggrepel::geom_text_repel(data = cities,
                           mapping = aes(x = long, y = lat, group = NULL, label = name)) +
  ggplot2::geom_point(data = cities, 
                      mapping = aes(x = long, y = lat, group = NULL, color = name),
                      size = 5,
                      alpha = 0.5,
                      show.legend = FALSE) +
  ggplot2::theme_void()


## How to put the continouse color in the map
# sample data
my_world_map <- map_data("world")
countries <- unique(my_world_map$region)
set.seed(987)
some_data_values <- data.frame(
  "region"=countries,
  "Score"=runif(252,0,100))

# map
dplyr::left_join(my_world_map, some_data_values, by = "region") -> question_1
ggplot2::ggplot(data = question_1, mapping = aes(x= long, y = lat, group = group))+
  ggplot2::geom_polygon(mapping = aes(fill = Score),
                        color = "black") +
  ggplot2::scale_fill_distiller(palette = 5)




###################### simple feature version geom_sf ##################
# For U.S data   # advantage of this: Alaska and Hawaii added to US map
library(albersusa)
ggplot2::ggplot() + 
  ggplot2::geom_sf(data = usa_sf()) + 
  ggplot2::theme_void()





###################################################################################################################
############################################## labeling & emphasizing series ######################################
###################################################################################################################

# Using r markdown way of label displaying 
g +
  ggplot2::labs(x = "Axis title with *italics* and **boldface**") +
  ggplot2::theme(axis.title.x = ggtext::element_markdown())

# put your label inside (use inward)
g +
  ggplot2::geom_text(mapping = aes(label = "a", "b", "c", "d"),
                     vjust = "inward",
                     hjust = "inward")

# labeling with paste0
g +
  ggplot2::geom_text(mapping = aes(label = paste0("(", "aaa", ")")),
                     nudgy_y = -5)   # this avoids overlapping with your data and your label

# when you have overlapped label
g +
  ggplot2::geom_text(mapping = aes(label = sample_label),
                     check_overlap = TRUE)



# using geom_rect(), time serise plot with points, rectangle, label (this is awesome)
presidential <- subset(presidential, start > economics$date[1])  # filtering referencing completely other data set,
# matching data range with two data. 


ggplot2::ggplot(economics) +
  ggplot2::geom_line(mapping = aes(x = date, y = unemploy)) +    # this is main plot with main ggplot2 data
  ggplot2::geom_rect(data = presidential,                      # How to put other data into original ggpolt data
                     mapping = aes(xmin = start, xmax = end, fill = party),
                     ymin = -Inf, ymax = Inf, alpha = 0.2)   +  
  ggplot2::geom_vline(data = presidential,                     # How to put other data into original ggpolt data
                      mapping = aes(xintercept = as.numeric(start)),
                      color = "grey50",
                      alpha = 0.5) +
  ggplot2::geom_text(data = presidential,                     # How to put other data into original ggpolt data
                     mapping = aes(x = start, y = 1000, label = name),
                     size = 3, vjust = 0, hjust = 0, nudge_x = 50,
                     check_overlap = TRUE) +
  ggplot2::scale_fill_manual(values = c("blue", "red")) +
  ggplot2::labs(x = "Date", y = "Unemployment", title = "Unemployment rate over time", 
                subtitle = "Unemployed rate by Presidents in the United States")


# labeling by using dplyr::filter  # ver 1
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = dplyr::filter(mpg, manufacturer == "subaru"),
                      color = "red",
                      size = 3,
                      shape = 24,
                      fill = "red") +
  ggplot2::annotate(geom = "point", x = 5.5, y = 40, color = "red", size = 3, shape = 24, fill = "red") +
  ggplot2::annotate(geom = "text", x = 5.6, y = 40, label = "Subaru", hjust = "left", size = 10)


# labeling by using dplyr::filter  # ver 2
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  ggplot2::geom_point() +
  ggplot2::geom_point(data = dplyr::filter(mpg, manufacturer == "subaru"),
                      color = "red",
                      size = 3,
                      shape = 24,
                      fill = "red") +
  ggplot2::annotate(geom = "curve", x = 4.5, xend = 2.65, y = 35, yend = 27, size = 2,
                    curvature = 0.5, arrow = arrow(length = unit(4, "mm"))) +
  ggplot2::annotate(geom = "text", x = 4.5, y = 35, label = "Subaru", hjust = "left", size = 8)

# labeling text overlapping solution with ggrepel
# drawing rect in the plot to emphasize
# inserting text in a plot to explain
# sample data: Data Visualization - Course 2 - Week 3

cel %>% 
  dplyr::filter(congress == 115) %>% 
  ggplot2::ggplot(mapping = aes(x = dwnom1, y = all_pass))+
  ggplot2::geom_point()+
  ggrepel::geom_text_repel(data = dplyr::filter(cel, congress == 115 & all_pass > 8),
                           mapping = aes(x = dwnom1, y= all_pass, label = thomas_name)) +   
  # above: make sure to match with your main ggplot
  
  ggplot2::annotate("rect", xmin = 0.08, xmax = 0.4, ymin = 13, ymax = 15, alpha = 0.2, fill = "red") +
  ggplot2::annotate("text", x = 0.52, y = 14, label = "Most Passed", color = "red")


# Direct labeling insidse of the plot
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  ggplot2::geom_point(mapping = aes(color = class),
                      show.legend = FALSE) +
  directlabels::geom_dl(mapping = aes(label = class, color = class), 
                        method = "smart.grid")


# Labeling by circling using ggforce::geom_mark_ellipse
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  ggplot2::geom_point(mapping = aes(color = class)) +
  ggforce::geom_mark_ellipse(mapping = aes(label = cyl, group = cyl))

# labeling and emphasizing using gghighlight::gghighlight
utils::data(Oxboys, package = "nlme")
ggplot2::ggplot(data = Oxboys, mapping = aes(x = age, y = height)) +
  ggplot2::geom_line(mapping = aes(group = Subject, color = Subject)) +
  ggplot2::geom_point() +
  gghighlight::gghighlight(Subject %in% 1:3)

# emphasizing point color using gghighlight with facet_wrap
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
  ggplot2::geom_point(mapping = aes(color = factor(cyl))) +
  gghighlight::gghighlight() +
  ggplot2::facet_wrap(vars(cyl))



###################################################################################################################
################################################## ggplot2::theme()  series #######################################
###################################################################################################################

library(hrbrthemes) # there are many good themes here
library(ggthemes)
plotly::ggplotly(g)  # This makes plot in a nicer way.. pretty much always. 


# Using ggthemes package (there are more in the package)
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class))+
  ggplot2::geom_point()+
  ggthemes::theme_igray()    # recommended



# theme(plot.) series
ggplot2::ggplot(data = mpg, mapping = aes(x = displ, y = hwy, color = class))+
  ggplot2::geom_point()+
  ggthemes::theme_igray() +
  ggplot2::labs(title = "This is Title",
                subtitle = "This is subtitle",
                x = "x axis",
                y = "y axis") -> g

# For main plot and panel
g +
  ggplot2::theme(plot.title = element_text(size = 18, face = "bold", color = "red", hjust = 0.5,
                                           margin = margin(t = 10, b = 10)),
                 plot.subtitle = element_text(face = "italic"),
                 axis.title.y = element_text(margin = margin(r = 10), color = "white", angle = 30, vjust = 0.5, hjust = 0,
                                             size = 15),
                 axis.title.x = element_text(margin = margin(r = 10), color = "yellow", angle = 30, size = 20),
                 panel.grid.major = element_line(color = "grey", size = 2, linetype = "dotted"),
                 plot.background = element_rect(fill = "black", color = "red", size = 2),
                 panel.background = element_rect(fill = "linen"),
                 panel.grid.minor = element_blank(),
                 axis.line = element_line(color = "grey50"))


# For Legend
  ggplot2::theme(legend.background = element_rect(fill = "lemonchiffon", color = "grey50", size = 1),
                 legend.key = element_rect(color = "red"),
                 legend.key.width = unit(0.4, "cm"),
                 legend.key.height = unit(0.75, "cm"),
                 legend.text = element_text(size = 15),
                 legend.title = element_text(size = 15, face = "bold"))

# For ratio
g + 
  ggplot2::theme(aspect.ratio = 9 / 16)  # Wide (9 / 16), Long and Skinny (2 / 1), Square (1)

g + 
  ggplot2::coord_equal()  # for square



# How to enter new line in the title of the graph
ggplot2::labs(title = "title 1st line\nof 2nd line")  # don't forget to add \n

# How to put the title inside of the graph
ggplot2::labs(title = "This is title") +
  ggplot2::theme(plot.title = element_text(vjust = -2.5))


###################################################################################################################
################################################### Network Analysis ## ###########################################
###################################################################################################################

# Data can be found in "Gies-Course 5 - Week 2"

# wrangled_data (edges)
# name_list (nodes)
# col_name: freq (weight)

network::network(edges)
network::network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE) -> network_plot

plot(network_plot, vertex.cex = 3)  # vertex.cex = circle size
plot(network_plot, vertex.cex = 3, mode = "circle")  


# using igraph
igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE) -> network_plot
plot(network_plot, edge.arrow.size = 0.3, layout = layout_with_graphopt)

# using tidygraph & ggraph
tidygraph::tbl_graph(nodes = nodes, edges = edges, directed = TRUE) -> network_plot
tidygraph::as_tbl_graph(network_plot) -> network_plot   

ggraph::ggraph(network_plot, layout = "graphopt") +
  ggraph::geom_node_point() +
  ggraph::geom_edge_link(mapping = aes(width = weight), alpha = 0.8) +
  ggraph::scale_edge_width(range = c(0.5, 2)) +
  ggraph::geom_node_text(mapping = aes(label = label), repel =  TRUE) +
  ggplot2::labs(edge_width = "Freq of <>") +
  ggraph::theme_graph()


# using igraph & ggraph
igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE) -> network_plot

ggraph::ggraph(network_plot, layout = "linear") +
  ggraph::geom_edge_arc(mapping = aes(width = weight), alpha = 0.8) +
  ggraph::scale_edge_width(range = c(0.5, 2)) +
  ggraph::geom_node_text(mapping = aes(label = label)) +
  ggplot2::labs(edge_width = "Freq of <>") +
  ggraph::theme_graph()

# using visNetwork
visNetwork::visNetwork(nodes, edges)

edges <- dplyr::mutate(edges, width = weight/5 + 1)

visNetwork(nodes, edges) %>% 
  visNetwork::visIgraphLayout(layout = "layout_with_fr") %>% 
  visNetwork::visEdges(arrows = "middle")

# using networkD3
nodes_d3 <- dplyr::mutate(nodes, id = id - 1)
edges_d3 <- dplyr::mutate(edges, from = from - 1, to = to - 1)

networkD3::forceNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                        NodeID = "label", Group = "id", Value = "weight", 
                        opacity = 1, fontSize = 16, zoom = TRUE)


networkD3::sankeyNetwork(Links = edges_d3, Nodes = nodes_d3, Source = "from", Target = "to", 
                         NodeID = "label", Value = "weight", fontSize = 16, unit = "Letter(s)")





###### Alluvial Diagrams Using library(ggalluvial)
# sample data
group1 <- tibble::tibble(groupid = rep("group1", 7),
                         studentID = sample(seq(from = 1,to = 20), 7),
                         gender = sample(c("M", "F"), 7, replace = TRUE),
                         grades = sample(c("High Pass", "Pass", "Fail"), 7, replace=TRUE))
group2 <- tibble::tibble(groupid = rep("group2", 7),
                         studentID = sample(seq(from = 21, to = 30), 7),
                         gender = sample(c("M", "F"), 7, replace = TRUE),
                         grades = sample(c("High Pass", "Pass", "Fail"), 7, replace=TRUE))
group3 <- tibble::tibble(groupid = rep("group3", 7),
                         studentID = sample(seq(from = 31, to = 40), 7),
                         gender = sample(c("M", "F"), 7, replace = TRUE),
                         grades = sample(c("High Pass", "Pass", "Fail"), 7, replace = TRUE))
students < dplyr::bind_rows(group1, group2, group3)
####Summarize data
students %>%
  dplyr::group_by(groupid, gender, grades) %>%
  dplyr::count() -> students_table

#### Alluvial Diagrams
ggplot2::ggplot(data = students_table, mapping = aes(axis1 = groupid, axis2 = gender, y = n)) +
  ggalluvial::geom_alluvium(mapping = aes(fill = gender)) +
  ggalluvial::geom_stratum() +
  ggplot2::geom_text(stat = "stratum", 
                     mapping = aes(label = after_stat(stratum))) +
  ggplot2::scale_fill_manual(values = c("lightblue","lightpink"))



#####################################################################################################
######################################### Animation series ##########################################
#####################################################################################################

############### gganimate ###############
library(gganimate)
library(gifski)
library(transformr)

# Saving animated visual
gganimate::anim_save("test.gif", animation = anim3)
gganimate::anim_save("287-smooth-animation-with-tweenr.gif")



# gganimate::transition_states (Using same variable as your xaxis)

ggplot2::ggplot(data = mtcars, mapping = aes(x = factor(cyl), y = mpg)) +
  ggplot2::geom_boxplot()

ggplot2::ggplot(data = mtcars, mapping = aes(x = factor(cyl), y = mpg)) +
  ggplot2::geom_boxplot() +
  gganimate::transition_states(factor(cyl))

# gganimate::transition_states  (using different variable(to group) for your animate)
ggplot2::ggplot(data = mtcars, mapping = aes(x = factor(cyl), y = mpg)) +
  ggplot2::geom_boxplot()+
  ggplot2::facet_wrap(~gear)

ggplot2::ggplot(data = mtcars, mapping = aes(x = factor(cyl), y = mpg)) +
  ggplot2::geom_boxplot() +
  gganimate::transition_states(gear)

# gganimate::transition_time
# sample data
cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))
cel %>%
  dplyr::mutate(recode(party, "1" = "Democrat", "0" = "Republican")) %>% 
  dplyr::group_by(year,party) %>%
  dplyr::summarise("Seats"=n()) -> cong_dat

ggplot2::ggplot(data = cong_dat, mapping = aes(x = year, y = Seats, fill = party)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_hline(yintercept = 217) +
  ggplot2::scale_fill_manual(values = c("blue", "red")) +
  gganimate::transition_time(year)


# gganimate::transition_layers
cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))

ggplot2::ggplot() +
  ggplot2::geom_jitter(data = dplyr::filter(cel, congress == 115 & party == "Democrat"),
                       mapping = aes(x = seniority, y = all_pass, color = party)) +
  ggplot2::geom_jitter(data = dplyr::filter(cel, congress == 115 & party == "Republican"), 
                       mapping = aes(x = seniority, y = all_pass, color = party)) +
  ggplot2::geom_smooth(data = filter(cel, congress == 115 & party == "Democrat"), 
                       mapping = aes(x = seniority, y = all_pass, color = party)) +
  ggplot2::geom_smooth(data = filter(cel, congress == 115 & party == "Republican"), 
                       mapping = aes(x = seniority, y = all_pass, color = party)) +
  ggplot2::scale_color_manual(values = c("blue", "red")) +
  gganimate::transition_layers()



# gganimate::enter_fade(), exit_fade()

# Fade-in, fade-out
ggplot_data %>% 
  gganimate::enter_fade() +
  gganimate::exit_fade()


# shadowing using gganimate::shadow_wake()
ggplot2::ggplot(data = cong_dat, mapping = aes(x = year, y = Seats, fill = party)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::geom_hline(yintercept = 217) +
  ggplot2::scale_fill_manual(values = c("blue", "red")) +
  gganimate::transition_time(year) +
  gganimate::shadow_wake(wake_length = 1, alpha = FALSE, wrap = FALSE)



# Yet another example (Transition Time, how to set your title by the change)
# sample data library(gapminder)

ggplot2::ggplot(data = gapminder, mapping = aes(x = gdpPercap, y = lifeExp)) +
  ggplot2::geom_point(mapping = aes(size = pop, color = country),
                      show.legend = FALSE,
                      alpha = 0.7) +
  ggplot2::facet_wrap(~ continent) +
  ggplot2::scale_color_manual(values = country_colors) +
# country_colors are like.. usually same group of colors in the same continent
  ggplot2::scale_size(range = c(2, 12)) +
  ggplot2::scale_x_log10() +
# here comes the animation part
  ggplot2::labs(title = "Year: {frame_time}", x = "GDP per capita", y = "life expectancy") +
  gganimate::transition_time(year) +
  gganimate::ease_aes("linear")


# Transition Reveal
# for data sample
library(babynames)
library(hrbrthemes)
don <- babynames %>% 
  dplyr::filter(name %in% c("Ashley", "Patricia", "Helen")) %>%
  dplyr::filter(sex=="F")

don %>%
  ggplot2::ggplot(mapping = aes(x = year, y = n, group = name, color = name)) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::labs(title = "Popularity of American names in the previous 30 years",
                y = "Number of babies born") +
  hrbrthemes::theme_ft_rc() +
  gganimate::transition_reveal(year)


############### plotly ###############

#sample data
cel <- read_csv(url("https://www.dropbox.com/s/4ebgnkdhhxo5rac/cel_volden_wiseman%20_coursera.csv?raw=1"))
cel %>% 
  dplyr::mutate(Party = recode(dem, "1" = "Democrat", "0" = "Republican")) -> cel

# plotly animation 

plotly::ggplotly(
ggplot2::ggplot(data = cel, mapping = aes(x = seniority, 
                                          y = les, 
                                          color = Party, 
                                          frame = year,             # this adds the animation
                                          ids=thomas_name)) +       # this adds the object consistency
  ggplot2::geom_point() +
  ggplot2::labs(x = "Seniority", y = "Leg. Effectiveness") +
  ggplot2::scale_color_manual(values = c("blue", "red"))
)


################### PatchWork ################# how to combind figures into one page

load("gg_tx_map.rds")
load("gg_tx_timeseries.rds")
load("gg_tx_ridge.rds")

library(patchwork)

gg_tx_map + (gg_tx_timeseries / gg_tx_ridge) +
  plot_layout(widths = c(3,2), tag_level = "new") +
  plot_annotation(
    title      = "Texas Real-Estate Statistics",
    subtitle   = "The untold secrets of prime-real estate in the Lonestar State.\n",
    tag_levels = "A",
    tag_prefix = "Fig. ",
    tag_suffix = ":"
  ) &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 8, hjust = 0, vjust = 0))


