

############################################# code chunks ##############################################

# it prevents code from being  evaluated. (this code is not run. no results will be generated)
# useful for displaying example code
eval = FALSE

# it runs the code, but doesn't show the code or results in the final document. 
# Use this for setup code that you don't want cluttering your report
include = FALSE

# it prevents code, but not the results from appearing in the finished file. 
# Use this when  writing reports aimed at people who don't want to see the underlying R code. 
echo = FALSE

# it prevents messages or warnings from appearing in the finished line. 
message = FALSE # or
warning = FALSE

# this hides printed output
results = 'hide'

# this hides plots
fig.show = 'hide'

# causes the render to continue even if code runs an error. 
# it's rarely used
error = TRUE


# format your table nice and clean
knitr::kable(dataset)


#################################################### In general ###############################################
# Interactive dashboards
flexdashboard::flex_dashboard()

# PDF handouts in the style of Edward Tufte
tufte::tufte_handout()

# Book in the style of Edward Tufte
tufte::tufte_book()

# html in the style of Edward Tufte
tufte::tufte_html()

# create r markdown website
# it requires 
# 1. a file namaed "_site.yml"
# 2. a .RMD file named "index.Rmd"
rmarkdown::render_site()





