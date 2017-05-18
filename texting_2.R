##########
# How to make a grouped bar chart in R with the lattice package
#
# We are comparing the reasons for using the internet
# among normal people and addicts
# Step 1
# Put your data into Excel into the right format

# Step 2
# import the package "lattice"
library(lattice)

# Step 3
# import your data in, in the right format (usually comma separated)
# the table will be tab-separated in Excel, so we have to convert to comma-separated for R
# Convert the windows endline (/r/n) to newline (/n)
# convert all the "\t" to ", "
# and put it into a table variable
effect_size_table <- read.table(text = "Effect_size, Measure_of_adherence, Number_of_papers
0 - no data available, Self-report, 2
0 - no data available, Something other than self-report, 3
1 - Very small (<0.2), Self-report, 1
1 - Very small (<0.2), Something other than self-report, 7
2 - Small (0.2 - 0.5), Self-report, 3
2 - Small (0.2 - 0.5), Something other than self-report, 3
3 - Medium (0.5 - 0.8), Self-report, 1
3 - Medium (0.5 - 0.8), Something other than self-report, 3
4 - Large (>0.8), Self-report, 3
4 - Large (>0.8), Something other than self-report, 5
",
                          
  # we want to tell the read table that we have headers
  header = TRUE,
  # we want to set the separater value
  sep = ","
)

effect_size_table


# if you want word wrap for a column
# Core wrapping function
wrap.it <- function(x, len)
{ 
  sapply(x, function(y) paste(strwrap(y, len), 
                              collapse = "\n"), 
         USE.NAMES = FALSE)
}


# Call this function with a list or vector
wrap.labels <- function(x, len)
{
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}


effect_size_table$Measure_of_adherence <- wrap.labels(effect_size_table$Measure_of_adherence, 20)
#grade_table$Measure_of_adherence <- wrap.labels(grade_table$Measure_of_adherence, 20)


# Let's make the bar chart
# We are going to plot the number of dances choreographed (y axis)
# by the gender of the choreographers (x axis)
# grouped by problem of users (Problem)

# in my experience, lattice doesn't like it when you declare color in side the barplot function
# so we have to declare it outside

colors = c("azure", "mediumblue", "forestgreen", "goldenrod1", "mediumorchid", "deeppink4")

# sort the User column by descending order
# in the bar charts, we want "walthrough" on the left, and then "participant 1 and 2"
# but that is not alphabetical
# so we have to reverse sort it
#addiction_table$Problem <- factor(addiction_table$Problem,
#                                  levels = unique( as.character(addiction_table$Problem) ) )
#addiction_table


# open up a blank image that we want to save our chart in
png(filename = "textmessaging_cohens_d.png",
    width = 10,
    height = 10,
    units = "in",
    res = 600)


barchart(
  
  # Input the data in
  data = effect_size_table,
  
  # y axis by x axis
  Number_of_papers ~ Measure_of_adherence,
  
  # set the groups
  # This is what the x axis is grouped by
  groups = Effect_size,
  
  # Turn the graph 90 degrees
  horizontal = FALSE,
  
  # Add a title to our graph
  # I want the font of the title to be bigger
  main = list(
    
    label = "Effect sizes of medication adherence interventions
    that promote text messaing among teens
    with chronic illness",
    cex = 2.2
  ),
  
  
  # label the x axis
  # make the font bigger
  xlab = list (
    label = "Medication adherence measure",
    cex = 1.5
  ),
  
  # label the y axis
  # make the font bigger
  ylab = list(
    label = "Number of papers\n",
    cex = 1.5
  ),
  
  # Let's change the scale tick marks font size
  # x and y axis marks
  # we want to make their font a little bigger
  scales = list (
    x = list (
      # if you want to rotate your values
      # rot = 90
      cex = 1.2
    ),
    
    y = list (
      # if you want to rotate your values
      # rot = 90
      cex = 1.2
    )
  ),
  
  # add a legend
  auto.key = list (
    space = list (space = "top"),
    columns = 3,
    title = "Effect size (Cohen's d)",
    cex.title = 1.1
  ),
  
  # set the origin so that values start at 0
  origin = 0,
  
  # set colors
  par.settings = list(superpose.polygon = list(col = colors))
  
)


# save our image
dev.off()




