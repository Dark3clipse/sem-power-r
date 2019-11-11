
# install & load junction
load <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# add ternary operator
`?` <- function(x, y)
  eval(
    sapply(
      strsplit(
        deparse(substitute(y)), 
        ":"
      ), 
      function(e) parse(text = e)
    )[[2 - as.logical(x)]])

# prepare
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!
