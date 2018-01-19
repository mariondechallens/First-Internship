#Charge les packages, et les installe s'ils ne le sont pas deja

pkg <- c("chron","scales","plyr","stringr")

new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
if (length(new.pkg)) install.packages(new.pkg, dependencies = TRUE)
sapply(pkg, require, character.only = TRUE)
