# FHE 30 Mar 2025
# try sourcing this file before you source any others

source("common/mysource.R")
mysource_addpath("common/")
mysource_addpath(".")

# source stuff in common/
mysource("pv.R")
mysource("util.R")
mysource("export.R")
mysource("lastind.R")

mysource("generic-deparse.R")

if(Sys.getenv("STY") != "") {
  message("Setting screen window to 13");
  system("screen -S $STY -X number 13");
}
