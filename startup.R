# FHE 30 Mar 2025
# try sourcing this file before you source any others

source("common/mysource.R")
mysource_addpath("common/")
mysource_addpath(".")

mysource("pv.R")
mysource("util.R")

if(Sys.getenv("STY") != "") {
  message("Setting screen window to 13");
  system("screen -S $STY -X number 13");
}
