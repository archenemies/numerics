
source("common/mysource.R")
mysource_addpath("common/")
mysource_addpath(".")

mysource("pv.R")
mysource("util.R")

if(Sys.getenv("STY") != "") {
  message("Setting screen window to 13");
  system("screen -S $STY -X number 13");
}
