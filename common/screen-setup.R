# FHE 31 Mar 2025
# we put this line in a separate file so we don't execute
# it from all-tests.R

if(Sys.getenv("STY") != "") {
  message("Setting screen window to 13");
  system("screen -S $STY -X number 13");
}
