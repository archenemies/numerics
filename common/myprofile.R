# FHE 05 Apr 2025
# common configurations related to interactive sessions.
# moved from ~/.Rprofile so that we can share it with projects more
# easily

mysource("mycli.R");

options(setWidthOnResize=TRUE)

suppressMessages(require(grDevices))
# FHE 20 Sep 2018 why was i turning buffering off? that's the worst
# of all worlds
## X11.options(type="nbcairo")
X11.options(type="dbcairo")
options(menu.graphics=FALSE)
options(max.print=5e2)
options("show.error.locations"=T)

# FHE 13 Jul 2021 already true?
# https://stackoverflow.com/questions/1445964/r-script-line-numbers-at-error
options(keep.source.pkgs=TRUE);
options(keep.source=TRUE);

# FHE 07 Apr 2025
options(digits=3)

# FHE 13 Jul 2021 for searchable documentation (from mailing list)
options(useFancyQuotes=FALSE)
if(!exists(".hist_loaded")) {
  loadhistory("~/.Rhistory");
  .hist_loaded=1;
}
.Last <- function() {
  message("Saving history");
  try(savehistory("~/.Rhistory"))
}

# from run-R-devel
if(Sys.getenv("STY") != "") {
  message("Setting screen window to 13");
  system("screen -S $STY -X number 13");
}
