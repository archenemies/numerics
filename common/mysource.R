# FHE ?? very old

# Source an R script like source(), but look for it in a list of
# directories (like with the Unix PATH variable). Also record a list
# of all the files that have been sourced, so that we can do
# dependency analysis.

# FHE 30 Mar 2025 this is one of the oldest R scripts I have written
# and perhaps should be rewritten, probably as a wrapper to 'source'
# so that we can use the same syntax? or at least we should provide
# such a wrapper?

{
  # pick up verbosity level from shell environment and from R global
  # environment
  vrbenv = Sys.getenv("MYSOURCE_VERBOSE");
  if(vrbenv!="" && vrbenv!="0") { mySourceVerbose <<- as.numeric(vrbenv); }
  if(!exists("mySourceVerbose")) { mySourceVerbose <<- 0; }
}

# FHE 25 Jun 2021 clean up messages
ms_vmsg = function(...,level=0) {
  if(mySourceVerbose > level) { message("mysource: ",...); }
}

#mySourcePath=c(".","~/scripts-misc/R");
if(!exists("mySourcePath")) { # FHE 21 Jun 2021 for eqtool
  mySourcePath=unlist(strsplit(Sys.getenv("MY_R_IMPORTS"),":"))
}
if(!exists("mySourcedFiles")) { mySourcedFiles <<- list();
} else { ms_vmsg("Using pre-existing file list"); }
if(!exists("mySourceLevel")) {
  # FHE 31 Mar 2025 for code with tests at the end
  # so we can only run them when sourced from REPL
  mySourceLevel <<- 0
}

findFileInPath=function(file,path) {
  for(p in path) {
    fp <- file.path(p, file)
    if(file.exists(fp)) {
      return(fp);
    }
  }
  return(NULL);
}

# expand home directory...
mysource_expand = function(file) {
  gsub(x=file,"^~",Sys.getenv("HOME"))
}

# look up file in path, also adds it to list of sourced files (unless visit=F)
mysource_which = function(file,visit=T) {
  file = mysource_expand(file)
  # allow absolute paths
  if(substr(file,0,1)=="/") {
    return(file);
  }
  fp = findFileInPath(file,mySourcePath)
  if(visit && !is.null(fp))
  # FHE 22 Jul 2021 this is to help with getting dependencies on C and
  # .so files which we look up using mysource_which
    mySourcedFiles[[fp]] <<- file.mtime(fp);
  fp
}

# prepend path to search path
# mysource_addpath =
mysource_addpath = mysource_prepath = function(str) {
  str = mysource_expand(str);
  mySourcePath<<-c(str,mySourcePath);
}

mysource <- function(..., force=F) {
  files=list(...) # FHE 10 Mar 2022 why was i using substitute here
  for(file in files) {
    fp = mysource_which(file,visit=F);
    ## fp = findFileInPath(file,mySourcePath);
    if(is.null(fp)) {
      stop("file ", sQuote(file), " not found in path ",paste(mySourcePath,collapse=":"))
    }
    #    mt = as.numeric(file.mtime(fp));
    mt = file.mtime(fp);
    if(!force) {
      old_mt = mySourcedFiles[[fp]];
      if(!is.null(old_mt) && ("POSIXct" %in% class(old_mt))) {
        if(old_mt < mt) {
          warning("Source file ",file," changed, re-reading");
        } else {
          ms_vmsg("Already sourced ",file,", skipping",level=1);
          return(invisible());
        }
      }
    }
    ms_vmsg("Sourcing ",fp);
    mysource_current <<- fp
    mySourcedFiles[[fp]] <<- "current";
    # XXX put something here to let the file know whether we are
    # being sourced from the toplevel
    mySourceLevel <<- mySourceLevel+1
    on.exit({mySourceLevel <<- mySourceLevel-1})
    source(fp)
    # FHE 17 Feb 2020 moved this below in case it fails
    ms_vmsg("Done sourcing ",fp,level=1);
    mySourcedFiles[[fp]] <<- mt;
  }
  invisible(NULL);
}

pretend_source = function(file) {
  mt = file.mtime(file);
  mySourcedFiles[[file]] <<- mt;
}

# function to get path of current script called by 'source'
# hadley's solution
get_script_path=function() {
  #https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script
  frame_files <- lapply(sys.frames(), function(x) x$ofile)
  frame_files <- Filter(Negate(is.null), frame_files)
  PATH <- dirname(frame_files[[length(frame_files)]])
}

# FHE 25 Jun 2021 not good to combine these
#mysource("pv.R");

# talks about 'here' package:
# https://stackoverflow.com/questions/42815889/r-source-and-path-to-source-files

# XXX FHE 25 Mar 2025 would better to create an override for 'source()' and use the same interface?
