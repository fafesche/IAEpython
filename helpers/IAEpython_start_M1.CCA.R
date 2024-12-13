IAE.fn.env<- function(full=TRUE) {
  ssif <- Sys.info()
  if (tolower(ssif["sysname"]) == "windows") {
	pth <- gsub("\\\\", "/", path.expand("~"))
    s <- strsplit(pth, "/")[[1]]
    check <- TRUE
    check <- check && (s[1] == "C:" || s[1] == "c:")
    check <- check && s[2] == "Users"
    if (check) {
      if (full)
        return(paste0(s[1], paste0("/IAEpython","_",s[3]), "/myEnv"))
      else
        return(paste0(s[1], paste0("/IAEpython","_",s[3])))
    }
  }
}

IAE.base <- IAE.fn.env(full=FALSE)
IAE.R.lib <- paste(IAE.base,"library", paste(R.version$major,sub("\\..*$", "", R.version$minor),sep="."), sep="/")
if (!dir.exists(IAE.base)) { # Go in install mode
	print("Install mode")
    dir.create(IAE.base, recursive=TRUE)
	if (!dir.exists(IAE.R.lib)) { 
		dir.create(IAE.R.lib, recursive=TRUE)
		# R installation
		install.packages("devtools",lib=IAE.R.lib)
		install.packages("reticulate",lib=IAE.R.lib)
		.libPaths(new = c(IAE.R.lib, .libPaths()))
		devtools::install_github("fafesche/IAEpython", lib=IAE.R.lib)
	}
} # end of Install mode

# Start mode
print("Start mode")
.libPaths(new = c(IAE.R.lib, .libPaths()))
print(IAEpython::IAE.M1.CCA())


