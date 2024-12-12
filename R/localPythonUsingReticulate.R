##### Local function to generalize script

#' Fix env from ~ expansion in AppData (Windows) and ~(macOS, linux)
#'
#' @param full boolean. TRUE means complete name, FALSE means only main path
#' @returns Either a string corresponding to a directory or NULL when ~ cannot be extended for a user.
IAE.fn.env.local <- function(full) {
  ssif <- Sys.info()
  if (tolower(ssif["sysname"]) == "windows") {
    s <- strsplit(path.expand("~"), "/")[[1]] # Not working on Rscript in PowerShell (see global) TODO
    check <- TRUE
    check <- check && (s[1] == "C:" || s[1] == "c:")
    check <- check && s[2] == "Users"
    if (check) {
      if (full) {
        return(paste0(s[1], "/", s[2], "/", s[3], "/AppData/Local/r-reticulate/myEnv"))
      } else {
        return(paste0(s[1], "/", s[2], "/", s[3], "/AppData/Local/r-reticulate"))
      }
    }
  }
  if ((tolower(ssif["sysname"]) == "linux") ||
      (tolower(ssif["sysname"]) == "osx")) {
    s <- path.expand("~")
    if (full)
      return(paste0(s,"/.myEnv"))
    else
      return(s)
  }
  return(NULL)
}

#' Fix env from ~ expansion into C:
#'
#' @param full boolean. TRUE means complete name, FALSE means only main path
#' @returns Either a string corresponding to a directory or NULL when ~ cannot be extended for a user.
IAE.fn.env.global <- function(full) {
  print("Global installation.")
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
  if ((tolower(ssif["sysname"]) == "linux") ||
      (tolower(ssif["sysname"]) == "osx")) {
    s <- path.expand("~")
    if (full)
      return(paste0(s,"/.myEnv"))
    else
      return(s)
  }
  return(NULL)
}

#' Fix env from ~ expansion
#'
#' @param choice either "local" to set up in AppData (windows) of user, or "global" to set up at C:
#' @param full (optional, default is TRUE), set to TRUE to get complete path, to false to have main path only
#' @returns Either a string corresponding to a directory or NULL when ~ cannot be extended for a user.
IAE.fn.env <- function(choice="global", full=TRUE) {
  choices <- list(local = IAE.fn.env.local, global = IAE.fn.env.global)
  if (!is.null(choices[[choice]])) {
    return(choices[[choice]](full))
  }
  return(NULL)
}

#' Install a python package after verification.
#'
#' @param name The name of the package.
#' @param my.env An optional virtual environment for the python local installation.
#' @returns TRUE if the package was not already installed and FALSE if it was.
IAE.fn.pkg_install <- function(name = "", my.env = "") {
  if (requireNamespace("reticulate", quietly = TRUE)) {
    if (!reticulate::py_module_available(name)) {
      reticulate::py_install(name, my.env)
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Configure python using list of field.
#'
#' @param course An (optional) named argument to check package installation.
#' @param my.env An optional virtual environment for the python local installation.
#' @returns None
IAE.fn.config <- function(course = ".", my.env = "") {
  # Load config file
  IAE.base <- IAE.fn.env(full=FALSE)
  config <- read.csv2(paste(IAE.base,"configIAEpython",sep="/"),sep='|',encoding='UTF-8',fileEncoding='UTF-8')
  # Get packages list
  if (course %in% names(config)) {
	  packagesCross <- as.data.frame(apply(config[course],2,function (x) gsub("[[:space:]]", "", x)))
	  pkgs<-gsub("[[:space:]]", "", config[packagesCross[course]=='X','Packages'])
	  print(pkgs)
	  if (!is.null(pkgs)) {
		res <- sapply(pkgs, IAE.fn.pkg_install, my.env = my.env)
		print(res)
	  }
  }
  # Always check spyder
  # Currently Spyder depends on jellyfish but last version requires cargo (rust) updates as a dependency
  # So jellyfish does not install correctly must switch to an old version before installing spyder
  IAE.fn.pkg_install("jellyfish==1.0.4", my.env = my.env)
  # Then spyder should install correctly
  IAE.fn.pkg_install("spyder", my.env = my.env)
}

#' Use python with unload of reticulate and reload after system environment parameters correction. WORK_HOME from reticulate package is used.
#'
#' @param course An (optional) list of named arguments which are used to check package installation.
#' @param spyder a boolean to launch or not spyder (default is TRUE)
#' @returns NULL if the python virtual environment was not accessible otherwise a string containing its path on disk.
IAE.fn.python <- function(course = '.', spyder = TRUE) {
  # Unload if necessary
  if (isNamespaceLoaded("reticulate")) {
    print("Unload reticulate package for reconfiguration.")
    unloadNamespace("reticulate")
  }
  # Prepare env by building and check
  print("Check local environment.")
  IAE.env <- IAE.fn.env()
  print(IAE.env)
  if (!is.null(IAE.env)) {
    print(paste0("     Choose: ", IAE.env))
    # Library for local load
	IAE.base <- IAE.fn.env(full=FALSE)
    IAE.lib <- paste0(IAE.base,"/library/",paste(R.version$major,sub("\\..*$", "", R.version$minor),sep="."))
	# Get general configuration file from main source
    options(timeout = max(15, getOption("timeout")))  # Short timeout
    download.file("https://cloud.data-auvergne.fr/dataLearning/configIAEpython",paste(IAE.base,"configIAEpython",sep="/"))
    # Set global env
    Sys.setenv(WORKON_HOME = IAE.env)
    if (!dir.exists(IAE.env)) { # Python was not installed for sure
      print(paste0("Create directory: ", IAE.env))
      dir.create(IAE.env, recursive=TRUE)
    }
    # Now charge the virtual env
    if (requireNamespace("reticulate", quietly = TRUE, lib=IAE.lib)) {
	  # Must set R_USER_DATA_DIR to force python install outside AppData
	  Sys.setenv(R_USER_DATA_DIR = IAE.base)
      # install python (currently no check because install_python makes a check it self and update if necessary)
      pver <- "3.12.7"
      reticulate::install_python(version=pver)
      myEnv.name <- "12_IAE"
      if (!reticulate::virtualenv_exists(myEnv.name)) { # Not already created so do it
        print(paste0("Create environment: ", myEnv.name))
        reticulate::virtualenv_create(myEnv.name, python = pver)
      }
      print(paste0("Use python environment: ", myEnv.name))
      reticulate::use_virtualenv(myEnv.name) # Only use it
      # Configure Python using packages installation
      print("Configure installation adding necessary python packages.")
      IAE.fn.config(course, myEnv.name)
      # Provide a function to spyder
      if (spyder) {
        ssif <- Sys.info()
        if (tolower(ssif["sysname"]) == "windows") {
          system2(path.expand(paste0(IAE.env, "/", myEnv.name, "/Scripts/spyder.exe")), wait = FALSE)
        } else {
          # Should work but does not
          #cmd <- paste0("/usr/bin/bash --rcfile <(echo '",
          #       "source ", path.expand(paste0(IAE.env, "/", myEnv.name, "/bin", "/activate")),
          #       "; spyder ; exit')")
          #print(cmd)
          #system2(cmd, wait = FALSE)
          message("/home/fafesche/.myEnv/12_IAE/bin/spyder cannot be run in linux directly, sorry.")
        }
      }
    } else {
      print("Package reticulate is mandatory. Please install it.")
    }
  }
  print("Done.")
  return(IAE.env)
}

#' Configuration for course in Master 1 spécialité CCA
#'
#' Auto-configure for students, installation of necessary packages are done as needed.
IAE.M1.CCA <- function() {
  print(IAE.fn.python("M1.CCA"))
}

#' Configuration for course in Master 1 spécialité AAC
#'
#' Auto-configure for students, installation of necessary packages are done as needed.
IAE.M1.AAC <- function() {
  print(IAE.fn.python("M1.AAC"))
}

#' Configuration for course in Master 1 spécialité Finance parcours Conformité
#'
#' Auto-configure for students, installation of necessary packages are done as needed.
IAE.M1.CONFORMITE <- function() {
  print(IAE.fn.python("M1.CONFORMITE"))
}

#' Configuration for course in Master 1 spécialité Finance parcours Marchés Financiers
#'
#' Auto-configure for students, installation of necessary packages are done as needed.
IAE.M1.MFI <- function() {
  print(IAE.fn.python("M1.MFI"))
}

#' Configuration for course in Master 2 Finance spécialités Marchés Financiers
#'
#' Auto-configure for students, installation of necessary packages are done as needed.
IAE.M2.MFI <- function() {
  print(IAE.fn.python("M2.MFI"))
}

#' Configuration for course for teacher
#'
#' Auto-configure, installation of necessary packages are done as needed.
IAE.teacher <- function() {
  print(IAE.fn.python())
}

#' Configuration without launching spyder for use in quarto
#'
#' Auto-configure for students
IAE.quarto <- function() {
  print(IAE.fn.python(".", spyder=FALSE))
}

#' Configuration foronly launching spyder in the env
#'
#' Auto-configure for students in their AppData, installation of necessary packages are done as needed.
IAE.spyder <- function() {
  print(IAE.fn.python('.'))
}

#' .onLoad .Renviron in case it contains an http_proxy which is not allowed at the IAE
#'
#' @param libname name of the lib
#' @param pkgname name of the package
#' @returns Message for restarting R when invalid proxy are found at the IAE.
.onLoad <- function(libname, pkgname) {
  f <- path.expand("~/.Renviron")
  if (file.exists(f)) {
    myf <- file(f, open="r")
    cnt <- readLines(myf)
    close(myf)
    myWhere <- charmatch(c("http_proxy", "https_proxy"), cnt)
    if (!all(is.na(myWhere))) {
      modOrCopy <- function(i, wh) {
        if (i %in% wh) {
          return(paste0("#", cnt[i]))
        }
        return (cnt[i])
      }
      cnt <- sapply(seq(from = 1, to= length(cnt)),modOrCopy, wh=myWhere)
      myf <- file(f, open="w+")
      writeLines (cnt, con=myf)
      close(myf)
      packageStartupMessage("Proxy configuration error for IAEPython package, please restart R.")
    }
  }
}

############################################################################
#### SANBOX
############################################################################
# # Prepare env: before loading reticulate
# myEnv = "~/../AppData/Local/r-reticulate/myEnv"
# if (!dir.exists(myEnv)) dir.create(myEnv)
# Sys.setenv(WORKON_HOME=myEnv)
#
# library(reticulate)
# #version <- "3.12:latest"
# #install_python(version)
# # Local env -> install numpy only
# myEnv.name = "12_IAE-M1"
# virtualenv_create(myEnv.name, python="3.12")
# use_virtualenv(myEnv.name)
# # add necessary package
# py_install("pandas")
# py_install("pathlib")
# py_install("scipy")
# py_install("alpha-vantage")
# py_install("yfinance")
# # Install spyder and launch from R
# py_install("spyder")
# system2(path.expand(paste0(myEnv,"/",myEnv.name,"/Scripts/spyder.exe")), wait = FALSE)

# # Manage a new environment for a package
# init <- local({
#   e <- NULL
#   function() {
#     e <<- new.env(parent = emptyenv())
#   }
# })
#
# init.main <- function() {
#   e <- get("e", envir = environment(init))
# }
