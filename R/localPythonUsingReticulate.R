##### Local function to generalize script

#' Fix env from ~ expansion
#'
#' @returns Either a string corresponding to a subdirectories in AppData r-reticulate package directory or NULL when ~ cannot be extended for a user.
IAE.fn.env <- function() {
  s <- strsplit(path.expand("~"), "/")[[1]]
  check <- TRUE
  check <- check && (s[1] == "C:" || s[1] == "c:")
  check <- check && s[2] == "Users"
  if (check) {
    return(paste0(s[1], "/", s[2], "/", s[3], "/AppData/Local/r-reticulate/myEnv"))
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

#' Configure python using list of functionalities.
#'
#' @param course An (optional) list of named arguments which are used to check package installation.
#' @param my.env An optional virtual environment for the python local installation.
#' @returns Currently none formaly but TRUE or FALSE depending on the installation of the spyder package which is mandatory here.
IAE.fn.config <- function(course = NULL, my.env = "") {
  # This depends on course list of properties
  pkgs <- vector()
  if (!is.null(course[["data"]])) {
    pkgs <- c(pkgs, "pandas", "numpy")
  }
  if (!is.null(course[["file"]])) {
    pkgs <- c(pkgs, "pathlib")
  }
  if (!is.null(course[["opt"]])) {
    pkgs <- c(pkgs, "scipy")
  }
  if (!is.null(course[["finance"]])) {
    pkgs <- c(pkgs, "alpha-vantage", "yfinance")
  }
  print(pkgs)
  res <- sapply(pkgs, IAE.fn.pkg_install, my.env = my.env)
  print(res)
  # Always check spyder
  IAE.fn.pkg_install("spyder", my.env = my.env)
}

#' Use python with unload of reticulate and reload after system environment parameters correction. WORK_HOME from reticulate package is used.
#'
#' @param course An (optional) list of named arguments which are used to check package installation.
#' @returns NULL if the python virtual environement was not accessible otherwise a string containing its path on disk. Should be a path inside AppData of current user.
IAE.fn.python <- function(course = NULL) {
  # Unload if necessary
  if (isNamespaceLoaded("reticulate")) {
    print("Unload reticulate package for reconfiguration.")
    unloadNamespace("reticulate")
  }
  # Prepare env by building and check
  print("Check local environment.")
  IAE.env <- IAE.fn.env()
  if (!is.null(IAE.env)) {
    print(paste0("     Choose: ", IAE.env))
    # Set global env
    Sys.setenv(WORKON_HOME = IAE.env)
    if (!dir.exists(IAE.env)) { # Python was not installed for sure
      print(paste0("Create directory: ", IAE.env))
      dir.create(IAE.env)
    }
    # Now charge the virtual env
    if (requireNamespace("reticulate", quietly = TRUE)) {
      myEnv.name <- "12_IAE-M1"
      if (!reticulate::virtualenv_exists(myEnv.name)) { # Not already created so do it
        print(paste0("Create environment: ", myEnv.name))
        reticulate::virtualenv_create(myEnv.name, python = "3.12")
      }
      print(paste0("Use python environment: ", myEnv.name))
      reticulate::use_virtualenv(myEnv.name) # Only use it
      # Configure Python using packages installation
      print("Configure installation adding necessary python packages.")
      IAE.fn.config(course, myEnv.name)
      # Provide a function to spyder
      assign("spyder", function() system2(path.expand(paste0(IAE.env, "/", myEnv.name, "/Scripts/spyder.exe")), wait = FALSE), envir = .GlobalEnv)
    } else {
      print("Package reticulate is mandatory. Please install it.")
    }
  }
  print("Done.")
  return(IAE.env)
}

#' Configuration for course in Master 1 spécialité CCA
#'
#' Auto-configure for students in their AppData, installation of necessary packages are done as needed.
IAE.M1.CCA <- function() {
  course.type <- list()
  course.type[["data"]] <- TRUE
  course.type[["file"]] <- TRUE
  print(IAE.fn.python(course.type))
}

#' Configuration for course in Master 1 spécialité AAC
#'
#' Auto-configure for students in their AppData, installation of necessary packages are done as needed.
IAE.M1.AAC <- function() {
  course.type <- list()
  course.type[["data"]] <- TRUE
  course.type[["file"]] <- TRUE
  print(IAE.fn.python(course.type))
}

#' Configuration for course in Master 1 spécialité Finance parcours Conformité
#'
#' Auto-configure for students in their AppData, installation of necessary packages are done as needed.
IAE.M1.CONFORMITE <- function() {
  course.type <- list()
  course.type[["data"]] <- TRUE
  course.type[["file"]] <- TRUE
  print(IAE.fn.python(course.type))
}

#' Configuration for course in Master 1 spécialité Finance parcours Marchés Financiers
#'
#' Auto-configure for students in their AppData, installation of necessary packages are done as needed.
IAE.M1.MFI <- function() {
  course.type <- list()
  course.type[["data"]] <- TRUE
  course.type[["file"]] <- TRUE
  course.type[["opt"]] <- TRUE
  course.type[["finance"]] <- TRUE
  print(IAE.fn.python(course.type))
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
