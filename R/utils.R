# Options are set in .Rprofile, so no need for this:
# set_path_options <- function(x, cah_path = FALSE) {
#
#   ops <- options()
#   toset <- !(names(x) %in% names(ops))
#   if (any(toset)) options(x[toset])
#
#   for (i in names(x)) {
#     if (!file.exists(options()[[i]])) {
#       warning(
#         "Issue with a folder path. Problem with global option ", i,
#         ": No file exists with path \"", options()[[i]], "\".",
#         ifelse(
#           cah_path,
#           paste0(" Tell cah what the path is by setting the global option ",i," with option(",i," = /this/is/the/correct/path), preferably in .Rprofile so it is saved for future sessions. Note that all of this assumes you are syncing your Box and Drive folders. If you aren't please set it up first."),
#           ""
#         )
#       )
#     }
#   }
#
#   invisible(x)
# }

get_path_option <- function(x) {
  y <- options()[[x]]
  if (length(y)==0) {
    y <- NA
    warning(
      "No global option ",x," set. ",
      "Tell cah what the path is by setting option(",x," = /this/is/the/correct/path). Do it in .Rprofile, so it is saved for future sessions. Note that all of this assumes you are syncing your Box and Drive folders. If you aren't please set up syncing first."
      )
  } else {
    if (!file.exists(y)) {
      #browser()
      warning(
        "You specified a folder path that doesn't exist. You set the global option ",x,
        " to \"", options()[[x]], "\". ",
        "Tell cah what the path is by setting option(",x," = /this/is/the/correct/path). Do it in .Rprofile, so it is saved for future sessions. Note that all of this assumes you are syncing your Box and Drive folders. If you aren't please set up syncing first."
      )
    }
  }
  return(y)
}

set_child_path_option <- function(x, parent, child) {
  # sets and returns a child path option
  y <- paste0(parent,child)
  if (x %in% names(options())) {
    if (y != options()[[x]]) {
      message(
        "Discrepancy between cah-constructed path and your path set as global option. \n- cah: ", y,
        "\n - Global option: ", options()[[x]],
        "\ncah will NOT override your set option."
        )
      y <- options()[[x]]
    }
    if (!file.exists(y)) {
      warning(
        "Option ",x," was set to ",y,
        " but this file/folder does not exist. Consider not setting this option and let cah create it automatically."
      )
    }
  } else {
    if (!file.exists(y)) {
      if (!file.exists(parent)) {
        warning(
          "Attempt to automatically set global option ",x," to ",y,
          " but the parent folder ",parent," does not exist. You probably need to set / correct that option, in your .Rprofile. It's also possible that you're not syncing your folders and that's the problem."
        )
      } else {
        warning(
          "Option ",x," was set to ",y," but this file/folder does not exist. It's possible that the problem is that you're not syncing this file/folder. But if you think it's a bug, contact the developper of cah. You can also manually set this option (for instance, in .Rprofile)."
        )
      }
    }
    ylist <- list(temp = y)
    names(ylist) <- x
    options(ylist)
  }

  return(y)
}



check_in_options <- function(name, option) {
  # check if already in options, and if same value
  y <- list(in_options = FALSE, same_value = TRUE)
  #browser()
  if (name %in% names(options())) { # already a value found in options
    message("`",name,"` already found in global options.")
    y$in_options <- TRUE
    if (option != options()[[name]]) { # value is different
      message(
        "Value of preexisting global option different from value provided:\n- Provided: ", option,
        "\n- Preexisting: ", options()[[name]]
      )
      y$same_value <- FALSE
    } else { # value is same
      message(
        "Value of preexisting global option same as value provided:\n  ", option
      )
    }
  } else { # not yet value in options
    # no need for a message
  }
  return(y)
}

set_path_options <- function(..., force = FALSE) {
  # set path options, and check (and warn) if the parent folder exists, and the path

  x <- list(...) # list of split paths with option name as name: list(better.path = c("folder","file"))
  opts_list <- list() # to feed to options

  message("Setting path options.")
  for (i in 1:length(x)) { # do it per path, all the checks
    #browser()
    path_parent <- x[[i]][1]
    path <- paste0(path_parent,x[[i]][2]) # full path
    option_name <- names(x)[i]
    names(path) <- option_name

    message("Path: ", option_name)

    # Check if already in options:
    #browser()
    chk <- check_in_options(name = option_name, option = path)

    if (chk$in_options ) {
      if (!chk$same_value) {
        if (force) {
          message("Overriding preexisting global option.")
          opts_list <- c(opts_list, as.list(path))
        } else {
          message("Not overriding preexisting global option.")
          path <- options()[[option_name]]
          #path_parent <- dirname(path)
        }
      }
    } else {
      opts_list <- c(opts_list, as.list(path))
    }

    # Check if folder/file exists
    if (!file.exists(path)) {
      if (chk$in_options & !chk$same_value & !force) {
        warning(
          "Global option ",option_name," is set to:\n",path,
          "\n  But this file/folder does not exist. It's possible that the problem is that you're not syncing this file/folder. But if you think it's a bug, contact the developper of cah. You can also manually set this option (for instance, in .Rprofile)."
        )
      } else {
        if (!file.exists(path_parent)) {
          warning(
            "Global option ",option_name," is set to:\n  ",path,
            "\n  But the parent folder ",path_parent," does not exist. You probably need to set / correct that option, in your .Rprofile. It's also possible that you're not syncing your folders and that's the problem."
          )
        } else {
          warning(
            "Option ",option_name," is set to:\n",path,
            "\n  This file/folder does not exist. But the parent folder exists:\n",path_parent,
            "\n  It's possible that the problem is that you're not syncing the (child) file/folder. But if you think it's a bug, contact the developper of cah. You can also manually set this option (for instance, in .Rprofile)."
          )
        }
      }


    }

  }
  y <- options(opts_list)
  invisible(y)
}#; cah_set_options()

#' Set global options
#'
#' Sets global options for things like the `better` package, specifying the default paths `better` will use in many of its functions.
#'
#' @param force Whether or not to force setting cah options if options are already found
#' @return NULL
#' @export
#'
#' @examples
#' # `cah_set_options` assumes you already specified your local path
#' # to the `CAH Shared` folder in Box.
#' # Hopefully you did this in your .Rprofile file (see README).
#' # Something like this (but then with your path):
#' options(
#'   cah.box_path = "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared"
#' )
#'
#' # If you did this, then setting `better` options is easy:
#' cah_set_options()
cah_set_options <- function(force = FALSE) {
  if (is.null(options()$cah.box_path)) {
    warning("No global option named `cah.box_path` found. Not setting path options. Consider setting the `cah.box_path` option in your .Rprofile.")
  } else {
    set_path_options(
      better.gbd_path = c( # split into subfolder and path
        options()$cah.box_path,
        paste0("/IRB\ Projects/Health\ Projects/Health\ Team/Public\ datasets\ on\ health",
               "/Global\ Burden\ of\ Disease\ Study\ Data")
      ),
      force = force
    )
  }

  set_options(
    better.gbd_rei = "behavioral risks",
    better.gbd_location = "United States of America",
    better.gbd_cause = "all causes",
    force = force
  )
}

set_options <- function(..., force) {
  x <- list(...)
  opts_list <- list() # to feed to options
  message("\nSetting other options.")
  #browser()
  for (i in 1:length(x)) {

    opt <- x[[i]]
    option_name <- names(x[i])
    names(opt) <- option_name

    #browser()
    chk <- check_in_options(name = option_name, option = opt)
    if (chk$in_options ) {
      if (!chk$same_value) {
        if (force) {
          message("Overriding preexisting global option.")
          opts_list <- c(opts_list, as.list(opt))
        } else {
          message("Not overriding preexisting global option.")
          #path <- options()[[option_name]]
          #path_parent <- dirname(path)
        }
      } # if same value, no need to do anything
    } else {
      opts_list <- c(opts_list, as.list(opt))
    }
  }
  y <- options(opts_list)
  invisible(y)
}

get_cahfls <- function(x) {

  BOX_PATH <- get_path_option("cah.box_path")
  DRIVE_PATH <- get_path_option("cah.drive_path")

  # store nested data on file paths
  fls <- list(
    cah = list(
      box = list(
        path = BOX_PATH,
        children = list(
          health = list(
            path = "/IRB\ Projects/Health\ Projects",
            children = c(
              projects_sheet = "/Health\ Team/Health\ Team\ principal/CAH\ Projects.xlsx"
            )
          )
        )
      ),
      drive = list(
        path = DRIVE_PATH,
        children = list()
      )
    ),
    better = list(
      gbd = list(
        path = set_child_path_option(
          "better.gbd_path",
          parent = BOX_PATH,
          child = paste0(
            "/IRB\ Projects/Health\ Projects/Health\ Team/Public\ datasets\ on\ health",
            "/Global\ Burden\ of\ Disease\ Study\ Data"
            )
        )
      )
    )
  )

  # create dataframe out of nested list:
  cahfls <- tibble::tribble()
  floop <- function(x, pth, org) {
    if (is.list(x)) {
      for (i in 1:length(x)) {
        cahfls <<- dplyr::bind_rows(cahfls, tibble::tibble(
          "short" = names(x)[i],
          "path" = paste0(pth,x[[i]]$path),
          "orig" = org,
        ))
        #browser()
        if (length(x[[i]]$children)  > 0) {
          floop(x[[i]]$children,paste0(pth,x[[i]]$path),org)
        }
      }
    } else {
      for (i in 1:length(x)) {
        cahfls <<- dplyr::bind_rows(cahfls, tibble::tibble(
          "short" = names(x)[i],
          "path" = paste0(pth,x[[i]]),
          "orig" = org,
        ))
        #browser()
      }
    }
  }
  for (i in 1:length(fls)) {
    floop(fls[[i]], "", names(fls)[i])
  }

  cahfls <- dplyr::mutate(cahfls,
                          #path = shQuote(path), # not good
                          name = basename(cahfls$path),
                          parent = basename(dirname(cahfls$path))
  )
  # todo: kind: Box or Drive
  cahfls <- dplyr::select(cahfls, "short", "name", "parent", dplyr::everything())

  return(cahfls)
}
#cahfls <- get_cahfls()


cahpth <- function(x) {
  cahfls <- get_cahfls()
  y <- unname(purrr::map_chr(x,~cahfls$path[cahfls$short == .]))
  return(y)
}
#' Get the paths of CAH files or folders
#'
#' @param ... The short name(s), quoted or unquoted, of one or more files and/or folders you want paths for. If nothing specified, all paths will be returned.
#'
#' @return The path(s) as a string(s).
#' @export
#'
#' @examples
#' # In your R scripts you can refer to paths in folders other than the one your script is located in.
#' # Other people can still run your code.
#' # Something like this (but then with your paths):
#' options(
#'   cah.box_path = "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
#'   cah.drive_path = paste0(
#'      "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com",
#'      "/.shortcut-targets-by-id/1mCSu5zzZGk4BAPPBaZCMQXwnKmOPQSfU/CAH/CAH Shared Drive"
#'   )
#' )
#' readr::read_csv(paste0(
#'   cah_path(box),
#'   "/IRB\ Projects/Health\ Projects/Health\ Team/Public\ datasets\ on\ health",
#'   "/Global\ Burden\ of\ Disease\ Study\ Data",
#'   "/Raw\ Data\ GBD/IHME-GBD_2019_DATA-1a56c841-1/IHME-GBD_2019_DATA-1a56c841-1.csv"
#'   ))
cah_path <- function(...) {
  arg <- rlang::ensyms(...)
  if (length(arg) > 0) {
    y <- unname(purrr::map_chr(arg, rlang::as_string))
    #browser()
    y <- cahpth(y)
  } else {
    y <- get_cahfls()$path
  }
  return(y)
}

#' Get metadata on CAH files and folders
#'
#' @param ... The short name(s), quoted or unquoted, of one or more files and/or folders you want metadata for. If nothing specified, all files and folders will be returned.
#'
#' @return A dataframe with metadata on the folders.
#' @export
#'
#' @examples
#' # Specify your local paths to the `CAH Shared` folder in Box
#' # and the `CAH Shared Drive` folder in Drive.
#' # Best to do this in your .Rprofile file (see README).
#' # Something like this (but then with your path):
#' options(
#'   cah.box_path = "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared",
#'   cah.drive_path = paste0(
#'      "/Users/jwl38/Library/CloudStorage/GoogleDrive-janwillem.lindemans@gmail.com",
#'      "/.shortcut-targets-by-id/1mCSu5zzZGk4BAPPBaZCMQXwnKmOPQSfU/CAH/CAH Shared Drive"
#'   )
#' )
#' cah_files()
#' cah_files(box)
#' cah_files(projects_sheet)
cah_files <- function(...) {
  arg <- rlang::ensyms(...)
  cahfls <- get_cahfls()
  if (length(arg) > 0) {
    y <- unname(purrr::map_chr(arg, rlang::as_string))
    #browser()
    y <- cahfls[cahfls$short %in% y,]
  } else {
    y <- cahfls
  }
  return(y)
}

.onLoad <- function(libname, pkgname) {

  # library(better)
  # packageStartupMessage("Loaded the better package.")

  #testcahfls <- get_cahfls()

  # set_path_options(list(
  #   cah.box_path = cah_path("box"),
  #   cah.drive_path = cah_path("drive")
  # ), cah_path = TRUE)
  #\
  # set_path_options(list(
  #   better.gbd_path = cah_path("gbd")
  # ))

  # I think options should be set
  cah_set_options()

  invisible()

}
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the cah package!",
    "\nTo learn more, read the vignette by running `vignette(\"cah\")`."
    )
}
