
# Developer functions -----------------------------------------------------

dvl_before_install <- function() {

  # copy html s into inst
  PACKAGE_PATH <- find.package("cah")
  ARTICLES <- list.files(
    paste0(PACKAGE_PATH,"/vignettes"),
    full.names = TRUE,
    recursive = TRUE
  )
  ARTICLES <- stringr::str_subset(ARTICLES,".html$")
  ARTICLES <- tibble::tibble(path = ARTICLES)
  ARTICLES$basename <- basename(ARTICLES$path)
  ARTICLES <- dplyr::arrange(ARTICLES, dplyr::desc(nchar(ARTICLES$path)))
  ARTICLES <- dplyr::mutate(ARTICLES, duplicated = duplicated(basename))
  ARTICLES <- ARTICLES[!ARTICLES$duplicated,]
  ARTICLES <- ARTICLES$path
  file.copy(ARTICLES, paste0(PACKAGE_PATH,"/inst/cah_vignettes"), overwrite = TRUE)
}

# Main --------------------------------------------------------------------



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
      rlang::warn(paste0(
        "Discrepancy between cah-constructed path and your path set as global option. \n- cah: ", y,
        "\n- Global option: ", options()[[x]],
        "\nNote that cah will NOT override your set option."
        ), .frequency = "once", .frequency_id = "Discrepancy between cah-constructed path and your path set as global option")
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

  message("Setting path options:")
  for (i in 1:length(x)) { # do it per path, all the checks
    #browser()
    path_parent <- x[[i]][1]
    path <- paste0(path_parent,x[[i]][2]) # full path
    option_name <- names(x)[i]
    names(path) <- option_name

    message("  ",option_name," = ",path)

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
#' Sets global options for things like the `better` package, specifying the default paths `better` will use in many of its functions. Automatically called when loading `cah`.
#'
#' @param force Whether or not to force setting cah options if options are already found
#' @return NULL
#' @export
#'
#' @examples
#' # Normally the options are already set when loading `cah`.
#' # But you can also do it manually, perhaps after you changed some paths.
#' # `cah_set_options` assumes you already specified your local path
#' # to the `CAH Shared` folder in Box.
#' # Hopefully you did this in your .Rprofile file (see README).
#' # Something like this (but then with your path):
#' options(
#'   cah.box_path = "/Users/jwl38/Library/CloudStorage/Box-Box/CAH/CAH Shared"
#' )
#' # If you did this, you can now set `better` options:
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

  #browser()
  cahfls <- dplyr::arrange(cahfls, cahfls$short)

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

#' (Re)install the `cah` and `better` packages
#'
#' Wrapper around `remotes::install_github`, which you may know as `devtools::install_github`. Since the `cah` and `better`  packages are currently in development, they may change often, and you may want to update to the latest version often.
#'
#' @param ... Packages you want to (re)install, unquoted. Options: `cah`, `better`.
#' @param build_vignettes Whether or not to build vignettes when installing, by default TRUE, and passed on to `remotes::install_github()`.
#'
#' @return Package(s) installed, string, invisible.
#' @export
#'
#' @examples
#' \dontrun{
#' cah_install()
#' cah_install(better)
#' cah_install(cah, better)
#' }
cah_install <- function(..., build_vignettes = TRUE) {

  arg <- rlang::ensyms(...)
  if (length(arg) > 0) {
    arg <- unname(purrr::map_chr(arg, rlang::as_string))
  } else {
    arg <- "cah"
  }
  for (i in arg) {
    #browser()
    remotes::install_github(
      paste0("janlindemans/",i),
      build_vignettes = build_vignettes,
    )
  }
  invisible(arg)
}#; cah_install(cah, better)


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
    "\nWelcome to the cah package!",
    "\nTo learn more, read the vignette with `cah_vignette()`."
    )
}

#' CAH theme for ggplot
#'
#' @return Plot
#' @export
#'
#' @examples
#' library(tidyverse)
#' p1 <- tibble(
#'   gender = c(rep("Male",10),rep("Female",10)),
#'   x = c(1:10,1:10),
#'   y = c(10:1,1:10)
#'   ) %>%
#'   ggplot(aes(x,y,color=gender)) +
#'   geom_line() +
#'   labs(x = "Some variable", y = "Some other variable", title = "Some title")
#' p1 # default theme
#' p1 + theme_cah()
theme_cah <- function(){

  #Setting parameters for plots
  #resolution_dpi <- 600 #If we want to change the resolution for ggsave, we need to change it here too
  sysfonts::font_add_google(name = "Montserrat", family = "Montserrat") #Grabbing Montserrat
  sysfonts::font_add_google(name = "Roboto", family = "Roboto") #Grabbing Roboto (Don't know how to pick "light roboto" yet...)
  #showtext::showtext_opts(dpi = resolution_dpi)
  showtext::showtext_auto(enable = TRUE)
  font <- "Roboto"   #assign font family up front - this one is used in everything but the title

  cahDarkBlue <- "#00607A"
  cahMedBlue <- "#0089AA"
  cahLightBlue <- "#7CD1E8"
  cahGrey <- "#B4B4B3"
  cahLightOrange <- "#FFBC84"
  cahMedOrange <- "#FF9954"
  cahDarkOrange <- "#E25B00"
  cahYellow <- "#FEC010"
  cahDarkGrey <- "#848586"

  cahColor3Rev <- c(cahGrey, cahMedOrange,cahLightBlue)
  cahColor2 <- c(cahLightBlue, cahMedOrange)
  cahColor2Rev <- c(cahMedOrange, cahLightBlue)
  cahColor3 <- c(cahLightBlue,  cahMedOrange, cahGrey)
  cahColor5 <- c(cahDarkBlue, cahLightBlue, cahGrey, cahLightOrange, cahDarkOrange)
  cahColor7 <- c(cahDarkBlue, cahMedBlue, cahLightBlue, cahGrey, cahLightOrange, cahMedOrange, cahDarkOrange)

  ggplot2::`%+replace%`(ggplot2::theme_minimal(),     #replace elements we want to change

    ggplot2::theme(

      #grid elements
      panel.grid.major = ggplot2::element_blank(),    #strip major gridlines
      panel.grid.minor = ggplot2::element_blank(),    #strip minor gridlines
      axis.line.x.bottom = ggplot2::element_line(size = 1, color = "#b4b4b3"),      #create x-axis line
      axis.line.y = ggplot2::element_blank(),          #strip axis ticks

      #since theme_minimal() already strips axis lines,
      #we don't need to do that again

      #text elements
      plot.title = ggplot2::element_text(             #title
        family = "Montserrat",            #set font family
        size = 12,                #set font size
        face = NULL,            #bold typeface
        hjust = 0.5,                #left align
        vjust = 0,
        color = cahDarkGrey,
        margin = ggplot2::margin(t = 0, r = 0, b = .17, l = 0, unit = "inches")),

      plot.subtitle = ggplot2::element_text(          #subtitle
        family = "Montserrat",            #font family
        size = 10,
        color = cahDarkGrey),               #font size

      plot.caption = ggplot2::element_text(           #caption
        family = font,            #font family
        size = 9 ,                 #font size
        hjust = 1,
        color = cahDarkGrey),               #right align

      axis.title = ggplot2::element_text(             #axis titles
        family = font,                #font family
        size = 9,
        color = cahDarkGrey),               #font size

      axis.text = ggplot2::element_text(              #axis text
        family = font,            #axis famuly
        size = 9,
        color = cahDarkGrey,
        lineheight = 11),
      axis.title.x = ggplot2::element_text(size=10,
                                family = font,
                                color = cahDarkGrey,
                                lineheight = 11,
                                margin  = ggplot2::margin(t = .1,  unit = "inches")), #Give some breathing room for x-axis title
      legend.position = "bottom", #Putting the legend on the bottom
      legend.title = ggplot2::element_text(color = cahDarkGrey, size = 10, margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
      legend.text = ggplot2::element_text(color = cahDarkGrey, size = 7, margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0)),
      strip.text = ggplot2::element_text(size = 9, color = cahGrey, lineheight = 11) #Facets
    ))
}
if (FALSE) {
  library(ggplot2)
  library(gridExtra)

  p1 <- tibble(
    gender = c(rep("Male",10),rep("Female",10)),
    x = c(1:10,1:10),
    y = c(10:1,1:10)
  ) %>%
    ggplot(aes(x,y,color=gender)) +
    geom_line() +
    labs(x = "Some variable", y = "Some other variable")

  grid.arrange(
    p1 +
      labs(title = "Default theme"),
    p1 +
      labs(title = "CAH theme"),
    ncol = 2
  )

  grid.arrange(
    p1 +
      labs(title = "Default theme"),
    p1 +
      labs(title = "CAH theme") +
      theme_cah(),
    ncol = 2
  )
}

#' Get pre-installed `cah` vignettes
#'
#' Open the HTML files of pre-installed vignettes.
#'
#' Some `cah` vignettes require things that you may not had set up yet at the time of installing `cah`. Since such vignettes can't be built at the time of installation (which is how regular vignettes are created), they are pre-installed as HTML files. Use `cah_vignette` to access them. Strictly speaking, they aren't vignettes. You can't access them with `vignette(*, package = "cah")`. But they are articles that fulfill the role of typical vignettes. Note that you can also use `cah_vignette()` for true vignettes that are also accessible with `vignette(*, package = "cah")`.
#'
#' @param ... The name(s) of the vignette(s) you want to read, unquoted. By default, if you don't specify anything, it's all the vignettes. Options are: `r paste(stringr::str_remove(list.files(system.file("cah_vignettes", package = "cah")),".html$"), collapse = ", ")`.
#'
#' @return Names of the vignettes, invisible.
#' @export
#'
#' @examples
#' \dontrun{
#' cah_vignette()
#' cah_vignette(cah)
#' cah_vignette(`better-demo`)
#' cah_vignette(cah, "better-demo")
#' }
cah_vignette <- function(...) {
  arg <- rlang::ensyms(...)
  if (length(arg) > 0) {
    arg <- unname(purrr::map_chr(arg, rlang::as_string))
  } else {
    #browser()
    arg <- stringr::str_remove(
      list.files(system.file("cah_vignettes", package = "cah")),
      ".html$"
    )
    arg <- unique(c("cah", arg)) # start with cah, so that's the page that will be open
    #arg <- "cah"
  }
  #browser()
  for (i in rev(arg)) {
    #browser()
    namei <- paste0(i,".html")
    filei <- system.file("cah_vignettes", namei, package = "cah")
    if (filei == "") {
      warning("The following file not found in cah_vignettes: ", namei)
    } else {
      system2(
        "open",
        shQuote(filei)
      )
    }
  }
  invisible(arg)
}#; cah_vignette()#; cah_vignette(`better.demo`, cah)

