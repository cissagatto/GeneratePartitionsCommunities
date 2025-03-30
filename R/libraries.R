#' Generate Partitions Communities
#'
#' This script sets up the environment for generating partition communities by
#' installing and loading necessary R packages. It ensures that required packages
#' are installed before use.
#'
#' @details
#' The script defines the root folder for storing scripts and data,
#' installs missing packages from a predefined list, and loads the required
#' libraries. The packages used include `igraph`, `dplyr`, `tidyr`, `stringr`,
#' `parallel`, and `doParallel`, which are essential for graph-based community
#' detection and parallel computing.
#'
#' @author
#' PhD Elaine Cecilia Gatto
#' Federal University of Lavras (UFLA), Applied Computer Department (DAC)
#'
#' PhD Ricardo Cerri
#' State University os São Paulo
#'
#' PhD Mauri Ferrandin
#' Federal University of Santa Catarina
#'
#' PhD Alan Demetrius
#' Federal University of Sao Carlos (UFSCar), Computer Department (DC)
#'
#' @note
#' Any errors, please contact: elainececiliagatto@gmail.com
#'
#' @examples
#' # Running the script ensures required packages are installed and loaded
#'
#' @seealso
#' \code{install.packages}, \code{library}, \code{require}
#'
# Define root directories
FolderRoot <- "~/GeneratePartitionsCommunities"
FolderScripts <- "~/GeneratePartitionsCommunities/R"

# List of required packages
required_packages <- c("igraph", "dplyr",
                       "stringr", "tidyr",
                       "parallel", "doParallel")

#' Install missing packages and load them
#'
#' @param pkg Character string with the package name.
#' @return Loads the package after installation.
#' @examples
#' install_if_missing("igraph")
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Apply the installation function to all required packages
sapply(required_packages, install_if_missing)

# Load libraries
library(igraph)
library(dplyr)
library(tidyr)
library(stringr)
library(parallel)
library(doParallel)

#############################################################################
# Please, any errors, contact us: elainececiliagatto@gmail.com
# Thank you very much!
#############################################################################
