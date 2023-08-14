# Copyright 2023 Craig Hutton
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

#' Import data from MedPC file to an R list.
#'
#' Parses MedPC data file and returns variables in a named list. Uses 'filename'
#' if filename is a path to a file, otherwise, opens a GUI to select a file.
#' This function is used internally by the other `*_read*` functions in the
#' package.
#'
#' @param filename full path to MedPC data file
#' @return list containing each variable in MedPC data file, referenced by variable name
#'
#' @author Gary Kane, \email{gkane@@alumni.princeton.edu}; Craig P. Hutton, \email{craig.hutton@@gmail.com}
#'
#' @references
#' Code was adapted from [https://github.com/gkane26/rmedpc]
#'
#' @noRd
import_medpc <- function(filename = ""){

  if(!file.exists(filename)){
    filename <- rstudioapi::selectFile()
  }

  #initialize list of variables
  varList = list()

  #Read file line by line, split line by labels
  line = scan(file=filename, character(0), sep="\n", strip.white=TRUE, quiet = TRUE)
  line = gsub("\\s+", " ", line)
  spl = strsplit(line, ": ")

  #Pull out general info
  for(i in 1:10) varList[[spl[[i]][1]]] = spl[[i]][2]

  #remove general info, split rest into label & values
  line=line[-(1:10)]
  spl = strsplit(line, " ")

  ############################################################
  #Identify variables and arrays, assign values
  #Loop through every line
  #if character followed by one number, assign variable letter to number
  #if character alone, assign array to all following variables until next character
  ############################################################

  getLabels = function(x) strsplit(x[[1]][1], ":")[[1]][1] #takes list, returns label at beginning of line
  labels = sapply(spl, getLabels)

  cnt=1
  while(cnt<=length(spl)){

    if(is.na(strtoi(labels[cnt]))){ #if label is a letter (start of variable or array)

      if(length(spl[[cnt]]) == 2){ #if variable
        varList[[labels[cnt]]] = as.numeric(spl[[cnt]][2])
      }else if(length(spl[[cnt]]) == 1){ #if start of array
        current = labels[[cnt]]
        varList[[current]] = NULL
        totalRay = NULL
      }

    }else{ #if label is number (continuation of array)
      totalRay = c(totalRay, as.numeric(spl[[cnt]][2:length(spl[[cnt]])]))
      varList[[current]] = totalRay
    }

    cnt = cnt +1
  }

  return(varList)

}
