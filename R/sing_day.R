#' Takes a noun and makes it plural
#'
#' @param dataset A data frame containing information about gifts
#' @param line The number of the line for the day you want to sing about
#' @param phrase_col The variable name for the column in the dataset that
#' contains the gift phrases
#'
#' @return A string singing the line of the song with all gifts for the given day.
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export

sing_line <- function(dataset, line, phrase_col){

  phrases <- dataset %>% pull({{phrase_col}})

  my_line <- paste("On the", dataset$phrases[line], "day of Christmas, my true love sent to me")

  if(line == 1){

    my_line <- paste(my_line, phrases[line])

  } else{

    for (j in line:2){

      my_line <- cat(paste0(" ", my_line, sep="\n", phrases[j], "," ))

    }
    my_line <- (cat(paste("\n", my_line, "and", phrases[1], "\t")))
  }

  return(my_line)

  sapply(1:12, function(x) sing_line(xmas, x))
  map_chr(1:12, ~sing_line(xmas, .x))

}
