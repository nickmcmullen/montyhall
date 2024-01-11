#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'   [1] ["goat", "goat", "car"]
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'  Select a door at random in a Monte Hall game
#' @description
#'  Randomly generates a number between 1 and 3, representing the door number 
#'  selected by the contestant
#' @details
#' The door selected by the contestant will be used later in the game and
#' presented as a choice, stay or switch and win the prize behind the final 
#' door selected
#' @param ... no arguments are used by this function
#' @return The function returns a numeric value between 1 and 3
#' @examples select_door()
#' [1] 2
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title 
#' Opens a door not selected by the contestant that has a goat behind it
#' @description
#' Randomly generates a number between 1 and 3, taking inputs of the game
#' structure and initial contestant pick into account
#' @details
#' If the contestant has selected the car door, the function will return one of
#' the two goat door numbers at random. If the contestant has selected a goat
#' door, the function will return the only remaining goat door number.
#' @param game = a vector of length three representing the game structure - see 
#' create_game()
#' @param a.select = a numeric value between 1 and 3 representing the contestant's
#' initial door selection - see select_door()
#' @return Generates a numeric value between 1 and 3
#' @examples 
#' open_goat_door(game = ["goat", "car", "goat"], a.pick = 1)
#' [1] 3
#' open_goat_door(game = ["goat", "car", "goat"], a.pick = 2)
#' [1] 1
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title 
#' Indicates whether a contestant would like to switch doors or keep their 
#' original selection
#' @description
#' After a goat door is revealed by the host, the contestant can choose to 
#' switch their door selection to the other unopened door, or keep their 
#' original selection.
#' @details
#' If the contestant elects to switch doors, the function will return the
#' number of the remaining unopened door. If the contestant elects to keep
#' their selection, the function will return the numeric value associated
#' with the original selection
#' @param stay = T/F logical, indicates whether the contestant will keep their
#' original selection
#' @param opened.door = numeric value between 1 and 3 representing the door opened
#' by the host (see select_door())
#' @param a.pick = numeric value between 1 and 3 representing the door originally
#' selected by the contestant (see open_goat_door())
#' @return Generates a numeric value between 1 and 3
#' @examples
#' change_door(stay=T, opened.door = 1, a.pick = 2) 
#' [1] 2
#' change_door(stay = F, opened.door = 1, a.pick = 2)
#' [1] 3
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title 
#' Determines if the contestant won the game
#' @description
#' This function compares the contestant's final selection against the original
#' game structure and returns the final game result
#' @details
#' If the contestant's final selection is the door number with the car behind
#' it, the function will return "WIN", if not the function will return "LOSE"
#' @param final.pick = a numeric value between 1 and 3 representing the 
#' contestant's final door choice 
#' @return character object "WIN" or "LOSE"
#' @examples
#' determine_winner(final.pick = 1, game = ["goat", "car", "goat"])
#' [1] "LOSE"
#' determine_winner(final.pick = 2, game = ["goat", "car", "goat"])
#' [1] "WIN"
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' A wrapper function that allows a user to simulate a full Monte Hall game
#' @description
#' Call play_game() to simulate a monte hall game outcome and output the final
#' result in the form of a data frame containing the strategy used by the
#' contestant and the final game outcome if that strategy was used
#' @details
#' This function calls all of the helper functions defined in this package
#' to create a new game, select a door, open a goat door, and simulate what
#' the contestant's final selection would be if they keep their original
#' choice or switch. The final results are written into a data frame that 
#' shows both stay and switch strategies and what the outcome of each strategy
#' would have been in actual game play.
#' @param ... no arguments are used by the function
#' @return A data.frame object containing stay/switch strategy and final game
#' outcome
#' @examples play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' A wrapper function that allows a user to simulate a specified number of 
#' Monte Hall games
#' @description
#' This function will simulate a user-defined number (n) of Monte Hall games 
#' and store the aggregated results in a data table. 
#' @details
#' Simulates all steps of the Monte Hall game from the game setup 
#' (assigning objects behind doors), initial contestant door selection, the 
#' host opening a goat door, the contestant strategy of switch door selection 
#' or stay, and the final game result. The results are simulated a user-defined 
#' n-times and stored in a results data frame. The function returns a 
#' proportion table to make the results more easily legible.
#' @param n = numeric value indicating the number of Monte Hall simulations
#' to run. The default value of n is 100.
#' @return A proportion table indicating the outcomes of both switch and stay
#' strategies over n simulations
#' @examples play_n_games(n=1000)
#' play_n_games()
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
