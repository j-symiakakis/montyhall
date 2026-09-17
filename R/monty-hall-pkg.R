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
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   Select a door.
#'
#' @description
#'   `select_door()` randomly selects one of three doors.
#'
#' @details
#'   The function creates three numbered doors and randomly
#'   selects one of them for the contestant.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a number between 1 and 3
#'   indicating the selected door.
#'
#' @examples
#'   select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   Open a goat door.
#'
#' @description
#'   `open_goat_door()` selects a door to open that has a goat
#'   behind it.
#'
#' @details
#'   The function opens a door with a goat behind it while
#'   avoiding the door originally selected by the contestant.
#'
#' @param game A length 3 character vector indicating the
#'   positions of goats and the car.
#' @param a.pick A number between 1 and 3 indicating the
#'   contestant's selected door.
#'
#' @return The function returns a number between 1 and 3
#'   indicating the opened door.
#'
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   open_goat_door( game, a.pick )
#'
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
#'   Change the selected door.
#'
#' @description
#'   `change_door()` determines the contestant's final door
#'   based on whether they stay or switch.
#'
#' @details
#'   The function keeps the contestant's original selection
#'   when staying and selects the other unopened door when
#'   switching.
#'
#' @param stay A logical value indicating whether the contestant
#'   stays with the original door.
#' @param opened.door A number between 1 and 3 indicating the
#'   door that was opened.
#' @param a.pick A number between 1 and 3 indicating the
#'   contestant's original selected door.
#'
#' @return The function returns a number between 1 and 3
#'   indicating the contestant's final selected door.
#'
#' @examples
#'   game <- create_game()
#'   a.pick <- select_door()
#'   opened.door <- open_goat_door( game, a.pick )
#'   change_door( stay=TRUE, opened.door, a.pick )
#'
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
#'   Determine the winner.
#'
#' @description
#'   `determine_winner()` determines whether the contestant
#'   wins or loses the game.
#'
#' @details
#'   The function checks whether the contestant's final selected
#'   door has a car or a goat behind it.
#'
#' @param final.pick A number between 1 and 3 indicating the
#'   contestant's final selected door.
#' @param game A length 3 character vector indicating the
#'   positions of goats and the car.
#'
#' @return The function returns "WIN" if the selected door
#'   contains the car and "LOSE" if it contains a goat.
#'
#' @examples
#'   game <- create_game()
#'   final.pick <- select_door()
#'   determine_winner( final.pick, game )
#'
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
#'   Play a Monty Hall game.
#'
#' @description
#'   `play_game()` plays one Monty Hall game using both the
#'   stay and switch strategies.
#'
#' @details
#'   The function creates a new game, selects a door, opens a
#'   goat door, and determines the outcome for both staying
#'   and switching.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a data frame containing the
#'   strategy and outcome for staying and switching.
#'
#' @examples
#'   play_game()
#'
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
#'   Play multiple Monty Hall games.
#'
#' @description
#'   `play_n_games()` plays multiple Monty Hall games.
#'
#' @details
#'   The function repeatedly plays the Monty Hall game and
#'   combines the results for the stay and switch strategies.
#'
#' @param n A numeric value indicating the number of games
#'   to play.
#'
#' @return The function returns a data frame containing the
#'   strategy and outcome for each game.
#'
#' @examples
#'   play_n_games( n=100 )
#'
#' @export
play_n_games <- function( n=5 )
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
