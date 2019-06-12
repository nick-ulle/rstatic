#' Counter for Strings
#'
#' This data structure keeps a running total of the number of times it has seen
#' each unique string. This is an R6 class, so the object can be modified in
#' place and assignment has reference semantics.
#'
#' @export
Counter = R6::R6Class("Counter",
  "private" = list(
    counter = integer(0)
  ),

  "public" = list(
    increment = function(key) {
      if (key %in% names(private$counter)) {
        counter = private$counter[[key]] + 1L
      } else {
        counter = 1L
      }
      
      private$counter[[key]] = counter

      return (counter)
    }
  )
)


#' Next Name from Counter
#'
#' This function looks up the given name in the given counter, increments the
#' counter for that name, and returns a string with the name and new count.
#'
#' Side effects: The counter is modified in-place.
#'
#' @param counter (Counter) The counter for lookup.
#' @param name (character) The name to look up in the counter.
#' @param ... Additional arguments, currently ignored.
#'
#' @return (character) A string containing the name and the new count.
#' @export
next_name = function(counter, name, ...) {
  paste0(name[1L], counter$increment(name))
}
