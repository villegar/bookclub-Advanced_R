new_animal <- function(name = character(), eyes = integer()) {
  # Simple checks
  stopifnot(is.character(name))
  stopifnot(is.integer(eyes))

  structure(list(name = name,
                 eyes = eyes),
            class = "animal")
}

print.animal <- function(x, ...) {
  cat(paste0("Hello, this is ", x$name, " and I have:\n"))
  cat(paste0("- ", x$eyes, " eye", ifelse(x$eyes == 1, "", "s"), "\n"))
  invisible(x)
}

new_fish <- function(fins = integer(), ...) {
  # Simple checks
  stopifnot(is.integer(fins))
  stopifnot(length(list(...)) == 2)
  stopifnot(names(list(...))[1] %in% c("name", "eyes"))
  stopifnot(names(list(...))[2] %in% c("name", "eyes"))

  structure(list(...,
                 fins = fins),
            class = c("fish", "animal"))
}

print.fish <- function(x, ...) {
  NextMethod() # Or NextMethod("print", x)
  cat(paste0("- ", x$fins, " fin", ifelse(x$fins == 1, "", "s"), "\n"))
  invisible(x)
}

swim <- function(x, ...) {
  UseMethod("swim", x)
}

swim.fish <- function(x, ...) {
  cat(paste0(x$name, ": I can swim .... <º))))><\n"))
}

swim.default <- function(x, ...) {
  cat(paste0(x$name, ": I can't swim ... ='<\n"))
}

# Create some "objects"
a <- new_animal(name = "Dumbo", eyes = 2L)
b <- new_fish(fins = 2L, name = "Nemo", eyes = 2L)

purrr::walk(list(a, b), print)
purrr::walk(list(a, b), swim)
