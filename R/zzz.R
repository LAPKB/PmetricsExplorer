# Define the %||% (null-coalescing) operator if not already defined
`%||%` <- function(x, y) if (!is.null(x)) x else y
