# FHE 24 Mar 2025
#https://grok.com/chat/c7aace3d-072c-4f47-aa28-f5a1923dfdfa

# Define a simple class for 2D points
point2d <- function(x, y) {
  structure(list(x = x, y = y), class = "point2d")
}

# Define the + method for point2d
`+.point2d` <- function(a, b) {
  if (inherits(b, "point2d")) {
    # Add two points: component-wise addition
    point2d(a$x + b$x, a$y + b$y)
  } else if (is.numeric(b) && length(b) == 2) {
    # Add a numeric vector of length 2 to the point
    point2d(a$x + b[1], a$y + b[2])
  } else {
    stop("Cannot add ", class(b), " to point2d")
  }
}

# Define the - method for point2d
`-.point2d` <- function(a, b) {
  if (inherits(b, "point2d")) {
    # Subtract two points: component-wise subtraction
    point2d(a$x - b$x, a$y - b$y)
  } else if (is.numeric(b) && length(b) == 2) {
    # Subtract a numeric vector of length 2 from the point
    point2d(a$x - b[1], a$y - b[2])
  } else {
    stop("Cannot subtract ", class(b), " from point2d")
  }
}
