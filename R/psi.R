# Convert the following Python code to R:
#   Psi = collections.namedtuple("Psi", ["name", "param"], defaults=["sum", 0.0])

Psi <- function(name, param) {
  structure(list(name = name, param = param), class = "Psi")
}

# Convert the following Python code to R:
#    psi = Psi("sum")
#    psi2 = Psi("GSL", 0.5)

psi <- Psi("sum", 0.0)
psi2 <- Psi("GSL", 0.5)
