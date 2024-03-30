#### Exploring IO ####

### Starting from the Z matrix ###

Z <- matrix(c(80, 20, 50, 30,
              10, 120, 30, 60,
              80, 100, 175, 100,
              40, 120, 90, 140), ncol = 4, byrow = T)

x <- c(290, 400, 725, 670)  # Output

feu <- c(50, 80, 150, 180) # European final demand

frow <- c(60, 100, 120, 100) # Rest of the World final demand

va <- c(80, 40, 380, 340)    # Value added

A <- Z %*% solve(diag(x))    # Technical coefficients matrix

L <- solve(diag(4)-A)

L

### let's simulate a demand shock in the first sector of the first country

s <- c(-34, 0, 0, 0)

x1 <- L %*% s 

x + x1

x


## Let's see the effect on GOS

gos <- 0.55 * va

goso <- gos / x

# Multiplier of GOS

Sgos <- diag(goso) %*% L

# shock on GOS

kgos <- Sgos %*% s

gos + kgos

gos


# start doing it with the big dataset
###################### Very interesting ##################



f <- c(12.07, 5.25, 24.14, 18.28) # ghg emissions
diagf <- diag(f)

### Total amount of emission per unit of output

Fghg <- t(L) %*% diagf

Fghg # replicated results from Ceglar, but there is a margin or error
