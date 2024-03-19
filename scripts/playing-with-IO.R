#### Exploring IO ####

### Starting from the Z matrix ###

Z <- matrix(c(80, 20, 50, 30,
              10, 120, 30, 60,
              80, 100, 175, 100,
              40, 120, 90, 140), ncol = 4, byrow = T)

Y <- c(290, 400, 725, 670)  # Yield

va <- c(80, 40, 380, 340)    # Value added

A <- matrix(ncol = 4, nrow = 4)

for (i in c(1:4)) {A[i,] <- Z[i,] / Y}

L <- solve(diag(4)-A)

L

f <- c(12.07, 5.25, 24.14, 18.28) # ghg emissions
diagf <- diag(f)

### Total amount of emission per unit of output

Fghg <- t(L) %*% diagf

Fghg # replicated results from Ceglar, but there is a margin or error

## Toy IUCN matrix

pxt <- matrix(c(1, 1, 1,
                1, 1, 1,
                1, 0, 1,
                0, 2, 1), nrow = 4, byrow = T)


t(pxt)
