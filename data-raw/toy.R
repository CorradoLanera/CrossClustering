set.seed(123)

toy <- matrix(NA, nrow = 10, ncol = 7)

colnames(toy) <- paste("Sample", seq_len(ncol(toy)), sep = "")
rownames(toy) <- paste("Gene"  , seq_len(nrow(toy)), sep = "")

toy[, 1:2] <- rnorm(n = nrow(toy) * 2, mean = 10, sd  = 0.1)
toy[, 3:4] <- rnorm(n = nrow(toy) * 2, mean = 20, sd  = 0.1)
toy[, 5:6] <- rnorm(n = nrow(toy) * 2, mean = 5 , sd  = 0.1)
toy[,   7] <- runif(n = nrow(toy)    , min  = 0 , max = 1  )

use_data(toy)
