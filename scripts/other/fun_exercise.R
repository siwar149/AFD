# Define group size
group_size1 <- 6

# Calculate number of groups
num_groups1 <- ncol(f) %/% group_size1

# Initialize empty list to store results
f_sum <- list()

# Loop through each group and calculate row sums
for (i in 0:(num_groups1 - 1)) {
  f_sum[[i + 1]] <- f[, rowSums(.SD, na.rm = TRUE), 
                      .SDcols = ((i * group_size1 + 1):((i + 1) * group_size1))]
}

# Combine results into a single data.table
f_sum <- as.data.table(do.call(cbind, f_sum))

# Saving the final
s3write_using(x = as.data.table(f_sum), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/f_sum.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))

f_sum <- as.matrix(f_sum)

# calculate output for European countries
x_w <- L %*% f_sum

x_w <- as.data.table(x_w)

s3write_using(x = as.data.table(x_w), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/x_w.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))


x_eu <- x_w[, ..which(unique(label_f$V1) %in% eu)]

s3write_using(x = as.data.table(x_eu), FUN = data.table::fwrite, na = "", 
              object = paste(set_wd2,"/x_eu.rds",sep=""),
              bucket = bucket2, opts = list("region" = ""))