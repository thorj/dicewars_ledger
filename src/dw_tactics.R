pacman::p_load(tidyverse, here)
set.seed(1)
L <- 1000000
n_dice <- 8
results <- list()
for (i in 1:n_dice) {
  sim <-
    data.frame(sums = replicate(L, sum(sample(1:6, i, replace = T))))  
  colnames(sim) <- sprintf("n_%s_sums", i)
  results[[i]] <- sim
}

final <- do.call(cbind, results)
as_tibble(final)

dice_wars_matrix <- matrix(0, nrow = n_dice, ncol = n_dice)
for (i in 1:n_dice) {
  for (j in i:n_dice) {
    dice_wars_matrix[i, j] <- sum(final[, i] > final[, j])/L
    dice_wars_matrix[j, i] <- 1 - dice_wars_matrix[i, j]
  }
}


diag(dice_wars_matrix) <- 0.5

write.table(x = final, file = here("data", "dw_tactics_raw.txt"), quote = F, sep = ";", row.names = F)
write.table(x = dice_wars_matrix, file = here("data", "dw_tactics_matrix.txt"), quote = F, sep = ";", row.names= F)
