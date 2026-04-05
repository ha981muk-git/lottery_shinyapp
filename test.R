suppressMessages(library(dplyr))
suppressMessages(library(purrr))
n <- 12000
df <- data.frame(
  ball_1 = sample(1:49, n, replace=TRUE),
  ball_2 = sample(1:49, n, replace=TRUE),
  ball_3 = sample(1:49, n, replace=TRUE),
  ball_4 = sample(1:49, n, replace=TRUE),
  ball_5 = sample(1:49, n, replace=TRUE),
  ball_6 = sample(1:49, n, replace=TRUE)
)

print("Current mutating approach:")
print(system.time({
  res1 <- df %>% mutate(
        sorted = purrr::pmap(list(ball_1, ball_2, ball_3, ball_4, ball_5, ball_6), ~ sort(c(...))),
        ball_1 = map_dbl(sorted, 1),
        ball_2 = map_dbl(sorted, 2),
        ball_3 = map_dbl(sorted, 3),
        ball_4 = map_dbl(sorted, 4),
        ball_5 = map_dbl(sorted, 5),
        ball_6 = map_dbl(sorted, 6)
      ) %>% select(-sorted)
}))

print("Matrix approach:")
print(system.time({
  cols <- paste0("ball_", 1:6)
  m <- as.matrix(df[, cols])
  m <- t(apply(m, 1, sort))
  res2 <- df
  res2[, cols] <- m
}))
