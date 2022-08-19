base_plot <- ggplot(ggplot2::txhousing, aes(x = date, y = volume, group = city, label = city)) +
  geom_line()

p <- base_plot %>%
  ch_add_stacked_labels()

test_that("plot with ch_add_stacked_labels() is a ggplot2 object",{
  expect_is(p, "gg")
})
