context("test grattan arrow")

mt <- mtcars
mt$car <- rownames(mt)


# Make a basic chart with the mtcars data

# Makign one cahrt with the grattan arrow
grattan_arrow_chart <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
  geom_point() +
  theme_grattan() +
  geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
               arrow = grattan_arrow(),
               colour = grattan_red)

# And one chart with the normal arrow function with the correct settings
normal_arrow_chart <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
  geom_point() +
  theme_grattan() +
  geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
               arrow = grid::arrow(type = "closed", angle = 20, length = unit(0.5, "lines")),
               colour = grattan_red)



test_that("Plots are identical", {

  expect_equal(
    grattan_arrow_chart,
    normal_arrow_chart
  )

})
