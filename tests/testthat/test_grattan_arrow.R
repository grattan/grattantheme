# Create a dataset -- mtcars with rownames as column
 mt <- mtcars
 mt$car <- rownames(mt)

 # Make a basic chart with the mtcars data
 p <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
  geom_point() +
  theme_grattan()

# Add a simple callout arrow to the chart, using geom_segment, with arrow = grattan_arrow()
garrow_chart <- p +
geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
  arrow = grattan_arrow(),
  colour = grattan_red)


# Do the same but using the default arrow function with correct settings
arrow_chart <- p +
  geom_segment(aes(x = 15, y = 2, xend = 19.5, yend = 2.74),
               arrow = grid::arrow(type = "closed", angle = 20, length = unit(0.5, "lines")),
               colour = grattan_red)

test_that("grattan_arrow plot appears correct", {

expect_equal(
  garrow_chart,
  arrow_chart
)

})
