context("test that default font settings remain unchanged")

base_plot <- mtcars %>%
  ggplot(aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       subtitle = "My subtitle",
       caption = "Notes: Blah Source: somewhere") +
  theme_grattan()

base_plot$theme$text

test_that("default font settings are unchanged", {
  expect_equal(base_plot$theme$text$family, "sans")
  expect_equal(base_plot$theme$text$size, 18)

})
