context("test axis labels not shown when disabled")


base_plot <- ggplot(mtcars, aes(x = wt,
             y = mpg)) +
  geom_point() +
  labs(title = "Here goes a Grattan title, blah blah lots of words go here extremely orange",
       caption = "Notes: Blah Source: somewhere")

base_plot + theme_grattan(flipped = TRUE) + theme(axis.ticks= element_blank())

flipped_plot <-  base_plot +
  theme_grattan(flipped = T) +
  theme(axis.ticks = element_blank())

test_that("no ticks on flipped plot when disabled", {
  expect_is(flipped_plot$theme$axis.ticks,
            "element_blank")
  expect_null(flipped_plot$theme$axis.ticks.y)
  expect_is(flipped_plot$theme$axis.ticks.x,
            "element_blank")

})

mt_no_rownames <- mtcars
mt_no_rownames$car <- rownames(mtcars)
rownames(mt_no_rownames) <- NULL

bar_chart <- ggplot(mt_no_rownames,
       aes(x = reorder(car, mpg),
           y = mpg)) +
  geom_col()

test_that("bar chart ticks shown when appropriate", {
  regular_bar <- bar_chart + theme_grattan()
  expect_null(regular_bar$theme$axis.ticks.x)
  expect_is(regular_bar$theme$axis.ticks.y, "element_blank")
  expect_is(regular_bar$theme$axis.ticks, "element_line")
  vdiffr::expect_doppelganger("regular bar chart", regular_bar)


  flipped_bar_default <- bar_chart + coord_flip() + theme_grattan()
  expect_null(flipped_bar_default$theme$axis.ticks.x)
  expect_is(flipped_bar_default$theme$axis.ticks.y, "element_blank")
  vdiffr::expect_doppelganger("flipped bar default", flipped_bar_default)

  flipped_bar_fliptrue <- bar_chart + coord_flip() + theme_grattan(flipped = T)
  expect_is(flipped_bar_fliptrue$theme$axis.ticks.x, "element_blank")
  expect_null(flipped_bar_fliptrue$theme$axis.ticks.y)
  vdiffr::expect_doppelganger("flipped bar with flipped = TRUE",
                              flipped_bar_fliptrue)

  flipped_bar_noticks <- bar_chart +
    coord_flip() +
    theme_grattan(flipped = TRUE) +
    theme(axis.ticks = element_blank())

  expect_null(flipped_bar_noticks$theme$axis.ticks.y)
  expect_is(flipped_bar_noticks$theme$axis.ticks.x,
            "element_blank")
  expect_is(flipped_bar_noticks$theme$axis.ticks,
            "element_blank")

  vdiffr::expect_doppelganger("flipped bar with axis.ticks off",
                              flipped_bar_noticks)
})


