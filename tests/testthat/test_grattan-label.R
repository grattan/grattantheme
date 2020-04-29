context("test grattan label")

set.seed(123)

mt <- mtcars
mt$car <- rownames(mt)

# Create a plot without labels
p <- ggplot(mt, aes(x = mpg, y = wt, label = car)) +
     geom_point() +
     theme_grattan()

# A chart with Grattan labels
p_label <- p +
    grattan_label()

# A chart with Grattan labels that are repelled from each other
p_label_repel <- p +
    grattan_label_repel()

test_that("grattan_label plots appear correct", {
  vdiffr::expect_doppelganger("labelled plot",
                              p_label)

  vdiffr::expect_doppelganger("labelled plot with repel",
                              p_label_repel)
})
