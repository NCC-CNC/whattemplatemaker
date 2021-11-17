context("resize_table")

test_that("resize_table", {
  expect_equal(
    resize_table(iris[1:3, ], 4),
    `rownames<-`(iris[c(1:3, NA), ], 1:4)
  )
  expect_equal(resize_table(iris[1:3, ], 1), iris[1, ])
  expect_equal(resize_table(iris[1:3, ], 3), iris[1:3, ])
})
