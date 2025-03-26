
test_that("get_similar_icons returns relevant suggestions", {
  expect_snapshot(get_similar_icons("chart-bar"))
  expect_snapshot(get_similar_icons("person-add"))
  expect_snapshot(get_similar_icons("person-+"))
  expect_snapshot(get_similar_icons("person", max_suggestions = 5))
})

test_that("get_similar_icons handles edge cases", {
  expect_snapshot(get_similar_icons(""))
  expect_error(get_similar_icons(NULL))
})
