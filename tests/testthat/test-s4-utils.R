context("test-s4-utils")

#
## s4_to_list
#
test_that("s4_to_list", {
  methods::setClass("Person", slots = c(name = "character", age = "numeric"))
  john <- methods::new("Person", name = "John Smith", age = NA_real_)
  expect_identical(s4_to_list(john), list(name = "John Smith", age = NA_real_))
})

#
## list_to_s4
#
test_that("list_to_s4: typical case", {
  methods::setClass("Person", slots = c(name = "character", age = "numeric"))
  lst <- list(name = "John Smith", age = NA_real_)
  john <- methods::new("Person", name = "John Smith", age = NA_real_)
  expect_identical(list_to_s4(lst, "Person"), john)
})

test_that("list_to_s4: exceptions", {
  methods::setClass("Person", slots = c(name = "character", age = "numeric"))
  # lst is missing an element: age.
  lst <- list(name = "John Smith")
  # lst2 has an extra element: nationality.
  lst2 <- list(name = "Guilhermina I. S. Duarte", age = 39, nationality = 'portuguese')
  expect_error(list_to_s4(lst, "Person"), "names of 'list' do not match 'class'")
  expect_error(list_to_s4(lst2, "Person"), "names of 'list' do not match 'class'")
})
