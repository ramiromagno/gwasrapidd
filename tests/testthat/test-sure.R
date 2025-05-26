context("test-sure")

test_that('stop_quietly', {
  #expect_silent(stop_quietly())
  expect_error(stop_quietly())
})

test_that("sure: non-interactive mode", {
  expect_true(sure(default_answer = 'y'))
  expect_true(sure(default_answer = 'yes'))
  expect_true(sure(default_answer = 'Yes'))
  expect_true(sure(default_answer = 'Y'))
  expect_false(sure(default_answer = 'n'))
  expect_false(sure(default_answer = 'no'))
  expect_false(sure(default_answer = 'N'))
  expect_false(sure(default_answer = 'No'))
  expect_false(sure(default_answer = 'some other answer')) # equivalent to 'No'.
})

test_that("sure: mock interactive mode", {
  with_mocked_bindings(readline = function(...) 'y', expect_true(sure()))
  with_mocked_bindings(readline = function(...) 'yes', expect_true(sure()))
  with_mocked_bindings(readline = function(...) 'n', expect_false(sure()))
  with_mocked_bindings(readline = function(...) 'no', expect_false(sure()))
  with_mocked_bindings(readline = function(...) 'some other answer', expect_false(sure()))
})

test_that("sure: mock interactive mode with messages", {
  with_mocked_bindings(readline = function(...) 'y', code = expect_message(sure(before_question = 'hello'), 'hello'))
  with_mocked_bindings(readline = function(...) 'yes', code = expect_message(sure(after_saying_yes = 'alright!'), 'alright'))
  with_mocked_bindings(readline = function(...) 'n', code = expect_message(sure(before_question = 'hello'), 'hello'))
  with_mocked_bindings(readline = function(...) 'no', code = expect_message(sure(after_saying_no = 'nooo!'), 'nooo!'))

  with_mocked_bindings(readline = function(...) 'yes', code = expect_silent(sure(after_saying_no = 'nooo!')))
  with_mocked_bindings(readline = function(...) 'no', code = expect_silent(sure(after_saying_yes = 'alright!')))
})
