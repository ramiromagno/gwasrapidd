context("test-tests")

test_that('set_testing_fast', {
  # save current value of TEST_FAST
  test_fast <- Sys.getenv("TEST_FAST")
  # Now erase it.
  Sys.unsetenv("TEST_FAST")
  expect_true(set_testing_fast())
  expect_identical(Sys.getenv("TEST_FAST"), 'true')
  # Restore the original value before the test
  Sys.setenv(TEST_FAST = test_fast)
})

test_that('set_testing_slow', {
  # save current value of TEST_FAST
  test_fast <- Sys.getenv("TEST_FAST")
  # Now erase it.
  Sys.unsetenv("TEST_FAST")
  expect_true(set_testing_slow())
  expect_identical(Sys.getenv("TEST_FAST"), 'false')
  # Restore the original value before the test
  Sys.setenv(TEST_FAST = test_fast)
})

test_that('skip_if_testing_is_fast: TEST_FAST is set to true', {
  # save current value of TEST_FAST
  test_fast <- Sys.getenv("TEST_FAST")
  # Now erase it.
  set_testing_fast()
  local_mocked_bindings(skip = function(...) FALSE)
  expect_false(skip_if_testing_is_fast())
  # Restore the original value before the test
  Sys.setenv(TEST_FAST = test_fast)
})

test_that('skip_if_testing_is_fast: TEST_FAST is set to false', {
  # save current value of TEST_FAST
  test_fast <- Sys.getenv("TEST_FAST")
  # Now erase it.
  set_testing_slow()
  expect_true(skip_if_testing_is_fast())
  # Restore the original value before the test
  Sys.setenv(TEST_FAST = test_fast)
})
