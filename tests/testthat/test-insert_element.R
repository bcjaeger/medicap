

testthat::test_that("insert element works", {

  vec_test <- c(a = 1, b = 2)

  testthat::expect_equal(
    insert_element(vec_test,
                   element_value = 3,
                   element_name = 'c',
                   insert_before = 1),
    c(c=3, a=1, b=2)
  )

  testthat::expect_equal(
  insert_element(vec_test,
                 element_value = 3,
                 element_name = 'c',
                 insert_before = 2),
  c(a=1, c=3, b=2)
  )


  testthat::expect_error(
    insert_element(vec_test,
                   element_value = 3,
                   element_name = 'c',
                   insert_before = 3)
  )



  testthat::expect_equal(
    insert_element(vec_test,
                   element_value = 3,
                   element_name = 'c',
                   insert_before = 'a'),
    c(c=3, a=1, b=2)
  )



  testthat::expect_equal(
    insert_element(vec_test,
                   element_value = 3,
                   element_name = 'c',
                   insert_before = 'b'),
    c(a=1, c=3, b=2)
  )

})
