test_that("shared organizations are found", {

  ## both in org 00581, non-overlapping dates
  share_1 <- get_shared_orgs("MEM0132", "MEM0152")
  expect_equal(c("ORG00907", "ORG00317", "ORG00581"),
               share_1$Org.ID)


  ## No shared orgs
  share_0 <- get_shared_orgs("MEM0132", "MEM0004")

  expect_equal(nrow(share_0), 0)

  expect_equal(names(share_1), names(share_0))


})


test_that("shared members are found", {

  ## both in org 00581, non-overlapping dates
  share_1 <- get_shared_members("ORG00907", "ORG00581")
  expect_equal(c("MEM0132", "MEM0152"),
               share_1$Member.ID)


  ## No shared orgs
  share_0 <- get_shared_members("ORG00907", "ORG00908")

  expect_equal(nrow(share_0), 0)

  expect_equal(names(share_1), names(share_0))


})

