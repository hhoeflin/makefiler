context("Rule")

test_that("Rule", {
    rule <- Rule$new(target="foo", phony=TRUE, target_types="fake", prereqs=c("bar", "baz"),
                     prereq_types=c("type_bar", "type_baz"), recipe=c("command1", "command2"))
    expect_equal(rule$get_rule(), "foo: bar baz\n\tcommand1\n\tcommand2")
    expect_equal(rule$get_targets(), "foo")
    expect_equal(rule$get_phony(), "foo")
    expect_equal(rule$get_prereqs(), c("bar", "baz"))
    expect_equal(rule$get_recipe(), c("command1", "command2"))
    expect_equal(rule$get_node_df_noID(), data.frame(id=1:3, type=rep(as.character(NA), 3), label=c("foo", "bar", "baz"), stringsAsFactors=FALSE))
    expect_equal(rule$get_edge_df_noID(), data.frame(from_label=c("foo", "foo"), to_label=c("bar", "baz"), stringsAsFactors = FALSE))
       
})
