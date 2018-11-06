context("test-ebi_server")

#
# is_ebi_reachable()
#

# Still to be found a good way of testing is_ebi_reachable().
# This is because this function is not pure, and hence depends
# on the network connectivity status. I need to figure out a way
# of simulating the network status... need something lower level
# than httptest.
