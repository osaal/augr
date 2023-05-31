# augr 0.2.0

This is the first functional version of Augr! Note that things are still very unstable (and untested). Use with caution, and check results. Documentation and convenience functions upcoming (hopefully) soon!

## Major changes

* Added `augr()`. Documentation to come!
* Added internal functions:
  * `enable_internals()`: Sets a global flag to allow use of internal functions.
  * `disable_internals()`: Removes the flag set by `enable_internals()`.
  * `enable_verbose()`: Sets a global flag which currently does nothing.
  * `disable_verbose()`: Removes the flag set by `enable_verbose()`.
  * `ext_check()`: Helper function to check for global flag enabling external use. Not for external use itself!
  * `extract_data()`: Extracts data from numerous data objects and creates an AugrData object. Data objects currently supported: Matrix, tibble, data frame (technically, anything that inherits from data frame).
  * `extract_network()`: Extracts relevant information from numerous network objects and creates an AugrNetwork object. Networks currently supported: `qgraph`, `bootnet` and `bootnetResult`, `psychonetrics`, edge weight matrix as `matrix`.
  * `probabilities()`: Calculates cumulative updating probabilities for all nodes in a network, based on a standardization function. Functions currently supported: sigmoid normalization of additive probability, independent multiplicative probability.
  * `update_data()`: Updates a dataset based on a probability matrix from a network.
* Added S4 classes:
  * `AugrData`: Object containing data used in Augr.
  * `AugrNetwork`: Object containing networks used in Augr.

# augr 0.1.0.9003

* Added a `NEWS.md` file to track changes to the package.
