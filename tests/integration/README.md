# Integration Test Suite

Each level-2 subdirectory `<test_kind>/<test_name>` defines a test. A test is defined by an arbitrary shell script named `README` that typically runs KaSim and/or other analysis tools, outputting results in an `output` directory. For every generated file `output/<file>`, an expected output can be provided in the form of an `output/<file>.ref` file.

To run a particular test, run `make <test_dir>/error`. This executes the `README` script and compares all generated outputs to their expected versions. A diff is stored in an `error` file. The test is considered passing if and only if `error` is empty. While running the `README` script, `stdout` is captured and stored in `output/LOG` while `stderr` is captured and stored in `output/errors.log`.

To run all integration tests, run `make all`. To clean all generated files, run `make clean`.