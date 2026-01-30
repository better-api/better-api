.PHONY: help
help:
	@echo "make test      # nextest: run all tests, show all statuses"
	@echo "make check     # clippy: all targets & features, deny warnings"
	@echo "make cov       # run tests with coverage report"
	@echo "make cov-html  # run tests with HTML coverage report"
	@echo "make cov-open  # run tests with HTML coverage report and open it after tests"

.PHONY: test
test:
	cargo nextest run

.PHONY: check
check:
	cargo clippy --all-targets --all-features -- -D warnings

.PHONY: cov
cov:
	cargo llvm-cov nextest

.PHONY: cov-html
cov-html:
	LLVM_COV_FLAGS="-coverage-watermark=80,50" cargo llvm-cov nextest --html

.PHONY: cov-open
cov-open:
	LLVM_COV_FLAGS="-coverage-watermark=80,50" cargo llvm-cov nextest --open
