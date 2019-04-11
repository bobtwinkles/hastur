default: foo bar

foo : a
	@echo "dep a"

foo : b
	@echo "dep b"

bar : b

bar : a
	@echo "depends on $^"

.PHONY: default foo bar a b
