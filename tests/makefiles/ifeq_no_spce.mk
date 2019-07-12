ifeq "a""a"
	FOO := bar
else
	FOO := baz
endif

default:
	@echo $(FOO)

.PHONY: default
