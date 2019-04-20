foo := $ bar
baz := $
qux := $	bar

default:
	@echo $(foo)
	@echo $(baz)

.PHONY: default
