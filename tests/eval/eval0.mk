define fragment
 foo := bar
endef

foo := baz

$(eval $(fragment))

default:
	@echo $(foo)

.PHONY: default
