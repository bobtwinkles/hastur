empty :=
space := $(empty) $(empty)

define $(space)
 asdf
endef

default:
	@echo $ 

.PHONY: default
