define mkrule =
$(1): $(2:.c=.o)
	@echo $$@
	@echo ----
	@echo $$^
endef

$(eval $(call mkrule,default,a.c b.c))

.PHONY: default a.o b.o
