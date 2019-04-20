define \\
	bar
endef

colon := :

$(colon) := baz

default:
	@echo $\\
	
	@echo $\

.PHONY: default
