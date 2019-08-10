a:
	@echo '$(words a,b)'
	@echo '$(words a, b)'
	@echo '$(word 1,foo, bar)'
	@echo '$(word 1,foo,bar)'

.PHONY: a
