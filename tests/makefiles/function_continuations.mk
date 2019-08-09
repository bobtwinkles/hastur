a:
	echo '(a \
	 b)'
	echo '$(if t,a \
	 b)'
	echo '$(if t,a \\\
	 b)'
	echo '$(if t,a \\\\\
	 b)'
	echo '$(if t,a \\\\\
	 b)'
	echo '(a \
 b)'
	echo '$(if t,a \
	\
	b \
	\
\
	c)'
