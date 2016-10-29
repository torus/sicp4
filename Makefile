default: driver-loop

test:
	gosh sicp4-1.scm

driver-loop:
	gosh -l ./sicp4-1.scm -e '(driver-loop)'
