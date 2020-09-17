# 3D

.SUFFIXES: .erl .beam .yrl

.erl.beam:
		erlc -W $<

MODS = math_ele math_ele_lid unit_test
all:	${MODS:%=%.beam} math_ele math_ele_drv.so
		@erl -noshell -s unit_test start
math_ele:	math_ele.c erl_comm.c math_driver.c
		gcc -o math_ele math_ele.c erl_comm.c math_driver.c
math_ele_drv.so:	math_ele_lid.c math_ele.c
		# this line need to be updated with your erlang location
		gcc -arch i386 -I /usr/local/lib/erlang/usr/include\ 
		-o math_ele_drv.so -fPIC -bundle -flat_namespace -undefined suppress\
		math_ele.c math_ele_lid.c
clean:
	rm math_ele math_ele_drv.so *.beam
