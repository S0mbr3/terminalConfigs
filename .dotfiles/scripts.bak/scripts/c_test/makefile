compile_dynamic: dynamic_input
	@echo "Compilation terminated"
clean_dynamic:
	rm dynamic_input.o dynamic_input
dynamic_input: dynamic_input.o
	gcc -o dynamic_input dynamic_input.o
dynamic_input.o: dynamic_input.c
	gcc -c -Wall -O3 dynamic_input.c


compile_parser: number_parser
	@echo "number_parser compiled"

clean_parser:
	rm number_parser.o number_parser
number_parser: number_parser.o
	gcc -o number_parser number_parser.o
number_parser.o: number_parser.c
	gcc -c -Wall number_parser.c

compile_recursive_parser: recursive_parser
	@echo "recirsive_parser compiled"

clean_recursive_parser:
	rm recursive_parser.o recursive_parser

recursive_parser: recursive_parser.o
	gcc -o recursive_parser recursive_parser.o

recursive_parser.o: recursive_parser.c
	gcc -c -Wall recursive_parser.c
