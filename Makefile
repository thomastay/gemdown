all: parser_combinator_javac

parser_combinator_javac:
	javac src/me/ttay/parser_combinators/CharStream.java -d classes/
