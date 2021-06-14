all: parser_combinator_javac

parser_combinator_javac: src/me/ttay/parser_combinators/CharStream.java
	javac --release 16 src/me/ttay/parser_combinators/CharStream.java -d classes/
