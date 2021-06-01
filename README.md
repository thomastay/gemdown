# Thomas's Gemini Markdown format

Turns Markdown into Gemini!
Project is still in alpha stage.

## Building from source

You will need Clojure 1.10.3, as well as Java 16 and above (only used for the Records feature.).

Build the java files by running `make all`. Alternatively, run `javac src/me/ttay/parser_combinators/CharStream.java -d classes/`

No building of the Clojure files is needed, you can run it if you have Clojure installed.

## Usage

Run the project directly, like so:

    $ clojure -M -m me.ttay.thomas-gemdown resources/test1.md

Output:

> # Hello!
>
> This is just regular markdown
>
> But it can be converted into gemini text!
>
> - thing 1
> - thing 2
>
> < more gemtext omitted... >

Run the project's tests:

    $ clojure -X:test:runner

Build an uberjar:

    $ clojure -X:uberjar

This will update the generated `pom.xml` file to keep the dependencies synchronized with
your `deps.edn` file. You can update the version (and SCM tag) information in the `pom.xml` using the
`:version` argument:

    $ clojure -X:uberjar :version '"1.2.3"'

If you don't want the `pom.xml` file in your project, you can remove it, but you will
also need to remove `:sync-pom true` from the `deps.edn` file (in the `:exec-args` for `depstar`).

Run that uberjar:

    $ java -jar thomas-gemdown.jar

## Options

FIXME: listing of options this app accepts.

## Examples

...

### Bugs

...

## License

Copyright © 2021 Thomas

BSD-2 License.
