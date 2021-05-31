# Thomas's Gemini Markdown format

Turns Markdown into Gemini!
Project is still in alpha stage.

## Usage

You will need Java 16 and above (only used for the Records feature.)

Run the project directly, via `:main-opts` (`-m me.ttay.thomas-gemdown`):

    $ clojure -M:run-m
    Hello, World!

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
