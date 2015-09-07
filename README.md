How to run the test suite
=========================


Chibi
-----

- Install Chibi Scheme.

- Clone <https://github.com/larcenists/larceny>.

- Enter the directory of this repository and run:

    ```
    chibi -A . \
          -A "$larceny_sources"/tools/R6RS \
          run-tests.scm
    ```

and it will print the results as well as indicate via process exit
status whether the suite passed.


Larceny
-------

- Install Larceny, including the `tools/R6RS/`.

- Enter the directory of this repository and run:

    ```
    larceny -path .:"$larceny_sources"/tools/R6RS \
            -r7rs \
            -program run-tests.scm
    ```
