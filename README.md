# banks2ledger

![Build Status](https://github.com/laurio/banks2ledger/actions/workflows/build.yaml/badge.svg)

A tool to convert bank account CSV files to ledger. Guesses account
name via simple Bayesian inference based on your existing ledger file.
Read about the motivation, the algorithm and the workflow in [this
article].

## Running and installation

banks2ledger is written in [Clojure], and [Babashka] is all you
should need to get up and running.

After installing Babashka the program can be run directly: `./src/banks2ledger/banks2ledger.clj`.

## Usage

    Usage: banks2ledger [options]
      available options (syntax to set: -x value)
        --ledger-file :
             Ledger file to get accounts and probabilities
             default: ledger.dat
        --csv-file :
             Input transactions in CSV format
             default: transactions.csv
        --csv-file-encoding :
             Encoding of the CSV file
             default: UTF-8
        --account :
             Originating account of transactions
             default: Assets:Checking
        --csv-field-separator :
             CSV field separator
             default: ,
        --csv-skip-header-lines :
             CSV header lines to skip
             default: 0
        --csv-skip-trailer-lines :
             CSV trailer lines to skip
             default: 0
        --currency :
             Currency
             default: SEK
        --date-format :
             Format of date field in CSV file
             default: yyyy-MM-dd
        --date-col :
             Date column index (zero-based)
             default: 0
        --ref-col :
             Payment reference column index (zero-based)
             default: -1
        --amount-col :
             Amount column index (zero-based)
             default: 2
        --descr-col :
             Text (descriptor) column index specs (zero-based)
             default: %3
        --amount-decimal-separator :
             Decimal sign character
             default: .
        --amount-grouping-separator :
             Decimal group (thousands) separator character
             default: ,
        --hooks-file :
             Hooks file defining customized output entries
             default: nil
        --debug :
             Include debug information in the generated output
             default: false

`banks2ledger` will write ledger transactions to standard output. It
will not modify the contents of any file unless you use an output
redirect (eg. `> out.dat`) in the shell.

The program expects to be set up so the structure of the CSV can be
correctly parsed (no guessing there). It also expects to be able to
read your main ledger file containing all those transactions that will
form the basis of the Bayesian inference for newly created
transactions.

The program can also be compiled to a jar file by executing
`./uberdeps/package.sh` in the projects root folder.

### CSV column mapping

The default value for `--ref-col` (-1) means that in case you don't have a
reference column in the CSV, you can simply omit this option and no
reference column will be used. Otherwise, set it to the column number
for the payment reference, and the data from there will be printed as
part of the generated ledger entry.

The `--descr-col` option takes something called 'column index specs' that
warrants further explanation. Since the description string forms
the basis of the account inference, and different banks provide
different layouts in their CSV files (even multiple possible layouts
for the same file provider) this is used as a flexible way to create
the descriptor.

The specs string provided for the `--descr-col` option is a string similar to a
printf format string, but allows multiple alternatives to be
specified. Alternatives will be tried in order from left to right,
and the next alternative is considered only if the current one results
in an empty string. Alternatives are separated by the exclamation
mark, and individual columns in the CSV are referenced by `%n` (n is
the column number; columns are numbered starting with 0).

Examples to provide as the `--descr-col` option:

- `"%4"`: get fourth column
- `"%4 %5"`: get fourth and fifth column separated by a space
- `"%4!%1 %2 %3!%7"`: fourth column by default, but if that is empty
  (contains only whitespace) concatenate the first three columns; if
  that in turn is empty, take the seventh column.

### Amount format

The `--amount-decimal-separator` and `--amount-grouping-separator` options allow parsing almost arbitrarily formatted
decimal numbers from the amount column. Their usage is entirely
optional. With both options omitted, the accepted numbers are of the
usual "Western" format, i.e., a string of digits starting with an
optional minus for negative numbers, grouped with optional commas for
thousands (or other) separation, followed by an optional fractional
part after a decimal dot.

Evident garbage (text that cannot be possibly part of a number) both
before and after the number is implicitly discarded, so having a
currency as part of the amount field should not be a problem.

Examples of setting `--amount-decimal-separator` and `--amount-grouping-separator` for parsing different amount
formats:

|   Example amount | Parse options                                                    |
|-----------------:|------------------------------------------------------------------|
| `"1,234,567.89"` | *(defaults)*                                                     |
| `"1.234.567,89"` | `--amount-decimal-separator ',' --amount-grouping-separator '.'` |
| `"1 234 567,89"` | `--amount-decimal-separator ',' --amount-grouping-separator ' '` |
|  `"123_4567.89"` | `--amount-grouping-separator '_'`                                |

### Custom hooks to generate output transactions

The option `--hooks-file` allows passing a file containing Clojure code that
defines custom hooks. These hooks are invoked when generating the
output ledger entries; the hooks have the ability to alter the output
for certain transactions. This mechanism provides high flexibility as
all data concerning the ledger entry, as well as the full power of
Clojure (and several helper functions in the code of `banks2ledger`)
are available to the hook.

An example hooks file is provided as part of the test suite at
`test/data/hooks.clj`. A [detailed tutorial][hooks-tutorial] on this
feature is also available.

## Status

`banks2ledger` development is governed, first and foremost, by the
author's own needs. Naturally, pull requests to add features or fix
problems by other developers are gladly considered.

Because of the above, the program is not aiming to be complete. In
particular, it does not implement every documented feature of
`ledger-cli` file syntax. If you run into problems (i.e. because
`banks2ledger` does not parse your ledger file that is flawlessly
parsed by ledger itself) please open an issue with a specific minimal
example demonstrating the problem.

## Development

Feel free to open a pull request if you find a bug, or have a feature
you would like to see included.

There are several unit tests you can run via `bb test-runner.clj`. Make sure
they don't break; also, add coverage for any new functionality you
might add or regression tests for bugs you might fix.

The command `bb test.clj` runs the unit tests, and if they are successful,
proceeds with doing some end-to-end testing with "real" files. The
input files are under `test/data/` along with the reference output,
which is used to validate the results. The test script also shows the
usual invocation (parameterization) of banks2ledger for differently
structured CSV files. For real production usage, it is recommended to
roll a `Makefile` or similar solution to process your input files; see
[this article] for an example.


[this article]:               https://tomscii.sig7.se/2016/04/Payment-matching-done-right

[hooks-tutorial]:             https://tomscii.sig7.se/2018/08/Custom-transactions-in-banks2ledger

[Clojure]:                    https://clojure.org

[Babashka]:                   https://babashka.org
