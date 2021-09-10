# json-to-hasql

## How to start?

Run the following commands in the terminal.

```console
git clone https://github.com/zhukovdm/json-to-hasql.git
cd json-to-hasql/src/
ghc Main.hs -o main
./main <database name>
```

## How to use?

The following interaction scheme via command line is implemented.

1.  The user starts interaction with the shell commands described above.

2.  The program verifies input arguments, tries to open and read file.
    `Read` error is raised if smth is wrong.

3.  The program tries to parse raw database into `JSON` structure. `parse`
    error is raised if smth is wrong.

4.  The user is asked to provide **requests** described below to show
    or modify current database. Only valid commands are executed, invalid
    commands are skipped. During interaction, modified database is kept only
    in the **RAM** memory. It is stored on a **HDD** at a time user exits
    the program via `quit` command.

5.  Once `quit` command is issued by the user and the name of output file is
    provided, the program tries to store modified database. If file does not
    exist, new file is created. If file is not empty, the content is
    overwritten by modified database. If write operation is not possible,
    the user is repeatedly asked to provide another location. The program
    successfully exits if write operation succeeds.

## Supported request formats

- [x] `show <table name>`, ex. `show people`
- [x] `pick <col1, col2, ...> from <table name>`, ex. `pick firstname, surname from people`
- [x] `find <table name> with <json value>`, ex. `find fruits with "yellow"`
- [x] `bulk <json value> to <json value> in <table name>`, ex. `bulk null to 42 in people`
- [x] `cadd <column name> into <table name>`, ex. `cadd "taste" into fruits`
- [x] `radd <table name>`, ex. `radd fruits`
- [x] `evic <row #> from <table name>`, ex. `evic 1 from fruits`
- [x] `yank <col #> from <table name>`, ex. `yank 1 from fruits`
- [ ] `modi <row #> <col #> to <json value> in <table name>`, ex. `modi 1 1 to null in fruits`
- [x] `quit`

## References

- [Official JSON webpage](https://www.json.org/json-en.html)
- [JSON formatter and validator](https://jsonformatter.curiousconcept.com/#)
- [ECMA-404. The JSON data interchange syntax](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/)
