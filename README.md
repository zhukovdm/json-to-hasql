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

The program has only command line interface. The following interaction scheme
is implemented.

1.  The user starts interaction with the shell commands described above.

2.  The program verifies input arguments, tries to open and read file.
    `Read` error is raised in case read operation is not possible.

3.  After raw database is read, the program tries to parse text into `JSON`
    structure. `parse` error is raised if parsing is not possible.

4.  The user is asked to provide `SQL` commands described below to show
    or modify current database. Only valid commands are executed, invalid
    commands are skipped. Modified database is kept only in the `RAM` memory
    and stored on a `HDD` at a time user exits the program.

5.  Once `exit` command is issued by the user and the name of output file is
    provided, the program tries to store modified database. If file does not
    exist, new file is created. If file is not empty, the content is overwritten
    by modified database. If write operation is not possible, the user is
    repeatedly asked to provide another location. The program successfully
    exits if write operation succeeds.

Supported `SQL` commands:

- `show <table name>`
- `find <object name> where <condition>`
- `select <list of cols | *> from <table name>`
- `select <list of cols | *> from <table name> where <condition>`
- `modify <table name> <column name> <new value>`
- `exit`

## References

- [Official JSON webpage](https://www.json.org/json-en.html)
- [JSON formatter and validator](https://jsonformatter.curiousconcept.com/#)
- [ECMA-404. The JSON data interchange syntax](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/)
