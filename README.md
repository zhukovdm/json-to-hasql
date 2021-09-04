# json-to-hasql

[JSON](https://www.json.org/json-en.html)

## How to start?

Execute the following commands in the shell.

```console
git clone https://github.com/zhukovdm/json-to-hasql.git
cd json-to-hasql/src/
ghc Main.hs -o main
./main
```

## How to use?

The following interaction scheme is applied:

1.  Get database name from the user via interactive shell.

2.  Try to open file by the name and read raw text.
    Exit with the error if not possible.

3.  Try to parse raw text into flat JSON database.
    Exit with the error if not possible.

4.  Execute SQL commands entered by the user via shell. Only valid
    commands are executed, invalid commands are skipped.

5.  The user is asked to enter database file name, where modified database
    will be stored. The program tries to open the file and overwrite it
    with the modified database. If the file cannot be overwriten or open,
    the user is asked to enter a different name. The program keeps trying
    unless write operation succeeds.

Supported SQL commands:

- `show tables` prints all available (parsed) table names
- `select * from table` prints the entire `table`
- `select ? from table where ?`
- ...

## References

- [ECMA-404. The JSON data interchange syntax](https://www.ecma-international.org/publications-and-standards/standards/ecma-404/)
