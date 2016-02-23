# WebSQLCipher

WebSQLCipher aims to scale and distribute SQLCipher very easily.

**Right now, this project is a prototype, totally broken, and using SQLite**

## How to run it

    $ rebar get-deps
    $ rebar compile
    # Run it in development mode (the port 8080 is hardcoded in the code)
    $ erl -pa ebin -pa deps/*/ebin -eval "application:ensure_all_started(websqlcipher).

## How to query it

    $ curl http://localhost:8080/foocom \
           -X POST \
           -H "Accept: application/json" \
           -H "Content-Type: application/json" \
           -d '{"query": "select * from sqlite_master;", "parameters": []}' \
           -v

## TODO

  * Refactor `func(Object, Param1, Param2)` to the erlang style `func(Param1, Param2, Object)`
  * Annotate types
  * Write tests
  * Remove duplicated/copy-pasted code
