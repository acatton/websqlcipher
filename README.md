# WebSQLCipher

WebSQLCipher aims to scale and distribute SQLCipher very easily.

**Right now, this project is a prototype, totally broken, and using SQLite**

## How to run it

    $ rebar get-deps
    $ rebar compile
    # Run it in development mode (the port 8080 is hardcoded in the code)
    $ erl -pa ebin -pa deps/*/ebin -eval "application:ensure_all_started(websqlcipher).
