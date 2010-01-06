#! /bin/sh

set -e

mkdir -p local

sq() {
    echo "$*" | sqlite3 local/bills.db 
}

#sq "create table users(username primary key not null);"
#sq "create table bills(billident INTEGER primary key, date TEXT, description TEXT);"
#sq "create table charges(bill REFERENCES bills (billident), chargeident INTEGER primary key autoincrement, user REFERENCES users(username), amount REAL);"

#sq "alter table users add column password;"

#sq "alter table users add column is_admin;"

sq "alter table bills add column owner REFERENCES users (username) NOT NULL;"
