#! /bin/sh

set -e

mkdir -p local

sq() {
    echo "$*"
    echo "$*" | sqlite3 local/bills.db 
}

sq "create table users(username primary key not null);"
sq "create table bills(billident INTEGER primary key, date TEXT, description TEXT, owner TEXT REFERENCES users (username) NOT NULL);"
sq "create table charges(bill REFERENCES bills (billident), chargeident INTEGER primary key autoincrement, user REFERENCES users(username), amount REAL);"

sq "alter table users add column password;"

sq "alter table users add column is_admin;"

sq "create table bill_attachments(attach_ident INTEGER PRIMARY KEY AUTOINCREMENT, bill_ident REFERENCES bills(billident), content BLOB, filename);"
