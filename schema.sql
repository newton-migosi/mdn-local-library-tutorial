create table if not exists greetings (
    id integer primary key autoincrement,
    lang_code text not null,
    msg text not null
);