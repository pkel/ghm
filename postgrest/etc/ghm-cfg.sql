set search_path to internal;

/*
 * some useful values for our own setup
 */

insert into rooms (name, beds) values ('Zimmer 1',     1);
insert into rooms (name, beds) values ('Zimmer 2',     2);
insert into rooms (name, beds) values ('Zimmer 3',     1);
insert into rooms (name, beds) values ('Zimmer 4',     2);
insert into rooms (name, beds) values ('Zimmer 5',     3);
insert into rooms (name, beds) values ('Zimmer 6',     4);
insert into rooms (name, beds) values ('Zimmer 7',     2);
insert into rooms (name, beds) values ('Zimmer 8',     2);
insert into rooms (name, beds) values ('Zimmer 9',     2);
insert into rooms (name, beds) values ('Zimmer 10',    2);
insert into rooms (name, beds) values ('Zimmer 11',    2);
insert into rooms (name, beds) values ('Zimmer 12',    2);
insert into rooms (name, beds) values ('Außenzimmmer', 2);

/* id 0 is default, remember! */
insert into booking_states (booking_state_id, name, consider) values (0, 'Nicht bestätigt', false);
insert into booking_states (booking_state_id, name, consider) values (1, 'Bestätigt',       true);
insert into booking_states (booking_state_id, name, consider) values (2, 'Storniert',       false);
insert into booking_states (booking_state_id, name, consider) values (3, 'ex combit',       false);
