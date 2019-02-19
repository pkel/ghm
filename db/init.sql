/* customers */

drop table if exists customers cascade;

create table customers (
    customer_id bigint generated always as identity primary key,
    created timestamp not null default now (),
    modified timestamp not null default now (),
    data jsonb not null);

-- automatic ids
create or replace function set_modified_now ()
returns trigger as $$
begin
   new.modified = now();
   return new;
end;
$$ language 'plpgsql';

create trigger customers_set_modified_now before update on customers
for each row execute procedure set_modified_now ();

-- keyword search
create or replace function keyword(customers) returns text as $$
  select $1.data->>'keyword';
$$ language sql;


/* users */

drop role if exists anonymous;
drop role if exists ghm_user;

-- users with minimal rights
create role anonymous with noinherit nologin;
create role ghm_user with noinherit nologin;

grant all on customers to ghm_user;

drop schema if exists auth cascade;
create schema auth;

create table auth.users (
  id text not null,
  pass text not null,
  role name not null);

-- enforce role being a postgres db role
create or replace function auth.check_role_exists() returns trigger as $$
begin
  if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
    raise foreign_key_violation using message =
      'unknown database role: ' || new.role;
    return null;
  end if;
  return new;
end
$$ language plpgsql;

drop trigger if exists ensure_user_role_exists on auth.users;
create constraint trigger ensure_user_role_exists
  after insert or update on auth.users
  for each row
  execute procedure auth.check_role_exists();

-- keep passwords safe
create or replace function auth.encrypt_pass() returns trigger as $$
begin
  if tg_op = 'INSERT' or new.pass <> old.pass then
    new.pass = crypt(new.pass, gen_salt('bf'));
  end if;
  return new;
end
$$ language plpgsql;

drop trigger if exists encrypt_pass on basic_auth.users;
create trigger encrypt_pass
  before insert or update on auth.users
  for each row
  execute procedure auth.encrypt_pass();


-- public login function
create type auth.jwt_token as (token text);

create or replace function login(id text, pass text) returns auth.jwt_token as $$
declare
  _role name;
  result auth.jwt_token;
begin
  -- check email and password
  select auth.user_role(id, pass) into _role;
  if _role is null then
    raise invalid_password using message = 'invalid user or password';
  end if;

  select jwt.sign(
      row_to_json(r), 'reallyreallyreallyreallyverysafe'
    ) as token
    from (
      select _role as role, login.id as id,
         extract(epoch from now())::integer + 60*60 as exp
    ) r
    into result;
  return result;
end;
$$ language plpgsql;

-- dummy user
-- TODO: remove dummy user
insert into auth.users(id, pass, role) values('dummy', 'trivial', 'ghm_user');

/* authentication */

create or replace function auth.user_role(id text, pass text) returns name as $$
begin
  return (
  select role from auth.users
   where users.id = user_role.id
     and users.pass = crypt(user_role.pass, users.pass)
  );
end;
$$ language plpgsql;

