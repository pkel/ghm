set search_path = auth;

create table users (
  id text not null,
  pass text not null,
  role name not null);

-- enforce role being a postgres db role
create or replace function check_role_exists() returns trigger as $$
begin
  if not exists (select 1 from pg_roles as r where r.rolname = new.role) then
    raise foreign_key_violation using message =
      'unknown database role: ' || new.role;
    return null;
  end if;
  return new;
end
$$ language plpgsql;

create constraint trigger ensure_user_role_exists
  after insert or update on users
  for each row
  execute procedure check_role_exists();

-- keep passwords safe
create or replace function encrypt_pass() returns trigger as $$
begin
  if tg_op = 'INSERT' or new.pass <> old.pass then
    new.pass = crypto.crypt(new.pass, crypto.gen_salt('bf'));
  end if;
  return new;
end
$$ language plpgsql;

create trigger encrypt_pass
  before insert or update on users
  for each row
  execute procedure encrypt_pass();

/* authentication TODO: This is now done by php. Drop */

create or replace function user_role(id text, pass text) returns name as $$
begin
  return (
  select role from auth.users
   where users.id = user_role.id
     and users.pass = crypto.crypt(user_role.pass, users.pass)
  );
end;
$$ language plpgsql;

-- public login function
create type jwt_token as (token text);

create or replace function api.login(id text, pass text) returns auth.jwt_token as $$
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
      row_to_json(r), current_setting('app.settings.jwt_secret')
    ) as token
    from (
      select _role as role, login.id as id,
         extract(epoch from now())::integer + 60*5 as exp -- token valid for 5 minutes
    ) r
    into result;
  return result;
end;
$$ language plpgsql;
