set search_path to public;

/*
 * combit csv export structure
 */
create temporary table combit (
/* create temporary table combit ( */
  firma      text, firma2     text, firma3     text, abteilung  text,
  anrede     text, anredebr   text, name       text, vorname    text,
  such       text, land       text, plzz       text, ort        text,
  strasse    text, hnr        text, plzp       text, postfach   text,
  kind_unt16 text, kind_ueb16 text, geb_jahr01 text, geb_jahr02 text,
  anz_tag    text, anreise    text, abreise    text, geb_dat01  text,
  geb_dat02  text, geb_dat03  text, geb_dat04  text, geb_dat05  text,
  geb_dat06  text, p_ort      text, p_plz      text, p_land     text,
  p_strasse  text, p_anrede01 text, p_anrede02 text, p_vorname  text,
  p_vorname3 text, p_vorname4 text, p_vorname5 text, p_vorname6 text,
  p_name     text, p_name3    text, p_name4    text, p_name5    text,
  p_name6    text, telefon    text, telefax    text, telefax2   text,
  telefon2   text, mobiltel   text, email      text, internet   text,
  email2     text, kundennr   text, kategorie  text, k_code     text,
  bemerkung  text, kontakte   text, dokumente  text, verweise   text,
  stichwort1 text, stichwort2 text, zusatz1    text, zusatz2    text,
  zusatz3    text, zusatz4    text, zusatz5    text, blz        text,
  ktonr      text, bank       text, grafik1    text, grafik2    text,
  von        text, bis        text, art        text, uebernacht text,
  ue01_von   text, ue01_bis   text, ue01_zahl  text, ue02_von   text,
  ue02_bis   text, ue02_zahl  text, ue03_von   text, ue03_bis   text,
  ue03_zahl  text, ue04_von   text, ue04_bis   text, ue04_zahl  text,
  ue05_von   text, ue05_bis   text, ue05_zahl  text, ue06_von   text,
  ue06_bis   text, ue06_zahl  text, ue07_von   text, ue07_bis   text,
  ue07_zahl  text, ue08_von   text, ue08_bis   text, ue08_zahl  text,
  ue09_von   text, ue09_bis   text, ue09_zahl  text, ue10_von   text,
  ue10_bis   text, ue10_zahl  text, anzahl1    text, anzahl2    text,
  anzahl3    text, anzahl4    text, art1       text, art2       text,
  art3       text, art4       text, art5       text, art6       text,
  preis1     text, preis2     text, preis3     text, preis4     text,
  proz1      text, proz2      text, proz3      text, proz4      text,
  summe1     text, summe2     text, summe3     text, summe4     text,
  summe5     text, summe6     text, resumme    text, zimmer     text,
  archivja   text, agef       text, aeing      text, bemaufe    text,
  avon       text, abis       text, uea_zahl   text, erfdat     text,
  erfuser    text, l_dat      text, l_user     text, recordid   text,
  groupid    text, land___ausgeschrieben text
  );

/*
 * import csv
 */
\copy combit from './combit/data.csv' delimiter ';' csv header;

/*
 * lets keep a backup
 */
select * into table combit_import_source from combit;

/*
 * drop nonsense
 */
alter table combit
  drop column kontakte,
  drop column zusatz1,
  drop column zusatz2,
  drop column zusatz3,
  drop column zusatz4,
  drop column zusatz5,
  drop column blz,
  drop column ktonr,
  drop column bank,
  drop column von,
  drop column bis,
  drop column art,
  drop column uebernacht,
  drop column grafik1,
  drop column grafik2,
  drop column firma2,
  drop column firma3,
  drop column dokumente,
  drop column stichwort1,
  drop column stichwort2,
  drop column summe1,
  drop column summe2,
  drop column summe3,
  drop column summe4,
  drop column summe5,
  drop column summe6,
  drop column resumme,
  drop column verweise,
  drop column archivja,
  drop column ue01_von,
  drop column ue01_bis,
  drop column ue01_zahl,
  drop column ue02_von,
  drop column ue02_bis,
  drop column ue02_zahl,
  drop column ue03_von,
  drop column ue03_bis,
  drop column ue03_zahl,
  drop column ue04_von,
  drop column ue04_bis,
  drop column ue04_zahl,
  drop column ue05_von,
  drop column ue05_bis,
  drop column ue05_zahl,
  drop column ue06_von,
  drop column ue06_bis,
  drop column ue06_zahl,
  drop column ue07_von,
  drop column ue07_bis,
  drop column ue07_zahl,
  drop column ue08_von,
  drop column ue08_bis,
  drop column ue08_zahl,
  drop column ue09_von,
  drop column ue09_bis,
  drop column ue09_zahl,
  drop column ue10_von,
  drop column ue10_bis,
  drop column ue10_zahl,
  drop column k_code;


/*
 * convert types, where needed
 */

alter table combit
  alter column recordid type int using cast(nullif(recordid, '') as int),
  alter column groupid type int using cast(nullif(groupid, '') as int),
  alter column avon type date using to_date(nullif(avon,''),'DD.MM.YYYY'),
  alter column abis type date using to_date(nullif(abis,''),'DD.MM.YYYY'),
  alter column anreise type date using to_date(nullif(anreise,''),'DD.MM.YYYY'),
  alter column abreise type date using to_date(nullif(abreise,''),'DD.MM.YYYY'),
  alter column geb_dat01 type date using to_date(nullif(geb_dat01,''),'DD.MM.YYYY'),
  alter column geb_dat02 type date using to_date(nullif(geb_dat02,''),'DD.MM.YYYY'),
  alter column geb_dat03 type date using to_date(nullif(geb_dat03,''),'DD.MM.YYYY'),
  alter column geb_dat04 type date using to_date(nullif(geb_dat04,''),'DD.MM.YYYY'),
  alter column geb_dat05 type date using to_date(nullif(geb_dat05,''),'DD.MM.YYYY'),
  alter column geb_dat06 type date using to_date(nullif(geb_dat06,''),'DD.MM.YYYY'),
  alter column preis1 type numeric using replace(preis1,',','.')::numeric,
  alter column preis2 type numeric using replace(preis2,',','.')::numeric,
  alter column preis3 type numeric using replace(preis3,',','.')::numeric,
  alter column preis4 type numeric using replace(preis4,',','.')::numeric,
  alter column proz1 type numeric using cast(proz1 as numeric),
  alter column proz2 type numeric using cast(proz2 as numeric),
  alter column proz3 type numeric using cast(proz3 as numeric),
  alter column proz4 type numeric using cast(proz4 as numeric),
  alter column agef type numeric using cast(nullif(replace(agef,',','.'), '0.00') as numeric),
  alter column aeing type numeric using cast(nullif(replace(aeing,',','.'), '0.00') as numeric);

/*
 * add group where missing
 */
update combit set groupid = coalesce(groupid, recordid);

/*
 * negative values for deposit are not what we want
 */
update combit set agef = NULL where agef < 0;
update combit set aeing = NULL where aeing < 0;

/* fill in date from "Stammdaten", where nothing else provided */
update combit set avon = coalesce(avon, anreise);
update combit set abis = coalesce(abis, abreise);

/* Combit prefilled telefon&co with <vorwahl>/ */
update combit set telefon  = '' where trim(telefon ) like '%/';
update combit set telefon2 = '' where trim(telefon2) like '%/';
update combit set telefax  = '' where trim(telefax ) like '%/';
update combit set telefax2 = '' where trim(telefax2) like '%/';
update combit set mobiltel = '' where trim(mobiltel) like '%/';

/*
 * sanity on ids
 */
alter table combit
  alter column recordid set not null,
  add primary key (recordid),
  alter column groupid set not null;

/*
 * copy new customer data
 */

/* TODO: old revisions are ignores here. */

create function customerNote(text, text, text, text)
  returns text
  as $$
  declare
    a text;
    b text;
    c text;
    d text;
    note text;
  begin
    select case when trim($1) like '' then NULL else concat('**Kunden-Nr.:** ' , $1, '  ') end into a;
    select case when trim($2) like '' then NULL else concat('**Kategorie:** '  , $2, '  ') end into b;
    select case when trim($3) like '' then NULL else concat('**Plzp:** '       , $3, '  ') end into c;
    select case when trim($4) like '' then NULL else concat('**Fach:** '       , $4, '  ') end into d;
    select concat_ws (E'\n', a , b, c, d) into note;
    select case when trim(note) like '' then '' else
      concat_ws (E'\n', '###### Zusatzfelder (Combit)', note) end into note;
    return note;
  end;
  $$ language plpgsql;

insert into customers (
  customer_id,
  title,
  title_letter,
  given,
  family,
  company,
  company_address,
  keyword,
  street,
  street_number,
  city,
  postal_code,
  country,
  country_code,
  phone,
  phone2,
  mobile,
  fax,
  fax2,
  mail,
  mail2,
  web,
  note
  )
select distinct on (groupid)
/* the following lines are reference. use copy and paste to fill the above */
  groupid               as customer_id,
  anrede                as title,
  anredebr              as title_letter,
  vorname               as given,
  name                  as family,
  firma                 as company,
  abteilung             as company_address,
  such                  as keyword,
  strasse               as street,
  hnr                   as street_number,
  ort                   as city,
  plzz                  as postal_code,
  land___ausgeschrieben as country,
  land                  as country_code,
  telefon               as phone,
  telefon2              as phone2,
  mobiltel              as mobile,
  telefax               as fax,
  telefax2              as fax2,
  email                 as mail,
  email2                as mail2,
  internet              as web,
  customerNote(kundennr, kategorie, plzp, postfach)
                        as note
from combit
order by combit.groupid asc, combit.recordid desc;

/*
 * bookings
 */

create function bookingNote(int)
  returns text
  as $$
  declare
    note text;
    a text;
    b text;
    c text;
    d text;
  begin
    select case when trim(bemaufe) like '' then '' else
      concat_ws(E'\n'
        ,'###### Bemerkung Aufenthalt (Combit)'
        , replace(bemaufe,E'\n',E'\n\n')
      )
      end
      from combit where recordid = $1
      into note;
    select
      case when concat(trim(art1),preis1) like '0.00' then Null else
        concat('  - ', anzahl1, 'x ', art1, ' à ', preis1, '€ (', proz1, '%)')
      end
      from combit where recordid = $1
      into a;
    select
      case when concat(trim(art2),preis2) like '0.00' then Null else
        concat('  - ', anzahl2, 'x ', art2, ' à ', preis2, '€ (', proz2, '%)')
      end
      from combit where recordid = $1
      into b;
    select
      case when concat(trim(art3),preis3) like '0.00' then Null else
        concat('  - ', anzahl1, 'x ', art3, ' à ', preis3, '€ (', proz3, '%)')
      end
      from combit where recordid = $1
      into c;
    select
      case when concat(trim(art4),preis4) like '0.00' then Null else
        concat('  - ', anzahl4, 'x ', art4, ' à ', preis4, '€ (', proz4, '%)')
      end
      from combit where recordid = $1
      into d;
    select case when trim(concat(a,b,c,d)) like '' then note else
      concat_ws(E'\n',note,'', '###### Posten (Combit)', a, b, c, d)
      end into note;
    return note;
  end;
  $$ language plpgsql;

insert into bookings (
  booking_id,
  customer_id,
  state,
  deposit_asked,
  deposit_got,
  note
  )
select
  recordid,
  groupid,
  3,
  agef,
  aeing,
  bookingNote(recordid)
from combit;


/* individuals */

create temporary table booked_individuals (
  booking_id int ,
  given text,
  family text,
  date_of_birth date
  );

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  vorname,
  name,
  geb_dat01
from combit;

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  p_vorname,
  p_name,
  geb_dat02
from combit
where trim ( concat (
  p_vorname,
  p_name,
  geb_dat02
) ) not like '' ;

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  p_vorname3,
  p_name3,
  geb_dat03
from combit
where trim ( concat (
  p_vorname3,
  p_name3,
  geb_dat03
) ) not like '' ;

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  p_vorname4,
  p_name4,
  geb_dat04
from combit
where trim ( concat (
  p_vorname4,
  p_name4,
  geb_dat04
) ) not like '' ;

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  p_vorname5,
  p_name5,
  geb_dat05
from combit
where trim ( concat (
  p_vorname5,
  p_name5,
  geb_dat05
) ) not like '' ;

insert into booked_individuals (
  booking_id,
  given,
  family,
  date_of_birth
  )
select
  recordid,
  p_vorname6,
  p_name6,
  geb_dat06
from combit
where trim ( concat (
  p_vorname6,
  p_name6,
  geb_dat06
) ) not like '' ;


/* build json and update bookings */
update bookings b
set individuals=i.j
from (
  select booking_id, to_json(array_agg(t)) j
  from booked_individuals t
  group by t.booking_id
  ) i
where b.booking_id = i.booking_id;


/* Booked rooms
 *
 * /* */
 *  * Now lets try to contruct stuff which is not given explicitly.
 *  * TODO: Comparing with number of persons, could yield better results
 *  */ /*
 * alter table combit add column beds1 int;
 * alter table combit add column beds2 int;
 * alter table combit add column beds3 int;
 * alter table combit add column beds4 int;
 * update combit set beds1 = 1 where art1 ilike '%Einzelzimmer%';
 * update combit set beds2 = 1 where art2 ilike '%Einzelzimmer%';
 * update combit set beds3 = 1 where art3 ilike '%Einzelzimmer%';
 * update combit set beds4 = 1 where art4 ilike '%Einzelzimmer%';
 * update combit set beds1 = 2 where art1 ilike '%Doppelzimmer%';
 * update combit set beds2 = 2 where art2 ilike '%Doppelzimmer%';
 * update combit set beds3 = 2 where art3 ilike '%Doppelzimmer%';
 * update combit set beds4 = 2 where art4 ilike '%Doppelzimmer%';
 * update combit set beds1 = 3 where art1 ilike '%Dreibettzimmer%';
 * update combit set beds2 = 3 where art2 ilike '%Dreibettzimmer%';
 * update combit set beds3 = 3 where art3 ilike '%Dreibettzimmer%';
 * update combit set beds4 = 3 where art4 ilike '%Dreibettzimmer%';
 * update combit set beds1 = 4 where art1 ilike '%Vierbettzimmer%';
 * update combit set beds2 = 4 where art2 ilike '%Vierbettzimmer%';
 * update combit set beds3 = 4 where art3 ilike '%Vierbettzimmer%';
 * update combit set beds4 = 4 where art4 ilike '%Vierbettzimmer%';
 *
 * insert into booked_rooms (
 *   booking_id,
 *   beds,
 *   price_per_bed,
 *   factor,
 *   description,
 *   from_date,
 *   to_date
 *   )
 * select
 *   recordid,
 *   beds1,
 *   preis1,
 *   proz1,
 *   art1,
 *   avon,
 *   abis
 * from combit where art1 not like '';
 *
 * insert into booked_rooms (
 *   booking_id,
 *   beds,
 *   price_per_bed,
 *   factor,
 *   description,
 *   from_date,
 *   to_date
 *   )
 * select
 *   recordid,
 *   beds2,
 *   preis2,
 *   proz2,
 *   art2,
 *   avon,
 *   abis
 * from combit where art2 not like '';
 *
 * insert into booked_rooms (
 *   booking_id,
 *   beds,
 *   price_per_bed,
 *   factor,
 *   description,
 *   from_date,
 *   to_date
 *   )
 * select
 *   recordid,
 *   beds3,
 *   preis3,
 *   proz3,
 *   art3,
 *   avon,
 *   abis
 * from combit where art3 not like '';
 *
 * insert into booked_rooms (
 *   booking_id,
 *   beds,
 *   price_per_bed,
 *   factor,
 *   description,
 *   from_date,
 *   to_date
 *   )
 * select
 *   recordid,
 *   beds4,
 *   preis4,
 *   proz4,
 *   art4,
 *   avon,
 *   abis
 * from combit where art4 not like '';
 *
 * delete from booked_rooms where description ilike 'Kurtaxe';
 */

/*
 * Set serial to highest inserted value
 * TODO: Build import in a way, that this is not possible, i.e. give new id's
 */

SELECT setval('customers_customer_id_seq', max(customer_id)) FROM customers;
SELECT setval('bookings_booking_id_seq', max(booking_id)) FROM bookings;

