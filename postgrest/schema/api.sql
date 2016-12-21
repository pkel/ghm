create schema api;
set search_path to api;

/*
 * customers
 */
create view customers as select
      customer_id
    , title
    , title_letter

    , given
    , second
    , family

    , company
    , company_address

    , street
    , street_number
    , city
    , postal_code
    , country
    , country_code

    , phone
    , phone2
    , mobile
    , fax
    , fax2
    , mail
    , mail2
    , web

    , keyword
    , note
  from internal.customers;

/*
 * insert to customers
 *
 * customer_id can't be specified
 * null is set to sober default
 */
create rule customers_insert as on insert to customers do instead
  insert into internal.customers
    ( title
    , title_letter

    , given
    , second
    , family

    , company
    , company_address

    , street
    , street_number
    , city
    , postal_code
    , country
    , country_code

    , phone
    , phone2
    , mobile
    , fax
    , fax2
    , mail
    , mail2
    , web

    , keyword
    , note
  ) values
    ( coalesce(new.title           ,'')
    , coalesce(new.title_letter    ,'')

    , coalesce(new.given           ,'')
    , coalesce(new.second          ,'')
    , coalesce(new.family          ,'')

    , coalesce(new.company         ,'')
    , coalesce(new.company_address ,'')

    , coalesce(new.street          ,'')
    , coalesce(new.street_number   ,'')
    , coalesce(new.city            ,'')
    , coalesce(new.postal_code     ,'')
    , coalesce(new.country         ,'')
    , coalesce(new.country_code    ,'')

    , coalesce(new.phone           ,'')
    , coalesce(new.phone2          ,'')
    , coalesce(new.mobile          ,'')
    , coalesce(new.fax             ,'')
    , coalesce(new.fax2            ,'')
    , coalesce(new.mail            ,'')
    , coalesce(new.mail2           ,'')
    , coalesce(new.web             ,'')

    , coalesce(new.keyword         ,'')
    , coalesce(new.note            ,'')
    )
  ;

/*
 * update customers
 *
 * customer_id can't be changed
 * changes are logged
 */
create rule customers_update as on update to customers do instead
  ( insert into internal.customers_log values (old.*)
  ; update internal.customers set
      title           = coalesce(new.title           , old.title          ),
      title_letter    = coalesce(new.title_letter    , old.title_letter   ),

      given           = coalesce(new.given           , old.given          ),
      second          = coalesce(new.second          , old.second         ),
      family          = coalesce(new.family          , old.family         ),

      company         = coalesce(new.company         , old.company        ),
      company_address = coalesce(new.company_address , old.company_address),

      street          = coalesce(new.street          , old.street         ),
      street_number   = coalesce(new.street_number   , old.street_number  ),
      city            = coalesce(new.city            , old.city           ),
      postal_code     = coalesce(new.postal_code     , old.postal_code    ),
      country         = coalesce(new.country         , old.country        ),
      country_code    = coalesce(new.country_code    , old.country_code   ),

      phone           = coalesce(new.phone           , old.phone          ),
      phone2          = coalesce(new.phone2          , old.phone2         ),
      mobile          = coalesce(new.mobile          , old.mobile         ),
      fax             = coalesce(new.fax             , old.fax            ),
      fax2            = coalesce(new.fax2            , old.fax2           ),
      mail            = coalesce(new.mail            , old.mail           ),
      mail2           = coalesce(new.mail2           , old.mail2          ),
      web             = coalesce(new.web             , old.web            ),

      keyword         = coalesce(new.keyword         , old.keyword        ),
      note            = coalesce(new.note            , old.note           )
    where customer_id = new.customer_id;
  );

/*
 * delete from customers
 *
 * customers are logged before deletion
 */
create rule customers_delete as on delete to customers do
  insert into internal.customers_log values (old.*);

create view bookings as select * from internal.bookings;
