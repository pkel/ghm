open Interfaces

type env = { reload : unit inject }

include Interfaces.FORM_COMPONENT with type edit := Ghm.Invoice.t and type env := env
