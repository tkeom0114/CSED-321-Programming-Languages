
type location = int

type value

type label

type env    = location Env.env

type store  = value Store.store

type config = label list * env * store

val initial : Tml.exp -> config

val final : config -> value option

val config2str : config -> string

val value2exp : value -> Tml.exp

val step : config -> config
