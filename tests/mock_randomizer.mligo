type storage = nat

type param = unit

let main_randomizer (_, s: param * storage): operation list * storage =
    ([]: operation list),
    s

[@view]
let getRandomBetween ((_from, _to), s: (nat * nat) * storage): storage = s