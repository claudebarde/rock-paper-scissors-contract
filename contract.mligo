type games_results = 
{
    won: { amount: nat; total: tez };
    lost: nat;
    last_game: { player: nat; contract: nat };
}

type mvp =
{
    player: address;
    wins: nat;
}

type storage =
{
    played_games: nat;
    players: (address, games_results) big_map;
    prize: tez;
    mul_factor: nat;
    admin: address;
    randomizer_address: address;
    randomizer_creator: address;
    paused: bool;
    mvp: mvp option;
}

type parameter =
| Play of nat
| Withdraw of tez * bool
| Add_funds
| Update_admin of address
| Update_randomizer_address of address
| Update_randomizer_creator of address
| Update_prize of tez
| Update_mul_factor of nat
| Pause

type return = operation list * storage

let is_admin (addr: address) (s: storage): bool = 
    addr = s.admin

(*
    ENTRYPOINT TO START A NEW GAME
*)
let play (s, player_input: storage * nat): return =
    // checks that the user has attached the correct amount to play
    if Tezos.get_amount () <> s.prize
    then (failwith "INCORRECT_AMOUNT": return)
    // checks that the contract has enough balance in case of a win
    else if Tezos.get_balance () < s.prize * s.mul_factor
    then (failwith "INSUFFICIENT_PRIZE_BALANCE": return)
    else
        // gets random value from randomizer
        let random_nat: nat = 
            match ((Tezos.call_view "getRandomBetween" (1n, 3n) s.randomizer_address): nat option) with
            | None -> (failwith "NO_RESULT_FROM_VIEW": nat)
            | Some val -> val
        in
        let outcome: bool =
            if (player_input = 1n && random_nat = 3n) || 
                (player_input = 2n && random_nat = 1n) || 
                (player_input = 3n && random_nat = 2n)
            then true
            else false
        in
        let game_data = { player = player_input ; contract = random_nat } in
        
        let (ops, (new_players, mvp)): operation list * ((address, games_results) big_map * (mvp option)) = 
            if outcome = true
            then
                let target: unit contract = Tezos.get_contract_with_error (Tezos.get_sender ()) "UNABLE_TO_FIND_PLAYER_ADDRESS" in
                let prize = s.prize * s.mul_factor in
                let win_op = [Tezos.transaction unit prize target] in
                let (new_players, new_mvp) = 
                    match Big_map.find_opt (Tezos.get_sender ()) s.players with
                    | None -> 
                        let new_players = 
                            Big_map.add 
                                (Tezos.get_sender ()) 
                                { won = { amount = 1n; total = prize } ; lost = 0n ; last_game = game_data } 
                                s.players
                        in
                        let new_mvp = 
                            match s.mvp with 
                            | None -> { player = Tezos.get_sender () ; wins = 1n }
                            | Some last_mvp -> last_mvp
                        in new_players, new_mvp
                    | Some val -> 
                        let new_wins = val.won.amount + 1n in
                        let new_players = 
                            Big_map.update 
                                (Tezos.get_sender ()) 
                                (Some ({ val with won = { amount = new_wins ; total = val.won.total + prize } ; last_game = game_data })) 
                                s.players
                        in
                        let new_mvp = 
                            match s.mvp with 
                            | None -> { player = Tezos.get_sender () ; wins = new_wins }
                            | Some last_mvp -> 
                                (if last_mvp.wins < new_wins 
                                then { player = Tezos.get_sender () ; wins = new_wins }
                                else last_mvp)
                        in new_players, new_mvp
                    in
                win_op, (new_players, (Some new_mvp))
            else
                let new_players =
                    match Big_map.find_opt (Tezos.get_sender ()) s.players with
                    | None -> Big_map.add (Tezos.get_sender ()) { won = { amount = 0n ; total = 0tez } ; lost = 1n ; last_game = game_data } s.players
                    | Some val -> Big_map.update (Tezos.get_sender ()) (Some ({ val with lost = val.lost + 1n ; last_game = game_data })) s.players
                in ([]: operation list), (new_players, s.mvp)
        in
            
        ops,
        {
            s with
                played_games    = s.played_games + 1n;
                players         = new_players;
                mvp             = mvp;
        }

(*
    ENTRYPOINT TO WITHDRAW CONTRACT BALANCE
*)
let withdraw (s, (value, force): storage * (tez * bool)): return =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": return)
    else
        let target: unit contract = Tezos.get_contract_with_error s.admin "UNABLE_TO_FIND_ADMIN_ADDRESS" in

        if force
        then
            [
                Tezos.transaction unit value target
            ],
            s
        else
            let creator: unit contract = Tezos.get_contract_with_error s.randomizer_creator "UNABLE_TO_FIND_CREATOR_ADDRESS" in
            let (admin_share, creator_share): tez * tez = 
                match ediv (value * 95n) 100mutez with
                | None -> (failwith "EDIV_ERROR": tez * tez)
                | Some (quotient, remainder) ->
                    if quotient > 0n
                    then
                        let admin_share = (quotient * 1mutez) + remainder in
                        let creator_share =
                            (match value - admin_share with
                            | None -> (failwith "SUB_MUTEZ_ERROR": tez)
                            | Some v -> v) 
                        in (admin_share, creator_share)
                    else 
                        (value, 100_000mutez) // flat fee of 0.1 tez
            in

            [
                Tezos.transaction unit admin_share target; 
                Tezos.transaction unit creator_share creator
            ],
            s     

(*
    ENTRYPOINT TO ADD FUNDS (IF EVER NECESSARY)
*)
let add_funds (s: storage): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else s

(*
    ENTRYPOINT TO UPDATE THE ADMIN ADDRESS
*)
let update_admin (s, addr: storage * address): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with admin = addr }

(*
    ENTRYPOINT TO UPDATE THE RANDOMIZER ADDRESS
*)
let update_randomizer_address (s, addr: storage * address): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with randomizer_address = addr }

(*
    ENTRYPOINT TO UPDATE THE RANDOMIZER CREATOR ADDRESS FOR ROYALTIES
*)
let update_randomizer_creator (s, addr: storage * address): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with randomizer_creator = addr }

(*
    ENTRYPOINT TO UPDATE THE GAME PRIZE
*)
let update_prize (s, new_prize: storage * tez): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with prize = new_prize }

(*
    ENTRYPOINT TO UPDATE THE MULTIPLICATION FACTOR FOR THE PRIZE
*)
let update_mul_factor (s, new_mul_factor: storage * nat): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with mul_factor = new_mul_factor }

(*
    ENTRYPOINT TO PAUSE/UNPAUSE THE CONTRACT
*)
let pause (s: storage): storage =
    if not is_admin (Tezos.get_sender ()) s
    then (failwith "NOT_AN_ADMIN": storage)
    else
        { s with paused = not s.paused }
   
let main (action, store : parameter * storage) : return =
    match action with
    | Play g -> play (store, g)
    | Withdraw v -> withdraw (store, v)
    | Add_funds -> ([]: operation list), add_funds store
    | Update_admin admin -> ([]: operation list), update_admin (store, admin)
    | Update_randomizer_address addr -> ([]: operation list), update_randomizer_address (store, addr)
    | Update_randomizer_creator addr -> ([]: operation list), update_randomizer_creator (store, addr)
    | Update_prize p -> ([]: operation list), update_prize (store, p)
    | Update_mul_factor p -> ([]: operation list), update_mul_factor (store, p)
    | Pause -> ([]: operation list), pause store

[@view]
let get_player_wins (player, s: address * storage): (nat * tez) option =
    match Big_map.find_opt player s.players with
    | None -> None
    | Some results -> Some (results.won.amount, results.won.total)
