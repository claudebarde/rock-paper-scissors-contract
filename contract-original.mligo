type games_results = 
{
    won: { amount: nat; total: tez };
    lost: nat;
}

type game_choice =
| Rock
| Paper
| Scissors

type storage =
{
    played_games: nat;
    players: (address, games_results) big_map;
    // 1 game_choice => beats game_choice
    rules: (game_choice, game_choice) map;
    // randomizer returns a number that is converted into a game_choice
    correspondences: (nat, game_choice) map;
    prize: tez;
    mul_factor: nat;
    last_random_choice: game_choice option;
    admin: address;
    randomizer_address: address;
    randomizer_creator: address;
    paused: bool;
}

type parameter =
| Play of game_choice
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
let play (s, player_choice: storage * game_choice): return =
    // checks that the user has attached the correct amount to play
    if Tezos.get_amount () <> s.prize
    then (failwith "INCORRECT_AMOUNT": return)
    // checks that the contract has enough balance in case of a win
    else if Tezos.get_balance () < s.prize * s.mul_factor
    then (failwith "INSUFFICIENT_PRIZE_BALANCE": return)
    else
        // converts random value from randomizer into game_choice
        let random_choice: game_choice = 
            match ((Tezos.call_view "getRandomBetween" (1n, 3n) s.randomizer_address): nat option) with
            | None -> (failwith "NO_RESULT_FROM_VIEW": game_choice)
            | Some val ->
                begin
                    // finds the corresponding game_choice
                    match Map.find_opt val s.correspondences with
                    | None -> (failwith "INVALID_RANDOM_VAL": game_choice)
                    | Some g_c -> g_c
                end
        in
        // checks outcome against sender's choice
        let outcome: bool = 
            match Map.find_opt player_choice s.rules with
            | None -> (failwith "UNEXPECTED_GAME_CHOICE": bool)
            | Some val -> 
                if val = random_choice
                then
                    // player wins
                    true
                else
                    // player loses
                    false
        in
        let (ops, new_players): operation list * (address, games_results) big_map = 
            if outcome = true
            then
                let target: unit contract = Tezos.get_contract_with_error (Tezos.get_sender ()) "UNABLE_TO_FIND_PLAYER_ADDRESS" in
                let prize = s.prize * s.mul_factor in
                
                [Tezos.transaction unit prize target],
                (match Big_map.find_opt (Tezos.get_sender ()) s.players with
                | None -> Big_map.add (Tezos.get_sender ()) { won = { amount = 1n; total = prize } ; lost = 0n } s.players
                | Some val -> 
                    Big_map.update 
                        (Tezos.get_sender ()) 
                        (Some ({ val with won = { amount = val.won.amount + 1n; total = val.won.total + prize } })) 
                        s.players)
            else
                ([]: operation list),
                (match Big_map.find_opt (Tezos.get_sender ()) s.players with
                | None -> Big_map.add (Tezos.get_sender ()) { won = { amount = 0n ; total = 0tez } ; lost = 1n } s.players
                | Some val -> Big_map.update (Tezos.get_sender ()) (Some ({ val with lost = val.lost + 1n })) s.players)
        in
            
        ops,
        {
            s with
                played_games        = s.played_games + 1n;
                players             = new_players;
                last_random_choice  = Some random_choice;
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
            (* let target_value: tez option = value - (((value * 95n) / 100tez) * 1tez) in
            let creator_value: tez option = value - (((value * 5n) / 100tez) * 1tez) in *)
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
