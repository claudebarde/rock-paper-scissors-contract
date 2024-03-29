#include "../contract-jackpot.mligo"
// #include "./mock_randomizer.mligo"
#import "./assert_module/ligo_assert.mligo" "TEST"
module ASSERT = TEST.ASSERT
module FORMAT = TEST.FORMAT

let test =
    let _ = Test.reset_state 4n ([]: tez list) in
    let player_address = Test.nth_bootstrap_account 1 in
    let admin_address = Test.nth_bootstrap_account 2 in
    let creator_address = Test.nth_bootstrap_account 3 in

    let _ = FORMAT.add_title "INITIALIZATION" in

    let randomizer_initial_storage = 3n in
    // let randomizer_addr, _, _ = Test.originate main_randomizer randomizer_initial_storage 0tez in
    let randomizer_addr, _, _ = 
        Test.originate_from_file 
            "./tests/mock_randomizer.mligo" 
            "main_randomizer" 
            ([] : string list) 
            (Test.compile_value randomizer_initial_storage) 
            0tez 
    in
    let randomizer_typed_addr: (unit, nat) typed_address = Test.cast_address randomizer_addr in
    let _ = ASSERT.NAT.to_be_equal (Test.get_storage randomizer_typed_addr) randomizer_initial_storage in
    
    let rps_initial_storage = 
    {
        played_games        = 0n;
        players             = (Big_map.empty: (address, games_results) big_map);
        prize               = 300_000mutez;
        play_fee            = 100_000mutez;
        paused              = false;
        mvp                 = (None: mvp option);
        jackpot_factor      = 3n;
        jackpot             = 10tez;
        accrued_fees        = 0tez;
        admin               = admin_address;
        randomizer_address  = randomizer_addr;
        randomizer_creator  = creator_address;
    } in
    // originates the contract
    let rps_addr, _, _ = 
        Test.originate_from_file 
            "./contract-jackpot.mligo" 
            "main" 
            ([] : string list) 
            (Test.compile_value rps_initial_storage) 
            200tez 
    in
    let rps_typed_addr: (parameter, storage) typed_address = Test.cast_address rps_addr in
    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_initial_storage.played_games rps_storage.played_games in
    let _ = ASSERT.ADDRESS.to_be_equal rps_storage.randomizer_address randomizer_addr in

    let contract = Test.to_contract rps_typed_addr in

    (*
        TESTING WITHDRAWALS
    *)

    let _ = FORMAT.add_title "TESTING WITHDRAWALS" in

    let _ = Test.set_source (player_address) in
    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Withdraw (5tez, true)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot withdraw funds - 1")
    in
    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Withdraw (3tez, false)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot withdraw funds - 2")
    in

    let _ = Test.set_source (admin_address) in
    let original_admin_balance = Test.get_balance admin_address in
    let original_creator_balance = Test.get_balance creator_address in

    let value_to_withdraw = 100tez in
    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Withdraw (value_to_withdraw, false)) 0tez) 
            (Some "Admin can withdraw funds")
    in

    let new_admin_balance = Test.get_balance admin_address in
    let new_creator_balance = Test.get_balance creator_address in

    let _ = ASSERT.MUTEZ.to_be_greater_than new_admin_balance original_admin_balance in
    let _ = ASSERT.MUTEZ.to_be_less_or_equal original_admin_balance (original_admin_balance + 95tez) in
    let _ = ASSERT.MUTEZ.to_be_greater_than new_creator_balance original_creator_balance in
    let _ = ASSERT.MUTEZ.to_be_equal new_creator_balance (original_creator_balance + 5tez) in

    (*
        TESTING PLAY FUNCTION
    *)

    let _ = FORMAT.add_title "TESTING PLAY FUNCTION" in

    let _ = Test.set_source (player_address) in
    let rps_storage = Test.get_storage rps_typed_addr in

    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 0n in
    let _ = ASSERT.BIG_MAP.NOT.to_have_key player_address rps_storage.players "PLAYERS" in
    let _ = ASSERT.OPTION.to_be_none rps_storage.mvp in

    let amount_to_attach = rps_storage.prize + rps_storage.play_fee in

    // losing games
    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Play (3n)) amount_to_attach) 
            (Some "Player played Rock")
    in

    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 1n in
    let _ = ASSERT.BIG_MAP.to_have_key player_address rps_storage.players "PLAYERS" in
    let _ = 
        ASSERT.BIG_MAP.value_equals 
            player_address 
            { 
                won             = { amount = 0n; total = 0tez }; 
                lost            = 1n; 
                wins_in_a_row   = 0n;
                last_game       = { player = 3n ; contract = 3n };
            } 
            rps_storage.players 
    in
    let _ = ASSERT.OPTION.to_be_none rps_storage.mvp in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.accrued_fees (rps_initial_storage.play_fee * 1n) in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Play (2n)) amount_to_attach) 
            (Some "Player played Paper")
    in

    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 2n in
    let _ = ASSERT.BIG_MAP.to_have_key player_address rps_storage.players "PLAYERS" in
    let _ = 
        ASSERT.BIG_MAP.value_equals 
        player_address 
        { 
            won             = { amount = 0n; total = 0tez}; 
            lost            = 2n; 
            last_game       = { player = 2n ; contract = 3n };
            wins_in_a_row   = 0n;
        } 
        rps_storage.players 
    in
    let _ = ASSERT.OPTION.to_be_none rps_storage.mvp in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.accrued_fees (rps_initial_storage.play_fee * 2n) in

    // winning game
    let _ = Test.set_source (player_address) in
    let player_balance = Test.get_balance player_address in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Play (1n)) amount_to_attach) 
            (Some "Player played Scissors")
    in

    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 3n in
    let _ = ASSERT.BIG_MAP.to_have_key player_address rps_storage.players "PLAYERS" in
    let _ = 
        ASSERT.BIG_MAP.value_equals 
        player_address 
        { 
            won             = { amount = 1n; total = 100_000mutez} ; 
            lost            = 2n ; 
            last_game       = { player = 1n ; contract = 3n } ;
            wins_in_a_row   = 1n;
        } 
        rps_storage.players 
    in
    let _ = ASSERT.OPTION.to_be_some_value rps_storage.mvp { player = player_address; wins = 1n } in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.jackpot (rps_initial_storage.jackpot + 600_000mutez) in 
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.accrued_fees (rps_initial_storage.play_fee * 3n) in

    let new_player_balance = Test.get_balance player_address in
    let _ = Test.log (player_balance, new_player_balance) in
    // let _ = ASSERT.MUTEZ.to_be_greater_than new_player_balance player_balance in
    // let _ = ASSERT.MUTEZ.to_be_equal new_player_balance (player_balance + expected_prize) in

    // 3 winning games in a row

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Play (1n)) amount_to_attach) 
            (Some "Player played Scissors x2")
    in

    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 4n in
    let _ = ASSERT.BIG_MAP.to_have_key player_address rps_storage.players "PLAYERS" in
    let _ = 
        ASSERT.BIG_MAP.value_equals 
        player_address 
        { 
            won             = { amount = 2n; total = 200_000mutez} ; 
            lost            = 2n ; 
            last_game       = { player = 1n ; contract = 3n } ;
            wins_in_a_row   = 2n;
        } 
        rps_storage.players 
    in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.jackpot (rps_initial_storage.jackpot + 600_000mutez) in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.accrued_fees (rps_initial_storage.play_fee * 4n) in

    let expected_jackpot = rps_storage.jackpot in
    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Play (1n)) amount_to_attach) 
            (Some "Player played Scissors x3")
    in

    let rps_storage = Test.get_storage rps_typed_addr in
    let _ = ASSERT.NAT.to_be_equal rps_storage.played_games 5n in
    let _ = ASSERT.BIG_MAP.to_have_key player_address rps_storage.players "PLAYERS" in

    let _ = 
        match Big_map.find_opt player_address rps_storage.players with
        | None -> ()
        | Some val -> Test.log val
    in 

    let _ = 
        ASSERT.BIG_MAP.value_equals 
        player_address 
        { 
            won             = { amount = 3n; total = expected_jackpot + 200_000mutez} ; 
            lost            = 2n ; 
            last_game       = { player = 1n ; contract = 3n } ;
            wins_in_a_row   = 0n;
        } 
        rps_storage.players 
    in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.jackpot 0tez in
    let _ = ASSERT.MUTEZ.to_be_equal rps_storage.accrued_fees (rps_initial_storage.play_fee * 5n) in

    (*
        TESTING ADMIN FUNCTIONS
    *)

    let _ = FORMAT.add_title "TESTING ADMIN FUNCTIONS" in

    let _ = Test.set_source (player_address) in

    // FAILING

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Add_funds ()) 10tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot add funds")
    in
    
    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Update_admin (player_address)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot set himself as the admin")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Update_randomizer_address (player_address)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot change the randomizer address")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Update_randomizer_creator (player_address)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot change the randomizer creator address")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Update_prize (10tez)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot change the prize")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Update_jackpot_factor (5n)) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot change the jackpot factor")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_fail_with_message 
            (Test.transfer_to_contract contract (Pause ()) 0tez) 
            "NOT_AN_ADMIN"
            (Some "Player cannot pause the contract")
    in

    // PASSING
    let _ = Test.set_source (admin_address) in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Add_funds ()) 10tez) 
            (Some "Admin can add funds")
    in
    
    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Update_randomizer_address (player_address)) 0tez) 
            (Some "Admin can change the randomizer address")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Update_randomizer_creator (player_address)) 0tez) 
            (Some "Admin can change the randomizer creator address")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Update_prize (10tez)) 0tez) 
            (Some "Admin can change the prize")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Update_jackpot_factor (5n)) 0tez) 
            (Some "Admin can change the jackpot factor")
    in

    let _ =
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Pause ()) 0tez) 
            (Some "Admin can pause the contract")
    in

    let _ = 
        ASSERT.ENTRYPOINT.to_succeed 
            (Test.transfer_to_contract contract (Update_admin (player_address)) 0tez)
            (Some "Admin successfully updated!")
    in

    ()