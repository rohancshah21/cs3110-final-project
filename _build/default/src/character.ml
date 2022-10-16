type t = {
  balance : int; (* current balance *)
  currgame : int; (* current game *)
  prevgames : int; (* previous games *)
}

let current_game adv = adv.currgame
let get_balance adv = adv.balance
let previous_games adv = adv.prevgames
