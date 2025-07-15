open! Core
open  Async
open  Import

module State = struct
  type t = { mutable positions : Position.t Symbol.Map.t } [@@deriving sexp]

  let create () = { positions = Symbol.Map.empty }
  let next_id t = 
    let order_id_generator = Order_id_generator.create () in
    Order_id_generator.next_id order_id_generator

  let add_order t ~symbol ~dir ~price ~size ~order_id ~exchange_driver =
    Exchange_driver.add_order exchange_driver ~order_id ~symbol ~dir ~price ~size |> don't_wait_for

  let on_hello t (hello_message : (Symbol.t * Position.t) list) = 
    t.positions <- match Symbol.Map.of_alist hello_message with
      | `Ok result -> result
      | `Duplicate_key _ -> failwith("duplicate key")

  let on_ack t order_id = ()
  let on_fill t (fill : Exchange_message.Fill.t) = 
    t.positions <- Map.update t.positions fill.symbol ~f:(fun current_fill -> 
        match current_fill with
        | Some some_fill -> 
          let position_int = Position.to_int some_fill in
          let to_add_position = (match fill.dir with 
          | Buy -> Size.to_int fill.size
          | Sell -> -1 * Size.to_int fill.size) in
          Position.of_int_exn (position_int + to_add_position)
        | None -> failwith("error")
      );
end

module Bond_strategy = struct
  type t = { mutable orders: Order_id.t list }

  let create () = { orders = [] }

  let initialize_bond_orders t (state : State.t) exchange_driver = 
    let current_pos = match Map.find state.positions Symbol.bond with
    | Some pos -> pos
    | None -> failwith("error") in

    let current_pos_int = Position.to_int current_pos in
    let bond_position_limit = 100 in
    let sell_price = Price.of_int_exn 1001 in
    let buy_price = Price.of_int_exn 999 in
    let buy_amount = Size.of_int_exn (bond_position_limit - current_pos_int) in
    let sell_amount = Size.of_int_exn (bond_position_limit + current_pos_int) in

    let buy_order_id = State.next_id state in
    State.add_order state ~symbol:Symbol.bond ~dir:Buy ~price:buy_price ~size:buy_amount ~exchange_driver ~order_id:buy_order_id;

    let sell_order_id = State.next_id state in
    State.add_order state ~symbol:Symbol.bond ~dir:Sell ~price:sell_price ~size:sell_amount ~exchange_driver ~order_id:sell_order_id;

    t.orders <- t.orders @ [buy_order_id; sell_order_id]

  let reset_all_bond_orders t state exchange_driver = 
    List.iter t.orders ~f:(fun order ->
      don't_wait_for (Exchange_driver.cancel exchange_driver ~order_id:order)
      );
    t.orders <- []
end

(* [run_every seconds ~f] is a utility function that will run a given function, [f], every
   [num_seconds] seconds. *)
let run_every seconds ~f = Async.Clock.every (Time_float.Span.of_sec seconds) f

(* This is an example of what your ETC bot might look like. You should treat this as a
   starting point, and do not feel beholden to anything that this code is doing! Feel free
   to delete, add, and reorganize code however you would like. (Indeed, you should change
   the logic here! It does not make sense!) *)
let run exchange_type =
  (* Set up a connection to the exchange. *)
  Exchange_driver.connect_and_run
    exchange_type
    ~f:(fun ~exchange_driver ~exchange_messages ->
      (* Initiate the [Order_id_generator], which will help us get unique ids
         to attach to the orders we send to the exchange. You don't have to
         use this, but it'll make coming up with valid order ids a bit
         easier. Feel free to open up order_id_generator.ml to see how the
         code works. *)
      (* let order_id_generator = Order_id_generator.create () in *)
      let state = State.create () in
      let bond_strat = Bond_strategy.create () in

      (* let latest_order       = ref None                     in *)

      (* let send_first_two_orders () = 
        let order_id = Order_id_generator.next_id order_id_generator in
        Exchange_driver.add_order exchange_driver ~order_id:order_id ~symbol:Symbol.bond ~dir:Buy ~price:(Price.of_int_exn 999) ~size:(Size.of_int_exn 50) |> don't_wait_for;
        let order_id = Order_id_generator.next_id order_id_generator in
        Exchange_driver.add_order exchange_driver ~order_id:order_id ~symbol:Symbol.bond ~dir:Sell ~price:(Price.of_int_exn 1001) ~size:(Size.of_int_exn 50) |> don't_wait_for;
      in *)

      let read_messages_and_do_some_stuff () =
        (* Read the messages from the exchange. In general, a good rule of
           thumb is to read a LOT more than you write messages to the exchange.
           (Can you see why? Feel free to ask a TA if you're not sure.)

           [iter_without_pushback] is a way of reminding us that the stuff we
           do while reading the message probably shouldn't cause us to slow
           down reading, but feel free to change it if you would like. *)
        Async.Pipe.iter_without_pushback exchange_messages ~f:(fun message ->
          (* This is only an example of what you might want to do when you
             see a message from the exchange. You should change this, and
             definitely don't feel constrained to writing code that looks
             similar to what's written here! *)
          (match message with
           | Exchange_message.Hello hello_msg ->
              State.on_hello state hello_msg;
              Bond_strategy.initialize_bond_orders bond_strat state exchange_driver;
           | Exchange_message.Fill fill_msg ->
              State.on_fill state fill_msg;
              Bond_strategy.reset_all_bond_orders bond_strat state 
              exchange_driver;
              Bond_strategy.initialize_bond_orders bond_strat state exchange_driver;

              (* print_s [%sexp (state : State.t)]; *)

         (* When the exchanges open, send an order to buy BOND for $950.
                This is probably not a good order to send (do you understand
                why?), and is just an example to show how you can send an
                order to the exchange. *)
              
             (* let order_id = Order_id_generator.next_id order_id_generator in
             Exchange_driver.add_order
               exchange_driver
               ~order_id
               ~symbol:Symbol.bond
               ~dir:Buy
               ~price:(Price.of_int_exn 950)
               ~size:(Size.of_int_exn 1)
             |> don't_wait_for *)
           | _ ->
             (* Ignore all other messages (for now) *)
             ());
          (* Regardless of what the message is, print it. You probably won't
             actually want to print all of the message you see from the
             exchange once you are running your bot in production, because
             there's a lot of messages! *)
          (* Core.printf !"%{sexp: Exchange_message.t}\n%!" message) *))
      in
      let schedule_periodic_nonsense () =
        run_every 1.0 ~f:(fun () ->
          (* Cancel the most recent order, if it exists. *)
          (* (match !latest_order with
           | None          -> ()
           | Some order_id ->
             Exchange_driver.cancel exchange_driver ~order_id |> don't_wait_for);
          (* Send a new order. *)
          let next_order_id = Order_id_generator.next_id order_id_generator in
          Exchange_driver.add_order
            exchange_driver
            ~order_id:next_order_id
            ~symbol:Symbol.bond
            ~dir:Buy
            ~price:(Price.of_int_exn 950)
            ~size:(Size.of_int_exn 1)
          |> don't_wait_for;
          latest_order := Some next_order_id) *)())
      in
      (* schedule_periodic_nonsense (); *)
      (* don't_wait_for (read_hello_message ()); *)
      ignore (schedule_periodic_nonsense);
      (* ignore (send_first_two_orders ()); *)
      read_messages_and_do_some_stuff ())
;;

let command =
  Async.Command.async
    ~summary:"My etc bot"
    (* This bot starts off with flags to choose between available exchanges
       and set which log levels are displayed. Feel free to add any other
       flags you need! *)
    [%map_open.Command
      let exchange_type = Exchange_type.param in
      fun () -> run exchange_type]
;;
