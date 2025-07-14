open! Core
open  Async
open  Import

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
      let order_id_generator = Order_id_generator.create () in
      let latest_order       = ref None                     in
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
           | Open _ ->
             (* When the exchanges open, send an order to buy BOND for $950.
                This is probably not a good order to send (do you understand
                why?), and is just an example to show how you can send an
                order to the exchange. *)
             let order_id = Order_id_generator.next_id order_id_generator in
             Exchange_driver.add_order
               exchange_driver
               ~order_id
               ~symbol:Symbol.bond
               ~dir:Buy
               ~price:(Price.of_int_exn 950)
               ~size:(Size.of_int_exn 1)
             |> don't_wait_for
           | _ ->
             (* Ignore all other messages (for now) *)
             ());
          (* Regardless of what the message is, print it. You probably won't
             actually want to print all of the message you see from the
             exchange once you are running your bot in production, because
             there's a lot of messages! *)
          Core.printf !"%{sexp: Exchange_message.t}\n%!" message)
      in
      let schedule_periodic_nonsense () =
        run_every 1.0 ~f:(fun () ->
          (* Cancel the most recent order, if it exists. *)
          (match !latest_order with
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
          latest_order := Some next_order_id)
      in
      schedule_periodic_nonsense ();
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
