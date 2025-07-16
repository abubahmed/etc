open! Core
open! Async
open! Import

  let command =
    Command.group
       ~summary:"run a prod or dev bot"
      [ "prod", Etc_bot_prod.command
      ; "dev", Etc_bot_dev.command
      ]

(* 
      while true; do dune exec bin/main.exe -- prod -connect-to-prod; sleep 5; done
      dune exec bin/main.exe -- prod -connect-to-prod *)
      (* dune exec bin/main.exe -- dev -connect-to-test prod-like *)