module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)

import Node.Datagram (createSocket, UDPv4, Socket)

main :: Effect Unit
main = do
  log "🍝"
  bindSocket (createSocket UDPv4 true) 6000 "127.0.0.1"
