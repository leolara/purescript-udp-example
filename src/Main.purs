module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(Just))
import Node.Buffer (Buffer, toString)
import Node.Encoding (Encoding(ASCII))

import Node.Datagram (createSocket, bindSocket, SocketType(UDPv4), Socket, SocketInfo, onMessage)

msgHandler :: Buffer -> SocketInfo -> Effect Unit
msgHandler b i =
  log (toString ASCII b)

main :: Effect Unit
main = do
  log "🍝"
  socket <- createSocket UDPv4 (Just true)
  bindSocket socket (Just 6000) (Just "127.0.0.1")
  onMessage socket msgHandler
