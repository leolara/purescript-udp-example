module Main where

import Prelude

import Effect (Effect)
import Data.Int(round, toNumber, toStringAs, decimal)
import Effect.Console (log)
import Node.Buffer (Buffer, toString, readString, writeString, size, fromArray, write, create)
import Node.Encoding (Encoding(ASCII))
import Node.Buffer.Internal(read)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (length)
import Node.Buffer.Types (BufferValueType(UInt8, UInt16LE), Octet)
import Node.Datagram (createSocket, bindSocket, SocketType(UDPv4), Socket, SocketInfo, onMessage, send)

import Lora.UDP.Pkt as Pkt
import Lora.UDP.Server as Server

handler :: Server.PktHandler
handler respond pkt = case pkt of
  Pkt.PUSH_DATA { token, mac, json } -> do
    log "PUSH_DATA"
    log $ show token
    log $ show mac
    log $ show json
    respond (Pkt.PUSH_ACK { token })
  Pkt.PUSH_ACK { token } -> do
    log "PUSH_ACK"
    log $ show token
  Pkt.PULL_DATA { token, mac } -> do
    log "PULL_DATA"
    log $ show token
    log $ show mac
    respond (Pkt.PULL_ACK { token })
  _ -> do
    log "unimplemented packet"

main :: Effect Unit
main = do
  log "🍝"
  Server.start "0.0.0.0" 7000 handler
  log "listening to UDP packets at 7000 port"
