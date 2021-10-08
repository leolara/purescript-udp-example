module Lora.UDP.Server where

import Prelude

import Effect (Effect)
import Data.Int(round, toNumber, toStringAs, decimal)
import Effect.Console (log)
import Node.Buffer as BufferMod
import Node.Buffer (Buffer, toString, readString, writeString, size, fromArray, create)
import Node.Encoding (Encoding(ASCII))
import Node.Buffer.Internal as BufferInternal
import Data.Maybe (Maybe(..))
import Data.String (length)
import Node.Buffer.Types (BufferValueType(UInt8, UInt16LE), Octet)
import Node.Datagram (createSocket, bindSocket, SocketType(UDPv4), Socket, SocketInfo, onMessage, send, close)

import Lora.UDP.Pkt as Pkt

type StopServer = Unit -> Effect Unit
type Respond = Pkt.LoraUDPPkt -> Effect Unit
type PktHandler = (Respond) -> Pkt.LoraUDPPkt -> Effect Unit

start :: String -> Int -> (PktHandler) -> Effect Unit
start addr port handler = do
  socket <- createSocket UDPv4 (Just true)
  bindSocket socket (Just port) (Just addr)
  onMessage socket (msgHandler socket handler)

msgHandler :: Socket -> (PktHandler) -> Buffer -> SocketInfo -> Effect Unit
msgHandler socket handler buff socketInfo = do
  log ""
  log $ "received UDP packet from " <> socketInfo.address <> ":" <> (toStringAs decimal socketInfo.port)
  maybePkt <- Pkt.read buff
  case maybePkt of
    Just pkt -> do
      handler (responder socket socketInfo.address socketInfo.port) pkt
    _ -> do
      log "unrecognized packet"
      s <- show <$> toString ASCII buff
      log s

responder :: Socket -> String -> Int -> Pkt.LoraUDPPkt -> Effect Unit
responder socket addr port pkt = do
  log "sending pkt"
  buff <- Pkt.write pkt
  send socket buff Nothing Nothing port addr (Just $ log "sent")
