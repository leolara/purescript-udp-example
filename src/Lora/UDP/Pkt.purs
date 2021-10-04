module Lora.UDP.Pkt
  (
  LoraUDPPkt(..),
  read,
  write
  ) where

import Prelude

import Effect (Effect)
import Data.Int(round, toNumber)
import Effect.Console (log)
import Node.Buffer as BufferMod
import Node.Buffer (Buffer, toString, readString, writeString, size, fromArray, create)
import Node.Encoding (Encoding(ASCII))
import Node.Buffer.Internal as BufferInternal
import Data.Maybe (Maybe(..))
import Data.String (length)
import Node.Buffer.Types (BufferValueType(UInt8, UInt16LE), Octet)
import Node.Datagram (createSocket, bindSocket, SocketType(UDPv4), Socket, SocketInfo, onMessage, send)

data LoraUDPPkt
    = PUSH_DATA Int String String
    | PUSH_ACK Int
    | PULL_DATA Int String
    | PULL_ACK Int

read :: Buffer -> Effect (Maybe LoraUDPPkt)
read b = do
  loraType <- decodeLoraPktType b
  case loraType of
    Just 0 -> readPUSH_DATA b
    Just 1 -> readPUSH_ACK b
    Just 2 -> readPULL_DATA b
    _ -> pure Nothing

readPUSH_DATA :: Buffer -> Effect (Maybe LoraUDPPkt)
readPUSH_DATA buff = do
  len <- size buff
  if len < 14 then
    pure Nothing
  else do
    token <- round <$> (BufferInternal.read UInt16LE 1 buff)
    gw_id <- (readString ASCII 4 11 buff)
    json <- (readString ASCII 12 len buff)
    pure $ Just $ PUSH_DATA token gw_id json

readPUSH_ACK :: Buffer -> Effect (Maybe LoraUDPPkt)
readPUSH_ACK buff = do
  len <- size buff
  if len < 4 then
    pure Nothing
  else do
    token <- round <$> (BufferInternal.read UInt16LE 1 buff)
    pure $ Just $ PUSH_ACK token

readPULL_DATA :: Buffer -> Effect (Maybe LoraUDPPkt)
readPULL_DATA buff = do
  len <- size buff
  if len < 12 then
    pure Nothing
  else do
    token <- round <$> (BufferInternal.read UInt16LE 1 buff)
    gw_id <- (readString ASCII 4 11 buff)
    pure $ Just $ PULL_DATA token gw_id

write :: LoraUDPPkt -> Effect Buffer
write (PUSH_DATA token mac json) = do
  let jsonLen = length json
  let buffLen = jsonLen + 11
  buff <- create buffLen
  BufferMod.write UInt8 2.0 0 buff
  BufferMod.write UInt16LE (toNumber token) 1 buff
  BufferMod.write UInt8 0.0 3 buff
  _ <- writeString ASCII 4 (11-4) mac buff -- TODO check result
  _ <- writeString ASCII 12 jsonLen json buff -- TODO check result
  pure buff

write (PUSH_ACK token) = do
  buff <- (fromArray :: Array Octet -> Effect Buffer) [2, 0, 0, 1]
  BufferMod.write UInt16LE (toNumber token) 1 buff
  pure buff

write (PULL_DATA _ _) = create 0 -- TODO

write (PULL_ACK token) = do
   buff <- (fromArray :: Array Octet -> Effect Buffer) [2, 0, 0, 4]
   BufferMod.write UInt16LE (toNumber token) 1 buff
   pure buff

bufLoraVersion :: Buffer -> Effect Int
bufLoraVersion b = round <$> (BufferInternal.read UInt8 0 b)

bufLoraTypeIdentifier :: Buffer -> Effect Int
bufLoraTypeIdentifier b = round <$> (BufferInternal.read UInt8 3 b)

decodeLoraPktType :: Buffer -> Effect (Maybe Int)
decodeLoraPktType buff = do
  len <- size buff
  if len < 4 then
    pure Nothing
  else do
    version <- bufLoraVersion buff
    case version of
      2 -> Just <$> bufLoraTypeIdentifier buff
      _ -> pure Nothing
