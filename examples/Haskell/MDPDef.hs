{-
    Majordomo protocol constants
-}
module MDPDef (
        mdpcClient
    ,   mdpwWorker

    ,   mdpwReady
    ,   mdpwRequest
    ,   mdpwReply
    ,   mdpwHeartbeat
    ,   mdpwDisconnect

    ,   mdpsCommands
    ,   mdpGetIdx
    ) where

import Data.ByteString.Char8 (empty, pack, ByteString(..))


mdpcClient = pack "MDPC01"

mdpwWorker = pack "MDPW01"

-- MDP/Server commands
mdpwReady = pack "01"
mdpwRequest = pack "02"
mdpwReply = pack "03"
mdpwHeartbeat = pack "04"
mdpwDisconnect = pack "05"

mdpsCommands = [ empty, pack "READY", pack "REQUEST", pack "REPLY", 
                        pack "HEARTBEAT", pack "DISCONNECT" ] 

mdpGetIdx :: String -> Int
mdpGetIdx ('0':num) = read num
mdpGetIdx cmd = error $ "Invalid cmd " ++ cmd
