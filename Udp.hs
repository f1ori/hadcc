module Udp where

import Network.Socket

-- | run UDP-Server on random port
initUdpServer :: IO (Socket, PortNumber)
initUdpServer = withSocketsDo $ do
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                        Nothing (Just "0")
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    port <- socketPort sock
    return (sock, port)

-- | wait for UDP-Packet
receiveUdp :: Socket -> IO String
receiveUdp sock = do
    (msg, _, addr) <- recvFrom sock 1024
    return msg

-- | send UDP-Packet to ip:port via a socket
sendUdp :: Socket -> String -> Integer -> String -> IO ()
sendUdp sock ip port str = do
    host <- inet_addr ip
    sendTo sock str (SockAddrInet (fromInteger port) host)
    return ()


-- vim: sw=4 expandtab
    
