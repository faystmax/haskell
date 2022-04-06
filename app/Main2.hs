import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)


-- Логика другого процесса – в ответ на ping шлет pong
replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, "ping") = send sender "pong"


-- Для нового релиза Transport.TCP необходима эта функция
computeLocalSocket:: String -> (String, String)
computeLocalSocket serviceName = ("127.0.0.1", serviceName)

main :: IO ()
main = do
 Right t <- createTransport "127.0.0.1" "10501" computeLocalSocket defaultTCPParameters
 node <- newLocalNode t initRemoteTable

 runProcess node $ do
 -- Spawn еще один процесс на локальной ноде
    echoPid <- spawnLocal $ forever $ do
 -- процесс ждет сообщение, которое соответсвует паттерну в функции replyBack
        receiveWait [match replyBack]
    say "sending ping message..."
    self <- getSelfPid
    send echoPid (self, "ping")
    -- ждем ответ с таймаутом
    m <- expectTimeout 1000000
    case m of
      --при ошибке выходим
      Nothing -> die "nothing came back!"
      --иначе выводим, что пришло
      Just s -> say $ "received: " ++ s ++ " back!"

 --ждем в процессе окончания работы
 liftIO $ threadDelay 2000000