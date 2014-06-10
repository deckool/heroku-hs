{-# LANGUAGE OverloadedStrings #-}
 
module Yamulator where
 
import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.HashMap.Strict  as HM
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Text as T
 
 
data Message = Message {
      mid       :: Integer,
      plainText :: T.Text,
      byUserId       :: Integer,
      likes   :: Integer,
      inReplyTo :: Maybe Integer,
      createdAt :: T.Text
 
} deriving (Eq, Show)
 
data User = User {
      name :: T.Text,
      userId  :: Integer
 
} deriving (Eq, Show)
 
 
data Yammers = Yammers {
      messages :: [Message],
      users    :: [User] 
} deriving (Eq, Show)

instance FromJSON Yammers where
  parseJSON (Object o) = do
      messages <- parseJSON =<< (o .: "messages")
      users <- mapM parseJSON . filter (\(Object ref) -> HM.lookup "type" ref == Just (String "user")) =<< o .: "references"
      return $ Yammers messages users
  parseJSON _ = mzero
 
instance FromJSON Message where
    parseJSON (Object v) = Message <$>
                          v .: "id" <*>
                          ((v .: "body") >>= (.: "plain")) <*>
                          v .: "sender_id" <*>
                          ((v .: "liked_by") >>= (.: "count")) <*> -- note how we can keep drilling into nested structures like this.
                          v .:? "replied_to_id"  <*>
                          v .: "created_at"
 
    parseJSON _ = mzero
decodeYammers :: C.ByteString -> Maybe Yammers
decodeYammers response = decode response

main = do
  let file = "{\n  \"messages\": [\n    {\n      \"client_url\": \"https://www.yammer.com/\",\n      \"created_at\": \"2011/03/28 20:39:12 +0000\",\n      \"system_message\": false,\n      \"body\": {\n        \"parsed\": \"message with photo attachment.\",\n        \"plain\": \"message with photo attachment.\"\n      },\n      \"sender_type\": \"user\",\n      \"network_id\": 104604,\n      \"thread_id\": 84402777,\n      \"web_url\": \"https://www.yammer.com/yammerdeveloperstestcommunity/messages/84402777\",\n      \"direct_message\": false,\n      \"id\": 84402777,\n      \"url\": \"https://www.yammer.com/api/v1/messages/84402777\",\n      \"client_type\": \"Web\",\n      \"message_type\": \"update\",\n      \"sender_id\": 4022984,\n      \"replied_to_id\": null,\n      \"attachments\": [\n        {\n          \"type\": \"image\",\n          \"content_type\": \"\",\n          \"uuid\": null,\n          \"web_url\": \"https://www.yammer.com/yammerdeveloperstestcommunity/uploads/857663/Firefly.jpg\",\n          \"y_id\": 857663,\n          \"image\": {\n            \"thumbnail_url\": \"https://www.yammer.com/api/v1/file/857663/Firefly.jpg?view=thumbnail\",\n            \"url\": \"https://www.yammer.com/api/v1/file/857663/Firefly.jpg\",\n            \"size\": 0\n          },\n          \"name\": \"Firefly.jpg\",\n          \"id\": 974915\n        }\n      ],\n      \"liked_by\": {\n        \"count\": 0,\n        \"names\": []\n      },\n      \"privacy\": \"public\"\n    },\n    {\n      \"client_url\": \"http://www.yammer.com\",\n      \"created_at\": \"2011/03/25 00:49:29 +0000\",\n      \"system_message\": false,\n      \"body\": {\n        \"parsed\": \"new test message 1\",\n        \"plain\": \"new test message 1\"\n      },\n      \"network_id\": 104604,\n      \"thread_id\": 83957686,\n      \"sender_type\": \"user\",\n      \"direct_message\": false,\n      \"web_url\": \"https://www.yammer.com/yammerdeveloperstestcommunity/messages/83957686\",\n      \"id\": 83957686,\n      \"url\": \"https://www.yammer.com/api/v1/messages/83957686\",\n      \"client_type\": \"testingtest\",\n      \"sender_id\": 4022984,\n      \"replied_to_id\": null,\n      \"message_type\": \"update\",\n      \"liked_by\": {\n        \"count\": 0,\n        \"names\": []\n      },\n      \"attachments\": [],\n      \"privacy\": \"public\"\n    }\n  ],\n \n    {\n      \"type\": \"user\",\n      \"stats\": {\n        \"followers\": 1,\n        \"updates\": 14,\n        \"following\": 2\n      },\n      \"web_url\": \"https://www.yammer.com/yammerdeveloperstestcommunity/users/mikealrogers-guest\",\n      \"mugshot_url\": \"https://assets3.yammer.com/images/no_photo_small.gif\",\n      \"url\": \"https://www.yammer.com/api/v1/users/4022984\",\n      \"full_name\": \"mikeal\",\n      \"name\": \"mikealrogers-guest\",\n      \"state\": \"active\",\n      \"job_title\": \"Test Title\",\n      \"id\": 4022984\n    },\n    {\n      \"type\": \"user\",\n      \"stats\": {\n        \"followers\": 1,\n        \"updates\": 4,\n        \"following\": 2\n      },\n      \"web_url\": \"https://www.yammer.com/yammerdeveloperstestcommunity/users/mknopp\",\n      \"mugshot_url\": \"https://assets1.yammer.com/user_uploaded/photos/p1/0141/2640/n1644278019_46479_62_small.jpg\",\n      \"url\": \"https://www.yammer.com/api/v1/users/1452329\",\n      \"full_name\": \"Matt Knopp\",\n      \"name\": \"mknopp\",\n      \"state\": \"active\",\n      \"job_title\": null,\n      \"id\": 1452329\n    }\n \n  ]\n}"
  print $ decodeYammers file