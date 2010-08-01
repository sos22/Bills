module Types(Attachment(..),
             StatementEntry(..),
             Statement(..),
             BillEntry(..),
             BillCharge(..),
             ChargeRecord(..)) where

import HAppS.Server.JSON
import GHC.Int

import Json

data ChargeRecord = ChargeRecord { cr_user :: String,
                                   cr_amount :: Double } deriving Show

instance FromJSON ChargeRecord where
    fromJSON (JObj fields) =
        let u = lookup "user" fields
            a = lookup "charge" fields
        in case (a, u) of
             (Just (JString a'), Just (JString u')) ->
                 Just $ ChargeRecord { cr_user = u', cr_amount = read a' }
             _ -> Nothing
    fromJSON _ = Nothing

data BillCharge = BillCharge { bc_ident :: Int,
                               bc_who :: String,
                               bc_amount :: Double } deriving Show

instance ToJSON BillCharge where
    toJSON bc = JObj [("ident", toJSON $ bc_ident bc),
                      ("charge", toJSON $ bc_amount bc),
                      ("uname", JString $ bc_who bc)]

data BillEntry = BillEntry { be_ident :: Int,
                             be_date :: String,
                             be_description :: String,
                             be_owner :: String,
                             be_charges :: [BillCharge],
                             be_attachments :: [Attachment], 
                             be_changed :: String } deriving Show

instance ToJSON BillEntry where
    toJSON be = JObj [("ident", JInt $ be_ident be),
                      ("date", JString $ be_date be),
                      ("description", JString $be_description be),
                      ("owner", JString $ be_owner be),
                      ("charges", toJSON $ be_charges be),
                      ("attachments", toJSON $ be_attachments be),
                      ("changed", JString $ be_changed be)]

data Attachment = Attachment { at_ident :: Int64,
                               at_filename :: String }
                deriving Show

instance ToJSON Attachment where
    toJSON att = JObj [("attach_id", toJSON $ at_ident att),
                       ("filename", JString $ at_filename att)]

data StatementEntry = StatementEntry { se_date :: String,
                                       se_description :: String,
                                       se_amount :: Double,
                                       se_balance_after :: Double,
                                       se_attachments :: [Attachment] }
                    deriving Show

instance ToJSON StatementEntry where
    toJSON se = JObj [("date", JString $ se_date se),
                      ("description", JString $ se_description se),
                      ("amount", toJSON $ se_amount se),
                      ("balance_after", toJSON $ se_balance_after se),
                      ("attachments", toJSON $ se_attachments se)]

data Statement = Statement { stmt_starting_balance :: Double,
                             stmt_entries :: [ StatementEntry ] }
instance ToJSON Statement where
    toJSON stmt = JObj [("starting_balance", toJSON $ stmt_starting_balance stmt),
                        ("charges", toJSON $ stmt_entries stmt)]
                         
