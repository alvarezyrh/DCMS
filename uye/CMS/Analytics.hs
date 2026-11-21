-- | Analitik kunjungan sederhana tanpa cookie: hanya jumlah per hari, halaman, dan
-- sumber (mesin pencari, media sosial, langsung, situs lain). Tidak menyimpan IP
-- atau data pribadi pengunjung.
module CMS.Analytics
  ( classifyReferer
  , isBot
  , trackable
  , sourceLabel
  , sourceOrder
  ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

-- | (sumber, rincian) dari header Referer dan host situs sendiri.
--   sumber: "search" | "social" | "direct" | "internal" | "referral"
classifyReferer :: Text -> Maybe Text -> (Text, Text)
classifyReferer _ Nothing = ("direct", "")
classifyReferer self (Just ref)
  | T.null host = ("direct", "")
  | host == T.toLower self = ("internal", "")
  -- Aplikasi Android: hanya aplikasi Google Search yang dihitung sebagai pencarian.
  | "android-app://" `T.isPrefixOf` T.toLower ref =
      if "googlequicksearchbox" `T.isInfixOf` host then ("search", "Google") else ("referral", T.take 60 host)
  | Just e <- match engines = ("search", e)
  | Just s <- match socials = ("social", s)
  | otherwise = ("referral", T.take 60 (dropWww host))
  where
    host = T.toLower (T.takeWhile (\ch -> ch /= '/' && ch /= '?' && ch /= '#') (dropScheme ref))
    dropScheme r = let (_, b) = T.breakOn "://" r in if T.null b then r else T.drop 3 b
    dropWww h = fromMaybe h (T.stripPrefix "www." h)
    hostNoPort = T.takeWhile (/= ':') host
    -- Cocokkan per bagian domain: ".t.co" tidak boleh cocok dengan "test.com".
    match table = case [name | (needle, name) <- table, ("." <> needle) `T.isInfixOf` ("." <> hostNoPort <> ".")] of
      (n : _) -> Just n
      [] -> Nothing
    engines =
      [ ("google.", "Google"), ("bing.com", "Bing"), ("duckduckgo.", "DuckDuckGo"), ("search.yahoo.", "Yahoo")
      , ("yandex.", "Yandex"), ("baidu.", "Baidu"), ("ecosia.", "Ecosia"), ("search.brave.", "Brave")
      , ("startpage.", "Startpage"), ("qwant.", "Qwant")
      ]
    socials =
      [ ("facebook.", "Facebook"), ("fb.me", "Facebook"), ("instagram.", "Instagram"), ("t.co", "X (Twitter)")
      , ("twitter.", "X (Twitter)"), ("x.com", "X (Twitter)"), ("linkedin.", "LinkedIn"), ("lnkd.in", "LinkedIn")
      , ("tiktok.", "TikTok"), ("youtube.", "YouTube"), ("pinterest.", "Pinterest"), ("reddit.", "Reddit")
      , ("whatsapp.", "WhatsApp"), ("t.me", "Telegram"), ("threads.", "Threads")
      ]

-- | Perayap, pratinjau tautan, dan alat otomatis tidak dihitung.
isBot :: Text -> Bool
isBot ua =
  T.null ua
    || any (`T.isInfixOf` T.toLower ua)
      [ "bot", "crawl", "spider", "slurp", "preview", "facebookexternalhit", "whatsapp", "telegram"
      , "curl", "wget", "python", "headless", "lighthouse", "httpclient", "go-http", "java/", "monitor"
      ]

-- | Halaman publik yang dihitung (bukan dashboard, login, atau berkas).
trackable :: [Text] -> Bool
trackable path = case path of
  ("admin" : _) -> False
  ("static" : _) -> False
  ("asset" : _) -> False
  [p] | p `elem` ["login", "register", "logout", "sitemap.xml", "robots.txt", "favicon.ico"] -> False
  _ -> True

sourceLabel :: Text -> Text
sourceLabel = \case
  "search" -> "Mesin pencari"
  "social" -> "Media sosial"
  "direct" -> "Langsung"
  "referral" -> "Situs lain"
  "internal" -> "Antar halaman"
  s -> s

-- | Urutan tetap sumber agar warna di grafik konsisten.
sourceOrder :: [Text]
sourceOrder = ["search", "social", "referral", "direct", "internal"]
