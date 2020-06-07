import Data.ByteString.Lazy.Char8 as BL hiding (putStrLn, getLine)
import Prelude hiding (take, drop, length, reverse, lines, head, tail, readFile, elemIndices, (!!))
import Data.List as L (head, tail, length, (!!), drop, take, init, last, (++))
import Data.Char 
import Data.Bits
import Data.Time.Clock.POSIX 
import Data.Time (formatTime, defaultTimeLocale)
import Data.Binary.Get
import Data.Int
import Data.Maybe (fromJust)
import Data.Array.IArray

--Function for prInt64ing a messages
outp :: String -> IO ()
outp a = do
 putStrLn a

--Main function
main :: IO ()
main = do
 outp "Which NTFS MFT shall we parse?  ..."
 path <- getLine
 outp "Loading MFT..."
 stuff <- readFile (path)
 outp "MFT loaded successfully!\n"
 outp "Doing some magic stuff..." 
 let parsed = append (csvHead 1) (breakEntry stuff) --(breakEntry (BL.take 1024000 stuff))) -------Limit the size for debugging
 outp ((show (div (BL.length stuff) 1024)) ++ " entries analyzed!\n")
 BL.writeFile (path ++ "-raw.csv") parsed
 outp ("A raw CSV of all parsed records was saved as: " ++ path ++ "-raw.csv"++"\n")
 outp "Toss in a little mysticism..."
 csvdata <- readFile (path ++ "-raw.csv")
 let lns = lines csvdata
 outp ((show (L.length lns))++" lines parsed from raw csv!\n")
 BL.writeFile (path ++ "-index.csv") (idxConstruct (L.tail lns))
 outp ("An index containing all record number/parent number/filename sets was saved as: " ++ path ++ "-index.csv\n")
 outp "Invoking the dark arts..."
 idxData <- readFile (path ++ "-index.csv")
 let newlns = lines idxData
 outp ("Building paths for "++(show (L.length newlns))++" file names.\n")
 BL.writeFile (path ++ "-index2.csv") (idxPaths (idxArray newlns) 1)
 outp "Process complete."

-------------------------------Build paths from index and update index--------------------------------------------------------------------------
idxArray :: [ByteString] ->  Array Int ByteString
idxArray byt = listArray (1,(L.length byt)) byt

idxPaths :: Array Int ByteString -> Int -> ByteString
idxPaths lst plc 
 | L.length lst == plc = empty
 | otherwise = append (snoc (append (lst ! plc) (cons ',' (idxBuildPath lst (idxParts (lst ! plc)))  )) '\n') (idxPaths lst (plc+1))

--idxPaths :: Array Int ByteString -> Int -> ByteString
--idxPaths lst plc 
-- | L.length lst == plc = empty
-- | otherwise = append (snoc (append (lst ! plc) (cons ',' (idxBuildPath (idxBuildFam lst [(idxParts (lst ! plc))]))  )) '\n') (idxPaths lst (plc+1))

idxBuildPath :: Array Int ByteString -> [ByteString] -> ByteString
idxBuildPath lst rcrd
 | (L.head rcrd) == ((!!) rcrd 1) = ((!!) rcrd 2)
 | otherwise = append (idxBuildPath lst z) (cons '\\' ((!!) rcrd 2))
 where z = idxParts (lst ! ((Prelude.read (unpack ((!!) rcrd 1)) :: Int )+1))
 
--idxBuildFam :: Array Int ByteString -> [[ByteString]] -> [[ByteString]]
--idxBuildFam lst rcrd
-- | (L.head (L.last rcrd)) == ((!!) (L.last rcrd) 1) = rcrd
-- | otherwise = idxBuildFam lst (rcrd ++ [z])
-- where z = idxParts (lst ! ((Prelude.read (unpack ((!!) (L.last rcrd) 1)) :: Int )+1))
 
--idxBuildPath :: [[ByteString]] -> ByteString
--idxBuildPath x
-- | L.length x == 1 = (!!) (L.head x) 2
-- | otherwise = append (idxBuildPath (L.tail x)) (cons '\\' ((!!) (L.head x) 2))

-------------------------------Build index file----------------------------------------------------------------------------------------
idxConstruct :: [ByteString] -> ByteString
idxConstruct bslist
 | L.length bslist == 0 = empty
 | otherwise = append (idxFNA (idxParts (L.head bslist))) (idxConstruct (L.tail bslist))

idxFNA :: [ByteString] -> ByteString
idxFNA rcrd
 | x > 33 = snoc (append y (idxFNAThree rcrd)) '\n'
 | x > 24 = snoc (append y (idxFNATwo rcrd)) '\n'
 | x > 15 = snoc (append y (append (snoc ((!!) rcrd 7) ',')  ((!!) rcrd 15))) '\n'
 | otherwise = pack "x,x,<orphan>\n"
 where x = L.length rcrd
       y = snoc ((!!) rcrd 1) ','

idxFNATwo :: [ByteString] -> ByteString
idxFNATwo rcrd
 | (BL.length ((!!) rcrd 15) > BL.length ((!!) rcrd 24)) = append (snoc ((!!) rcrd 7) ',')  ((!!) rcrd 15)
 | otherwise = append (snoc ((!!) rcrd 16) ',')  ((!!) rcrd 24)

idxFNAThree :: [ByteString] -> ByteString
idxFNAThree rcrd
 | first > second && first > third = append (snoc ((!!) rcrd 7) ',') ((!!) rcrd 15)
 | second > first && second > third = append (snoc ((!!) rcrd 16) ',')  ((!!) rcrd 24)
 | otherwise = append (snoc ((!!) rcrd 25) ',')  ((!!) rcrd 33)
 where first = BL.length ((!!) rcrd 15)
       second = BL.length ((!!) rcrd 24)
       third = BL.length ((!!) rcrd 33)

idxParts :: ByteString -> [ByteString]
idxParts str = splitWith (==',') str 
----------------------------------Parse the MFT------------------------------------------------------
csvHead :: Int64 -> ByteString
csvHead x = pack "Type,Record#,Sequence#,SIA-Created,SIA-File Modified,SIA-MFT Modified,SIA-Accessed,FNA-1-Parent#,FNA-1-Parent Sequence#,FNA-1-Created,FNA-1-File Modified,FNA-1-MFT Modified,FNA-1-Accessed,FNA-1-Size on Disk,FNA-1-Actual Size,FNA-1-File Name,FNA-2-Parent#,FNA-2-Parent Sequence#,FNA-2-Created,FNA-2-File Modified,FNA-2-MFT Modified,FNA-2-Accessed,FNA-2-Size on Disk,FNA-2-Actual Size,FNA-2-File Name,FNA-3-Parent#,FNA-3-Parent Sequence#,FNA-3-Created,FNA-3-File Modified,FNA-3-MFT Modified,FNA-3-Accessed,FNA-3-Size on Disk,FNA-3-Actual Size,FNA-3-File Name,\n"

--Breaks off 1024 byte chunks of the data, and parses them until there is none left
breakEntry :: ByteString -> ByteString
breakEntry entries
 | x > 1024 = append (parseEntry (BL.take 1024 entries)) (breakEntry (BL.drop 1024 entries))
 | x == 1024 = parseEntry entries
 | otherwise = pack "Error: Too few bits remaining to parse.\n"
 where x = BL.length entries
 
--MFT ENTRY INFO--
--Tests if a 1024 byte list is a valid file entry, then continues to parse or returns an error.
parseEntry :: ByteString -> ByteString
parseEntry entry
 | ((pack "FILE0") == (BL.take 5 entry)) && ((BL.length entry) == 1024) = snoc (append (append (append (snoc (parseState (BL.drop 22 entry)) ',') (snoc parsedRecordNumber ',')) (snoc parsedSequenceNumber ',')) (parseSia (BL.drop 56 entry))) '\n'
 | otherwise = pack "Error: Invalid or corrupt MFT entry.\n"
 where parsedSequenceNumber = pack (show (reverseEndianbytesToInt (BL.take 2 (BL.drop 16 entry))))
       parsedRecordNumber = pack (show (reverseEndianbytesToInt (BL.take 4 (BL.drop 44 entry))))

--Tests for file state and continues parsing or throws back an error.
parseState :: ByteString -> ByteString
parseState entry
 | x == pack ([chr(0x00),chr(0x00)] :: [Char]) = pack "{DEL} File"
 | x == pack ([chr(0x01),chr(0x00)] :: [Char]) = pack "File"
 | x == pack ([chr(0x04),chr(0x00)] :: [Char]) = pack "{DEL} Directory"
 | x == pack ([chr(0x03),chr(0x00)] :: [Char]) = pack "Directory"
 | otherwise = pack "Unknown"
 where x = BL.take 2 entry

--SIA ENTRY INFO--
--Parses the useful information from the SIA then move on to parsing the FNAs
-- entry = sia timestamps ( magic and sia content start) + parsefnas (magic and sia BL.length)
parseSia :: ByteString -> ByteString
parseSia entry
 | (BL.take 4 entry) == pack ([chr(0x10),chr(0x00),chr(0x00),chr(0x00)]) = append (siaTimeStamps (BL.take 32 (BL.drop (reverseEndianbytesToInt (BL.take 4 (BL.drop 20 entry))) entry))) (parseFNAs (BL.drop (reverseEndianbytesToInt (BL.take 2 (BL.drop 4 entry))) entry))
 | otherwise = pack "Error: Invalid or corrupt SIA."
 
--Parses the SIA timestamps in the order (Created-File modified-MFT Modified-File Accessed)
siaTimeStamps :: ByteString -> ByteString
siaTimeStamps macs
 | x > 8 = append (snoc (timestampToString (bytesToInt (reverse (BL.take 8 macs)))) ',') (siaTimeStamps (BL.drop 8 macs))
 | x == 8 = snoc (timestampToString (bytesToInt (reverse macs))) ','
 | otherwise = pack "Error: SIA timestamps are invalid or corrupt.,error,error,error,"
 where x = BL.length macs

--FNA ENTRY INFO--
--parseFNA = fnaContent(BL.drop (fna content start magic) (fna BL.length magic)) + filname(filename BL.length magic) + recurse parseFNA (fna BL.length magic)
parseFNAs :: ByteString -> ByteString
parseFNAs entry
 | (BL.take 4 entry) == (pack ([chr(0x30),chr(0x00),chr(0x00),chr(0x00)])) = append (append (fnaContent (BL.drop (reverseEndianbytesToInt (BL.take 1 (BL.drop 20 entry))) (BL.take (reverseEndianbytesToInt (BL.take 4 (BL.drop 4 entry))) entry))) (fnaFileName (BL.take ((bytesToInt (BL.take 1 (BL.drop 88 entry)))*2) (BL.drop 90 entry)))) (parseFNAs (BL.drop (reverseEndianbytesToInt (BL.take 4 (BL.drop 4 entry))) entry))
 | otherwise = empty

--fnaContent = parent record + parent sequence + fna timestamps + size on disk + actual size
fnaContent :: ByteString -> ByteString
fnaContent fna = append (append (append (append (snoc (fnaSizeConvert (BL.take 4 fna)) ',') (snoc (fnaSizeConvert (BL.take 2 (BL.drop 6 fna))) ',')) (fnaTimestamps (BL.take 32 (BL.drop 8 fna)))) (snoc (fnaSizeConvert (BL.take 8 (BL.drop 40 fna))) ',')) (snoc (fnaSizeConvert (BL.take 8 (BL.drop 48 fna))) ',')

--Used for FNA Size on Disk and FNA Actual size, parent record number, and parent sequence
fnaSizeConvert :: ByteString -> ByteString 
fnaSizeConvert x = pack (show (bytesToInt (reverse x)))

--Sets up the FNA timestamps
fnaTimestamps :: ByteString -> ByteString
fnaTimestamps macs
 | x > 8 = append (snoc (timestampToString (bytesToInt (reverse (BL.take 8 macs)))) ',') (fnaTimestamps (BL.drop 8 macs))
 | x == 8 = snoc (timestampToString (bytesToInt (reverse macs))) ','
 | otherwise = pack "Error: FNA timestamps are invalid or corrupt.,error,error,error,"
 where x = BL.length macs
 
--Converts the FNA filename Int64o a string
fnaFileName :: ByteString -> ByteString
fnaFileName name
 | BL.length name == 0 = snoc name ','
 | otherwise = append (fnaTestChars (reverse (BL.take 2 name))) (fnaFileName (BL.drop 2 name))

fnaTestChars :: ByteString -> ByteString
fnaTestChars x
 | bytesToInt x < 33 = (pack "\0\0")
 | otherwise = x
 
--Convert a Windows timestamp Int64 Int64o a string
--First converts the Windows time from nanoseconds to seconds, then subtracts the offset to make it a UNIX time.
timestampToString :: Int64 -> ByteString
timestampToString stamp = pack (formatTime defaultTimeLocale "%D %H:%M:%S %Z" (posixSecondsToUTCTime $ fromIntegral ((div stamp 10000000) - 11644473600)))

--Converts bytes to Int64 via bytestrings
bytesToInt :: ByteString -> Int64
bytesToInt x
 | y == 1 = fromIntegral (runGet getWord8 x)
 | y == 2 = fromIntegral (runGet getWord16be x)
 | y == 4 = fromIntegral (runGet getWord32be x)
 | y == 8 = fromIntegral (runGet getWord64be x)
 | otherwise = 0
 where y = BL.length x
 
--Reverses the endianness then converts bytes to Int64 for (sia BL.length, sia content start, )
reverseEndianbytesToInt :: ByteString -> Int64
reverseEndianbytesToInt x = bytesToInt (reverse x)

idxBytesToInts2 :: ByteString -> Int
idxBytesToInts2 x = Prelude.read (unpack x) :: Int

--Read from csv file
idxBytesToInts :: ByteString -> Int -> Int
idxBytesToInts x y
 | x == empty = 0
 | BL.length x == 1 = (y + (ord (BL.head x)) - 48)
 | otherwise = idxBytesToInts (BL.tail x) (y + (((ord (BL.head x)) - 48) * (10^(BL.length (BL.tail x)))))