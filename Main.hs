import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Builder as B
import Data.Foldable
import System.Process
import Text.Printf
import Data.List

type Pulse    = Float
type Seconds  = Float
type Samples  = Float
type Hz       = Float
type Semitone = Float
type Beat     = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.5

sampleRate :: Samples
sampleRate = 48000

pitchStandard :: Hz
pitchStandard = 440

bpm :: Beat
bpm = 120

beatDuration :: Seconds
beatDuration = 60.0 / bpm

f :: Semitone -> Hz
f x = pitchStandard * (2 ** (1.0 / 12.0)) ** x

note :: Semitone -> Seconds -> [Pulse]
note n beats = freq (f n) (beats * beatDuration)

freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
    map (* volume) $ zipWith3 (\x y z -> x * y * z) attack output release
    where
        step = (hz * 2 * pi) / sampleRate

        attack :: [Pulse]
        attack = map (min 1.0) [0.0, 0.001 ..]

        output :: [Pulse]
        output = map sin $ map (* step) [0.0..sampleRate * duration]

        release :: [Pulse]
        release = reverse $ take (length output) attack


wave :: [Pulse]
wave = concat [ note 0 0.5
              , note 0 0.5
              , note 0 0.5
              , note 0 0.5
              , note 3 1.0
              , note 3 0.5
              , note 3 0.5
              , note 3 0.5
              , note 3 0.5
              , note 7 1.0
              ]

save :: FilePath -> IO ()
save file = B.writeFile file $ B.toLazyByteString $ fold $ map B.floatLE wave


play :: IO ()
play = do
    save outputFilePath
    _ <- runCommand $ printf "ffplay -f f32le -ar %f %s" sampleRate outputFilePath
    return ()


