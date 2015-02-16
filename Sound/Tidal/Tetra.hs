  
module Sound.Tidal.Tetra where

import qualified Sound.ALSA.Sequencer.Address as Addr
import qualified Sound.ALSA.Sequencer.Client as Client
import qualified Sound.ALSA.Sequencer.Port as Port
import qualified Sound.ALSA.Sequencer.Event as Event
import qualified Sound.ALSA.Sequencer as SndSeq
import qualified Sound.ALSA.Exception as AlsaExc
import qualified Sound.ALSA.Sequencer.Connect as Connect
import GHC.Word
import GHC.Int

import Sound.OSC.FD
import qualified Data.Map as Map
import Control.Applicative
import Control.Concurrent.MVar
--import Visual
import Data.Hashable
import Data.Bits
import Data.Maybe
import System.Process
import Control.Concurrent

import Sound.Tidal.Stream
import Sound.Tidal.Pattern
import Sound.Tidal.Parse

channel = Event.Channel 0

keys :: OscShape
keys = OscShape {path = "/note",
                 params = [ I "note" Nothing,
                            F "dur" (Just (0.05)),
                            F "kcutoff" (Just (164)),
                            F "kresonance" (Just (-1)),
                            F "atk" (Just (-1)),
                            F "dcy" (Just (-1)),
                            F "sus" (Just (-1)),
                            F "rel" (Just (-1)),
                            F "fgain" (Just (-1)),
                            F "fvol" (Just (-1)),
                            F "audiomod" (Just (-1)),
                            F "kamt" (Just (-1)),
                            F "oscmix" (Just (-1)),
                            F "sub1vol" (Just (-1)),
                            F "sub2vol" (Just (-1)),
                            F "noise" (Just (-1)),
                            F "fpoles" (Just (-1))
                          ],
                 timestamp = NoStamp,
                 latency = 0,
                 namedParams = False,
                 preamble = []
                }

keyStream = stream "127.0.0.1" 7303 keys

note         = makeI keys "note"

dur = makeF keys "dur"

kcutoff      =  makeF keys "kcutoff"
kresonance      =  makeF keys "kresonance"
atk       = makeF keys "atk"
dcy        = makeF keys "dcy"
sus      = makeF keys "sus"
rel      = makeF keys "rel"
fgain        = makeF keys "fgain"
fvol        = makeF keys "fvol"

audiomod    = makeF keys "audiomod"
kamt      = makeF keys "kamt"

oscmix    = makeF keys "oscmix"
sub1vol   = makeF keys "sub1vol"
sub2vol   = makeF keys "sub2vol"
noise     = makeF keys "noise"


fpoles    = makeF keys "fpoles"
twopole = fpoles (p "0")
fourpole = fpoles (p "1")

-- dur          = makeF keys "dur"
-- portamento   = makeF keys "portamento"
-- expression   = makeF keys "expression"
-- octave       = makeF keys "octave"
-- voice        = makeF keys "voice"
-- detune       = makeF keys "detune"
-- vcoegint     = makeF keys "vcoegint"
-- kcutoff      = makeF keys "kcutoff"
-- vcfegint     = makeF keys "vcfegint"
-- lforate      = makeF keys "lforate"
-- lfopitchint  = makeF keys "lfopitchint"
-- lfocutoffint = makeF keys "lfocutoffint"

-- dtime        = makeF keys "dtime"
-- dfeedback    = makeF keys "dfeedback"

paramRanges = [
              164, -- Cutoff, in semitones
              127, -- resonance
              127, -- attack
              127, -- decay
              127, -- sustain
              127, -- release
              127, -- fgain
              127, -- fvol
              127, -- audiomod
              127, -- kamt
              127, -- oscmix
              127, -- sub1vol
              127, -- sub2vol
              127, -- noise
              1 -- fpoles
              ]

keynames = map name (tail $ tail $ params keys)

keyproxy latency midiport = 
   do h <- SndSeq.openDefault SndSeq.Block
      Client.setName (h :: SndSeq.T SndSeq.OutputMode) "Tidal"
      c <- Client.getId h
      p <- Port.createSimple h "out"
           (Port.caps [Port.capRead, Port.capSubsRead]) Port.typeMidiGeneric
      conn <- Connect.createTo h p =<< Addr.parse h midiport
      x <- udpServer "127.0.0.1" 7303
      forkIO $ loop h conn x
      return ()
         where loop h conn x = do m <- recvMessage x
                                  act h conn m
                                  loop h conn x
               act h conn (Just (Message "/note" (note:dur:ctrls))) = 
                   do -- print $ "Got note " ++ show note
                      let note' = (fromJust $ d_get note) :: Int
                          dur' = (fromJust $ d_get dur) :: Float
                          ctrls' = (map (fromJust . d_get) ctrls) :: [Float]
                      sendmidi latency h conn (fromIntegral note', dur') ctrls'
                      return ()


sendmidi latency h conn (note,dur) ctrls = 
  do forkIO $ do threadDelay latency
                 Event.outputDirect h $ noteOn conn note 60
                 threadDelay (floor $ 1000000 * dur)
                 Event.outputDirect h $ noteOff conn note
                 return ()
     let ctrls' = map floor (zipWith (*) paramRanges ctrls)
         ctrls'' = filter ((>=0) . snd) (zip keynames ctrls')
         --ctrls''' = map (\x -> (x, fromJust $ lookup x ctrls'')) usectrls
     -- putStrLn $ show ctrls''
     sequence_ $ map (\(name, ctrl) -> Event.outputDirect h $ makeCtrl conn (ctrlN name ctrl)) ctrls''
     return ()


ctrlN "kcutoff" v         = (15, v)
ctrlN "kresonance" v         = (16, v)

-- filter envelope
ctrlN "atk" v         = (23, v)
ctrlN "dcy" v         = (24, v)
ctrlN "sus" v         = (25, v)
ctrlN "rel" v         = (26, v)

ctrlN "fgain" v         = (110, v)
ctrlN "fvol" v         = (116, v)

ctrlN "audiomod" v         = (18, v)
ctrlN "kamt" v         = (17, v)

ctrlN "oscmix" v         = (13, v)
ctrlN "sub1vol" v         = (114, v)
ctrlN "sub2vol" v         = (115, v)
ctrlN "noise" v         = (14, v)

ctrlN "fpoles" v         = (19, v)


ctrlN s _             = error $ "no match for " ++ s


noteOn :: Connect.T -> Word8 -> Word8 -> Event.T
noteOn conn val vel = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOn
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.Velocity vel)

noteOff :: Connect.T -> Word8 -> Event.T
noteOff conn val = 
  Event.forConnection conn 
  $ Event.NoteEv Event.NoteOff
  $ Event.simpleNote channel
                     (Event.Pitch (val))
                     (Event.normalVelocity)

makeCtrl :: Connect.T -> (Word32, GHC.Int.Int32) -> Event.T
makeCtrl conn (c, n) = 
  Event.forConnection conn 
  $ Event.CtrlEv Event.NonRegParam $ Event.Ctrl 
                                    channel 
                                    (Event.Parameter c) 
                                    (Event.Value n)
