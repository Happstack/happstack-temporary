module Happstack.State.Tests.SaverProperties
    ( saverProperties
    ) where

import Happstack.State.Saver
import Happstack.State.Saver.Impl.Memory
import Happstack.State.Tests.Helpers
import Happstack.Util.Testing (qctest, qccheck)

import System.Directory
import System.Random
import System.FilePath
import System.IO.Unsafe
import Control.Monad
import Control.Exception
import Control.Concurrent
import Data.List

import Test.QuickCheck (Args(maxSize), (==>), stdArgs)
import Test.HUnit (Test(TestCase),(~:), assertFailure)

--------------------------------------------------------------
-- Tests
--------------------------------------------------------------

-- read (write values) == values
prop_getSetId withSaver key cutoff values
    = not (null key) ==>
      cutoff > 0 ==>
      ioProperty $ withSaver $ \saver ->
      do writer <- createWriter saver key cutoff
         forM_ values $ \value -> writerAdd writer (value::Int) $ return ()
         writerClose writer

         reader <- createReader saver key cutoff
         out <- readerGetUncut reader
         return $ values == out

prop_seqReadWrite withSaver (NonEmpty key) (Abs cutoff) valss
    = ioProperty $ withSaver $ \saver ->
      do writer <- createWriter saver key cutoff
         forM_ valss $ \vals -> do forM_ vals $ \val -> writerAdd writer (val::Int) $ return ()
                                   writerCut writer
         writerClose writer

         reader <- createReader saver key cutoff
         (out,newCut) <- readerGet reader
         return $ out == concat valss && newCut == length valss + 1

prop_cutDrop withSaver (NonEmpty key) (Abs cutoff) a b
    = ioProperty $ withSaver $ \saver ->
      do writer <- createWriter saver key cutoff
         forM_ a $ \val -> writerAdd writer (val::Int) $ return ()
         newCut <- writerCut writer
         forM_ b $ \val -> writerAdd writer (val::Int) $ return ()
         writerClose writer

         reader <- createReader saver key newCut
         (out,_) <- readerGet reader
         return $ b `isSuffixOf` out

prop_atomic withSaver (NonEmpty key) (Abs cutoff) value
    = ioProperty $ withSaver $ \saver ->
      do writer <- createWriter saver key cutoff
         writerAtomicReplace writer (value::Int)
         writerClose writer

         reader <- createReader saver key cutoff
         out <- readerGetUncut reader
         readerClose reader
         return $ out == [value]
{-
checkSaverProperties :: IO ()
checkSaverProperties
    = forEachSaver_ $ \name withSaver ->
      tryTests (name ++ " saver") args [run (prop_getSetId withSaver)
                                          ,run (prop_seqReadWrite withSaver)
                                          ,run (prop_cutDrop withSaver)
                                          ,run (prop_atomic withSaver)]
  where args = stdArgs{maxSize=5}
-}
saverProperties :: Test
saverProperties 
    = "saverProperties" ~:
       ((forEachSaver $ \name withSaver ->
             name ~:
              [ "prop_getSetId"     ~: qccheck args (prop_getSetId withSaver)
              , "prop_seqReadWrite" ~: qccheck args (prop_seqReadWrite withSaver)
              , "prop_cutDrop"      ~: qccheck args (prop_cutDrop withSaver)
              , "prop_atomic"       ~: qccheck args (prop_atomic withSaver)
              ]))
  where args = stdArgs{maxSize=5}
