{-|
Module      : PhotoVideotex
Description : PhotoVideotex generator
Copyright   : (c) Frédéric BISSON, 2014
License     : GPL-3
Maintainer  : zigazou@free.fr
Stability   : experimental
Portability : POSIX

This module allows to send photo videotex.
-}
module Minitel.Generate.PhotoVideotex ( mJPEG ) where

import           Minitel.Constants.Constants
import           Minitel.Type.MNatural
import           Minitel.Type.MString
import           Minitel.Type.MLargeNumber
import           Minitel.Generate.Photo.Parameter
import           Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Lazy.Search as BSS

data PictureIdentifier = PIJPEG
                       | PICCITTT4
                       | PICCITTT6
                       | PICCITTT82

-- | A picture entity can be a header or data
data PictureDataType = PEHeader
                     | PEHeaderLast
                     | PEData
                     | PEDataLast

-- | A picture is sent in many picture entities, first some headers then some
--   data.
mPictureEntity :: PictureIdentifier -> PictureDataType -> MString -> MString
mPictureEntity pid pdt d = mPictureControlEntity (length pde) pid
                        ++ pde
                        where pde = mPictureDataEntity pdt d

mEndPictureEntity :: PictureIdentifier -> MString
mEndPictureEntity pid = mPictureEntity pid PEDataLast []

-- | A picture control entity describes what the picture entity contains
mPictureControlEntity :: Int -> PictureIdentifier -> MString
mPictureControlEntity l pid = mPictureCodingDelimiter
                           ++ mCodingMethodIdentifier pid
                           ++ mLengthIndicator l

mPictureCodingDelimiter :: MString
mPictureCodingDelimiter = [eESC, 0x70]

mCodingMethodIdentifier :: PictureIdentifier -> MString
mCodingMethodIdentifier pid = mPictureMode
                           ++ mPictureIdentifier pid

mPictureMode :: MString
mPictureMode = [0x23]

mPictureIdentifier :: PictureIdentifier -> MString
mPictureIdentifier PIJPEG     = [0x40]
mPictureIdentifier PICCITTT4  = [0x41]
mPictureIdentifier PICCITTT6  = [0x42]
mPictureIdentifier PICCITTT82 = [0x43]

-- | Length indicator is the length of the picture data entity in bytes
mLengthIndicator :: Int -> MString
mLengthIndicator l = 0x7f : showLargeNumber l

mPictureDataEntity :: PictureDataType -> MString -> MString
mPictureDataEntity PEHeader     d = 0x50 : d
mPictureDataEntity PEHeaderLast d = 0x51 : d
mPictureDataEntity PEData       d = 0x52 : d
mPictureDataEntity PEDataLast   d = 0x53 : d

jpegDimension :: BS.ByteString -> (Int, Int)
jpegDimension jpg = (width, height)
    where startd = head $ BSS.indices (SBS.pack [0xFF, 0xC0]) jpg
          height = 256 * fromIntegral (BS.index jpg (startd + 5))
                 +       fromIntegral (BS.index jpg (startd + 6))
          width  = 256 * fromIntegral (BS.index jpg (startd + 7))
                 +       fromIntegral (BS.index jpg (startd + 8))

mJPEG :: BS.ByteString -> Float -> Float -> MString
mJPEG jpg x y = mPictureEntity PIJPEG PEHeaderLast
                               (  mResetToDefault True
                               ++ mSourceComponentDescription ComponentY
                               ++ mPhotoAreaLocation x y
                               ++ mPhotoAreaSize (widthF / 320) (heightF / 320)
                               ++ mPicturePlacement 0 0 0 (heightF / 320)
                               )
             ++ mPictureEntity PIJPEG PEData
                               ((mnat . fromIntegral) <$> (BS.unpack jpg))
             ++ mEndPictureEntity PIJPEG
    where (width, height)   = jpegDimension jpg
          (widthF, heightF) = (fromIntegral width, fromIntegral height)

