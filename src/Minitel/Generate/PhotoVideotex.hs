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

-- | Picture identifier. The Minitel only supports PIJPEG.
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

-- | A picture entity end is a special case of a picture entity.
mEndPictureEntity :: PictureIdentifier -> MString
mEndPictureEntity pid = mPictureEntity pid PEDataLast []

-- | A picture control entity describes what the picture entity contains
mPictureControlEntity :: Int -> PictureIdentifier -> MString
mPictureControlEntity l pid = mPictureCodingDelimiter
                           ++ mCodingMethodIdentifier pid
                           ++ mLengthIndicator l

-- | Generate a picture coding delimiter.
mPictureCodingDelimiter :: MString
mPictureCodingDelimiter = [eESC, 0x70]

-- | Generate the coding method identifier given a picture identifier.
mCodingMethodIdentifier :: PictureIdentifier -> MString
mCodingMethodIdentifier pid = mPictureMode
                           ++ mPictureIdentifier pid

-- | Picture mode
mPictureMode :: MString
mPictureMode = [0x23]

-- | Generate an MString for a picture identifier. Minitel supports only PIJPEG.
mPictureIdentifier :: PictureIdentifier -> MString
mPictureIdentifier PIJPEG     = [0x40]
mPictureIdentifier PICCITTT4  = [0x41]
mPictureIdentifier PICCITTT6  = [0x42]
mPictureIdentifier PICCITTT82 = [0x43]

-- | Length indicator is the length of the picture data entity in bytes
mLengthIndicator :: Int -> MString
mLengthIndicator l = 0x7f : showLargeNumber l

-- | Generate a picture data entity for an MString
mPictureDataEntity :: PictureDataType -> MString -> MString
mPictureDataEntity PEHeader     d = 0x50 : d
mPictureDataEntity PEHeaderLast d = 0x51 : d
mPictureDataEntity PEData       d = 0x52 : d
mPictureDataEntity PEDataLast   d = 0x53 : d

-- | Return the width and height of a JPEG. In order to have the less
--   dependancies as possible, it directly looks for the 0xFFC0 JPEG tag
jpegDimension :: BS.ByteString -> (Int, Int)
jpegDimension jpg = (width, height)
    where startd = head $ BSS.indices (SBS.pack [0xFF, 0xC0]) jpg
          height = 256 * fromIntegral (BS.index jpg (startd + 5))
                 +       fromIntegral (BS.index jpg (startd + 6))
          width  = 256 * fromIntegral (BS.index jpg (startd + 7))
                 +       fromIntegral (BS.index jpg (startd + 8))

-- | Generate the MString used to display a grayscale JPEG at a specified
--   position on the Minitel screen. The Minitel understands standard JPEG as
--   long as they are preceded of special header. JPEG picture can only be sent
--   in 8 bits mode, it doesn’t work in the traditional 7 bits mode. JPEG is
--   given in the form of a ByteString.
mJPEG :: BS.ByteString -> Float -> Float -> MString
mJPEG jpg x y = mPictureEntity PIJPEG PEHeaderLast
                               (  mResetToDefault True
                               ++ mSourceComponentDescription ComponentY
                               ++ mPhotoAreaLocation x y
                               ++ mPhotoAreaSize (widthF / 320) (heightF / 320)
                               ++ mPicturePlacement 0 0 0 (heightF / 320)
                               )
             ++ mPictureEntity PIJPEG PEData
                               ((mnat . fromIntegral) <$> BS.unpack jpg)
             ++ mEndPictureEntity PIJPEG
    where (width, height)   = jpegDimension jpg
          (widthF, heightF) = (fromIntegral width, fromIntegral height)

