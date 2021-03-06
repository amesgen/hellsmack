{-# LANGUAGE CPP #-}

module HellSmack.Util.Meta
  ( name,
    version,
    OS (..),
    os,
    Arch (..),
    arch,
  )
where

import Data.Version (showVersion)
import Paths_hellsmack qualified as P

name :: Text
name = "hellsmack"

version :: Text
version = toText . showVersion $ P.version

data OS = Linux | Windows | OSX
  deriving stock (Show, Generic)

os :: OS

#ifdef OS_LINUX
os = Linux
#endif
#ifdef OS_OSX
os = OSX
#endif
#ifdef OS_WINDOWS
os = Windows
#endif

data Arch = X86 | X86_64
  deriving stock (Show, Generic)

arch :: Arch

#ifdef ARCH_X86
arch = X86
#endif
#ifdef ARCH_X86_64
arch = X86_64
#endif
