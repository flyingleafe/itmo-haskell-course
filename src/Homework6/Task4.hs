{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeFamilies              #-}
module Homework6.Task4
       ( runKernel
       , helloWorld
       , forkHelloWorld
       , earlyExit
       , yield
       ) where

import           Control.Lens        (Lens', lens, use, (%=), (^.))
import           Control.Monad.Cont
import           Control.Monad.State (State, execState)
import           Data.Void           (Void)
import           Prelude             hiding (read)

data KernelState = KernelState
  { _inputs  :: String      -- Wanna-be inputs from real world
  , _outputs :: String      -- The same for outputs
  , _running :: [Runnable]  -- Processes
  }

type Kernel = State KernelState

-- | `makeLenses` have difficulties with existential types
running :: Lens' KernelState [Runnable]
running = lens _running $ \ker r -> ker { _running = r }

inputs, outputs :: Lens' KernelState String
inputs = lens _inputs $ \ker ip -> ker { _inputs = ip }
outputs = lens _outputs $ \ker op -> ker { _outputs = op }

-- | Syscall - an action on outer world
class Syscall s where
  type SyscallArguments s :: *
  type SyscallResult s :: *

  kernelAction :: s -> SyscallArguments s -> Waiting (SyscallResult s) -> Kernel ()

-- | Continuation, which represents *computation until next syscall*
type Waiting r = r -> Paused

-- | Process awaiting for syscall return
data Paused = forall s . Syscall s => Paused
  { tag :: s
  , arg :: SyscallArguments s
  , cnt :: Waiting (SyscallResult s)
  }

-- | Process which is ready to run
data Runnable = forall r . Runnable
  { rcnt :: Waiting r
  , rarg :: r
  }

addRunnable :: Waiting r -> r -> Kernel ()
addRunnable w r = running %= (++ [Runnable w r])

-- | Computation with I/O to kernel
-- > OSIO r = Waiting (Waiting r) = (r -> Paused) -> Paused
type OSIO = Cont Paused

syscall :: Syscall s => s -> SyscallArguments s -> OSIO (SyscallResult s)
syscall tag args = cont $ \c -> Paused tag args c

-- | Syscalls definitions
data ExitStatus = Success
                | Fail

data ReadCall = ReadCall
data WriteCall = WriteCall
data ExitCall = ExitCall
data YieldCall = YieldCall
data ForkCall = ForkCall

instance Syscall ReadCall where
  type SyscallArguments ReadCall = Int
  type SyscallResult ReadCall = String

  kernelAction _ n cc =
    use inputs >>= \inp ->
    inputs %= drop n >>
    addRunnable cc (take n inp)

instance Syscall WriteCall where
  type SyscallArguments WriteCall = String
  type SyscallResult WriteCall = ()

  kernelAction _ s cc = outputs %= (++ s) >> addRunnable cc ()

instance Syscall ExitCall where
  type SyscallArguments ExitCall = ExitStatus
  type SyscallResult ExitCall = Void

  kernelAction _ _ _ = pure ()

instance Syscall YieldCall where
  type SyscallArguments YieldCall = ()
  type SyscallResult YieldCall = ()

  kernelAction _ _ cc = addRunnable cc ()

instance Syscall ForkCall where
  type SyscallArguments ForkCall = ()
  type SyscallResult ForkCall = ()

  kernelAction _ _ cc = addRunnable cc () >> addRunnable cc ()

-- | Main kernel process
kernel :: Kernel ()
kernel = use running >>= \runs ->
  case runs of
    [] -> pure ()
    (Runnable {..} : _) ->
      case rcnt rarg of
        Paused {..} ->
          kernelAction tag arg cnt >>
          running %= tail >>
          kernel

-- | Convenience runner
runKernel :: OSIO Void -> String -> String
runKernel process input = execState kernel initState ^. outputs
  where initRuns = [Runnable process' undefined]
        initState = KernelState
                    { _running = initRuns
                    , _inputs = input
                    , _outputs = ""
                    }
        process' = runCont process

-- | Syscall aliases
read :: Int -> OSIO String
read = syscall ReadCall

write :: String -> OSIO ()
write = syscall WriteCall

exit :: ExitStatus -> OSIO Void
exit = syscall ExitCall

yield :: OSIO ()
yield = syscall YieldCall ()

fork :: OSIO ()
fork = syscall ForkCall ()

-- | "Userspace"

-- I don't want to bother with buffered IO, so it's
-- terribly unefficient readLine
readLine :: OSIO String
readLine = read 1 >>= \s ->
  if null s || s == "\n"
  then pure ""
  else (head s :) <$> readLine

writeLine :: String -> OSIO ()
writeLine s = write $ s ++ "\n"

helloWorld :: OSIO Void
helloWorld = readLine >>= \s ->
  let str = "Hello, " ++ s ++ "!"
  in writeLine str >>
     exit Success

forkHelloWorld :: OSIO Void
forkHelloWorld = fork >> helloWorld

earlyExit :: OSIO Void
earlyExit = writeLine "Oops, abort!" >>
            exit Fail >>
            helloWorld
