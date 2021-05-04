
-- We define a complete 'class-hierarchy' of errors using the mechanisms from
-- Control.Exception. This is so that different OS-implementations can define
-- their own errors and hook them into the system so that we can handle errors
-- by their type.
--
-- This is arguably very over-engineered, because we already know we want to try
-- to recover from certain errors, but not others. However, I keep oscillating
-- between concrete errors or a hierarchy, and I'm just losing time. I am doing
-- it this way now, partly as an exercise, partly as 'the most flexible'
-- approach.
--
-- See the linux key-io modules to see how these errors are used.

-- base kio error

data SomeKioExc = forall e. Exception e => SomeKioExc e

instance Show SomeKioExc where show (SomeKioExc e) = show e

instance Exception SomeKioExc

kioException :: Exception e => Prism' SomeException e
kioException = prism' to from where
  to = toException . SomeKioExc
  from se = fromException se >>= \(SomeKioExc e) -> cast e

-- acquire key-io error

data SomeAcquireExc = forall e. Exception e => SomeAcquireExc e

instance Show SomeAcquireExc where show (SomeAcquireExc e) = show e

instance Exception SomeAcquireExc where
  toException   = review  kioException
  fromException = preview kioException

acqException :: Exception e => Prism' SomeException e
acqException = prism' to from where
  to = toException . SomeAcquireExc
  from se = fromException se >>= \(SomeAcquireExc e) -> cast e

-- release key-io error

data SomeReleaseExc = forall e. Exception e => SomeReleaseExc e

instance Show SomeReleaseExc where show (SomeReleaseExc e) = show e

instance Exception SomeReleaseExc where
  toException   = review  kioException
  fromException = preview kioException

relException :: Exception e => Prism' SomeException e
relException = prism' to from where
  to = toException . SomeReleaseExc
  from se = fromException se >>= \(SomeReleaseExc e) -> cast e

-- encode key-io error

data SomeEncodeExc = forall e. Exception e => SomeEncodeExc e

instance Show SomeEncodeExc where show (SomeEncodeExc e) = show e

instance Exception SomeEncodeExc where
  toException   = review  kioException
  fromException = preview kioException

encodeException :: Exception e => Prism' SomeException e
encodeException = prism' to from where
  to = toException . SomeEncodeExc
  from se = fromException se >>= \(SomeEncodeExc e) -> cast e

-- decode key-io error

data SomeDecodeExc = forall e. Exception e => SomeDecodeExc e

instance Show SomeDecodeExc where show (SomeDecodeExc e) = show e

instance Exception SomeDecodeExc where
  toException   = review  kioException
  fromException = preview kioException

decodeException :: Exception e => Prism' SomeException e
decodeException = prism' to from where
  to = toException . SomeDecodeExc
  from se = fromException se >>= \(SomeDecodeExc e) -> cast e
