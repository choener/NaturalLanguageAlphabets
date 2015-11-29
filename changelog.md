0.1.0.0
-------

- moved IMMC to LinguisticsTypes library and renamed to BTI
- removed MultiChar type
- using HashMap from unordered-containers instead of HashTable from hashtables.
  Now we do not have to unsafePerformIO anymore.
- JSON (de)serialization for SimpleScoring scheme

0.0.2.0
-------

- internalisation was *not* thread-safe, now it is
- some property tests
- scoring file suffix is now .score

0.0.1.0
-------

- cleanup for GHC 7.10
- travis-ci integration

0.0.0.2
-------

- thanks to a new system-filepath, we now have Stringable instances
- NFData instances
- added a simple scoring system

0.0.0.1
-------

- initial checkin
- internable MultiChar characters

