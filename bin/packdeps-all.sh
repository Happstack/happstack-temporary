#!/bin/sh

# OLD = happstack-util happstack-data happstack-ixset happstack-state

for package in ixset happstack-server happstack-server-tls happstack happstack-hamlet happstack-heist happstack-hsp happstack-hstringtemplate happstack-plugins happstack-jmacro happstack-lite hsx-jmacro happstack-clientsession happstack-foundation happstack-authenticate
do
  packdeps $package/$package.cabal
done

