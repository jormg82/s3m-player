#/bin/sh

cabal -v0 run s3m-player -- music/MECH8.S3M | aplay -r 11025 -f u8 -c2 -t raw
#cabal -v0 run s3m-player -- music/test.s3m | aplay -r 11025 -f u8 -c2 -t raw
#cabal -v0 run s3m-player -- music/concha.s3m | aplay -r 11025 -f u8 -c2 -t raw
##cabal -v0 run s3m-player -- music/concha.s3m > out.bin
