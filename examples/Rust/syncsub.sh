#!/usr/bin/env bash
echo "Starting publisher..."
./target/release/syncpub &
echo "Starting subscribers..."
for ((a=0; a<10; a++)); do
    ./target/release/syncsub &
done
wait
