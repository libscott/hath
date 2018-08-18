#!/bin/bash


tmux split-window -h \; split-window -h \; select-layout even-horizontal

tmux split-window -v -t 0
tmux split-window -v -t 1
tmux split-window -v -t 2

hath="stack exec hath --"

tmux send-keys -t 3 "$hath -- notarise seed" Enter
tmux send-keys -t 1 "$hath -- notarise ethkmd --config integration/hath-0.json" Enter
tmux send-keys -t 2 "$hath -- notarise ethkmd --config integration/hath-1.json" Enter
tmux send-keys -t 4 "$hath -- notarise ethkmd --config integration/hath-2.json" Enter
tmux send-keys -t 5 "$hath -- notarise ethkmd --config integration/hath-3.json" Enter

tmux select-pane -t 0

function ctrl_c() {
    for i in $(seq 5 -1 1); do
        tmux send-keys -t $i C-c C-c Enter "exit" Enter
    done
};

trap ctrl_c INT

sleep 100000000
