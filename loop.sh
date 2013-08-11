while [ true ]; do
    ./main.native -get_myproblems > problems-current.json
    ./main.native -myproblems problems-current.json -all 200
done
