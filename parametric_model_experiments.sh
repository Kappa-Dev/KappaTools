#!/bin/bash

KASA_BINARY="./_build/install/default/bin/KaSa"
TEMP_FILE="temp_experiment.ka"

# models
ENSEMBLE_MODEL="examples/boolean_predicates/benchmarks/ensemble.ka"
EGFR_MODEL="examples/boolean_predicates/benchmarks/causality_05_sos.ka"
REPRESSILATOR_MODEL="examples/boolean_predicates/benchmarks/Repressilator.ka"
AVAILABLE_MODELS="Available models: ENSEMBLE, EGFR, REPRESSILATOR"

# Generate a new output file name (the stderr output of time)
mkdir -p output
OUTPUT_FILE="output/experiments_output"
i=0
while [ -e "$OUTPUT_FILE~$i.txt" ]; do
    i=$((i + 1))
done
OUTPUT_FILE="$OUTPUT_FILE~$i.txt"
#generate a new output file name for the stdout output of the analysis
STDOUT_OUTPUT_FILE="output/analysis_output"
i=0
while [ -e "$STDOUT_OUTPUT_FILE~$i.txt" ]; do
    i=$((i + 1))
done
STDOUT_OUTPUT_FILE="$STDOUT_OUTPUT_FILE~$i.txt"

# Check if a model argument is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <model>"
    echo "$AVAILABLE_MODELS"
    exit 1
fi

# Set the model based on the argument
case "${1^^}" in
    ENSEMBLE)
        EXAMPLE_MODEL="$ENSEMBLE_MODEL"
        ;;
    EGFR)
        EXAMPLE_MODEL="$EGFR_MODEL"
        ;;
    REPRESSILATOR)
        EXAMPLE_MODEL="$REPRESSILATOR_MODEL"
        ;;
    *)
        echo "Invalid model: $1"
        echo "$AVAILABLE_MODELS"
        exit 1
        ;;
esac

# Clear the output file
> "$OUTPUT_FILE"

echo "Starting the analysis..."

for i in {0..35..5}; do
    # If the current file contains the annotation //i for the current i, then the analysis is executed
    if grep -q "//$i" "$EXAMPLE_MODEL"; then
        # Replace "//i" with "]//i" in the input file and replace "// working_set" with "%working_set:["
        sed "s|// working_set|%working_set:[|g" "$EXAMPLE_MODEL" | sed "s|//$i|]|g" > "$TEMP_FILE"
        # normal run
        echo >> "$OUTPUT_FILE"
        echo "Runtime with $i rules in the working set:" >> "$OUTPUT_FILE"
        { time "$KASA_BINARY" "$TEMP_FILE"; } >> "$STDOUT_OUTPUT_FILE" 2>> "$OUTPUT_FILE"
        # without printing
        echo >> "$OUTPUT_FILE"
        echo "Runtime with $i rules and without printing:" >> "$OUTPUT_FILE"
        { time "$KASA_BINARY" "$TEMP_FILE" --verbosity-level-for-reachability-analysis Mute ; } >> "$STDOUT_OUTPUT_FILE" 2>> "$OUTPUT_FILE"
        # without the site-across-bonds analysis
        echo >> "$OUTPUT_FILE"
        echo "Runtime with $i rules, without the site-across-bonds analysis:" >> "$OUTPUT_FILE"
        { time "$KASA_BINARY" "$TEMP_FILE" --no-sites-across-bonds-domain ; } >> "$STDOUT_OUTPUT_FILE" 2>> "$OUTPUT_FILE"
    fi
done

# Clean up temporary file
rm -f "$TEMP_FILE"

echo "Analysis completed."
echo "Stderr output can be found in: $OUTPUT_FILE"
echo "Stdout output can be found in: $STDOUT_OUTPUT_FILE"

# Check for exceptions in STDOUT_OUTPUT_FILE
if grep -qi "exception:" "$STDOUT_OUTPUT_FILE"; then
    echo "Some exceptions have been raised."
else
    echo "Execution finished without any exception."
fi

# Check for exceptions in OUTPUT_FILE
if grep -qi "Killed" "$OUTPUT_FILE"; then
    echo "Some programs were killed (probably because they used too much memory)."
fi
