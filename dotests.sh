#!/bin/bash

OUTPUTDIR=autotest-results
REMOVEASM=yes

# 0: Only failures in single tests
# 1: + summaries
# 2: + successes
DEBUGLEVEL=1

# 0: No warnings
# 1: Output on stderr
# 2: + I/O sanity
# 3: + original I/O existence (.compilation, .input, .output)
WARNINGLEVEL=3

TB="\033[1m"
TN="\033[0m"
TR="\033[31m"
TG="\033[32m"
TY="\033[33m"
TW="\033[37m"


function ma_retvalok
{
	if [ "$1" = 0 -a "$2" != 0 ]
	then
		return 1
	else
		if [ "$1" != 0 -a "$2" = 0 ]
		then
			return 1
		fi
	fi
	
	return 0
}

function ma_testsingle
{
	TESTFAILED=0
	FULLPATH="$1"				# The *whole* path to the .alice file
	BASEPATH=${FULLPATH%.alice}	# The whole path without .alice
	BASENAME=${BASEPATH##*/}	# The source name without .alice
	FILEDIR=${FULLPATH%/*}		# The directory the .alice file is in
	
	# Run the compiler
	./compile "$FULLPATH" "$OUTPUTDIR"/"$BASENAME" > "$OUTPUTDIR"/"$BASENAME".tco 2> "$OUTPUTDIR"/"$BASENAME".tce
	echo $? > "$OUTPUTDIR"/"$BASENAME".tcr
	
	# Check compiler stderr
	if [ -s "$OUTPUTDIR"/"$BASENAME".tce ]
	then
		if [ "$(<"$OUTPUTDIR"/"$BASENAME".tcr)" = 0 ]
		then
			out_warn 1 "$BASENAME: Compiler stderr"
		#else
			#rm "$OUTPUTDIR"/"$BASENAME".tce
		fi
	else
		rm "$OUTPUTDIR"/"$BASENAME".tce
	fi
	
	# Check compiler stdout
	if [ "$(<"$OUTPUTDIR"/"$BASENAME".tco)" != "Done." -a "$(<"$OUTPUTDIR"/"$BASENAME".tco)" != "" ]
	then
		if [ "$(<"$OUTPUTDIR"/"$BASENAME".tcr)" = 0 ]
		then
			out_warn 1 "$BASENAME: Compiler stdout"
		#else
		#	rm "$OUTPUTDIR"/"$BASENAME".tco
		fi
	else
		rm "$OUTPUTDIR"/"$BASENAME".tco
	fi
	
	# Check compiler return value
	if [ -e "$BASEPATH.compilation" ]
	then
		CRV=$(<"$BASEPATH.compilation")
	else
		out_warn 3 "$BASENAME: No retval for compilation defined, assuming 0"
		CRV=0
	fi
	
	if ma_retvalok "$CRV" "$(<"$OUTPUTDIR"/"$BASENAME".tcr)"
	then
		out_ok 2 "$BASENAME: Compile retval"
		rm "$OUTPUTDIR"/"$BASENAME".tcr
		rm -f "$OUTPUTDIR/$BASENAME.compilation"
	else
		out_fail 0 "$BASENAME: Compile retval: $(<$OUTPUTDIR/$BASENAME.tcr) <> $CRV"
		if [ -e "$BASEPATH".compilation ]
		then
			cp "$BASEPATH".compilation "$OUTPUTDIR/"
		fi
		TESTFAILED=1
	fi
	
	# If an executable was produced
	if [ -x "$OUTPUTDIR"/"$BASENAME" -a $TESTFAILED = 0 ]
	then
		for tn in "$BASEPATH"*.retvalue
		do
			TESTNUM=${tn%.retvalue}
			TESTNUM=${TESTNUM##*.}
			
			# Prepare the command line to run the program
			COMMAND="$OUTPUTDIR/$BASENAME 2> $OUTPUTDIR/$BASENAME.$TESTNUM.te"
			
			# Connect output
			if [ -e "$BASEPATH.$TESTNUM.output" ]
			then
				COMMAND="$COMMAND > $OUTPUTDIR/$BASENAME.$TESTNUM.to"
			else
				out_warn 3 "$BASENAME: No .output"
				COMMAND="$COMMAND > /dev/null"
			fi
			
			# Connect input
			if [ -e "$BASEPATH.$TESTNUM.input" ]
			then
				COMMAND="$COMMAND < $BASEPATH.$TESTNUM.input"
			else
				out_warn 3 "$BASENAME: No .input"
				COMMAND="$COMMAND < /dev/null"
			fi
			
			# RUN
			eval "$COMMAND"
			echo $? > "$OUTPUTDIR/$BASENAME.$TESTNUM.tr"
			
			# Anything on stderr?
			if [ -s "$OUTPUTDIR"/"$BASENAME.$TESTNUM.tce" ]
			then
				out_warn 1 "$BASENAME-$TESTNUM: Running stderr"
			else
				rm "$OUTPUTDIR/$BASENAME.$TESTNUM.te"
			fi
			
			# Compare the return value
			if cmp "$BASEPATH.$TESTNUM.retvalue" "$OUTPUTDIR/$BASENAME.$TESTNUM.tr" > /dev/null
			then
				out_ok 2 "$BASENAME-$TESTNUM: Running retval"
				rm "$OUTPUTDIR/$BASENAME.$TESTNUM.tr"
				rm -f "$OUTPUTDIR/$BASENAME.$TESTNUM.retvalue"
			else
				out_fail 0 "$BASENAME-$TESTNUM: Running retval: $(<$OUTPUTDIR/$BASENAME.$TESTNUM.tr) <> $(<$BASEPATH.$TESTNUM.retvalue)"
				cp "$BASEPATH.$TESTNUM.retvalue" "$OUTPUTDIR/"
				TESTFAILED=1
			fi
			
			# Compare the output if any given
			if [ -e "$BASEPATH.$TESTNUM.output" ]
			then
				if cmp "$BASEPATH.$TESTNUM.output" "$OUTPUTDIR/$BASENAME.$TESTNUM.to" > /dev/null
				then
					out_ok 2 "$BASENAME-$TESTNUM: Running output"
					rm "$OUTPUTDIR/$BASENAME.$TESTNUM.to"
					rm -f "$OUTPUTDIR/$BASENAME.$TESTNUM.output"
				else
					out_fail 0 "$BASENAME-$TESTNUM: Running output"
					cp "$BASEPATH.$TESTNUM.output" "$OUTPUTDIR/"
					TESTFAILED=1
				fi
			else
				if [ -s "$OUTPUTDIR/$BASENAME.$TESTNUM.to" ]
				then
					out_warn 2 "$BASENAME-$TESTNUM: Output even though no output given"
				fi
			fi
		done
	fi
	
	# Remove the exe
	rm -f "$OUTPUTDIR/$BASENAME"
	
	if [ $TESTFAILED = 0 ]
	then
		out_ok 1 "$BASENAME"
		if [ "$REMOVEASM" = "yes" ]
		then
			rm -f "$OUTPUTDIR"/"$BASENAME".asm
		fi
		rm -f "$OUTPUTDIR/$BASENAME.alice"
	else
		#out_fail 1 "$BASENAME"
		cp "$FULLPATH" "$OUTPUTDIR/"
	fi
	return $TESTFAILED
}


# Output for OK messages
function out_ok
{
	if [ "$DEBUGLEVEL" -ge "$1" ]
	then
		echo -e "${TB}${TW}[${TG}  OK  ${TW}]${TN} $2"
	fi
}

# Output for FAILURE messages
function out_fail
{
	if [ "$DEBUGLEVEL" -ge "$1" ]
	then
		echo -e "${TB}${TW}[${TR}FAILED${TW}]${TN} $2"
	fi
}

# Output for WARNING messages
function out_warn
{
	if [ "$WARNINGLEVEL" -ge "$1" ]
	then
		echo -e "${TB}${TW}[${TY} WARN ${TW}]${TN} $2"
	fi
}


# Function for processing an entire folder of tests
function ma_testfolder
{
	BASEDIR="$1"
	
	for fn in "$BASEDIR"/*.alice
	do
		ma_testsingle "$fn"
	done
}


# The main "program"

if [ $# = 1 ]
then
	if [ -d "$1" ]
	then
		if [ ! -e "$OUTPUTDIR" ]
		then
			mkdir "$OUTPUTDIR"
		#else
			#rm -rf "$OUTPUTDIR"/*
		fi
		
		ma_testfolder "$1"
	else
		echo "Error: Not a valid/existing folder."
	fi
else
	echo "Usage: $0 <testfolder>"
fi
